require './c64asm'
# require './c64patch'
require "./binwriter"
require "./rom"
require "./midireader"

CUR_SONG = 0x02
SONG_COUNT = 0x07

out_file = 'mush.nes'

# Enabling SPUDD mode will cause this behavior:
# https://imgur.com/a/FP1TH
SPUDD_MODE = false

NTBL = {
  cf: -1,
  c: 0,
  cs: 1,
  df: 1,
  d: 2,
  ds: 3,
  ef: 3,
  e: 4,
  ff: 4,
  es: 5,
  f: 5,
  fs: 6,
  gf: 6,
  g: 7,
  gs: 8,
  af: 8,
  a: 9,
  as: 10,
  bf: 10,
  b: 11,
  bs: 12
}

PPQN = 48.0

LTBL = [
  1, 2, 3, 4, 6, 8, 12, 16, 24, 48, 49, 50, 9
]

LTBLS = [
  [144, 72, 48, 36, 24, 18, 12, 9, 6, 3, 2, 1, 27, 1, 1, 1],
  [96, 48, 32, 24, 16, 12, 8, 6, 4, 2, 1, 1, 1, 1, 1, 1],
  [48, 24, 16, 12, 8, 6, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1]
]

def eot
  0
end

def lp
  0xFF
end

def len length
  LTBL.index(length) + 1
end

def inst num
  0x10 + num
end

NOISE_NOTES = [:n0, :n1, :n2, :n3]

def n notename
  if NOISE_NOTES.include?(notename)
    0x80 + NOISE_NOTES.index(notename)
  else
    nn = /([a-g][sf]?)([0-9])/.match(notename)
    [[0, ((nn[2].to_i || 0) * 12) + (NTBL[nn[1].to_sym] || 0)].max, 59].min + 0x80
  end
end
def r
  0xC9
end
def h
  0xC8
end

def cvt_midi_file(fn, trk_type, tracks, pulse, division, lpc, flen)
  mf = parse_midi fn
  evts = []
  tracks.each do |t, inst, note_choice, trans|
    trans = -36 unless trans
    ee = mf[:tracks][t].select { |a, b, c| c == :note_on }.map do |ts, type, subtype, ch, notenum, velo|
      {
        ts: (ts.to_f / pulse.to_f).to_i,
        inst: inst,
        nc: note_choice,
        note: notenum + ( trk_type == :noise ? 0 : trans )
      }
    end
    evts += ee
  end
  evts.sort! { |a, b| [a[:ts], a[:note]] <=> [b[:ts], b[:note]] }
  tsg = evts.group_by { |x| x[:ts] }
  evts = []
  tsg.each do |k, evs|
    nc = evs.first[:nc]
    if nc
      if nc == :high
        evts << evs.last
      elsif nc == :low && evs.size > 1
        evts << evs.first
      end
    else
      evts << evs.first
    end
  end

  fevts = evts.group_by { |x| x[:ts] }

  data = [len(division)]
  linst = -1
  flen.times do |tick|
    # set loop point
    data << lp if tick == lpc

    if ev = fevts[tick]
      ev = ev.first
      if linst != ev[:inst] && trk_type != :noise
        data << inst(ev[:inst])
        linst = ev[:inst]
      end

      if trk_type == :noise
        instmap = ev[:inst] || {}
        nnote = instmap[ev[:note]]
        puts "missing notemap for #{ev[:note]}" unless nnote
        nnote ||= 0
        data << (0x80 + nnote)
      else
        data << (0x80 + ev[:note])
      end
    else
      data << r
    end
  end
  data << eot
  data
end


def freq_to_period hz
  (111860.8 / hz.to_f - 1.0).round.to_i
end

def midi_note_to_freq note
  440.0 * 2.0 ** ((note.to_f - 69) / 12.0)
end

def init_rom out_file
  BinWriter.open out_file do |f|
    # NES Header
    f.write_bin 'NES'
    f.write_byte 0x1A
    f.write_byte 1 # prg pages
    f.write_byte 0 # chr pages
    f.write_byte 0 # control 1
    f.write_byte 0 # control 2
    f.write_byte 0
    f.write_byte 0
    f.write_byte 0 # other stuff
    f.write_byte 0
    f.write_byte 0
    f.write_byte 0
    f.write_byte 0
    f.write_byte 0
    f.write_bin rom_fill = (0xFF.chr) * 0x4000
  end
end


def patch_ram bank, addr, descriptor=nil, vars={}, &block
  macro = C64Asm::Macro.new(vars) do
    align addr
  end
  macro.add_code(&block)
  code = macro.call
  prg_data = code.to_binary
  puts code.dump
  [prg_data.size - 2, code.labels]
end

def patch_rom out_file, bank, addr, descriptor=nil, vars={}, &block
  macro = C64Asm::Macro.new(vars) do
    align addr
  end
  macro.add_code(&block)
  code = macro.call

  write_addr = 0x10 + (bank * 0x4000) + (addr % 0x4000)

  prg_data = code.to_binary
  patch_data = prg_data[2..(prg_data.size)]

  puts "Patching #{patch_data.size} bytes to #{bank.to_s(16)}:#{addr.to_s(16)} (#{write_addr.to_s(16)}) #{descriptor}"
  puts code.dump                  # print detailed

  rom = ROM.from_file out_file
  all_data = rom.read_bin 0x4010
  BinWriter.open out_file do |f|
    f.write_bin all_data
    f.seek write_addr
    f.write_bin patch_data
  end

  [patch_data.size - 2, code.labels]
end

def patch_raw out_file, descriptor=nil, &block
  rom = ROM.from_file out_file
  puts "Patching raw #{descriptor}"
  rom = ROM.from_file out_file
  all_data = rom.read_bin 0x4010
  BinWriter.open out_file do |f|
    f.write_bin all_data
    block.call rom, f
  end
  puts "----"
end


init_rom out_file

PRG_MAIN = 0xC000

ram_size, ram_labels = patch_ram 0, 0x0000, :ram do
  label :var_sleeping
  data [0x00]

  label :var_speed
  data [0x00]

  label :var_songqueue
  data [0x00]

  label :var_songcur
  data [0x00]

  label :var_songptr
  data [0x00, 0x00]

  label :var_curtrk
  data [0x00]

  label :var_0007
  data [0x00]

  label :var_jtpointer
  data [0x00, 0x00]

  align 0x10

  label :var_seqptr
  data [0x00, 0x00]
  data [0x00, 0x00]
  data [0x00, 0x00]
  data [0x00, 0x00]

  label :var_lpptr
  data [0x00, 0x00]
  data [0x00, 0x00]
  data [0x00, 0x00]
  data [0x00, 0x00]

  label :var_wait
  data [0x00]
  label :var_len
  data [0x00, 0x00, 0x00]
  data [0x00, 0x00, 0x00, 0x00]

  label :var_sq_env
  data [0x00]
  label :var_sq_lc
  data [0x00, 0x00, 0x00]
  data [0x00, 0x00, 0x00, 0x00]

  if SPUDD_MODE
    label :var_spudd_note
    data [0x00]
  end
end

prg_size, prg_labels = patch_rom out_file, 0, PRG_MAIN, :main, ram_labels do
  label :load_nsf
  label :main
  # disable IRQs
  sei

  # disable decimal
  cld

  # disable APU frame IRQ
  ldx.d 0x40
  stx.a 0x4017

  # Set stack pointer
  ldx.d 0xFF
  txs

  inx # 00
  stx.a 0x2000 # disable NMI
  stx.a 0x2001 # disable rendering
  stx.a 0x4010 # disable DMC IRQs
  bit.a 0x2002

  # wait first vblank for PPU init
  label :vblankwait1

  bit.a 0x2002
  bpl vblankwait1

  # clear memory
  # set $0000-$01FF, $0300-$07FF to #$00
  # set $0200-$02FF to #$FE

  label :clrmem

  lda.d 0x00

  sta.zx 0x00

  [1,3,4,5,6,7].each do |num|
    sta.ax num * 0x0100
  end

  lda.d 0xFE
  sta.ax 0x200

  inx
  bne clrmem

  # wait for second vblank
  label :vblankwait2

  bit.a 0x2002
  bpl vblankwait2

  # init APU
  label :init_apu

  ldy.d 0x13

  label :rloop

  lda.ay regs
  sta.ay 0x4000
  dey
  bpl rloop

  lda.d 0x0F
  sta.a 0x4015
  lda.d 0x40
  sta.a 0x4017

  # enable NMI interrupt
  lda.d 0x80
  sta 0x2000

  # load song
  lda.d CUR_SONG
  sta.z var_songqueue

  # loop forever
  label :inf

  inc.z var_sleeping
  label :wait_for_vblank
  lda.z var_sleeping
  bne wait_for_vblank

  jsr sound

  label :inf_end
  jmp inf

  label :init_nsf
  tay
  iny
  sty.z var_songqueue
  rts

  label :play_nsf

  label :sound

  lda.z var_songqueue
  beq cursongplay

  cmp.d 0xFF
  bne load_new_song


  lda.d 0x00
  sta.z var_songcur
  sta.z var_songqueue

  jmp :sound_end

  label :load_new_song
  sta.z var_songcur
  asl
  tax

  lda.ax sequences
  sta.z var_songptr
  lda.ax sequences, 1
  sta.z var_songptr + 1

  # initiate track pointers
  ldy.d 0x7
  ldx.d 0x7

  label :trkptrcpy
  lda.zyr var_songptr
  sta.zx var_seqptr
  dex
  dey

  bpl trkptrcpy

  lda.d 0x00
  sta.z var_wait
  sta.z var_songqueue
  jmp proc_song

  label :cursongplay

  lda.z var_songcur

  bne proc_song

  jmp sound_end

  label :proc_song

  lda.d 0x06
  sta.z var_curtrk

  label :trk_proc

  ldx.z var_curtrk
  lda.zx var_seqptr
  bne okproc
  lda.zx var_seqptr + 1
  bne okproc

  jmp trk_end

  label :okproc

  lda.zx var_wait
  beq read_seq
  jmp sound_wait

  label :read_seq
  lda.zxr var_seqptr
  bne not_end

  lda.zx var_lpptr
  sta.zx var_seqptr
  lda.zx var_lpptr + 1
  sta.zx var_seqptr + 1
  jmp read_seq

  label :not_end

  cmp.d 0xFF
  bne not_loop_marker

  lda.zx var_seqptr
  clc
  adc.d 0x01

  sta.zx var_lpptr

  lda.zx var_seqptr + 1
  adc.d 0x00
  sta.zx var_lpptr + 1
  jmp next_read_seq

  label :not_loop_marker
  cmp.d 0x80
  bcs read_note

  cmp.d 0x10
  bcs inst_reading

  # length reading
  tay
  dey

  if SPUDD_MODE
    lda.d 0x0
    sta.z var_spudd_note
  end

  tya
  sta.zx var_len
  lda.z var_speed
  asl
  asl
  asl
  asl
  clc
  adc.zx var_len
  tax
  lda.ax len_table
  tax
  dex
  txa
  ldx.z var_curtrk
  sta.zx var_len
  jmp next_read_seq

  label :inst_reading
  cpx.d 0x06
  bcs not_sq

  ane.d 0xF
  pha
  tax
  lda.ax sq_env_table
  ldx.z var_curtrk
  sta.zx var_sq_env
  pla
  tax
  lda.ax sq_lc_table
  ldx.z var_curtrk
  sta.zx var_sq_lc

  label :not_sq
  jmp next_read_seq

  label :read_note

  if SPUDD_MODE
    ldx.z var_curtrk
    cpx.d 0x00
    bne :cont_read_note
    tax
    lda.z var_spudd_note
    bmi :cont_read_note
    txa
    sta.z var_spudd_note
    label :cont_read_note
  end

  ane.d 0x7F

  cmp.d 0x49
  bne keep_reading_note

  jmp :the_wait

  label :keep_reading_note

  # blah
  pha
  ldx.z var_curtrk

  lda.ax jmp_table
  sta.z var_jtpointer
  lda.ax jmp_table, 1
  sta.z var_jtpointer + 1

  jmp.ar var_jtpointer

  label :jmp_table
  data sq1
  data sq2
  data tri
  data noi

  label :sq1

  pla

  asl
  tax
  lda.ax freq_table
  sta.a 0x4002

  lda.ax freq_table, 1
  ldx.z var_curtrk
  clc
  adc.zx var_sq_lc
  sta.a 0x4003

  lda.zx var_sq_env
  sta.a 0x4000

  jmp the_wait

  label :sq2

  pla

  asl
  tax
  lda.ax freq_table
  sta.a 0x4006

  lda.ax freq_table, 1
  ldx.z var_curtrk
  clc
  adc.zx var_sq_lc
  sta.a 0x4007

  lda.zx var_sq_env
  sta.a 0x4004

  jmp the_wait

  label :tri
  pla

  asl
  tax
  lda.ax freq_table
  sta.a 0x400A

  lda.ax freq_table, 1
  ldx.z var_curtrk
  clc
  adc.zx var_sq_lc
  sta.a 0x400B

  lda.zx var_sq_env
  sta.a 0x4008

  jmp the_wait

  label :noi
  pla

  tax
  lda.ax :noi_prd_table
  sta.a 0x400E
  lda.ax :noi_lc_table
  sta.a 0x400F
  lda.ax :noi_env_table
  sta.a 0x400C

  # jmp the_wait

  label :the_wait
  ldx.z var_curtrk
  lda.zx var_len
  sta.zx var_wait

  jsr inc_seq_ptr

  jmp trk_end

  label :next_read_seq
  jsr inc_seq_ptr
  jmp read_seq

  label :sound_wait
  ldx.z var_curtrk
  dec.zx var_wait
  label :trk_end

  dec.z var_curtrk
  dec.z var_curtrk
  bmi sound_end

  jmp trk_proc

  label :sound_end
  rts

  #####

  label :inc_seq_ptr

  ldx.z var_curtrk
  lda.zx var_seqptr
  clc
  adc.d 0x01
  sta.zx var_seqptr
  lda.zx var_seqptr + 1
  adc.d 0x00
  sta.zx var_seqptr + 1

  rts

  label :other_vec
  rti

  label :nmi_handler
  pha
  txa
  pha
  tya
  pha

  lda.d 0x00
  sta.z var_sleeping

  pla
  tay
  pla
  tax
  pla
  rti

  label :regs
  data [0x30, 0x08, 0x00, 0x00]
  data [0x30, 0x08, 0x00, 0x00]
  data [0x80, 0x00, 0x00, 0x00]
  data [0x30, 0x00, 0x00, 0x00]
  data [0x00, 0x00, 0x00, 0x00]

  label :freq_table
  (36...96).each do |note|
    period = freq_to_period midi_note_to_freq note
    data [period], false, :word
  end

  label :len_table
  LTBLS.each do |tbl|
    data tbl
  end

  label :sq_env_table
  data [0x43, 0x83, 0x45, 0x03, 0x85, 0x00, 0x00, 0x00]
  data [0x60, 0x60]

  label :sq_lc_table
  data [0xF8, 0xF8, 0xF8, 0xF8, 0xF8, 0x00, 0x00, 0x00]
  data [0x88, 0xF8]

  label :noi_env_table
  data [0x00, 0x00, 0x01, 0x00]

  label :noi_lc_table
  data [0xF8, 0xF8, 0xF8, 0xF8]

  label :noi_prd_table
  data [0x0D, 0x06, 0x09, 0x04]

  label :sequences
  data [SONG_COUNT], false, :word
  data song1
  data song5
  data song2
  data song2a
  data song3
  data song4
  data song6

  label :song1
  data song1_trk1
  data song1_trk2
  data song1_trk3
  data song1_trk4

  label :song1_trk1
  data [inst(1)]
  data [len(8)]
  data [r, n(:a2), n(:bf2), n(:b2), len(4), n(:c3), n(:f2)]

  data [lp]
  data [inst(1)]
  data [len(16)]
  data [n(:d3), r, n(:c3), n(:bf2), r, n(:a2), n(:g2), n(:f2)]
  data [n(:g2), r, n(:a2), n(:bf2), len(4), r]
  data [len(16)]
  data [n(:a2), r, n(:c3), n(:a2), r, n(:f2), n(:g2), n(:f2)]
  data [n(:g2), r, n(:a2), n(:bf2), len(4), r]
  data [len(16)]
  data [n(:d3), r, n(:c3), n(:bf2), r, n(:a2), n(:g2), n(:f2)]
  data [n(:g2), r, n(:a2), n(:bf2), len(4), r]
  data [len(16)]
  data [n(:a2), r, n(:c3), n(:a2), r, n(:f2), n(:g2), n(:f2)]
  data [n(:d3), r, n(:c3), n(:bf2), len(4), r]

  data [len(9), n(:g2), len(16), n(:bf2), len(9), n(:f2), len(16), n(:bf2)]
  data [len(8), n(:g2), n(:a2), n(:bf2), r]
  data [len(9), n(:g2), len(16), n(:bf2), len(9), n(:f2), len(16), n(:bf2)]
  data [len(16), n(:d3), r, n(:c3), n(:bf2), len(4), r]
  data [len(9), n(:g2), len(16), n(:bf2), len(9), n(:f2), len(16), n(:bf2)]
  data [len(8), n(:g2), n(:a2), n(:bf2), r]
  data [len(16)]
  data [n(:a2), n(:a2), n(:a2), n(:a2), n(:a2), n(:a2), n(:a2), n(:a2)]
  data [len(8)]
  data [n(:g2), n(:bf2), n(:a2), n(:c3)]

  data [len(16)]
  data [n(:d3), r, n(:c3), n(:bf2), r, n(:a2), n(:g2), n(:f2)]
  data [n(:g2), r, n(:a2), n(:bf2), len(4), r]
  data [len(16)]
  data [n(:a2), r, n(:c3), n(:a2), r, n(:f2), n(:g2), n(:f2)]
  data [n(:g2), r, n(:a2), n(:bf2), len(4), r]
  data [len(16)]
  data [n(:d3), r, n(:c3), n(:bf2), r, n(:a2), n(:g2), n(:f2)]
  data [n(:g2), r, n(:a2), n(:bf2), len(4), r]
  data [len(16)]
  data [n(:a2), r, n(:c3), n(:a2), r, n(:f2), n(:g2), n(:f2)]
  data [len(8)]
  data [n(:d3), n(:c3), n(:bf2), r]

  data [inst(0)]
  data [len(16)]
  data [n(:g2), n(:f2), r, n(:g2), n(:f2), r, n(:g2), n(:f2), r, n(:g2), n(:d2), n(:ef2), len(8), n(:f2), n(:g2)]
  data [len(16)]
  data [n(:f2), n(:ef2), r, n(:f2), n(:ef2), r, n(:f2), n(:ef2), r, n(:f2), n(:c2), n(:d2), len(8), n(:ef2), n(:f2)]
  data [len(16)]
  data [n(:g2), n(:f2), r, n(:g2), n(:f2), r, n(:g2), n(:f2), r, n(:g2), len(8), n(:f2), n(:bf2), n(:d3)]
  data [len(8)]
  data [n(:d3), n(:c3), n(:g2), n(:a2), len(2), n(:bf2)]

  data [len(16)]
  data [r, n(:gf2), n(:g2), n(:bf2), len(8), n(:f2), n(:bf2)]
  data [len(16)]
  data [r, n(:gf2), n(:g2), n(:bf2), len(8), n(:f2), n(:bf2)]
  data [len(16)]
  data [r, n(:gf2), n(:g2), n(:bf2), len(8), n(:f2), n(:bf2)]
  data [len(16)]
  data [n(:d3), n(:ef3), n(:d3), n(:c3), len(4), n(:bf2)]
  data [len(16)]
  data [r, n(:gf2), n(:g2), n(:bf2), len(8), n(:f2), n(:bf2)]
  data [len(16)]
  data [r, n(:gf2), n(:g2), n(:bf2), len(8), n(:f2), n(:bf2)]
  data [len(16)]
  data [r, n(:a2), r, n(:a2), n(:a2), r, n(:a2), n(:a2)]
  data [len(8)]
  data [n(:g2), n(:bf2), n(:c3), r]
  data [eot]

  label :song1_trk2
  data [inst(1)]
  data [len(8)]
  data [r, n(:f2), n(:g2), n(:af2), len(4), n(:a2), n(:a1)]

  data [lp]
  data [inst(1)]
  data [len(16)]
  data [n(:f2), r, n(:ef2), n(:d2), r, n(:c2), n(:bf1), n(:a1)]
  data [n(:bf1), r, n(:c2), n(:d2), len(4), r]
  data [len(16)]
  data [n(:c2), r, n(:ef2), n(:c2), r, n(:a1), n(:bf1), n(:a1)]
  data [n(:bf1), r, n(:c2), n(:d2), len(4), r]
  data [len(16)]
  data [n(:f2), r, n(:ef2), n(:d2), r, n(:c2), n(:bf1), n(:a1)]
  data [n(:bf1), r, n(:c2), n(:d2), len(4), r]
  data [len(16)]
  data [n(:c2), r, n(:ef2), n(:c2), r, n(:a1), n(:bf1), n(:a1)]
  data [n(:f2), r, n(:ef2), n(:d2), len(4), r]

  data [len(16)]
  data [n(:bf1), r, r, n(:ef2), n(:bf1), r, r, n(:d2)]
  data [len(8)]
  data [n(:c2), n(:f2), n(:d2), r]
  data [len(16)]
  data [n(:bf1), r, r, n(:ef2), n(:bf1), r, r, n(:d2)]
  data [n(:f2), r, n(:ef2), n(:d2), len(4), r]
  data [len(16)]
  data [n(:bf1), r, r, n(:ef2), n(:bf1), r, r, n(:d2)]
  data [len(8)]
  data [n(:c2), n(:f2), n(:d2), r]
  data [len(16)]
  data [n(:c2), n(:c2), n(:c2), n(:c2), n(:c2), n(:c2), n(:c2), n(:c2)]
  data [len(8)]
  data [n(:bf1), n(:d2), n(:c2), n(:ef2)]

  data [len(16)]
  data [n(:f2), r, n(:ef2), n(:d2), r, n(:c2), n(:bf1), n(:a1)]
  data [n(:bf1), r, n(:c2), n(:d2), len(4), r]
  data [len(16)]
  data [n(:c2), r, n(:ef2), n(:c2), r, n(:a1), n(:bf1), n(:a1)]
  data [n(:bf1), r, n(:c2), n(:d2), len(4), r]
  data [len(16)]
  data [n(:f2), r, n(:ef2), n(:d2), r, n(:c2), n(:bf1), n(:a1)]
  data [n(:bf1), r, n(:c2), n(:d2), len(4), r]
  data [len(16)]
  data [n(:c2), r, n(:ef2), n(:c2), r, n(:a1), n(:bf1), n(:a1)]
  data [len(8)]
  data [n(:f2), n(:ef2), n(:d2), r]

  data [inst(0)]
  data [len(16)]
  data [n(:ef2), n(:d2), r, n(:ef2), n(:d2), r, n(:ef2), n(:d2), r, n(:ef2), n(:bf1), n(:c2), n(:d2), r, n(:ef2), r]
  data [n(:d2), n(:c2), r, n(:d2), n(:c2), r, n(:d2), n(:c2), r, n(:d2), n(:a1), n(:bf1), n(:c2), r, n(:a1), r]
  data [n(:ef2), n(:d2), r, n(:ef2), n(:d2), r, n(:ef2), n(:d2), r, n(:ef2), len(8), n(:d2), n(:f2), n(:bf2)]
  data [n(:a2), n(:g2), n(:ef2), n(:f2), len(16), n(:d2), n(:c2), r, n(:d2), len(4), n(:bf1)]

  data [len(16)]
  data [r, n(:d2), n(:ef2), n(:f2), n(:d2), r, n(:f2), r]
  data [r, n(:d2), n(:ef2), n(:f2), n(:d2), r, n(:f2), r]
  data [r, n(:d2), n(:ef2), n(:f2), n(:d2), r, n(:f2), r]
  data [n(:a2), n(:a2), n(:a2), n(:ef2), len(4), n(:d2)]
  data [len(16)]
  data [r, n(:d2), n(:ef2), n(:f2), n(:d2), r, n(:f2), r]
  data [r, n(:d2), n(:ef2), n(:f2), n(:d2), r, n(:f2), r]
  data [r, n(:f2), r, n(:f2), n(:f2), r, n(:f2), n(:f2)]
  data [len(8)]
  data [n(:c2), n(:d2), n(:ef2), r]
  data [eot]

  label :song1_trk3
  data [inst(8)]
  data [len(8)]
  data [r, n(:f2), n(:g2), n(:af2), n(:a2), n(:f3), inst(9), n(:f1), r]
  data [lp]

  data [inst(8)]
  data [len(8)]
  data [n(:bf1), n(:bf2), n(:f1), n(:bf2)]
  data [n(:bf1), n(:bf2), n(:f1), n(:bf2)]
  data [n(:f1), n(:f2), n(:c2), n(:f2)]
  data [n(:f1), n(:f2), n(:f1), len(16), n(:g1), n(:a1)]
  data [len(8)]
  data [n(:bf1), n(:bf2), n(:f1), n(:bf2)]
  data [n(:bf1), n(:bf2), n(:f1), n(:bf2)]
  data [n(:f1), n(:f2), n(:c2), n(:f2)]
  data [n(:bf1), len(16), n(:bf2), n(:bf2), len(8), n(:bf2), n(:f1)]

  data [len(16)]
  data [n(:ef1), r, n(:ef2), n(:ef2), n(:bf1), r, n(:bf2), r]
  data [n(:f1), n(:f1), n(:f2), r, n(:bf1), r, n(:bf2), r]
  data [n(:ef1), r, n(:ef2), n(:ef2), n(:bf1), r, n(:bf2), r]
  data [n(:f1), n(:g1), n(:a1), n(:bf1), r, r, n(:bf0), r]
  data [n(:ef1), r, n(:ef2), n(:ef2), n(:bf1), r, n(:bf2), r]
  data [n(:f1), n(:f1), n(:f2), r, n(:bf1), r, n(:bf2), r]
  data [n(:f1), r, n(:f2), n(:f2), n(:f1), r, n(:f2), r]
  data [n(:g1), r, n(:g2), r, n(:a1), n(:g1), n(:f1), r]

  data [len(8)]
  data [n(:bf1), n(:bf2), n(:f1), n(:bf2)]
  data [n(:bf1), n(:bf2), n(:f1), n(:bf2)]
  data [n(:f1), n(:f2), n(:c2), n(:f2)]
  data [n(:f1), n(:f2), n(:f1), len(16), n(:g1), n(:a1)]
  data [len(8)]
  data [n(:bf1), n(:bf2), n(:f1), n(:bf2)]
  data [n(:bf1), n(:bf2), n(:f1), n(:bf2)]
  data [n(:f1), n(:f2), n(:c2), n(:f2)]
  data [n(:f2), n(:ef2), n(:d2), r]

  data [len(16)]
  data [n(:bf1), r, r, n(:f2), r, r, n(:bf2), r]
  data [n(:bf1), r, r, n(:e2), n(:f2), r, n(:bf2), r]
  data [n(:f1), r, r, n(:b1), n(:c2), r, n(:f2), r]
  data [n(:f1), r, r, n(:c2), r, n(:e2), n(:f2), r]
  data [n(:bf1), r, r, n(:f2), r, r, n(:bf2), r]
  data [n(:bf1), r, r, n(:e2), n(:f2), r, n(:bf2), r]
  data [n(:f1), r, r, n(:b1), n(:c2), r, n(:f2), r]
  data [n(:bf1), r, r, n(:f2), r, n(:a2), n(:bf2), r]
  data [n(:ef1), r, r, n(:bf1), r, r, n(:bf0), r]
  data [n(:ef1), r, r, n(:bf1), r, r, n(:bf0), r]
  data [n(:ef1), r, r, n(:bf1), r, r, n(:bf0), r]
  data [n(:f1), r, r, n(:bf1), r, r, n(:bf0), r]
  data [n(:ef1), r, r, n(:bf1), r, r, n(:bf0), r]
  data [n(:ef1), r, r, n(:bf1), r, r, n(:bf0), r]
  data [n(:f1), r, r, n(:c2), r, r, n(:c1), r]
  data [n(:f1), r, n(:f2), r, n(:f2), r, r, r]

  data [eot]

  label :song1_trk4
  data [len(4)]
  data [n(:n0), r, len(16), r, len(8), r, len(48), n(:n3), n(:n3), n(:n3), len(4), n(:n2)]
  data [lp]
  data [len(24)]
  23.times do
    data [n(:n0), r, r, n(:n1), r, n(:n1), n(:n2), r, r, n(:n1), r, n(:n1)]
  end
  data [n(:n0), r, r, n(:n1), r, n(:n1), len(4), n(:n2)]
  8.times do
    data [len(16)]
    data [n(:n1), r, r, n(:n1), n(:n3), r, n(:n1), r]
    data [n(:n1), n(:n1), r, n(:n1), len(4), n(:n3)]
  end
  data [eot]

  label :song2
  data song2_trk1
  data [0], false, :word
  data song2_trk3
  data [0], false, :word

  label :song2a
  data song2_trk1
  data [0], false, :word
  data song2_trk3
  data song2_trk4

  label :song2_trk1
  data [lp]
  data [inst(0)]
  data [len(12)]
  data [n(:bf1), n(:c2), n(:df2), n(:ef2), r, n(:gf2), n(:f2), r, n(:df2), len(4), n(:bf1)]
  data [len(12)]
  data [n(:bf1), n(:c2), n(:df2), n(:ef2), r, n(:gf2), n(:f2), r, n(:df2), len(4), n(:bf2)]
  data [len(12)]
  data [n(:ef2), n(:f2), n(:gf2), n(:af2), r, n(:b2), n(:bf2), r, n(:gf2), len(4), n(:ef2)]
  data [len(12)]
  data [n(:ef2), n(:f2), n(:gf2), n(:af2), r, n(:b2), n(:bf2), r, n(:gf2), len(4), n(:ef3)]
  data [len(12)]
  data [n(:bf1), n(:c2), n(:df2), n(:ef2), r, n(:gf2), n(:f2), r, n(:af2), n(:g2), r, n(:bf2)]
  data [len(16)]
  data [n(:f2), n(:bf2), len(24), n(:gf2), n(:c3), n(:df3), len(8), n(:ef3), n(:c3), len(2), n(:bf2)]
  data [eot]

  label :song2_trk3
  data [lp]
  data [inst(8)]
  data [len(12)]
  data [n(:bf1), n(:c2), n(:df2), n(:ef2), r, n(:gf2), n(:f2), r, n(:df2), n(:bf1), r, r]
  data [n(:bf1), n(:c2), n(:df2), n(:ef2), r, n(:gf2), n(:f2), r, n(:df2), n(:bf2), r, r]
  data [n(:ef2), n(:f2), n(:gf2), n(:af2), r, n(:b2), n(:bf2), r, n(:gf2), n(:ef2), r, r]
  data [n(:ef2), n(:f2), n(:gf2), n(:af2), r, n(:b2), n(:bf2), r, n(:gf2), n(:ef3), r, r]
  data [n(:bf1), n(:c2), n(:df2), n(:ef2), r, n(:gf2), n(:f2), r, n(:af2), n(:g2), r, n(:bf2)]
  data [len(16)]
  data [n(:f2), n(:bf2), len(24), n(:gf2), n(:c3), n(:df3), len(8), n(:ef3), n(:c3), n(:bf2), r, r, r]
  data [eot]

  label :song2_trk4
  data [lp]
  data [len(8)]
  data [n(:n2), n(:n3), n(:n0), n(:n3), n(:n2), n(:n3), len(16), n(:n3), n(:n3), n(:n0), r]
  data [eot]

  label :song3
  data song3_trk1
  data song3_trk2
  data song3_trk3
  data song3_trk4

  label :song3_trk1
  data cvt_midi_file('mb_waterspace.mid', :normal, [[2, 3, :high], [4, 2]], 80, 16, 24, 312)
  label :song3_trk2
  data cvt_midi_file('mb_waterspace.mid', :normal, [[2, 3, :low], [3, 4]], 80, 16, 24, 312)
  label :song3_trk3
  data cvt_midi_file('mb_waterspace.mid', :normal, [[1, 9]], 80, 16, 24, 312)
  label :song3_trk4
  data cvt_midi_file('mb_waterspace.mid', :noise, [[5, {39 => 1}]], 80, 16, 0, 24)

  label :song4
  data song4_trk1
  data song4_trk2
  data song4_trk3
  data song4_trk4

  label :song4_trk1
  data cvt_midi_file('mb_castle.mid', :normal, [[1, 1, nil, -48]], 60, 48, 128, 640)
  label :song4_trk2
  data cvt_midi_file('mb_castle.mid', :normal, [[2, 1, nil, -36]], 60, 48, 128, 640)
  label :song4_trk3
  data cvt_midi_file('mb_castle.mid', :normal, [[3, 9]], 60, 48, 128, 640)
  label :song4_trk4
  data cvt_midi_file('mb_castle.mid', :noise, [[4, {39 => 1}]], 60, 48, 0, 32)

  label :song5
  data song5_trk1
  data song5_trk2
  data song5_trk3
  data song5_trk4

  label :song5_trk1
  data cvt_midi_file('mb_athletic.mid', :normal, [[2, 1]], 80, 16, 0, 2496)
  label :song5_trk2
  data cvt_midi_file('mb_athletic.mid', :normal, [[3, 1]], 80, 16, 0, 2496)
  label :song5_trk3
  data cvt_midi_file('mb_athletic.mid', :normal, [[1, 9, nil, -48]], 80, 16, 0, 2496)
  label :song5_trk4
  data cvt_midi_file('mb_athletic.mid', :noise, [[4, {39 => 1}]], 80, 16, 0, 96)


  label :song6
  data song6_trk1
  data song6_trk2
  data song6_trk3
  data [0], false, :word

  label :song6_trk1
  data cvt_midi_file('mb_snow.mid', :normal, [[2, 1]], 80, 24, 0, 288)
  label :song6_trk2
  data cvt_midi_file('mb_snow.mid', :normal, [[1, 1, :low]], 80, 24, 0, 288)
  label :song6_trk3
  data cvt_midi_file('mb_snow.mid', :normal, [[1, 9, :high, -24]], 80, 24, 0, 288)

end

labels = ram_labels.merge(prg_labels)

prg_size, prg_labels = patch_rom out_file, 0, 0xFFFA, :handlers, labels do
  data nmi_handler
  data main
  data other_vec
end

def create_nsf in_file, out_file, labels, song_count, game_name, artist_name, copyright_name
  rom = ROM.from_file in_file
  rom.seek 0x10
  prg_data = rom.read_bin 0x4000
  BinWriter.open out_file do |f|
    f.write_bin "NESM"
    f.write_byte 0x1A
    f.write_byte 0x01
    f.write_byte song_count
    f.write_byte 0
    f.write_u16_le labels[:load_nsf]
    f.write_u16_le labels[:init_nsf]
    f.write_u16_le labels[:play_nsf]
    f.write_bin game_name unless game_name.empty?
    (32 - game_name.size).times do
      f.write_byte 0
    end
    f.write_bin artist_name unless artist_name.empty?
    (32 - artist_name.size).times do
      f.write_byte 0
    end
    f.write_bin copyright_name unless copyright_name.empty?
    (32 - copyright_name.size).times do
      f.write_byte 0
    end
    f.write_u16_le 0x40FF
    8.times do
      f.write_byte 0
    end
    f.write_u16_le 0x4E1D
    f.write_byte 0
    f.write_byte 0

    f.write_byte 0
    f.write_byte 0
    f.write_byte 0
    f.write_byte 0

    f.write_bin prg_data
  end

end

create_nsf out_file, "#{out_file}.nsf", labels, SONG_COUNT, "Mushroom Boy", "James Montagna, Madeline Lim", "James Montagna 2017"
