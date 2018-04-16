require "./rom"

def parse_midi in_file

  midi = ROM.from_file in_file

  head = midi.read_str(4)
  chksize = midi.read_u32_be

  raise "Not a MIDI file" unless head == 'MThd' && chksize == 6

  format, track_count, ppqn = [midi.read_u16_be, midi.read_u16_be, midi.read_u16_be]

  puts "Format: #{format}"
  puts "Tracks: #{track_count}"
  puts "PPQN: #{ppqn}"
  trks = []
  track_count.times do |ti|
    offs = midi.tell
    head = midi.read_str(4)
    raise "Expected a Track here" unless head == 'MTrk'
    chksize = midi.read_u32_be
    ts = 0
    rs = 0
    eot = false
    trk = [[0, :int, :offset, offs],[0, :int, :size, chksize]]
    while !eot
      dt = midi.read_vlq
      ts += dt
      cmd = midi.read_byte
      evt = [ts]

      parse_status = ->(status, first_status=nil) do
        evt << :status
        type, chan = [(status >> 4) & 0x7, status & 0xF]
        evt << [:note_off, :note_on, :note_pressure, :cc, :program, :pressure, :pitch][type]
        evt << chan
        cmd = status
        rs = status
        if first_status
          evt << first_status
        else
          evt << midi.read_byte
        end

        if ![0xC0,0xD0].include?(cmd & 0xF0)
          evt << midi.read_byte
        end
      end

      if cmd < 0x80
        parse_status[rs, cmd]
      elsif cmd == 0xFF
        evt << :meta
        type = midi.read_byte
        len = midi.read_byte
        case type
        when 0x3
          evt << :regid
          evt << midi.read_str(len)
        when 0x4
            evt << :trkid
            evt << midi.read_str(len)
        when 0x51
            evt << :tempo
            evt << 60000000.to_f / midi.read_u24_be.to_f
        when 0x2F
            evt << :eot
            eot = true
        else
            evt << :unknown
            evt << type
            data = midi.read_bin(len)
            evt << data
        end
      elsif cmd < 0xF0
        parse_status[cmd]
      else
        midi.msg "Unsupported Command #{cmd.to_s(16)}"
        raise "parse error"
      end
      trk << evt
    end
    trks << trk
  end
  { ppqn: ppqn, format: format, tracks: trks }
end
