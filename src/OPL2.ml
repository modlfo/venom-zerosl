type t =
  { registers : int option array
  ; port : RtMidi.midi_out
  }

let setRegister t reg value =
  let value = value land 0xFF in
  let update () =
    t.registers.(reg) <- Some value ;
    (*let () = print_endline (Printf.sprintf "0x%X = 0x%X" reg value) in*)
    let r_h = (0xF0 land reg) lsr 4 in
    let r_l = 0x0F land reg in
    let v_h = (0xF0 land value) lsr 4 in
    let v_l = 0x0F land value in
    RtMidi.sendMessageList t.port [ 0xF0; r_h; r_l; v_h; v_l; 0xF7 ]
  in
  match t.registers.(reg) with
  | Some current when value <> current -> update ()
  | Some _ -> ()
  | None -> update ()


let getRegister t reg =
  match t.registers.(reg) with
  | Some v -> v
  | None -> 0


let setBit t reg n v =
  let current = getRegister t reg in
  let value = 0x1 lsl n in
  let mask = lnot value in
  let new_value = current land mask lor if v = 1 then value else 0x0 in
  setRegister t reg new_value


let setBits t reg mask v =
  let current = getRegister t reg in
  let not_mask = lnot mask in
  let cleared = current land not_mask in
  let new_value = cleared lor v in
  (*let () = Printf.printf "0x%X, 0x%X, 0x%X, 0x%X\n" current not_mask cleared new_value in*)
  setRegister t reg new_value


type params =
  | CSM
  | NoteSel
  | AM
  | Vibrato
  | EnvelopeType
  | KeyScalingRate
  | Multiple
  | Level
  | KeyScalingLevel
  | Attack
  | Decay
  | Sustain
  | Release
  | Frequency10Bits
  | BlockOctave
  | KeyOn
  | Feedback
  | Connection
  | AMDepth
  | VibratoDepth
  | Rhythm
  | BassDrum
  | SnareDrum
  | TomDrum
  | TopCymbal
  | HighHat
  | WaveSelect

let set t param ?(offset = 0) value =
  match param with
  | CSM -> setBit t 0x08 7 value
  | NoteSel -> setBit t 0x08 6 value
  | AM -> setBit t (0x20 + offset) 7 value
  | Vibrato -> setBit t (0x20 + offset) 6 value
  | EnvelopeType -> setBit t (0x20 + offset) 5 value
  | KeyScalingRate -> setBit t (0x20 + offset) 4 value
  | Multiple -> setBits t (0x20 + offset) 0x0F value
  | Level -> setBits t (0x40 + offset) 0x3F value
  | KeyScalingLevel -> setBits t (0x40 + offset) 0xC0 (value lsl 6)
  | Attack -> setBits t (0x60 + offset) 0xF0 (value lsl 4)
  | Decay -> setBits t (0x60 + offset) 0x0F value
  | Sustain -> setBits t (0x80 + offset) 0xF0 (value lsl 4)
  | Release -> setBits t (0x80 + offset) 0x0F value
  | Frequency10Bits ->
      setRegister t (0xA0 + offset) (0xFF land value) ;
      setBits t (0xB0 + offset) 0x03 ((0x300 land value) lsr 8)
  | BlockOctave -> setBits t (0xB0 + offset) 0x1C (value lsl 2)
  | KeyOn -> setBit t (0xB0 + offset) 5 value
  | Feedback -> setBits t (0xC0 + offset) 0x0E (value lsl 1)
  | Connection -> setBits t (0xC0 + offset) 0x01 value
  | AMDepth -> setBit t 0xBD 7 value
  | VibratoDepth -> setBit t 0xBD 6 value
  | Rhythm -> setBit t 0xBD 5 value
  | BassDrum -> setBit t 0xBD 4 value
  | SnareDrum -> setBit t 0xBD 3 value
  | TomDrum -> setBit t 0xBD 2 value
  | TopCymbal -> setBit t 0xBD 1 value
  | HighHat -> setBit t 0xBD 0 value
  | WaveSelect -> setBits t (0xE0 + offset) 0x03 value


let getOperatorOffset channel op =
  let base =
    match channel with
    | 1 -> 0x0
    | 2 -> 0x1
    | 3 -> 0x2
    | 4 -> 0x8
    | 5 -> 0x9
    | 6 -> 0xA
    | 7 -> 0x10
    | 8 -> 0x11
    | 9 -> 0x12
    | _ -> failwith "Invalid channel"
  in
  match op with
  | 1 -> base
  | 2 -> base + 3
  | _ -> failwith "Invalid operator"


let defaultSettings t =
  let _ = List.init (Array.length t.registers) (fun i -> setRegister t i 0) in
  setBit t 1 5 1 ;
  t


let loadInstrument t channel i =
  match i with
  | [ _transpose; reg20op1; reg40op1; reg60op1; reg80op1; regC0; reg20op2; reg40op2; reg60op2; reg80op2; regE0 ] ->
      let offset_op1 = getOperatorOffset channel 1 in
      let offset_op2 = getOperatorOffset channel 2 in
      setRegister t (0x20 + offset_op1) reg20op1 ;
      setRegister t (0x40 + offset_op1) reg40op1 ;
      setRegister t (0x60 + offset_op1) reg60op1 ;
      setRegister t (0x80 + offset_op1) reg80op1 ;
      setRegister t (0xE0 + offset_op1) (regE0 land 0x7) ;
      setRegister t (0x20 + offset_op2) reg20op2 ;
      setRegister t (0x40 + offset_op2) reg40op2 ;
      setRegister t (0x60 + offset_op2) reg60op2 ;
      setRegister t (0x80 + offset_op2) reg80op2 ;
      setRegister t (0xE0 + offset_op2) ((regE0 land 0x70) lsr 3) ;
      setRegister t (0xC0 + channel) regC0
  | _ -> failwith "invalid instrument"


let freq_table = [| 337; 363; 385; 408; 432; 458; 485; 514; 544; 577; 611; 647 |]

let noteOff t channel _pitch _velocity = set t KeyOn ~offset:channel 0

let noteOn t channel pitch velocity =
  if velocity = 0 then
    noteOff t channel pitch velocity
  else
    let octave = pitch / 12 in
    let note = pitch mod 12 in
    set t Frequency10Bits ~offset:channel freq_table.(note) ;
    set t BlockOctave ~offset:channel octave ;
    set t KeyOn ~offset:channel 1


let init () =
  match RtMidi.openOutput "OPL2 Audio Board MIDI:OPL2 Audio Board MIDI Port 1" with
  | Some port -> defaultSettings { port; registers = Array.make 246 None }
  | None -> failwith "failed to open the OPL2 port"
