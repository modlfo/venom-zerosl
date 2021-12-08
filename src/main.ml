module Z = Zero

let showPorts () =
  let () = print_endline "Outputs" in
  let () = print_endline (RtMidi.getOutputNames () |> String.concat "\n") in
  let () = print_endline "Inputs" in
  let () = print_endline (RtMidi.getInputNames () |> String.concat "\n") in
  ()


let wave = [| "Sine"; "Half"; "Rect"; "PW" |]

let six_bit = Array.init 64 (fun i -> string_of_int i)

let four_bit = Array.init 16 (fun i -> string_of_int i)

let three_bit = Array.init 8 (fun i -> string_of_int i)

let on_off = [| "Off"; "On" |]

let groups =
  Zero.(
    makeTopGroup
      [| newGroup
           "OP"
           ~choke:1
           ~active:true
           [| enumEncoder "1-Level" 1 six_bit 1
            ; enumEncoder "1-Multi" 2 four_bit 1
            ; enumEncoder "1-Wave" 3 wave 1
            ; enumEncoder "2-Level" 4 six_bit 1
            ; enumEncoder "2-Multi" 5 four_bit 1
            ; enumEncoder "2-Wave" 6 wave 1
            ; enumEncoder "Feedback" 7 three_bit 1
            ; enumEncoder "Mode" 8 [| "FM"; "ADD" |] 0
            ; enumSlider "1-Attack" 1 four_bit 0
            ; enumSlider "1-Decay" 2 four_bit 0
            ; enumSlider "1-Sustain" 3 four_bit 0
            ; enumSlider "1-Release" 4 four_bit 0
            ; enumSlider "2-Attack" 5 four_bit 0
            ; enumSlider "2-Decay" 6 four_bit 0
            ; enumSlider "2-Sustain" 7 four_bit 0
            ; enumSlider "2-Release" 8 four_bit 0
            ; toggleButton "EG Type" Right Top 1 false
           |]
       ; newGroup
           "Notes"
           ~choke:2
           ~active:true
           [| pushButton "1" Right Bottom 1
            ; pushButton "2" Right Bottom 2
            ; pushButton "3" Right Bottom 3
            ; pushButton "4" Right Bottom 4
            ; pushButton "5" Right Bottom 5
            ; pushButton "6" Right Bottom 6
            ; pushButton "7" Right Bottom 7
            ; pushButton "8" Right Bottom 8
            ; pushButton "Prev" Right Top 7
            ; pushButton "Next" Right Top 8
           |]
      |])


let patch = ref 10

let action opl2 (groups : Zero.group) parameter =
  let () = print_endline (Zero.paramPath parameter ^ " " ^ string_of_int (Zero.paramInt parameter)) in
  match Zero.paramPath parameter with
  | "/OP/1-Level" ->
      OPL2.set opl2 Level ~offset:(OPL2.getOperatorOffset 1 1) (63 - Zero.paramInt parameter) ;
      groups
  | "/OP/1-Wave" ->
      OPL2.set opl2 WaveSelect ~offset:(OPL2.getOperatorOffset 1 1) (Zero.paramInt parameter) ;
      groups
  | "/OP/1-Multi" ->
      OPL2.set opl2 Multiple ~offset:(OPL2.getOperatorOffset 1 1) (Zero.paramInt parameter) ;
      groups
  | "/OP/2-Level" ->
      OPL2.set opl2 Level ~offset:(OPL2.getOperatorOffset 1 2) (63 - Zero.paramInt parameter) ;
      groups
  | "/OP/2-Wave" ->
      OPL2.set opl2 WaveSelect ~offset:(OPL2.getOperatorOffset 1 2) (Zero.paramInt parameter) ;
      groups
  | "/OP/2-Multi" ->
      OPL2.set opl2 Multiple ~offset:(OPL2.getOperatorOffset 1 2) (Zero.paramInt parameter) ;
      groups
  | "/OP/Feedback" ->
      OPL2.set opl2 Feedback ~offset:0 (Zero.paramInt parameter) ;
      groups
  | "/OP/Mode" ->
      OPL2.set opl2 Connection ~offset:0 (Zero.paramInt parameter) ;
      groups
  | "/OP/1-Attack" ->
      OPL2.set opl2 Attack ~offset:(OPL2.getOperatorOffset 1 1) (15 - Zero.paramInt parameter) ;
      groups
  | "/OP/1-Decay" ->
      OPL2.set opl2 Decay ~offset:(OPL2.getOperatorOffset 1 1) (15 - Zero.paramInt parameter) ;
      groups
  | "/OP/1-Sustain" ->
      OPL2.set opl2 Sustain ~offset:(OPL2.getOperatorOffset 1 1) (15 - Zero.paramInt parameter) ;
      groups
  | "/OP/1-Release" ->
      OPL2.set opl2 Release ~offset:(OPL2.getOperatorOffset 1 1) (15 - Zero.paramInt parameter) ;
      groups
  | "/OP/2-Attack" ->
      OPL2.set opl2 Attack ~offset:(OPL2.getOperatorOffset 1 2) (15 - Zero.paramInt parameter) ;
      groups
  | "/OP/2-Decay" ->
      OPL2.set opl2 Decay ~offset:(OPL2.getOperatorOffset 1 2) (15 - Zero.paramInt parameter) ;
      groups
  | "/OP/2-Sustain" ->
      OPL2.set opl2 Sustain ~offset:(OPL2.getOperatorOffset 1 2) (15 - Zero.paramInt parameter) ;
      groups
  | "/OP/2-Release" ->
      OPL2.set opl2 Release ~offset:(OPL2.getOperatorOffset 1 2) (15 - Zero.paramInt parameter) ;
      groups
  | "/Notes/1" ->
      OPL2.noteOn opl2 0 24 (100 * Zero.paramInt parameter) ;
      groups
  | "/Notes/2" ->
      OPL2.noteOn opl2 0 25 (100 * Zero.paramInt parameter) ;
      groups
  | "/Notes/3" ->
      OPL2.noteOn opl2 0 26 (100 * Zero.paramInt parameter) ;
      groups
  | "/Notes/4" ->
      OPL2.noteOn opl2 0 27 (100 * Zero.paramInt parameter) ;
      groups
  | "/Notes/5" ->
      OPL2.noteOn opl2 0 28 (100 * Zero.paramInt parameter) ;
      groups
  | "/Notes/6" ->
      OPL2.noteOn opl2 0 29 (100 * Zero.paramInt parameter) ;
      groups
  | "/Notes/7" ->
      OPL2.noteOn opl2 0 30 (100 * Zero.paramInt parameter) ;
      groups
  | "/Notes/8" ->
      OPL2.noteOn opl2 0 31 (100 * Zero.paramInt parameter) ;
      groups
  | "/Notes/Prev" ->
      if Zero.paramInt parameter = 1 then patch := !patch - 1 ;
      if !patch < 0 then patch := OPL2Instr.n_instruments - 1 ;
      let () = print_endline ("Loading :" ^ string_of_int !patch) in
      OPL2.loadInstrument opl2 1 OPL2Instr.instruments.(!patch) ;
      groups
  | "/Notes/Next" ->
      if Zero.paramInt parameter = 1 then patch := (!patch + 1) mod OPL2Instr.n_instruments ;
      OPL2.loadInstrument opl2 1 OPL2Instr.instruments.(!patch) ;
      let () = print_endline ("Loading :" ^ string_of_int !patch) in
      groups
  | _ -> groups


let actions opl2 modified group =
  let new_groups = Array.fold_left (action opl2) group modified in
  new_groups

;;

showPorts ()

let setAllChannels t param value =
  let open OPL2 in
  ignore
  @@ List.init 9 (fun offset ->
         set t param ~offset value ;
         Unix.sleepf 0.001 )


let setAllOperators t param value =
  let open OPL2 in
  ignore
  @@ List.init 22 (fun offset ->
         set t param ~offset value ;
         Unix.sleepf 0.001 )


let set9Notes t =
  setAllChannels t Frequency10Bits 686 ;
  setAllChannels t BlockOctave 3


let start t =
  let open OPL2 in
  set t CSM 0 ;
  set t NoteSel 0 ;
  setAllOperators t Attack 0xD ;
  setAllOperators t Decay 0x5 ;
  setAllOperators t Release 0x9 ;
  setAllOperators t Sustain 0xF ;
  setAllOperators t Level 5 ;
  setAllOperators t EnvelopeType 1 ;
  setAllOperators t AM 0 ;
  setAllOperators t Vibrato 0 ;
  set t AMDepth 0 ;
  set t VibratoDepth 0 ;
  set t Rhythm 0 ;
  setAllOperators t KeyScalingLevel 0 ;
  setAllOperators t KeyScalingRate 0 ;
  setAllOperators t Multiple 0 ;
  setAllChannels t Connection 0 ;
  setAllChannels t Feedback 0 ;
  setAllOperators t WaveSelect 0 ;
  (*loadInstrument t 1 OPL2Instr.iORGAN1 ;*)
  set9Notes t


let playNotes opl2 =
  let rec loop i =
    if i > 8 then
      ()
    else begin
      OPL2.set opl2 OPL2.KeyOn ~offset:i 1 ;
      Unix.sleepf 0.25 ;
      OPL2.set opl2 OPL2.KeyOn ~offset:i 0 ;
      loop (i + 1)
    end
  in
  loop 0


let test () =
  let opl2 = OPL2.init () in
  start opl2 ;
  let port = "ZeRO MkII:ZeRO MkII MIDI 2" in
  match Zero.openPort port groups (actions opl2) with
  | Some zero ->
      let n_inst = Array.length OPL2Instr.instruments in
      let rec loop count inst =
        let () = Zero.update zero in
        Unix.sleepf 0.01 ;
        (*
        let count = if count > 100 then 0 else count + 1 in
        if count = 0 then begin
          OPL2.loadInstrument opl2 1 OPL2Instr.instruments.(inst) ;
          OPL2.set opl2 OPL2.KeyOn ~offset:0 1
        end ;
        if count = 50 then
          OPL2.set opl2 OPL2.KeyOn ~offset:0 0 ;
        *)
        loop count ((inst + 1) mod n_inst)
      in
      loop 0 0
  | _ ->
      prerr_endline "Failed to open the ports" ;
      failwith "Failed to open the ports"

;;

test ()
