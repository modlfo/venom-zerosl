
let showPorts () =
   Remidi.getOutputNames () |> String.concat "\n"
   |> print_endline

let makeBotton index = Zero.Left, Zero.Bottom, index + 1

let lfo_waves = [|"Sine"; "Sine+"; "Triangle"; "Sawtooth"; "Square"; "S&H"; "LinS&H"; "LogS&H"; "ExpSqr"; "LogSqr"; "LogSaw"; "ExpSaw"|]
let osc_waves =
   [|
      "HP Sine"; "PB Sine"; "RP Sine";
      "SH Tri"; "MG Tri"; "RP Tri";
      "PB Saw"; "SH Saw"; "MG Saw"; "OB Saw"; "JX Saw"; "RP Saw"; "MS Saw";
      "PB Square"; "SH Square"; "MG Square"; "OB Square"; "JX Square"; "RP Square"; "MS Square";
      "AL Pulse"; "MG Pulse";
      "MG Sync"; "SH Sync"; "JX Sync";
      "BitWave1"; "BitWave2"; "BitWave3";
      "AL FM Wave"; "DP X Wave"; "RP FM Wave";
      "AL FM Bass"; "AL FM Quack"; "AL FM Woody"; "AL FM Science"; "AL FM O1"; "AL FM O2"; "AL FM Inharmonic";
      "MG White Noise"|]

let groups =
   Zero.[|
      newGroup "Global" ~choke:0 ~active:true
         [|
            radioButton "Selector" (Array.init 8 makeBotton) [|"Osc1"; "Osc2"; "Osc3"; "Env1"; "Env2"; "Env3"; "LFO1"; "LFO2"|] 1;
         |];
      newGroup "Osc1" ~choke:1 ~active:true
         [|
            enumEncoder       "Wave" 1 osc_waves 1;
            enumEncoder       "Keytrack" 2 [|"Off"; "On"|] 2;
            numericEncoder    "Coarse" 3 ~min:(-64.0) ~max:63.0 0.0;
            numericEncoder    "Fine" 4 ~min:(-64.0) ~max:63.0 0.0;
            percentualEncoder "Osc3>FM" 5 ~min:0.0 ~max:1.0 0.0;
            percentualEncoder "Waveshape" 6 ~min:0.0 ~max:1.0 0.0;
            blank 7;
            percentualEncoder "Level" 8 ~min:0.0 ~max:1.0 1.0;
         |];
      newGroup "Osc2" ~choke:1 ~active:false
         [|
            enumEncoder       "Wave" 1 osc_waves 1;
            enumEncoder       "Keytrack" 2 [|"Off"; "On"|] 2;
            numericEncoder    "Coarse" 3 ~min:(-64.0) ~max:63.0 0.0;
            numericEncoder    "Fine" 4 ~min:(-64.0) ~max:63.0 0.0;
            enumEncoder       "Sync" 5 [|"Off"; "On"|] 1;
            percentualEncoder "StartMod" 6 ~min:0.0 ~max:1.0 0.0;
            blank 7;
            percentualEncoder "Level" 8 ~min:0.0 ~max:1.0 0.0;
         |];
      newGroup "Osc3" ~choke:1 ~active:false
         [|
            enumEncoder       "Wave" 1 osc_waves 1;
            enumEncoder       "Keytrack" 2 [|"Off"; "On"|] 2;
            numericEncoder    "Coarse" 3 ~min:(-64.0) ~max:63.0 0.0;
            numericEncoder    "Fine" 4 ~min:(-64.0) ~max:63.0 0.0;
            enumEncoder       "Sync" 5 [|"Off"; "On"|] 1;
            percentualEncoder "Drift" 6 ~min:0.0 ~max:1.0 0.0;
            blank 7;
            percentualEncoder "Level" 8 ~min:0.0 ~max:1.0 0.0;
         |];
      newGroup "Env1" ~choke:1 ~active:false
         [|
            numericEncoder "Attack" 1 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Hold" 2 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Decay" 3 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Sustain" 4 ~min:(0.0) ~max:1.0 1.0;
            numericEncoder "Release" 5 ~min:(0.0) ~max:1.0 0.0;
            blank 6; blank 7; blank 8;
         |];
      newGroup "Env2" ~choke:1 ~active:false
         [|
            numericEncoder "Attack" 1 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Hold" 2 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Decay" 3 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Sustain" 4 ~min:(0.0) ~max:1.0 1.0;
            numericEncoder "Release" 5 ~min:(0.0) ~max:1.0 0.0;
            blank 6; blank 7; blank 8;
         |];
      newGroup "Env3" ~choke:1 ~active:false
         [|
            numericEncoder "Attack" 1 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Hold" 2 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Decay" 3 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Sustain" 4 ~min:(0.0) ~max:1.0 1.0;
            numericEncoder "Release" 5 ~min:(0.0) ~max:1.0 0.0;
            blank 6; blank 7; blank 8;
         |];
      newGroup "LFO1" ~choke:1 ~active:false
         [|
            enumEncoder    "Wave" 1 lfo_waves 1;
            enumEncoder    "Sync" 2 [|"Off"; "On"|] 1;
            numericEncoder "Rate" 3 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Delay" 4 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Attack" 5 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Start" 6 ~min:(0.0) ~max:1.0 0.0;
            blank 7; blank 8;
         |];
      newGroup "LFO2" ~choke:1 ~active:false
         [|
            enumEncoder    "Wave" 1 lfo_waves 1;
            enumEncoder    "Sync" 2 [|"Off"; "On"|] 1;
            numericEncoder "Rate" 3 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Delay" 4 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Attack" 5 ~min:(0.0) ~max:1.0 0.0;
            numericEncoder "Start" 6 ~min:(0.0) ~max:1.0 0.0;
            blank 7; blank 8;
         |];
      newGroup "Filter" ~choke:3 ~active:true
         [|
            numericKnob "Cutoff" 1 ~min:(0.0) ~max:1.0 0.0;
            numericKnob "Resonance" 2 ~min:(0.0) ~max:1.0 0.0;
            enumKnob    "Type" 3 [|"Off"; "LP 12"; "BP 12"; "HP 12"; "LP 24"; "BP 24"; "HP 24"|] 2;
            numericKnob "Boost" 4 ~min:(0.0) ~max:1.0 0.0;
         |];
   |]


let venom =
   match Venom.openPort "ZeRO MkII Port 1" with
   | Some value -> value
   | None ->
      showPorts ();
      failwith "Failed to open the venoms port"

let osc1Actions parameter =
   let () =
      match Zero.paramPath parameter with
      | "/Osc1/Wave" ->
         Venom.setOsc1Wave venom (Zero.paramInt parameter)
      | "/Osc1/Keytrack" ->
         Venom.setOsc1Keytrack venom (Zero.paramInt parameter)
      | "/Osc1/Coarse" ->
         Venom.setOsc1Coarse venom (Zero.paramInt parameter)
      | "/Osc1/Fine" ->
         Venom.setOsc1Fine venom (Zero.paramInt parameter)
      | "/Osc1/Osc3>FM" ->
         Venom.setOsc1FM3 venom (Zero.paramInt parameter)
      | "/Osc1/Level" ->
         Venom.setOsc1Level venom (Zero.paramInt parameter)
      | "/Osc1/Waveshape" ->
         Venom.setWaveshape venom (Zero.paramInt parameter)
      | _ -> ()
   in parameter

let osc2Actions parameter =
   let () =
      match Zero.paramPath parameter with
      | "/Osc2/Wave" ->
         Venom.setOsc2Wave venom (Zero.paramInt parameter)
      | "/Osc2/Keytrack" ->
         Venom.setOsc2Keytrack venom (Zero.paramInt parameter)
      | "/Osc2/Coarse" ->
         Venom.setOsc2Coarse venom (Zero.paramInt parameter)
      | "/Osc2/Fine" ->
         Venom.setOsc2Fine venom (Zero.paramInt parameter)
      | "/Osc2/Sync" ->
         Venom.setOsc2Sync venom (Zero.paramInt parameter)
      | "/Osc2/Level" ->
         Venom.setOsc2Level venom (Zero.paramInt parameter)
      | "/Osc2/StartMod" ->
         Venom.setStartMod venom (Zero.paramInt parameter)
      | _ -> ()
   in parameter

let filterActions parameter =
   let () =
      match Zero.paramPath parameter with
      | "/Filter/Cutoff" ->
         Venom.setCutoff venom (Zero.paramInt parameter)
      | "/Filter/Resonance" ->
         Venom.setResonance venom (Zero.paramInt parameter)
      | "/Filter/Type" ->
         Venom.setFilterType venom (Zero.paramInt parameter)
      | "/Filter/Boost" ->
         Venom.setFilterBoost venom (Zero.paramInt parameter)
      | _ -> ()
   in parameter


let action (groups:Zero.groups) parameter =
   match Zero.paramPath parameter with
   | "/Global/Selector" ->
      let active = Zero.paramString parameter in
      let groups = Zero.groupSetActive groups active in
      groups
   | other ->
      let () = print_endline (other ^ " " ^(Zero.paramInt parameter |> string_of_int)) in
      let _ : Zero.parameter =
         parameter
         |> filterActions
         |> osc1Actions
         |> osc2Actions
      in
      groups


let actions (modified:Zero.parameters) (groups:Zero.groups) : Zero.groups =
   let new_groups = Array.fold_left action groups modified in
   new_groups


let test () =
   let port = "ZeRO MkII Port 2" in
   match Zero.openPort port groups actions with
   | Some _zero -> ()
   | _ -> prerr_endline "Failed to open the ports"
;;

test ();;