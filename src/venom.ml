
type part =
   | Single
   | Part1
   | Part2
   | Part3
   | Part4

type t =
   {
      port : Remidi.output;
      part : int;
      mutable key_shape : int;
   }

let partCode part =
   match part with
   | Single -> 0x09
   | Part1 -> 0x0B
   | Part2 -> 0x0C
   | Part3 -> 0x0D
   | Part4 -> 0x0E


let send port part control value =
   Remidi.sendMessage port [|0xF0; 0x00; 0x01 ;0x05; 0x21; 0x00; 0x02; part; 0x00; control; 0x00; value; 0xF7|]

let openPort port : t option =
   match Remidi.openOutput port with
   | None -> None
   | Some port ->
      Some {
         port;
         part = partCode Single;
         key_shape = 0;
      }


(* Osc1 *)

let unset t ~mask =
   lnot mask land t

let set t ~mask =
   mask lor t


let setUnsetInverse t ~mask value =
   let new_value =
      if value <> 0 then
         unset t.key_shape ~mask
      else
         set t.key_shape ~mask
   in
   t.key_shape <- new_value;
   new_value

let setUnset t ~mask value =
   let new_value =
      if value <> 0 then
         set t.key_shape ~mask
      else
         unset t.key_shape ~mask
   in
   t.key_shape <- new_value;
   new_value

let setOsc1Level t value =
   send t.port t.part 0x64 value

let setOsc1Keytrack t value =
   let value = setUnsetInverse t ~mask:0x08 value in
   send t.port t.part 0x18 value

let setOsc1Wave t value =
   send t.port t.part 0x1A value

let setOsc1Coarse t value =
   send t.port t.part 0x1B value

let setOsc1Fine t value =
   send t.port t.part 0x1C value

let setOsc2Level t value =
   send t.port t.part 0x65 value

let setOsc2Keytrack t value =
   let value = setUnsetInverse t ~mask:0x10 value in
   send t.port t.part 0x18 value

let setOsc2Wave t value =
   send t.port t.part 0x1D value

let setOsc2Coarse t value =
   send t.port t.part 0x1E value

let setOsc2Fine t value =
   send t.port t.part 0x1F value

let setOsc2Sync t value =
   let value = setUnset t ~mask:0x02 value in
   send t.port t.part 0x18 value


let setStartMod t value =
   send t.port t.part 0x13 value

let setOsc1FM3 t value =
   send t.port t.part 0x17 value

let setWaveshape t value =
   let state = setUnset t ~mask:0x01 value in
   send t.port t.part 0x18 state;
   send t.port t.part 0x19 value

(* Fitler *)

let setFilterBoost t value =
   send t.port t.part 0x20 value

let setFilterType t value =
   send t.port t.part 0x69 value

let setCutoff t value =
   send t.port t.part 0x6A value;
   send t.port t.part 0x6B value

let setResonance t value =
   send t.port t.part 0x6C value;

