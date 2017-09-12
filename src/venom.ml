
type part =
   | Single
   | Multi
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
   | Multi -> 0x0A
   | Part1 -> 0x0B
   | Part2 -> 0x0C
   | Part3 -> 0x0D
   | Part4 -> 0x0E

let send port part c1 c2 value =
   Remidi.sendMessage port
      [|0xF0;
        (* header*)
        0x00; 0x01 ;0x05; 0x21; 0x00;
        (*cmd*)
        0x02;
        (* addr *)
        part; c1; c2;
        (* data *)
        0x00; value;
        0xF7|]

let openPort port : t option =
   match Remidi.openOutput port with
   | None -> None
   | Some port ->
      Some {
         port;
         part = partCode Single;
         key_shape = 0;
      }


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


let setGlideOnOff t value =
   send t.port t.part 0x00 0x00 value

let setGlideTime t value =
   send t.port t.part 0x00 0x01 value

(* SglParam.SaveBank 0x02 *)

(* SglParam.SavePatch 0x03 *)

(* Env 1 *)

let setEnv1Attack t value =
   send t.port t.part 0x00 0x04 value

let setEnv1Hold t value =
   send t.port t.part 0x00 0x05 value

let setEnv1Decay t value =
   send t.port t.part 0x00 0x06 value

let setEnv1Sustain t value =
   send t.port t.part 0x00 0x07 value

let setEnv1Release t value =
   send t.port t.part 0x00 0x08 value

(* Env 2 *)

let setEnv2Attack t value =
   send t.port t.part 0x00 0x09 value

let setEnv2Hold t value =
   send t.port t.part 0x00 0x0A value

let setEnv2Decay t value =
   send t.port t.part 0x00 0x0B value

let setEnv2Sustain t value =
   send t.port t.part 0x00 0x0C value

let setEnv2Release t value =
   send t.port t.part 0x00 0x0D value

(* Env 3 *)

let setEnv3Attack t value =
   send t.port t.part 0x00 0x0E value

let setEnv3Hold t value =
   send t.port t.part 0x00 0x0F value

let setEnv3Decay t value =
   send t.port t.part 0x00 0x10 value

let setEnv3Sustain t value =
   send t.port t.part 0x00 0x11 value

let setEnv3Release t value =
   send t.port t.part 0x00 0x12 value

(* Osc Misc *)

let setStartMod t value =
   send t.port t.part 0x00 0x13 value

let setDrift t value =
   send t.port t.part 0x00 0x14 value

let setBendRange t value =
   send t.port t.part 0x00 0x15 value

let setRingMod t value =
   send t.port t.part 0x00 0x16 value

let setOsc1FM3 t value =
   send t.port t.part 0x00 0x17 value

(* Osc Flags *)

let setOsc1Keytrack t value =
   let value = setUnsetInverse t ~mask:0x08 value in
   send t.port t.part 0x00 0x18 value

let setOsc2Keytrack t value =
   let value = setUnsetInverse t ~mask:0x10 value in
   send t.port t.part 0x00 0x18 value

let setOsc3Sync t value =
   let value = setUnset t ~mask:0x04 value in
   send t.port t.part 0x00 0x18 value

let setOsc2Sync t value =
   let value = setUnset t ~mask:0x02 value in
   send t.port t.part 0x00 0x18 value

let setOsc3Keytrack t value =
   let value = setUnsetInverse t ~mask:0x20 value in
   send t.port t.part 0x00 0x18 value

let setWaveshape t value =
   let state = setUnset t ~mask:0x01 value in
   send t.port t.part 0x00 0x18 state;
   send t.port t.part 0x00 0x19 value

(* Osc 1 *)

let setOsc1Wave t value =
   send t.port t.part 0x00 0x1A value

let setOsc1Coarse t value =
   send t.port t.part 0x00 0x1B value

let setOsc1Fine t value =
   send t.port t.part 0x00 0x1C value

(* Osc 2 *)

let setOsc2Wave t value =
   send t.port t.part 0x00 0x1D value

let setOsc2Coarse t value =
   send t.port t.part 0x00 0x1E value

let setOsc2Fine t value =
   send t.port t.part 0x00 0x1F value

(* Boost *)

let setFilterBoost t value =
   send t.port t.part 0x00 0x20 value

(* Osc 3 *)

let setOsc3Wave t value =
   send t.port t.part 0x00 0x21 value

let setOsc3Coarse t value =
   send t.port t.part 0x00 0x22 value

let setOsc3Fine t value =
   send t.port t.part 0x00 0x23 value

(* LFO 1 *)

let setLFO1Wave t value = (* 0 - 7 *)
   send t.port t.part 0x00 0x25 value

let setLFO1Rate t value =
   send t.port t.part 0x00 0x26 value

let setLFO1Delay t value =
   send t.port t.part 0x00 0x27 value

let setLFO1Attack t value =
   send t.port t.part 0x00 0x28 value

let setLFO1StartPhase t value =
   send t.port t.part 0x00 0x29 value

(* LFO 2 *)

let setLFO2Wave t value = (* 0 - 7 *)
   send t.port t.part 0x00 0x2A value

let setLFO2Rate t value =
   send t.port t.part 0x00 0x2B value

let setLFO2Delay t value =
   send t.port t.part 0x00 0x2C value

let setLFO2Attack t value =
   send t.port t.part 0x00 0x2D value

let setLFO2StartPhase t value =
   send t.port t.part 0x00 0x2E value

(* LFO 3 *)

let setLFO3Wave t value = (* 0 - 7 *)
   send t.port t.part 0x00 0x2F value

let setLFO3Rate t value =
   send t.port t.part 0x00 0x30 value

let setLFO3Delay t value =
   send t.port t.part 0x00 0x31 value

let setLFO3Attack t value =
   send t.port t.part 0x00 0x32 value

let setLFO3StartPhase t value =
   send t.port t.part 0x00 0x33 value

(* Modulation sources *)

(* Sources 0 - 30 *)

let setMod1Source t value =
   send t.port t.part 0x00 0x34 value

let setMod2Source t value =
   send t.port t.part 0x00 0x35 value

let setMod3Source t value =
   send t.port t.part 0x00 0x36 value

let setMod4Source t value =
   send t.port t.part 0x00 0x37 value

let setMod5Source t value =
   send t.port t.part 0x00 0x38 value

let setMod6Source t value =
   send t.port t.part 0x00 0x39 value

let setMod7Source t value =
   send t.port t.part 0x00 0x3A value

let setMod8Source t value =
   send t.port t.part 0x00 0x3B value

let setMod9Source t value =
   send t.port t.part 0x00 0x3C value

let setMod10Source t value =
   send t.port t.part 0x00 0x3D value

let setMod11Source t value =
   send t.port t.part 0x00 0x3E value

let setMod12Source t value =
   send t.port t.part 0x00 0x3F value

let setMod13Source t value =
   send t.port t.part 0x00 0x40 value

let setMod14Source t value =
   send t.port t.part 0x00 0x41 value

let setMod15Source t value =
   send t.port t.part 0x00 0x42 value

let setMod16Source t value =
   send t.port t.part 0x00 0x43 value

(* Modulation Destinations *)

(* Sources 0 - 79 *)

let setMod1Destination t value =
   send t.port t.part 0x00 0x44 value

let setMod2Destination t value =
   send t.port t.part 0x00 0x45 value

let setMod3Destination t value =
   send t.port t.part 0x00 0x46 value

let setMod4Destination t value =
   send t.port t.part 0x00 0x47 value

let setMod5Destination t value =
   send t.port t.part 0x00 0x48 value

let setMod6Destination t value =
   send t.port t.part 0x00 0x49 value

let setMod7Destination t value =
   send t.port t.part 0x00 0x4A value

let setMod8Destination t value =
   send t.port t.part 0x00 0x4B value

let setMod9Destination t value =
   send t.port t.part 0x00 0x4C value

let setMod10Destination t value =
   send t.port t.part 0x00 0x4D value

let setMod11Destination t value =
   send t.port t.part 0x00 0x4E value

let setMod12Destination t value =
   send t.port t.part 0x00 0x4F value

let setMod13Destination t value =
   send t.port t.part 0x00 0x50 value

let setMod14Destination t value =
   send t.port t.part 0x00 0x51 value

let setMod15Destination t value =
   send t.port t.part 0x00 0x52 value

let setMod16Destination t value =
   send t.port t.part 0x00 0x53 value

(* Modulation Scaling *)

let setMod1Scaling t value =
   send t.port t.part 0x00 0x54 value

let setMod2Scaling t value =
   send t.port t.part 0x00 0x55 value

let setMod3Scaling t value =
   send t.port t.part 0x00 0x56 value

let setMod4Scaling t value =
   send t.port t.part 0x00 0x57 value

let setMod5Scaling t value =
   send t.port t.part 0x00 0x58 value

let setMod6Scaling t value =
   send t.port t.part 0x00 0x59 value

let setMod7Scaling t value =
   send t.port t.part 0x00 0x5A value

let setMod8Scaling t value =
   send t.port t.part 0x00 0x5B value

let setMod9Scaling t value =
   send t.port t.part 0x00 0x5C value

let setMod10Scaling t value =
   send t.port t.part 0x00 0x5D value

let setMod11Scaling t value =
   send t.port t.part 0x00 0x5E value

let setMod12Scaling t value =
   send t.port t.part 0x00 0x5F value

let setMod13Scaling t value =
   send t.port t.part 0x00 0x60 value

let setMod14Scaling t value =
   send t.port t.part 0x00 0x61 value

let setMod15Scaling t value =
   send t.port t.part 0x00 0x62 value

let setMod16Scaling t value =
   send t.port t.part 0x00 0x63 value

(* Mix *)

let setOsc1Level t value =
   send t.port t.part 0x00 0x64 value

let setOsc2Level t value =
   send t.port t.part 0x00 0x65 value

let setOsc3Level t value =
   send t.port t.part 0x00 0x66 value

let setExtLevel t value =
   send t.port t.part 0x00 0x67 value

let setExtInput t value = (* 0 - 6 *)
   send t.port t.part 0x00 0x68 value

(* Filter *)

let setFilterType t value = (* 0 - 7 *)
   send t.port t.part 0x00 0x69 value

let setCutoff t value =
   send t.port t.part 0x00 0x6A value;
   send t.port t.part 0x00 0x6B value

let setResonance t value =
   send t.port t.part 0x00 0x6C value

(* Transpose *)

let setCoarse t value =
   send t.port t.part 0x00 0x6D value

let setFine t value =
   send t.port t.part 0x00 0x6E value

let setVoiceMode t value = (* 0 - 1 *)
   send t.port t.part 0x00 0x6F value

let setUnisonMode t value = (* 0 - 1 *)
   send t.port t.part 0x00 0x70 value

let setUnisonCount t value = (* 2 - 12 *)
   send t.port t.part 0x00 0x71 value

let setUnisonDetune t value =
   send t.port t.part 0x00 0x72 value

(* Channel Strip *)

let setChannelVolume t value =
   send t.port t.part 0x00 0x73 value

let setChannelPan t value =
   send t.port t.part 0x00 0x74 value

let setChannelDirect t value =
   send t.port t.part 0x00 0x75 value

let setChannelAux1 t value =
   send t.port t.part 0x00 0x76 value

let setChannelAux2 t value =
   send t.port t.part 0x00 0x77 value

let setChannelFxType t value = (* 0 - 5 *)
   send t.port t.part 0x00 0x78 value

(* Eq Low High *)

let setLowFreq t value =
   send t.port t.part 0x00 0x79 value

let setLowGain t value =
   send t.port t.part 0x00 0x7A value

let setHighFreq t value =
   send t.port t.part 0x00 0x7B value

let setHighGain t value =
   send t.port t.part 0x00 0x7C value

(* Tremolo *)

let setTremoloWave t value = (* 0 - 4 *)
   send t.port t.part 0x00 0x7D value

let setTremoloRate t value =
   send t.port t.part 0x00 0x7E value

let setTremoloVolDepth t value =
   send t.port t.part 0x00 0x7F value

let setTremoloPanDepth t value =
   send t.port t.part 0x01 0x00 value

(* Auto Wha*)

let setAutoWhaType t value = (* 0 - 1 *)
   send t.port t.part 0x01 0x01 value

let setAutoWhaCutoff t value =
   send t.port t.part 0x01 0x02 value

let setAutoWhaResonance t value =
   send t.port t.part 0x01 0x03 value

let setAutoWhaSensitivity t value =
   send t.port t.part 0x01 0x04 value

(* Compressor *)

let setCompAttack t value =
   send t.port t.part 0x01 0x05 value

let setCompRelease t value =
   send t.port t.part 0x01 0x06 value

let setCompThreshold t value =
   send t.port t.part 0x01 0x07 value

let setCompRatio t value =
   send t.port t.part 0x01 0x08 value

let setCompGain t value =
   send t.port t.part 0x01 0x09 value

(* Distortion *)

let setDistType t value =
   send t.port t.part 0x01 0x0A value

let setDistDepth t value =
   send t.port t.part 0x01 0x0B value

let setDistPreGain t value =
   send t.port t.part 0x01 0x0C value

let setDistPostGain t value =
   send t.port t.part 0x01 0x0D value

let setDistHighCut t value =
   send t.port t.part 0x01 0x0E value

(* Band pass *)

let setBandPassMidFreq t value =
   send t.port t.part 0x01 0x0F value

let setBandPassMidGain t value =
   send t.port t.part 0x01 0x10 value

let setBandPassMidQ t value =
   send t.port t.part 0x01 0x11 value

(* Reducer *)

let setReducerBitDepth t value =
   send t.port t.part 0x01 0x12 value

let setReducerSampleRate t value =
   send t.port t.part 0x01 0x13 value

(* Aux1 Reverb *)

let setReverbMode t value =
   send t.port t.part 0x01 0x14 value

let setReverbType t value =
   send t.port t.part 0x01 0x15 value

let setReverbDepth t value =
   send t.port t.part 0x01 0x16 value

let setReverbPreHP t value =
   send t.port t.part 0x01 0x17 value

let setReverbPreDelay t value =
   send t.port t.part 0x01 0x18 value

let setReverbHighDamp t value =
   send t.port t.part 0x01 0x19 value

let setReverbTime t value =
   send t.port t.part 0x01 0x1A value

let setReverbEchoFeedback t value =
   send t.port t.part 0x01 0x1B value

let setReverbGateDelayTime t value =
   send t.port t.part 0x01 0x1C value

let setReverbGateThreshold t value =
   send t.port t.part 0x01 0x1D value

let setReverbToneGuide t value =
   send t.port t.part 0x01 0x1E value

let setReverbToneFreq t value =
   send t.port t.part 0x01 0x1F value

(* Aux 2 *)

let setDelayMode t value =
   send t.port t.part 0x01 0x20 value

let setDelayType t value =
   send t.port t.part 0x01 0x21 value

let setDelayDepth t value =
   send t.port t.part 0x01 0x22 value

let setDelayToAux1 t value =
   send t.port t.part 0x01 0x23 value

let setDelayPreHP t value =
   send t.port t.part 0x01 0x24 value

let setDelayPreLP t value =
   send t.port t.part 0x01 0x25 value

let setDelayTime t value =
   send t.port t.part 0x01 0x26 value

let setDelayFeedback t value =
   send t.port t.part 0x01 0x27 value

let setDelayHighDamp t value =
   send t.port t.part 0x01 0x28 value

let setDelayLFORate t value =
   send t.port t.part 0x01 0x29 value

let setDelayLFODepth t value =
   send t.port t.part 0x01 0x2A value

(* Mixer *)

let setDelayLFODepth t value =
   send t.port t.part 0x01 0x2B value

(* Master EQ *)

let setMasterEQLowFreq t value =
   send t.port t.part 0x01 0x2C value

let setMasterEQLowGain t value =
   send t.port t.part 0x01 0x2D value

let setMasterEQMidFreq t value =
   send t.port t.part 0x01 0x2E value

let setMasterEQMidGain t value =
   send t.port t.part 0x01 0x2F value

let setMasterEQHighFreq t value =
   send t.port t.part 0x01 0x30 value

let setMasterEQHighGain t value =
   send t.port t.part 0x01 0x31 value


(* Multi configuration *)

let setPart1Channel t value =
   send t.port (partCode Multi) 0x00 0x1B value

let setPart1KeyLow t value =
   send t.port (partCode Multi) 0x00 0x1C value

let setPart1KeyHigh t value =
   send t.port (partCode Multi) 0x00 0x1D value

let setPart1VelLow t value =
   send t.port (partCode Multi) 0x00 0x1E value

let setPart1VelHigh t value =
   send t.port (partCode Multi) 0x00 0x1F value

let setPart1ControlEnable t value = (* 0 -> all on *)
   send t.port (partCode Multi) 0x00 0x20 value

let setPart1Bank t value = (* 0 -> 3 *)
   send t.port (partCode Multi) 0x00 0x21 value

let setPart1Program t value = (* 0 -> 127 *)
   send t.port (partCode Multi) 0x00 0x22 value



module Table = Map.Make(String)



let addr1 =
   [
      0x00, "Edit Buffer Dump";
      0x01, "Single Patch Dump";
      0x02, "Multi Patch Dump";
      0x03, "Arpeg Data Dump";
      0x04, "Arpeg Pattern Dump";
      0x05, "Single Name String";
      0x06, "Multi Name String";
      0x07, "Arpeg Name String";
      0x08, "Edit Global Param";
      0x09, "Edit Single Param";
      0x0A, "Edit Multi Param";
      0x0B, "Edit Multi Part1 Param";
      0x0C, "Edit Multi Part2 Param";
      0x0D, "Edit Multi Part3 Param";
      0x0E, "Edit Multi Part4 Param";
      0x0F, "Edit Effect Param";
      0x10, "Edit Arpeg Single Param";
      0x11, "Edit Arpeg Part1 Param";
      0x12, "Edit Arpeg Part2 Param";
      0x13, "Edit Arpeg Part3 Param";
      0x14, "Edit Arpeg Part4 Param";
   ]

(*
* MltParam.PartSrc_f [part] is a compilation of 3 bits with the following values
   Bit0: 1 = Flags that ChanStrip params are taken from the Single part.
   Bit1: 2 = Flags that Transpose params are taken from the Single part
   Bit2: 4 = Flags that Arpeg params are taken from the Single part
** KeyMap [part].CtrlEnable_f is a compilation of 6 bits with following values:
  Bit0: 1 = Flags that the Pitchbend wheel is enabled.
  Bit1: 2 = Flags that the Modulation wheel is enabled.
  Bit2: 4 = Flags that the Sustain pedal is enabled.
  Bit3: 8 = Flags that the Expression pedal is enabled.
  Bit4: 16 = Flags that the Keyboard is enabled.
  Bit5: 32 = Flags that the External MIDI Input is enabled.
*** Off/On is determined by evaluating the CC value received. If the CC value is 63 or lower, the state is “Off”. If it is 64 or higher, the state is “On.”

*)
let multi =
   (fun table ->
       table
       |> List.map (fun ((c1,c2),key) -> key, (fun t value -> send t.port (partCode Multi) c1 c2 value ))
       |> List.fold_left (fun s (key,fn) -> Table.add key fn s) Table.empty)
      [
         (0x00, 0x00), "MltParam.PartSrc_f [PART1]"; (* 0~7 * *)
         (0x00, 0x01), "MltParam.PartSrc_f [PART2]"; (* 0~7 * *)
         (0x00, 0x02), "MltParam.PartSrc_f [PART3]"; (* 0~7 * *)
         (0x00, 0x03), "MltParam.PartSrc_f [PART4]"; (* 0~7 * *)
         (0x00, 0x04), "MltParam.Aux1ParamSrc"; (* 0~ 4 *)
         (0x00, 0x05), "MltParam.Aux2ParamSrc"; (* 0~ 4 *)
         (0x00, 0x06), "MltParam.MixerSrc"; (* 0~ 4 *)
         (0x00, 0x07), "MltParam.SaveBank"; (* 0~ 1 *)
         (0x00, 0x08), "MltParam.SavePatch"; (* 0 ~ 127 *)
         (0x00, 0x09), "PartMap [PART1].Enable"; (* Off/On *** *)
         (0x00, 0x0A), "PartMap [PART2].Enable"; (* Off/On *** *)
         (0x00, 0x0B), "PartMap [PART3].Enable"; (* Off/On *** *)
         (0x00, 0x0C), "PartMap [PART4].Enable"; (* Off/On *** *)
         (0x00, 0x0D), "PartMap [PART1].Bank"; (* 0~ 3 *)
         (0x00, 0x0E), "PartMap [PART1].Program"; (* 0 ~ 127 *)
         (0x00, 0x0F), "Transpose [PART1].CoarseTune"; (* 0 ~ 127 *)
         (0x00, 0x10), "Transpose [PART1].FineTune"; (* 0 ~ 127 *)
         (0x00, 0x11), "Transpose [PART1].VoiceMode"; (* 0~ 1 *)
         (0x00, 0x12), "Transpose [PART1].UnisonMode"; (* Off/On *** *)
         (0x00, 0x13), "Transpose [PART1].UnisonCount"; (* 2 ~ 12 *)
         (0x00, 0x14), "Transpose [PART1].UnisonDetune"; (* 0 ~ 127 *)
         (0x00, 0x15), "ChanStrip [PART1].Volume"; (* 0 ~ 127 *)
         (0x00, 0x16), "ChanStrip [PART1].Pan"; (* 0 ~ 127 *)
         (0x00, 0x17), "ChanStrip [PART1].Direct"; (* 0 ~ 127 *)
         (0x00, 0x18), "ChanStrip [PART1].Aux1Send"; (* 0 ~ 127 *)
         (0x00, 0x19), "ChanStrip [PART1].Aux2Send"; (* 0 ~ 127 *)
         (0x00, 0x1A), "ChanStrip [PART1].FX_Type"; (* 0~ 5 *)
         (0x00, 0x1B), "KeyMap [PART1].Channel"; (* 0 ~ 16 *)
         (0x00, 0x1C), "KeyMap [PART1].KeyLow"; (* 0 ~ 127 *)
         (0x00, 0x1D), "KeyMap [PART1].KeyHigh"; (* 0 ~ 127 *)
         (0x00, 0x1E), "KeyMap [PART1].VelLow"; (* 0 ~ 127 *)
         (0x00, 0x1F), "KeyMap [PART1].VelHigh"; (* 0 ~ 127 *)
         (0x00, 0x20), "KeyMap [PART1].CtrlEnable_f"; (* 0 ~ 63 *)
         (0x00, 0x21), "PartMap [PART2].Bank"; (* 0~ 3 *)
         (0x00, 0x22), "PartMap [PART2].Program"; (* 0 ~ 127 *)
         (0x00, 0x23), "Transpose [PART2].CoarseTune"; (* 0 ~ 127 *)
         (0x00, 0x24), "Transpose [PART2].FineTune"; (* 0 ~ 127 *)
         (0x00, 0x25), "Transpose [PART2].VoiceMode"; (* 0~ 1 *)
         (0x00, 0x26), "Transpose [PART2].UnisonMode"; (* Off/On *** *)
         (0x00, 0x27), "Transpose [PART2].UnisonCount"; (* 2 ~ 12 *)
         (0x00, 0x28), "Transpose [PART2].UnisonDetune"; (* 0 ~ 127 *)
         (0x00, 0x29), "ChanStrip [PART2].Volume"; (* 0 ~ 127 *)
         (0x00, 0x2A), "ChanStrip [PART2].Pan"; (* 0 ~ 127 *)
         (0x00, 0x2B), "ChanStrip [PART2].Direct"; (* 0 ~ 127 *)
         (0x00, 0x2C), "ChanStrip [PART2].Aux1Send"; (* 0 ~ 127 *)
         (0x00, 0x2D), "ChanStrip [PART2].Aux2Send"; (* 0 ~ 127 *)
         (0x00, 0x2E), "ChanStrip [PART2].FX_Type"; (* 0~ 5 *)
         (0x00, 0x2F), "KeyMap [PART2].Channel"; (* 0 ~ 16 *)
         (0x00, 0x30), "KeyMap [PART2].KeyLow"; (* 0 ~ 127 *)
         (0x00, 0x31), "KeyMap [PART2].KeyHigh"; (* 0 ~ 127 *)
         (0x00, 0x32), "KeyMap [PART2].VelLow"; (* 0 ~ 127 *)
         (0x00, 0x33), "KeyMap [PART2].VelHigh"; (* 0 ~ 127 *)
         (0x00, 0x34), "KeyMap [PART2].CtrlEnable_f"; (* 0 ~ 63 *)
         (0x00, 0x35), "PartMap [PART3].Bank"; (* 0~ 3 *)
         (0x00, 0x36), "PartMap [PART3].Program"; (* 0 ~ 127 *)
         (0x00, 0x37), "Transpose [PART3].CoarseTune"; (* 0 ~ 127 *)
         (0x00, 0x38), "Transpose [PART3].FineTune"; (* 0 ~ 127 *)
         (0x00, 0x39), "Transpose [PART3].VoiceMode"; (* 0~ 1 *)
         (0x00, 0x3A), "Transpose [PART3].UnisonMode"; (* Off/On *** *)
         (0x00, 0x3B), "Transpose [PART3].UnisonCount"; (* 2 ~ 12 *)
         (0x00, 0x3C), "Transpose [PART3].UnisonDetune"; (* 0 ~ 127 *)
         (0x00, 0x3D), "ChanStrip [PART3].Volume"; (* 0 ~ 127 *)
         (0x00, 0x3E), "ChanStrip [PART3].Pan"; (* 0 ~ 127 *)
         (0x00, 0x3F), "ChanStrip [PART3].Direct"; (* 0 ~ 127 *)
         (0x00, 0x40), "ChanStrip [PART3].Aux1Send"; (* 0 ~ 127 *)
         (0x00, 0x41), "ChanStrip [PART3].Aux2Send"; (* 0 ~ 127 *)
         (0x00, 0x42), "ChanStrip [PART3].FX_Type"; (* 0~ 5 *)
         (0x00, 0x43), "KeyMap [PART3].Channel"; (* 0 ~ 16 *)
         (0x00, 0x44), "KeyMap [PART3].KeyLow"; (* 0 ~ 127 *)
         (0x00, 0x45), "KeyMap [PART3].KeyHigh"; (* 0 ~ 127 *)
         (0x00, 0x46), "KeyMap [PART3].VelLow"; (* 0 ~ 127 *)
         (0x00, 0x47), "KeyMap [PART3].VelHigh"; (* 0 ~ 127 *)
         (0x00, 0x48), "KeyMap [PART3].CtrlEnable_f"; (* 0 ~ 63 *)
         (0x00, 0x49), "PartMap [PART4].Bank"; (* 0~ 3 *)
         (0x00, 0x4A), "PartMap [PART4].Program"; (* 0 ~ 127 *)
         (0x00, 0x4B), "Transpose [PART4].CoarseTune"; (* 0 ~ 127 *)
         (0x00, 0x4C), "Transpose [PART4].FineTune"; (* 0 ~ 127 *)
         (0x00, 0x4D), "Transpose [PART4].VoiceMode"; (* 0~ 1 *)
         (0x00, 0x4E), "Transpose [PART4].UnisonMode"; (* Off/On *** *)
         (0x00, 0x4F), "Transpose [PART4].UnisonCount"; (* 2 ~ 12 *)
         (0x00, 0x50), "Transpose [PART4].UnisonDetune"; (* 0 ~ 127 *)
         (0x00, 0x51), "ChanStrip [PART4].Volume"; (* 0 ~ 127 *)
         (0x00, 0x52), "ChanStrip [PART4].Pan"; (* 0 ~ 127 *)
         (0x00, 0x53), "ChanStrip [PART4].Direct"; (* 0 ~ 127 *)
         (0x00, 0x54), "ChanStrip [PART4].Aux1Send"; (* 0 ~ 127 *)
         (0x00, 0x55), "ChanStrip [PART4].Aux2Send"; (* 0 ~ 127 *)
         (0x00, 0x56), "ChanStrip [PART4].FX_Type"; (* 0~ 5 *)
         (0x00, 0x57), "KeyMap [PART4].Channel"; (* 0 ~ 16 *)
         (0x00, 0x58), "KeyMap [PART4].KeyLow"; (* 0 ~ 127 *)
         (0x00, 0x59), "KeyMap [PART4].KeyHigh"; (* 0 ~ 127 *)
         (0x00, 0x5A), "KeyMap [PART4].VelLow"; (* 0 ~ 127 *)
         (0x00, 0x5B), "KeyMap [PART4].VelHigh"; (* 0 ~ 127 *)
         (0x00, 0x5C), "KeyMap [PART4].CtrlEnable_f"; (* 0 ~ 63 *)
         (0x00, 0x5D), "Aux1Reverb.Mode"; (* Off/On *** *)
         (0x00, 0x5E), "Aux1Reverb.Type"; (* 0~ 1 *)
         (0x00, 0x5F), "Aux1Reverb.Depth"; (* 0 ~ 127 *)
         (0x00, 0x60), "Aux1Reverb.PreHP"; (* 0 ~ 127 *)
         (0x00, 0x61), "Aux1Reverb.PreDelay"; (* 0 ~ 127 *)
         (0x00, 0x62), "Aux1Reverb.HighDamp"; (* 0 ~ 127 *)
         (0x00, 0x63), "Aux1Reverb.Time"; (* 0 ~ 127 *)
         (0x00, 0x64), "Aux1Reverb.EchoFeedback"; (* 0 ~ 127 *)
         (0x00, 0x65), "Aux1Reverb.GateDelayTime"; (* 0 ~ 123 *)
         (0x00, 0x66), "Aux1Reverb.GateThresh"; (* 0 ~ 127 *)
         (0x00, 0x67), "Aux1Reverb.ToneGain"; (* 0 ~ 127 *)
         (0x00, 0x68), "Aux1Reverb.ToneFreq"; (* 0 ~ 127 *)
         (0x00, 0x69), "Aux2Delay.Mode"; (* Off/On *** *)
         (0x00, 0x6A), "Aux2Delay.Type"; (* 0~ 4 *)
         (0x00, 0x6B), "Aux2Delay.Depth"; (* 0 ~ 127 *)
         (0x00, 0x6C), "Aux2Delay.ToAux1"; (* 0 ~ 127 *)
         (0x00, 0x6D), "Aux2Delay.PreHP"; (* 0 ~ 127 *)
         (0x00, 0x6E), "Aux2Delay.PreLP"; (* 0 ~ 127 *)
         (0x00, 0x6F), "Aux2Delay.Time"; (* 0 ~ 127 *)
         (0x00, 0x70), "Aux2Delay.Feedback"; (* 0 ~ 127 *)
         (0x00, 0x71), "Aux2Delay.HighDamp"; (* 0 ~ 127 *)
         (0x00, 0x72), "Aux2Delay.LfoRate"; (* 0 ~ 123 *)
         (0x00, 0x73), "Aux2Delay.LfoDepth"; (* 0 ~ 127 *)
         (0x00, 0x74), "Mixer.ProgVolume"; (* 0 ~ 127 *)
         (0x00, 0x75), "MasterEQ.LowFreq"; (* 0 ~ 127 *)
         (0x00, 0x76), "MasterEQ.LowGain"; (* 0 ~ 127 *)
         (0x00, 0x77), "MasterEQ.MidFreq"; (* 0 ~ 127 *)
         (0x00, 0x78), "MasterEQ.MidGain"; (* 0 ~ 127 *)
         (0x00, 0x79), "MasterEQ.HighFreq"; (* 0 ~ 127 *)
         (0x00, 0x7A), "MasterEQ.HighGain"; (* 0 ~ 127 *)
         (0x00, 0x7B), "ArpPatch [PART1].Enable"; (* Off/On *** *)
         (0x00, 0x7C), "ArpPatch [PART1].ArpSrc_f"; (* Off/On *** *)
         (0x00, 0x7D), "ArpPatch [PART1].Bank"; (* 0~ 1 *)
         (0x00, 0x7E), "ArpPatch [PART1].Pattern"; (* 0 ~ 127*)
         (0x00, 0x7F), "ArpData [PART1].ArpMode"; (* 0~ 2*)
         (0x01, 0x00), "ArpData [PART1].NoteOrder"; (* 0~ 4 *)
         (0x01, 0x01), "ArpData [PART1].OctaveRange"; (* -4 ~ +4 *)
         (0x01, 0x02), "ArpData [PART1].BiPolar"; (* Off/On *** *)
         (0x01, 0x03), "ArpData [PART1].LatchKeys"; (* Off/On *** *)
         (0x01, 0x04), "ArpData [PART1].RootNote"; (* 0 ~ 127 *)
         (0x01, 0x05), "ArpPatch [PART2].Enable"; (* Off/On *** *)
         (0x01, 0x06), "ArpPatch [PART2].ArpSrc_f"; (* Off/On *** *)
         (0x01, 0x07), "ArpPatch [PART2].Bank"; (* 0~ 1 *)
         (0x01, 0x08), "ArpPatch [PART2].Pattern"; (* 0 ~ 127 *)
         (0x01, 0x09), "ArpData [PART2].ArpMode"; (* 0~ 2 *)
         (0x01, 0x0A), "ArpData [PART2].NoteOrder"; (* 0~ 4 *)
         (0x01, 0x0B), "ArpData [PART2].OctaveRange"; (* -4 ~ +4 *)
         (0x01, 0x0C), "ArpData [PART2].BiPolar"; (* Off/On *** *)
         (0x01, 0x0D), "ArpData [PART2].LatchKeys"; (* Off/On *** *)
         (0x01, 0x0E), "ArpData [PART2].RootNote"; (* 0 ~ 127 *)
         (0x01, 0x0F), "ArpPatch [PART3].Enable"; (* Off/On *** *)
         (0x01, 0x10), "ArpPatch [PART3].ArpSrc_f"; (* Off/On *** *)
         (0x01, 0x11), "ArpPatch [PART3].Bank"; (* 0~ 1 *)
         (0x01, 0x12), "ArpPatch [PART3].Pattern"; (* 0 ~ 127 *)
         (0x01, 0x13), "ArpData [PART3].ArpMode"; (* 0~ 2 *)
         (0x01, 0x14), "ArpData [PART3].NoteOrder"; (* 0~ 4 *)
         (0x01, 0x15), "ArpData [PART3].OctaveRange"; (* -4 ~ +4 *)
         (0x01, 0x16), "ArpData [PART3].BiPolar"; (* Off/On *** *)
         (0x01, 0x17), "ArpData [PART3].LatchKeys"; (* Off/On *** *)
         (0x01, 0x18), "ArpData [PART3].RootNote"; (* 0 ~ 127 *)
         (0x01, 0x19), "ArpPatch [PART4].Enable"; (* Off/On *** *)
         (0x01, 0x1A), "ArpPatch [PART4].ArpSrc_f"; (* Off/On *** *)
         (0x01, 0x1B), "ArpPatch [PART4].Bank"; (* 0~ 1 *)
         (0x01, 0x1C), "ArpPatch [PART4].Pattern"; (* 0 ~ 127 *)
         (0x01, 0x1D), "ArpData [PART4].ArpMode"; (* 0~ 2 *)
         (0x01, 0x1E), "ArpData [PART4].NoteOrder"; (* 0~ 4 *)
         (0x01, 0x1F), "ArpData [PART4].OctaveRange"; (* -4 ~ +4 *)
         (0x01, 0x20), "ArpData [PART4].BiPolar"; (* Off/On *** *)
         (0x01, 0x21), "ArpData [PART4].LatchKeys"; (* Off/On *** *)
         (0x01, 0x22), "ArpData [PART4].RootNote"; (* 0 ~ 127 *)
         (0x01, 0x23), "PatchName [0]"; (* 0 ~ 127 *)
         (0x01, 0x24), "PatchName [1]"; (* 0 ~ 127 *)
         (0x01, 0x25), "PatchName [2]"; (* 0 ~ 127 *)
         (0x01, 0x26), "PatchName [3]"; (* 0 ~ 127 *)
         (0x01, 0x27), "PatchName [4]"; (* 0 ~ 127 *)
         (0x01, 0x28), "PatchName [5]"; (* 0 ~ 127 *)
         (0x01, 0x29), "PatchName [6]"; (* 0 ~ 127 *)
         (0x01, 0x2A), "PatchName [7]"; (* 0 ~ 127 *)
         (0x01, 0x2B), "PatchName [8]"; (* 0 ~ 127 *)
         (0x01, 0x2C), "PatchName [9]"; (* 0 ~ 127 *)
      ]

let single =
   (fun table ->
       table
       |> List.map (fun ((c1,c2),key) -> key, (fun t part value -> send t.port (partCode part) c1 c2 value ))
       |> List.fold_left (fun s (key,fn) -> Table.add key fn s) Table.empty)
      [
         (0x00, 0x00), "SglParam.GlideMode"; (* 0 ~ 127 *)
         (0x00, 0x01), "SglParam.GlideTime"; (* 0 ~ 127 *)
         (0x00, 0x02), "SglParam.SaveBank"; (* 0~ 1 *)
         (0x00, 0x03), "SglParam.SavePatch"; (* 0 ~ 127 *)
         (0x00, 0x04), "Envelope [ENV1].Attack"; (* 0 ~ 127 *)
         (0x00, 0x05), "Envelope [ENV1].Hold"; (* 0 ~ 127 *)
         (0x00, 0x06), "Envelope [ENV1].Decay"; (* 0 ~ 127 *)
         (0x00, 0x07), "Envelope [ENV1].Sustain"; (* 0 ~ 127 *)
         (0x00, 0x08), "Envelope [ENV1].Release"; (* 0 ~ 127 *)
         (0x00, 0x09), "Envelope [ENV2].Attack"; (* 0 ~ 127 *)
         (0x00, 0x0A), "Envelope [ENV2].Hold"; (* 0 ~ 127 *)
         (0x00, 0x0B), "Envelope [ENV2].Decay"; (* 0 ~ 127 *)
         (0x00, 0x0C), "Envelope [ENV2].Sustain"; (* 0 ~ 127 *)
         (0x00, 0x0D), "Envelope [ENV2].Release"; (* 0 ~ 127 *)
         (0x00, 0x0E), "Envelope [ENV3].Attack"; (* 0 ~ 127 *)
         (0x00, 0x0F), "Envelope [ENV3].Hold"; (* 0 ~ 127 *)
         (0x00, 0x10), "Envelope [ENV3].Decay"; (* 0 ~ 127 *)
         (0x00, 0x11), "Envelope [ENV3].Sustain"; (* 0 ~ 127 *)
         (0x00, 0x12), "Envelope [ENV3].Release"; (* 0 ~ 127 *)
         (0x00, 0x13), "OscMisc.StartMod"; (* 0 ~ 127 *)
         (0x00, 0x14), "OscMisc.OscDrift"; (* 0 ~ 127 *)
         (0x00, 0x15), "OscMisc.BendRange"; (* 0 ~ 127 *)
         (0x00, 0x16), "OscMisc.RingMod"; (* 0 ~ 127 *)
         (0x00, 0x17), "OscMisc.FM_Level"; (* 0 ~ 127 *)
         (0x00, 0x18), "OscMisc.OscFlags"; (* 0 ~ 127 ** *)
         (0x00, 0x19), "OscMisc.WaveShapeWidth"; (* 0 ~ 127 *)
         (0x00, 0x1A), "Oscillator [OSC1].Waveform"; (* 0 ~ 127 *)
         (0x00, 0x1B), "Oscillator [OSC1].CoarseTune"; (* 0 ~ 127 *)
         (0x00, 0x1C), "Oscillator [OSC1].FineTune"; (* 0 ~ 127 *)
         (0x00, 0x1D), "Oscillator [OSC2].Waveform"; (* 0 ~ 127 *)
         (0x00, 0x1E), "Oscillator [OSC2].CoarseTune"; (* 0 ~ 127 *)
         (0x00, 0x1F), "Oscillator [OSC2].FineTune"; (* 0 ~ 127 *)
         (0x00, 0x20), "PreMixer.Boost"; (* 0 ~ 127 *)
         (0x00, 0x21), "Oscillator [OSC3].Waveform"; (* 0 ~ 127 *)
         (0x00, 0x22), "Oscillator [OSC3].CoarseTune"; (* 0 ~ 127 *)
         (0x00, 0x23), "Oscillator [OSC3].FineTune"; (* 0 ~ 127 *)
         (0x00, 0x24), "Reserved.Data2"; (* 0 ~ 127 *)
         (0x00, 0x25), "Lfo [LFO1].Waveform"; (* 0~ 7 *)
         (0x00, 0x26), "Lfo [LFO1].Rate"; (* 0 ~ 123 *)
         (0x00, 0x27), "Lfo [LFO1].Delay"; (* 0 ~ 127 *)
         (0x00, 0x28), "Lfo [LFO1].Attack"; (* 0 ~ 127 *)
         (0x00, 0x29), "Lfo [LFO1].StartPhase"; (* 0 ~ 127 *)
         (0x00, 0x2A), "Lfo [LFO2].Waveform"; (* 0~ 7 *)
         (0x00, 0x2B), "Lfo [LFO2].Rate"; (* 0 ~ 123 *)
         (0x00, 0x2C), "Lfo [LFO2].Delay"; (* 0 ~ 127 *)
         (0x00, 0x2D), "Lfo [LFO2].Attack"; (* 0 ~ 127 *)
         (0x00, 0x2E), "Lfo [LFO2].StartPhase"; (* 0 ~ 127 *)
         (0x00, 0x2F), "Lfo [LFO3].Waveform"; (* 0~ 7 *)
         (0x00, 0x30), "Lfo [LFO3].Rate"; (* 0 ~ 123 *)
         (0x00, 0x31), "Lfo [LFO3].Delay"; (* 0 ~ 127 *)
         (0x00, 0x32), "Lfo [LFO3].Attack"; (* 0 ~ 127 *)
         (0x00, 0x33), "Lfo [LFO3].StartPhase"; (* 0 ~ 127 *)
         (0x00, 0x34), "ModRoute [MOD1].Source"; (* 0 ~ 30 *)
         (0x00, 0x35), "ModRoute [MOD2].Source"; (* 0 ~ 30 *)
         (0x00, 0x36), "ModRoute [MOD3].Source"; (* 0 ~ 30 *)
         (0x00, 0x37), "ModRoute [MOD4].Source"; (* 0 ~ 30 *)
         (0x00, 0x38), "ModRoute [MOD5].Source"; (* 0 ~ 30 *)
         (0x00, 0x39), "ModRoute [MOD6].Source"; (* 0 ~ 30 *)
         (0x00, 0x3A), "ModRoute [MOD7].Source"; (* 0 ~ 30 *)
         (0x00, 0x3B), "ModRoute [MOD8].Source"; (* 0 ~ 30 *)
         (0x00, 0x3C), "ModRoute [MOD9].Source"; (* 0 ~ 30 *)
         (0x00, 0x3D), "ModRoute [MOD10].Source"; (* 0 ~ 30 *)
         (0x00, 0x3E), "ModRoute [MOD11].Source"; (* 0 ~ 30 *)
         (0x00, 0x3F), "ModRoute [MOD12].Source"; (* 0 ~ 30 *)
         (0x00, 0x40), "ModRoute [MOD13].Source"; (* 0 ~ 30 *)
         (0x00, 0x41), "ModRoute [MOD14].Source"; (* 0 ~ 30 *)
         (0x00, 0x42), "ModRoute [MOD15].Source"; (* 0 ~ 30 *)
         (0x00, 0x43), "ModRoute [MOD16].Source"; (* 0 ~ 30 *)
         (0x00, 0x44), "ModRoute [MOD1].Destination"; (* 0 ~ 79 *)
         (0x00, 0x45), "ModRoute [MOD2].Destination"; (* 0 ~ 79 *)
         (0x00, 0x46), "ModRoute [MOD3].Destination"; (* 0 ~ 79 *)
         (0x00, 0x47), "ModRoute [MOD4].Destination"; (* 0 ~ 79 *)
         (0x00, 0x48), "ModRoute [MOD5].Destination"; (* 0 ~ 79 *)
         (0x00, 0x49), "ModRoute [MOD6].Destination"; (* 0 ~ 79 *)
         (0x00, 0x4A), "ModRoute [MOD7].Destination"; (* 0 ~ 79 *)
         (0x00, 0x4B), "ModRoute [MOD8].Destination"; (* 0 ~ 79 *)
         (0x00, 0x4C), "ModRoute [MOD9].Destination"; (* 0 ~ 79 *)
         (0x00, 0x4D), "ModRoute [MOD10].Destination"; (* 0 ~ 79 *)
         (0x00, 0x4E), "ModRoute [MOD11].Destination"; (* 0 ~ 79 *)
         (0x00, 0x4F), "ModRoute [MOD12].Destination"; (* 0 ~ 79 *)
         (0x00, 0x50), "ModRoute [MOD13].Destination"; (* 0 ~ 79 *)
         (0x00, 0x51), "ModRoute [MOD14].Destination"; (* 0 ~ 79 *)
         (0x00, 0x52), "ModRoute [MOD15].Destination"; (* 0 ~ 79 *)
         (0x00, 0x53), "ModRoute [MOD16].Destination"; (* 0 ~ 79 *)
         (0x00, 0x54), "ModRoute [MOD1].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x55), "ModRoute [MOD2].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x56), "ModRoute [MOD3].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x57), "ModRoute [MOD4].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x58), "ModRoute [MOD5].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x59), "ModRoute [MOD6].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x5A), "ModRoute [MOD7].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x5B), "ModRoute [MOD8].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x5C), "ModRoute [MOD9].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x5D), "ModRoute [MOD10].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x5E), "ModRoute [MOD11].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x5F), "ModRoute [MOD12].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x60), "ModRoute [MOD13].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x61), "ModRoute [MOD14].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x62), "ModRoute [MOD15].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x63), "ModRoute [MOD16].Scaling"; (* 0 ~ 127 *)
         (0x00, 0x64), "PreMixer.OscVolume [OSC1]"; (* 0 ~ 127 *)
         (0x00, 0x65), "PreMixer.OscVolume [OSC2]"; (* 0 ~ 127 *)
         (0x00, 0x66), "PreMixer.OscVolume [OSC3]"; (* 0 ~ 127 *)
         (0x00, 0x67), "PreMixer.ExtInVolume"; (* 0 ~ 127 *)
         (0x00, 0x68), "PreMixer.ExtInSource"; (* 0~ 6 *)
         (0x00, 0x69), "Filter.Type"; (* 0~ 7 *)
         (0x00, 0x6A), "Filter.Cutoff High"; (* 0 ~ 127 *)
         (0x00, 0x6B), "Filter.Cutoff Low"; (* 0 ~ 127 *)
         (0x00, 0x6C), "Filter.Resonance"; (* 0 ~ 127 *)
         (0x00, 0x6D), "Transpose.CoarseTune"; (* 0 ~ 127 *)
         (0x00, 0x6E), "Transpose.FineTune"; (* 0 ~ 127 *)
         (0x00, 0x6F), "Transpose.VoiceMode"; (* 0~ 1 *)
         (0x00, 0x70), "Transpose.UnisonMode"; (* Off/On *** *)
         (0x00, 0x71), "Transpose.UnisonCount"; (* 2 ~ 12 *)
         (0x00, 0x72), "Transpose.UnisonDetune"; (* 0 ~ 127 *)
         (0x00, 0x73), "ChanStrip.Volume"; (* 0 ~ 127 *)
         (0x00, 0x74), "ChanStrip.Pan"; (* 0 ~ 127 *)
         (0x00, 0x75), "ChanStrip.Direct"; (* 0 ~ 127 *)
         (0x00, 0x76), "ChanStrip.Aux1Send"; (* 0 ~ 127 *)
         (0x00, 0x77), "ChanStrip.Aux2Send"; (* 0 ~ 127 *)
         (0x00, 0x78), "ChanStrip.FX_Type"; (* 0~ 5 *)
         (0x00, 0x79), "HiLoEQ.LowFreq"; (* 0 ~ 127 *)
         (0x00, 0x7A), "HiLoEQ.LowGain"; (* 0 ~ 127 *)
         (0x00, 0x7B), "HiLoEQ.HighFreq"; (* 0 ~ 127 *)
         (0x00, 0x7C), "HiLoEQ.HighGain"; (* 0 ~ 127 *)
         (0x00, 0x7D), "Tremolo.Waveform"; (* 0~ 4 *)
         (0x00, 0x7E), "Tremolo.Rate"; (* 0 ~ 127 *)
         (0x00, 0x7F), "Tremolo.VolDepth"; (* 0 ~ 127 *)
         (0x01, 0x00), "Tremolo.PanDepth"; (* 0 ~ 127 *)
         (0x01, 0x01), "AutoWah.Type"; (* 0~ 1 *)
         (0x01, 0x02), "AutoWah.Cutoff"; (* 0 ~ 127 *)
         (0x01, 0x03), "AutoWah.Resonance"; (* 0 ~ 127 *)
         (0x01, 0x04), "AutoWah.Sensitivity"; (* 0 ~ 127 *)
         (0x01, 0x05), "Compressor.Attack"; (* 0 ~ 127 *)
         (0x01, 0x06), "Compressor.Release"; (* 0 ~ 127 *)
         (0x01, 0x07), "Compressor.Threshold"; (* 0 ~ 127 *)
         (0x01, 0x08), "Compressor.Ratio"; (* 0 ~ 127 *)
         (0x01, 0x09), "Compressor.Gain"; (* 0 ~ 127 *)
         (0x01, 0x0A), "Distortion.Type"; (* 0~ 2 *)
         (0x01, 0x0B), "Distortion.Depth"; (* 0 ~ 127 *)
         (0x01, 0x0C), "Distortion.PreGain"; (* 0 ~ 127 *)
         (0x01, 0x0D), "Distortion.PostGain"; (* 0 ~ 127 *)
         (0x01, 0x0E), "Distortion.HighCutoff"; (* 0 ~ 127 *)
         (0x01, 0x0F), "BandPass.MidFreq"; (* 0 ~ 127 *)
         (0x01, 0x10), "BandPass.MidGain"; (* 0 ~ 127 *)
         (0x01, 0x11), "BandPass.MidQ"; (* 0 ~ 127 *)
         (0x01, 0x12), "Reducer.BitDepth"; (* 0 ~ 12 *)
         (0x01, 0x13), "Reducer.SampleRate"; (* 0 ~ 127 *)
         (0x01, 0x14), "Aux1Reverb.Mode"; (* Off/On *** *)
         (0x01, 0x15), "Aux1Reverb.Type"; (* 0~ 1 *)
         (0x01, 0x16), "Aux1Reverb.Depth"; (* 0 ~ 127 *)
         (0x01, 0x17), "Aux1Reverb.PreHP"; (* 0 ~ 127 *)
         (0x01, 0x18), "Aux1Reverb.PreDelay"; (* 0 ~ 127 *)
         (0x01, 0x19), "Aux1Reverb.HighDamp"; (* 0 ~ 127 *)
         (0x01, 0x1A), "Aux1Reverb.Time"; (* 0 ~ 127 *)
         (0x01, 0x1B), "Aux1Reverb.EchoFeedback"; (* 0 ~ 127 *)
         (0x01, 0x1C), "Aux1Reverb.GateDelayTime"; (* 0 ~ 123 *)
         (0x01, 0x1D), "Aux1Reverb.GateThresh"; (* 0 ~ 127 *)
         (0x01, 0x1E), "Aux1Reverb.ToneGain"; (* 0 ~ 127 *)
         (0x01, 0x1F), "Aux1Reverb.ToneFreq"; (* 0 ~ 127 *)
         (0x01, 0x20), "Aux2Delay.Mode"; (* Off/On *** *)
         (0x01, 0x21), "Aux2Delay.Type"; (* 0~ 4 *)
         (0x01, 0x22), "Aux2Delay.Depth"; (* 0 ~ 127 *)
         (0x01, 0x23), "Aux2Delay.ToAux1"; (* 0 ~ 127 *)
         (0x01, 0x24), "Aux2Delay.PreHP"; (* 0 ~ 127 *)
         (0x01, 0x25), "Aux2Delay.PreLP"; (* 0 ~ 127 *)
         (0x01, 0x26), "Aux2Delay.Time"; (* 0 ~ 127 *)
         (0x01, 0x27), "Aux2Delay.Feedback"; (* 0 ~ 127 *)
         (0x01, 0x28), "Aux2Delay.HighDamp"; (* 0 ~ 127 *)
         (0x01, 0x29), "Aux2Delay.LfoRate"; (* 0 ~ 123 *)
         (0x01, 0x2A), "Aux2Delay.LfoDepth"; (* 0 ~ 127 *)
         (0x01, 0x2B), "Mixer.ProgVolume"; (* 0 ~ 127 *)
         (0x01, 0x2C), "MasterEQ.LowFreq"; (* 0 ~ 127 *)
         (0x01, 0x2D), "MasterEQ.LowGain"; (* 0 ~ 127 *)
         (0x01, 0x2E), "MasterEQ.MidFreq"; (* 0 ~ 127 *)
         (0x01, 0x2F), "MasterEQ.MidGain"; (* 0 ~ 127 *)
         (0x01, 0x30), "MasterEQ.HighFreq"; (* 0 ~ 127 *)
         (0x01, 0x31), "MasterEQ.HighGain"; (* 0 ~ 127 *)
         (0x01, 0x32), "ArpPatch.Enable"; (* Off/On *** *)
         (0x01, 0x33), "ArpPatch.ArpSrc_f"; (* Off/On *** *)
         (0x01, 0x34), "ArpPatch.Bank"; (* 0~ 1 *)
         (0x01, 0x35), "ArpPatch.Pattern"; (* 0 ~ 127 *)
         (0x01, 0x36), "ArpData.ArpMode"; (* 0~ 2 *)
         (0x01, 0x37), "ArpData.NoteOrder"; (* 0~ 4 *)
         (0x01, 0x38), "ArpData.OctaveRange"; (* -4 ~ +4 *)
         (0x01, 0x39), "ArpData.BiPolar"; (* Off/On *** *)
         (0x01, 0x3A), "ArpData.LatchKeys"; (* Off/On *** *)
         (0x01, 0x3B), "ArpData.RootNote"; (* 0 ~ 127 *)
         (0x01, 0x3C), "PatchName [0]"; (* 0 ~ 127 *)
         (0x01, 0x3D), "PatchName [1]"; (* 0 ~ 127 *)
         (0x01, 0x3E), "PatchName [2]"; (* 0 ~ 127 *)
         (0x01, 0x3F), "PatchName [3]"; (* 0 ~ 127 *)
         (0x01, 0x40), "PatchName [4]"; (* 0 ~ 127 *)
         (0x01, 0x41), "PatchName [5]"; (* 0 ~ 127 *)
         (0x01, 0x42), "PatchName [6]"; (* 0 ~ 127 *)
         (0x01, 0x43), "PatchName [7]"; (* 0 ~ 127 *)
         (0x01, 0x44), "PatchName [8]"; (* 0 ~ 127 *)
         (0x01, 0x45), "PatchName [9]"; (* 0 ~ 127 *)
      ]
