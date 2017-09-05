
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

let setLFO1Wave t value =
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

let setLFO2Wave t value =
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

let setLFO3Wave t value =
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

let setExtInput t value =
   send t.port t.part 0x00 0x68 value

(* Filter *)

let setFilterType t value =
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

let setVoiceMode t value =
   send t.port t.part 0x00 0x6F value

let setUnisonMode t value =
   send t.port t.part 0x00 0x70 value

let setUnisonCount t value =
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

let setChannelFxType t value =
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

let setTremoloWave t value =
   send t.port t.part 0x00 0x7D value

let setTremoloRate t value =
   send t.port t.part 0x00 0x7E value

let setTremoloVolDepth t value =
   send t.port t.part 0x00 0x7F value

let setTremoloPanDepth t value =
   send t.port t.part 0x01 0x00 value

(* Auto Wha*)

let setAutoWhaType t value =
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

