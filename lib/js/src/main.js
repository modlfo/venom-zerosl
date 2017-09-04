// Generated by BUCKLESCRIPT VERSION 1.9.1, PLEASE EDIT WITH CARE
'use strict';

var Zero       = require("./zero.js");
var $$Array    = require("bs-platform/lib/js/array.js");
var Venom      = require("./venom.js");
var Remidi     = require("./remidi.js");
var $$String   = require("bs-platform/lib/js/string.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");

function showPorts() {
  console.log($$String.concat("\n", Remidi.getOutputNames(/* () */0)));
  return /* () */0;
}

function makeBotton(index) {
  return /* tuple */[
          /* Left */0,
          /* Bottom */1,
          index + 1 | 0
        ];
}

var lfo_waves = /* array */[
  "Sine",
  "Sine+",
  "Triangle",
  "Sawtooth",
  "Square",
  "S&H",
  "LinS&H",
  "LogS&H",
  "ExpSqr",
  "LogSqr",
  "LogSaw",
  "ExpSaw"
];

var osc_waves = /* array */[
  "HP Sine",
  "PB Sine",
  "RP Sine",
  "SH Tri",
  "MG Tri",
  "RP Tri",
  "PB Saw",
  "SH Saw",
  "MG Saw",
  "OB Saw",
  "JX Saw",
  "RP Saw",
  "MS Saw",
  "PB Square",
  "SH Square",
  "MG Square",
  "OB Square",
  "JX Square",
  "RP Square",
  "MS Square",
  "AL Pulse",
  "MG Pulse",
  "MG Sync",
  "SH Sync",
  "JX Sync",
  "BitWave1",
  "BitWave2",
  "BitWave3",
  "AL FM Wave",
  "DP X Wave",
  "RP FM Wave",
  "AL FM Bass",
  "AL FM Quack",
  "AL FM Woody",
  "AL FM Science",
  "AL FM O1",
  "AL FM O2",
  "AL FM Inharmonic",
  "MG White Noise"
];

var groups = /* array */[
  Zero.newGroup("Global", 0, /* true */1, /* array */[Zero.radioButton("Selector", $$Array.init(8, makeBotton), /* array */[
              "Osc1",
              "Osc2",
              "Osc3",
              "Env1",
              "Env2",
              "Env3",
              "LFO1",
              "LFO2"
            ], 1)]),
  Zero.newGroup("Osc1", 1, /* true */1, /* array */[
        Zero.enumEncoder("Wave", 1, osc_waves, 1),
        Zero.enumEncoder("Keytrack", 2, /* array */[
              "Off",
              "On"
            ], 2),
        Zero.numericEncoder("Coarse", 3, -64.0, 63.0, 0.0),
        Zero.numericEncoder("Fine", 4, -64.0, 63.0, 0.0),
        Zero.percentualEncoder("Osc3>FM", 5, 0.0, 1.0, 0.0),
        Zero.percentualEncoder("Waveshape", 6, 0.0, 1.0, 0.0),
        Zero.blank(7),
        Zero.percentualEncoder("Level", 8, 0.0, 1.0, 1.0)
      ]),
  Zero.newGroup("Osc2", 1, /* false */0, /* array */[
        Zero.enumEncoder("Wave", 1, osc_waves, 1),
        Zero.enumEncoder("Keytrack", 2, /* array */[
              "Off",
              "On"
            ], 2),
        Zero.numericEncoder("Coarse", 3, -64.0, 63.0, 0.0),
        Zero.numericEncoder("Fine", 4, -64.0, 63.0, 0.0),
        Zero.enumEncoder("Sync", 5, /* array */[
              "Off",
              "On"
            ], 1),
        Zero.percentualEncoder("StartMod", 6, 0.0, 1.0, 0.0),
        Zero.blank(7),
        Zero.percentualEncoder("Level", 8, 0.0, 1.0, 0.0)
      ]),
  Zero.newGroup("Osc3", 1, /* false */0, /* array */[
        Zero.enumEncoder("Wave", 1, osc_waves, 1),
        Zero.enumEncoder("Keytrack", 2, /* array */[
              "Off",
              "On"
            ], 2),
        Zero.numericEncoder("Coarse", 3, -64.0, 63.0, 0.0),
        Zero.numericEncoder("Fine", 4, -64.0, 63.0, 0.0),
        Zero.enumEncoder("Sync", 5, /* array */[
              "Off",
              "On"
            ], 1),
        Zero.percentualEncoder("Drift", 6, 0.0, 1.0, 0.0),
        Zero.blank(7),
        Zero.percentualEncoder("Level", 8, 0.0, 1.0, 0.0)
      ]),
  Zero.newGroup("Env1", 1, /* false */0, /* array */[
        Zero.numericEncoder("Attack", 1, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Hold", 2, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Decay", 3, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Sustain", 4, 0.0, 1.0, 1.0),
        Zero.numericEncoder("Release", 5, 0.0, 1.0, 0.0),
        Zero.blank(6),
        Zero.blank(7),
        Zero.blank(8)
      ]),
  Zero.newGroup("Env2", 1, /* false */0, /* array */[
        Zero.numericEncoder("Attack", 1, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Hold", 2, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Decay", 3, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Sustain", 4, 0.0, 1.0, 1.0),
        Zero.numericEncoder("Release", 5, 0.0, 1.0, 0.0),
        Zero.blank(6),
        Zero.blank(7),
        Zero.blank(8)
      ]),
  Zero.newGroup("Env3", 1, /* false */0, /* array */[
        Zero.numericEncoder("Attack", 1, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Hold", 2, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Decay", 3, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Sustain", 4, 0.0, 1.0, 1.0),
        Zero.numericEncoder("Release", 5, 0.0, 1.0, 0.0),
        Zero.blank(6),
        Zero.blank(7),
        Zero.blank(8)
      ]),
  Zero.newGroup("LFO1", 1, /* false */0, /* array */[
        Zero.enumEncoder("Wave", 1, lfo_waves, 1),
        Zero.enumEncoder("Sync", 2, /* array */[
              "Off",
              "On"
            ], 1),
        Zero.numericEncoder("Rate", 3, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Delay", 4, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Attack", 5, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Start", 6, 0.0, 1.0, 0.0),
        Zero.blank(7),
        Zero.blank(8)
      ]),
  Zero.newGroup("LFO2", 1, /* false */0, /* array */[
        Zero.enumEncoder("Wave", 1, lfo_waves, 1),
        Zero.enumEncoder("Sync", 2, /* array */[
              "Off",
              "On"
            ], 1),
        Zero.numericEncoder("Rate", 3, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Delay", 4, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Attack", 5, 0.0, 1.0, 0.0),
        Zero.numericEncoder("Start", 6, 0.0, 1.0, 0.0),
        Zero.blank(7),
        Zero.blank(8)
      ]),
  Zero.newGroup("Filter", 3, /* true */1, /* array */[
        Zero.numericKnob("Cutoff", 1, 0.0, 1.0, 0.0),
        Zero.numericKnob("Resonance", 2, 0.0, 1.0, 0.0),
        Zero.enumKnob("Type", 3, /* array */[
              "Off",
              "LP 12",
              "BP 12",
              "HP 12",
              "LP 24",
              "BP 24",
              "HP 24"
            ], 2),
        Zero.numericKnob("Boost", 4, 0.0, 1.0, 0.0)
      ])
];

var match = Venom.openPort("ZeRO MkII Port 1");

var venom = match ? match[0] : (showPorts(/* () */0), Pervasives.failwith("Failed to open the venoms port"));

function osc1Actions(parameter) {
  var match = Zero.paramPath(parameter);
  switch (match) {
    case "/Osc1/Coarse" : 
        Venom.setOsc1Coarse(venom, Zero.paramInt(parameter));
        break;
    case "/Osc1/Fine" : 
        Venom.setOsc1Fine(venom, Zero.paramInt(parameter));
        break;
    case "/Osc1/Keytrack" : 
        Venom.setOsc1Keytrack(venom, Zero.paramInt(parameter));
        break;
    case "/Osc1/Level" : 
        Venom.setOsc1Level(venom, Zero.paramInt(parameter));
        break;
    case "/Osc1/Osc3>FM" : 
        Venom.setOsc1FM3(venom, Zero.paramInt(parameter));
        break;
    case "/Osc1/Wave" : 
        Venom.setOsc1Wave(venom, Zero.paramInt(parameter));
        break;
    case "/Osc1/Waveshape" : 
        Venom.setWaveshape(venom, Zero.paramInt(parameter));
        break;
    default:
      
  }
  return parameter;
}

function osc2Actions(parameter) {
  var match = Zero.paramPath(parameter);
  switch (match) {
    case "/Osc2/Coarse" : 
        Venom.setOsc2Coarse(venom, Zero.paramInt(parameter));
        break;
    case "/Osc2/Fine" : 
        Venom.setOsc2Fine(venom, Zero.paramInt(parameter));
        break;
    case "/Osc2/Keytrack" : 
        Venom.setOsc2Keytrack(venom, Zero.paramInt(parameter));
        break;
    case "/Osc2/Level" : 
        Venom.setOsc2Level(venom, Zero.paramInt(parameter));
        break;
    case "/Osc2/StartMod" : 
        Venom.setStartMod(venom, Zero.paramInt(parameter));
        break;
    case "/Osc2/Sync" : 
        Venom.setOsc2Sync(venom, Zero.paramInt(parameter));
        break;
    case "/Osc2/Wave" : 
        Venom.setOsc2Wave(venom, Zero.paramInt(parameter));
        break;
    default:
      
  }
  return parameter;
}

function filterActions(parameter) {
  var match = Zero.paramPath(parameter);
  switch (match) {
    case "/Filter/Boost" : 
        Venom.setFilterBoost(venom, Zero.paramInt(parameter));
        break;
    case "/Filter/Cutoff" : 
        Venom.setCutoff(venom, Zero.paramInt(parameter));
        break;
    case "/Filter/Resonance" : 
        Venom.setResonance(venom, Zero.paramInt(parameter));
        break;
    case "/Filter/Type" : 
        Venom.setFilterType(venom, Zero.paramInt(parameter));
        break;
    default:
      
  }
  return parameter;
}

function action(groups, parameter) {
  var other = Zero.paramPath(parameter);
  if (other === "/Global/Selector") {
    var active = Zero.paramString(parameter);
    return Zero.groupSetActive(groups, active);
  } else {
    console.log(other + (" " + Pervasives.string_of_int(Zero.paramInt(parameter))));
    osc2Actions(osc1Actions(filterActions(parameter)));
    return groups;
  }
}

function actions(modified, groups) {
  return $$Array.fold_left(action, groups, modified);
}

function test() {
  var match = Zero.openPort("ZeRO MkII Port 2", groups, actions);
  if (match) {
    return /* () */0;
  } else {
    console.error("Failed to open the ports");
    return /* () */0;
  }
}

test(/* () */0);

exports.showPorts     = showPorts;
exports.makeBotton    = makeBotton;
exports.lfo_waves     = lfo_waves;
exports.osc_waves     = osc_waves;
exports.groups        = groups;
exports.venom         = venom;
exports.osc1Actions   = osc1Actions;
exports.osc2Actions   = osc2Actions;
exports.filterActions = filterActions;
exports.action        = action;
exports.actions       = actions;
exports.test          = test;
/* groups Not a pure module */