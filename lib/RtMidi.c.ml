open Ctypes

let%c () = header {|
#include "rtmidi/rtmidi_c.h"
|}

type%c rtMidiApi =
  | RTMIDI_API_UNSPECIFIED (*!< Search for a working compiled API. *)
  | RTMIDI_API_MACOSX_CORE (*!< Macintosh OS-X CoreMIDI API. *)
  | RTMIDI_API_LINUX_ALSA (*!< The Advanced Linux Sound Architecture API. *)
  | RTMIDI_API_UNIX_JACK (*!< The Jack Low-Latency MIDI Server API. *)
  | RTMIDI_API_WINDOWS_MM (*!< The Microsoft Multimedia MIDI API. *)
  | RTMIDI_API_RTMIDI_DUMMY (*!< A compilable but non-functional API. *)
[@@cname "RtMidiApi"]

type%c rtMidiErrorType =
  | RTMIDI_ERROR_WARNING (*!< A non-critical error. *)
  | RTMIDI_ERROR_DEBUG_WARNING (*!< A non-critical error which might be useful for debugging. *)
  | RTMIDI_ERROR_UNSPECIFIED (*!< The default, unspecified error type. *)
  | RTMIDI_ERROR_NO_DEVICES_FOUND (*!< No devices found on system. *)
  | RTMIDI_ERROR_INVALID_DEVICE (*!< An invalid device ID was specified. *)
  | RTMIDI_ERROR_MEMORY_ERROR (*!< An error occured during memory allocation. *)
  | RTMIDI_ERROR_INVALID_PARAMETER (*!< An invalid parameter was specified to a function. *)
  | RTMIDI_ERROR_INVALID_USE (*!< The function was called incorrectly. *)
  | RTMIDI_ERROR_DRIVER_ERROR (*!< A system driver error occured. *)
  | RTMIDI_ERROR_SYSTEM_ERROR (*!< A system error occured. *)
[@@cname "RtMidiErrorType"]

type midi_in = unit ptr

type midi_out = unit ptr

type midi = unit ptr

let%c midi_in : midi_in typ = ptr void

let%c midi_out : midi_out typ = ptr void

let%c midi : midi typ = ptr void

external rtmidi_in_create_default : void -> midi = "rtmidi_in_create_default"

(* rtmidi_in_create api clientName queueSizeLimit *)
external rtmidi_in_create : rtMidiApi -> string -> int -> midi_in = "rtmidi_in_create"

(* rtmidi_get_port_count port *)
external rtmidi_get_port_count : midi -> int = "rtmidi_get_port_count"

(* rtmidi_get_port_name device portNumber *)
external rtmidi_get_port_name : midi -> int -> string = "rtmidi_get_port_name"

(* rtmidi_open_port device portNumber portName*)
external rtmidi_open_port : midi -> int -> string -> void = "rtmidi_open_port"

external rtmidi_in_ignore_types :
  midi -> midiSysex:bool -> midiTime:bool -> midiSense:bool -> void
  = "rtmidi_in_ignore_types"

external rtmidi_in_get_message : midi -> char ptr -> size_t ptr -> float = "rtmidi_in_get_message"

external rtmidi_out_create : rtMidiApi -> string -> midi_out = "rtmidi_out_create"

external rtmidi_out_create_default : void -> midi = "rtmidi_out_create_default"

external rtmidi_out_send_message : midi_out -> char ptr -> int -> int = "rtmidi_out_send_message"

external rtmidi_close_port : midi -> void = "rtmidi_close_port"

let list_init (n : int) (f : int -> 'a) : 'a list =
  let rec loop n acc =
    if n < 0 then
      acc
    else
      loop (n - 1) (f n :: acc)
  in
  loop (n - 1) []


let list_find_index (f : 'a -> bool) (l : 'a list) : int option =
  let rec loop index l =
    match l with
    | [] -> None
    | h :: _ when f h -> Some index
    | _ :: t -> loop (index + 1) t
  in
  loop 0 l


let getInputNames () : string list =
  let temp = rtmidi_in_create_default () in
  let n = rtmidi_get_port_count temp in
  let result = list_init n (rtmidi_get_port_name temp) in
  rtmidi_close_port temp ;
  result


let getOutputNames () : string list =
  let temp = rtmidi_out_create_default () in
  let n = rtmidi_get_port_count temp in
  let result = list_init n (rtmidi_get_port_name temp) in
  rtmidi_close_port temp ;
  result


let openInput (name : string) : midi_in option =
  let inputs = getInputNames () in
  match list_find_index (fun port -> port = name) inputs with
  | Some index ->
      print_endline ("Opening input '" ^ name ^ "'") ;
      let input = rtmidi_in_create RTMIDI_API_UNSPECIFIED name index in
      rtmidi_open_port input index name ;
      Some input
  | None -> None


let openOutput (name : string) : midi_out option =
  let outputs = getOutputNames () in
  match list_find_index (fun port -> port = name) outputs with
  | Some index ->
      print_endline ("Opening output '" ^ name ^ "'") ;
      let output = rtmidi_out_create RTMIDI_API_UNSPECIFIED name in
      rtmidi_open_port output index name ;
      Some output
  | None -> None


let closeInput (midi_in : midi_in) = rtmidi_close_port midi_in

let closeOutput (midi_in : midi_in) = rtmidi_close_port midi_in

let convert v =
  try Char.chr v with
  | Invalid_argument _ ->
      print_endline ("Trying to convert " ^ string_of_int v) ;
      '\x00'


let toCharPtr (msg : int list) =
  let count = List.length msg in
  let buffer = CArray.make char ~initial:'\x00' count in
  List.iteri (fun i v -> CArray.set buffer i (convert v)) msg ;
  buffer, count


let sendMessage output msg =
  let buffer, count = toCharPtr msg in
  ignore (rtmidi_out_send_message output (CArray.start buffer) count)


let message_buffer = CArray.make char ~initial:'\x00' 1024

let getMessage input =
  let message_size = allocate size_t (Unsigned.Size_t.of_int 128) in
  let stamp = rtmidi_in_get_message input (CArray.start message_buffer) message_size in
  let got = Unsigned.Size_t.to_int !@message_size in
  if got > 0 then
    let data = List.init got (fun i -> Char.code (CArray.get message_buffer i)) in
    (*let () = print_endline (String.concat "," (List.map string_of_int data)) in*)
    Some (stamp, data)
  else
    None


let sendMessageList (output : midi_out) (msg : int list) : unit = sendMessage output msg
