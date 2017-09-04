type input

external newInput : unit -> input = "input" [@@bs.new ][@@bs.module "midi"]
external getInputCount : input -> int = "getPortCount" [@@bs.send ]
external getInputName : input -> int -> string = "getPortName" [@@bs.send ]
external openInputNumber : input -> int -> unit = "openPort" [@@bs.send ]
external closeInput : input -> unit = "closePort" [@@bs.send ]

external on : input -> string -> (float -> int array -> unit) -> unit = "on" [@@bs.send ]
external ignoreTypes : input -> int -> int -> int -> unit = "ignoreTypes" [@@bs.send ]

type output

external newOutput : unit -> output = "output"[@@bs.new ][@@bs.module "midi"]
external getOutputCount : output -> int = "getPortCount"[@@bs.send ]
external getOutputName : output -> int -> string = "getPortName"[@@bs.send ]
external openOutputNumber : output -> int -> unit = "openPort"[@@bs.send ]
external closeOutput : output -> unit = "closePort"[@@bs.send ]

external sendMessage : output -> int array -> unit = "sendMessage"[@@bs.send                                                               ]


let list_init (n:int) (f:int -> 'a) : 'a list =
   let rec loop n acc =
      if n < 0 then acc
      else loop (n - 1) ((f n) :: acc)
   in
   loop (n - 1) []

let list_find_index (f : 'a -> bool) (l : 'a list) : int option =
   let rec loop index l =
      match l with
      | [] -> None
      | h::_ when f h -> Some index
      | _ :: t -> loop (index + 1) t
   in
   loop 0 l

let getInputNames () : string list =
   let temp = newInput () in
   let n = getInputCount temp in
   let result = list_init n (getInputName temp) in
   closeInput temp;
   result

let getOutputNames () : string list =
   let temp = newOutput () in
   let n = getOutputCount temp in
   let result = list_init n (getOutputName temp) in
   closeOutput temp;
   result

let openInput (name:string) : input option =
   let inputs = getInputNames () in
   match list_find_index (fun port  -> port = name) inputs with
   |Some index ->
      let input = newInput () in
      print_endline ("Opening input '" ^ (getInputName input index) ^ "'");
      openInputNumber input index;
      Some input
   | None -> None

let openOutput (name:string) : output option =
   let outputs = getOutputNames () in
   match list_find_index (fun port  -> port = name) outputs with
   | Some index ->
      let output = newOutput () in
      print_endline ("Opening output '" ^ (getOutputName output index) ^"'");
      openOutputNumber output index;
      Some output
   | None  -> None

let sendMessageList (output:output) (msg: int list) : unit =
   sendMessage output (Array.of_list msg)

let onMessage (input:input) (callback:float -> int array -> unit) =
   on input "message" callback

let ignoreTypes (input:input) ~(sysex:int) ~(timing:int) ~(activesense:int) =
   ignoreTypes input sysex timing activesense

