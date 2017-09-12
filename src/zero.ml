
type side =
   | Left
   | Right

type row =
   | Top
   | Bottom

type column = int

type position = side * row * column

type midi_message =
   | KnobMsg of column * int
   | EncoderMsg of column * int
   | SliderMsg of column * int
   | BottonMsg of position * int
   | IgnoredMsg of int array

type kind =
   | Encoder of column
   | Knob of column
   | Slider of column

   | Toggle of position
   | Radio of position array
   | Push of position
   | Count of position
   | PlusMinus of position * position

   | Blank of column

type display_function =
   | Percent
   | Numeric

type value =
   | Bool of bool
   | Float of display_function * float * float * float
   | Enum of string array * float * float

type parameter =
   {
      name : string;
      kind : kind;
      value : value;
      pushed : bool;
      group : string option;
   }

type parameters = parameter array

type group =
   {
      name : string;
      subs: sub array;
      choke : int;
      active : bool;
   }

and sub =
   | Parameter of parameter
   | Group of group

type t =
   {
      input : Remidi.input;
      output : Remidi.output;
      groups : group;
   }

(* Contains the basic operations to control the Zero *)
module Primitives = struct

   (** Turns a string to a list of integers *)
   let string_to_int_list (str:string) : int list =
      let acc = ref [] in
      String.iter (fun c -> acc := Char.code c :: !acc) str;
      List.rev !acc

   (** Adjust the string to 8 characters and centers it *)
   let crop_string (str:string) : string =
      let length = String.length str in
      if length >= 8 then String.sub str 0 8
      else
         let missing = 8 - length in
         let blank = String.make (missing/2) ' ' in
         if missing mod 2 = 0 then
            blank ^ str ^ blank
         else
            blank ^ str ^ blank ^" "

   (** Sets the encoders lights. E.g. [setRing t ring value] where [ring] represents the number
       (1 - 8) and the [value] goes from 0 - 11 *)
   let setRing (t:t) (ring:int) (value:int) : unit =
      Remidi.sendMessageList t.output [176; (112 + ring - 1); value]

   (** Sets the state of a button given the side (Left, Right), row (Top, Bottom) and column (1 - 8) *)
   let setButton (t:t) (side:side) (row:row) (column:int) (value:bool) : unit =
      let start =
         match side, row with
         | Left, Top -> 24
         | Left, Bottom -> 32
         | Right, Top -> 40
         | Right, Bottom -> 48
      in
      Remidi.sendMessageList t.output [176; start + column - 1; if value then 1 else 0]

   (** Sets the state of the button given the midi control number *)
   let setButtonRaw (t:t) (button:int) (value:int) : unit =
      Remidi.sendMessageList t.output [176; button; value]

   (** Clears the screens and buttons *)
   let clear (t:t) =
      Remidi.sendMessageList t.output [240; 0; 32; 41; 3; 3; 18; 0; 4; 0; 2; 2; 1; 247];
      let rec loop n =
         if n > 8 then ()
         else
            let () = setRing t n 0 in
            let () = setButtonRaw t (24 + n) 0 in
            let () = setButtonRaw t (32 + n) 0 in
            let () = setButtonRaw t (40 + n) 0 in
            let () = setButtonRaw t (49 + n) 0 in
            loop (n + 1)
      in loop 0

   (** Sends the initialization message *)
   let init (t:t) : unit =
      Remidi.sendMessageList t.output [240; 0; 32; 41; 3; 3; 18;0 ;4 ;0 ;1 ;1 ; 247];
      clear t

   (** Low level writing of a label *)
   let writeLabelRaw (t:t) (str:string) (display:int) (position:int) : unit =
      let start = [240; 0; 32; 41; 3; 3; 18; 0; 4; 0; 2; 1; position; display; 4] in
      let str_list = string_to_int_list str in
      let msg = start @ str_list @ [247] in
      Remidi.sendMessageList t.output msg

   (** Writes a label given the side (Left, Right) the row (Top, Bottom) and column (1 - 8) *)
   let writeLabel (t:t) (str:string) (side:side) (row:row) (column:int) : unit =
      let display =
         match side, row with
         | Left, Top -> 1
         | Right, Top -> 2
         | Left, Bottom -> 3
         | Right, Bottom -> 4
      in
      let msg = crop_string str in
      writeLabelRaw t msg display ((column-1)*9);

end

let array_find_index (array:'a array) (elem:'a) : int option =
   let found = ref None in
   Array.iteri (fun i e -> if compare e elem = 0 then found := Some i) array;
   !found


let show_side side =
   match side with
   | Left -> "Left"
   | Right -> "Right"

let show_row row =
   match row with
   | Top -> "Top"
   | Bottom -> "Bottom"

let _show_midi_message msg =
   match msg with
   | KnobMsg (col, value) -> Printf.sprintf "KnobMsg (%i, %i)" col value
   | SliderMsg (col, value) -> Printf.sprintf "SliderMsg (%i, %i)" col value
   | EncoderMsg (col, value) -> Printf.sprintf "EncoderMsg (%i, %i)" col value
   | BottonMsg ((side, row, col), value) ->
      Printf.sprintf "BottonMsg (%s, %s, %i, %i)" (show_side side) (show_row row) col value
   | IgnoredMsg msg -> "IgnoredMsg([" ^(Array.to_list msg |> List.map string_of_int |> String.concat ",") ^ "])"

let decode (message:int array) : midi_message =
   if Array.length message = 3 then
      let data1 = Array.get message 0 in
      let data2 = Array.get message 1 in
      let data3 = Array.get message 2 in
      match data1 with
      | 176 when data2 >=8 && data2 <= 15 ->
         KnobMsg (data2 - 7, data3)
      | 176 when data2 >=16 && data2 <= 23 ->
         SliderMsg (data2 - 15, data3)
      | 176 when data2 >=24 && data2 <= 31 ->
         BottonMsg ((Left, Top,data2 - 23), data3)
      | 176 when data2 >=32 && data2 <= 39 ->
         BottonMsg ((Left,  Bottom,data2 - 31), data3)
      | 176 when data2 >=40 && data2 <= 47 ->
         BottonMsg ((Right, Top,data2 - 39), data3)
      | 176 when data2 >=48 && data2 <= 55 ->
         BottonMsg ((Right, Bottom,data2 - 47), data3)
      | 176 when data2 >=56 && data2 <= 63 ->
         let delta = if data3 >= 65 then 64 - data3 else data3 in
         EncoderMsg (data2 - 55, delta)
      | _ -> IgnoredMsg message
   else IgnoredMsg message

(* ===== Parameters ======= *)

let paramName (parameter:parameter) : string =
   parameter.name

let paramGroup (parameter:parameter) : string =
   match parameter.group with
   | Some group -> group
   | None -> ""

let paramPath (parameter:parameter) : string =
   match parameter.group with
   | Some group -> "/" ^ group ^ "/" ^ parameter.name
   | None -> "/root/" ^ parameter.name

let applyAbsValue (parameter:parameter) (msg:int) : parameter =
   let value =
      match parameter.value with
      | Float (f, min, max, _) -> Float (f, min, max, float_of_int msg)
      | Bool _ -> Bool (msg <> 0)
      | Enum (elems, n, _) ->
         let raw = (float_of_int msg) /. 127.0 *. (n -. 1.0) in
         Enum (elems, n, raw)
   in
   { parameter with value}

let applyToggleValue (parameter:parameter) (msg:int) : parameter =
   let pushed = msg <> 0 in
   let value =
      match parameter.value with
      | Float (f, min, max, current) when current = 0.0 && pushed -> Float (f, min, max, 127.0)
      | Float (f, min, max, _) when pushed -> Float (f, min, max, 0.0)
      | Float _ as value -> value
      | Bool v when pushed -> Bool (not v)
      | Bool _ as value  -> value
      | Enum (elems, n, current) when current = 0.0 && pushed -> Enum (elems, n, n -. 1.0)
      | Enum (elems, n, _) when pushed -> Enum (elems, n, 0.0)
      | Enum _ as value -> value
   in
   { parameter with value; pushed }

let applyPushValue (parameter:parameter) (msg:int) : parameter =
   let pushed = msg <> 0 in
   let value =
      match parameter.value with
      | Float (f, min, max, _) -> Float (f, min, max, if pushed then 127.0 else 0.0)
      | Bool _ -> Bool (pushed)
      | Enum (elems, n, _) -> Enum (elems, n, if pushed then n -. 1.0 else 0.0)
   in
   { parameter with value; pushed }

let applyRadioValue (parameter:parameter) (msg:int) : parameter =
   let value =
      match parameter.value with
      | Float (f, min, max, _) -> Float (f, min, max, float_of_int msg)
      | Bool _ -> Bool true
      | Enum (elems, n, _) ->
         let value = float_of_int (msg - 1) in
         let value = if value >= n then (n -. 1.0) else value in
         Enum (elems, n, value)
   in
   { parameter with value }

let applyCountValue (parameter:parameter) (msg:int) : parameter =
   let pushed = msg <> 0 in
   let value =
      match parameter.value with
      | Float (f, min, max, v) ->
         let value = v +. 1.0 in
         let value = if value > 127.0 then 0.0 else value in
         Float (f, min, max, if pushed then value else v)
      | Bool v -> Bool (if pushed then not v else v)
      | Enum (elems, n, v) when pushed ->
         let value = v +. 1.0 in
         let value = if value >= n then 0.0 else value in
         Enum (elems, n, value)
      | Enum _ -> parameter.value
   in
   { parameter with value; pushed }

let applyPlusMinusValue (parameter:parameter) (msg:int) : parameter =
   let pushed = msg <> 0 in
   let value =
      match parameter.value with
      | Float (f, min, max, v) ->
         let delta = if msg < 0 then -1.0 else 1.0 in
         let value = v +. delta in
         let value = if value > 127.0 then 127.0 else if value < 0.0 then 0.0 else value in
         Float (f, min, max, if pushed then value else v)
      | Bool _ -> Bool (if msg < 0 then false else true)
      | Enum (elems, n, index) ->
         let delta = if msg < 0 then -1.0 else 1.0 in
         let index = if pushed then index +. delta else index in
         let index = if index >= n then (n -. 1.0) else if index < 0.0 then 0.0 else index in
         Enum (elems, n, index)
   in
   { parameter with value }

let applyRelValue (parameter:parameter) (msg:int) : parameter =
   let value =
      match parameter.value with
      | Float (f, min, max, v) ->
         let v = v +. float_of_int msg in
         let v = if v > 127.0 then 127.0 else if v < 0.0 then 0.0 else v in
         Float (f, min, max, v)
      | Bool v -> Bool (if msg < 0 && v then false else if msg > 0 && (not v) then true else v)
      | Enum (elems, n, v) ->
         let value = v +. (float_of_int msg) *. 0.25 in
         let value = if value < 0.0 then 0.0 else if value >= n then (n -. 1.0) else value in
         Enum (elems, n, value)
   in
   { parameter with value }

let percentFormat min max v =
   (Printf.sprintf "%.2f" (((min +. v *. (max -. min) /. 127.0) *. 100.0))) ^" %"

let numberFormat min max v =
   Printf.sprintf "%.2f" ((((min +. v *. (max -. min) /. 127.0))))

let paramString (parameter:parameter) =
   match parameter.value with
   | Float (Percent, min, max, v) -> percentFormat min max v
   | Float (Numeric, min, max, v) -> numberFormat min max v
   | Bool v -> if v then "on" else "off"
   | Enum (elems, n, index) ->
      if index >= n then print_endline (string_of_float index);
      let index = if index >= n then n -. 1.0 else if index < 0.0 then 0.0 else index in
      Array.get elems (int_of_float index)

let paramEncoder (parameter:parameter) : int =
   match parameter.value with
   | Float (_, _, _, v) -> int_of_float (12.0 *. v /. 127.0 )
   | Bool v -> if v then 11 else 0
   | Enum (_, n, index) ->
      index /. (n -. 1.0) *. 12.0 |> int_of_float

let paramInt (parameter:parameter) : int =
   match parameter.value with
   | Float (_,_, _, v) -> int_of_float v
   | Bool v -> if v then 1 else 0
   | Enum (_, _, index) ->
      (int_of_float index)

let paramFloat (parameter:parameter) : float =
   match parameter.value with
   | Float (_,_, _, v) -> v
   | Bool v -> if v then 127.0 else 0.0
   | Enum (_, _, index) -> index

let parameBool (parameter:parameter) : bool =
   match parameter.value with
   | Float (_, _, _, v) -> v > 64.0
   | Bool v -> v
   | Enum (_, _, index) -> index <> 0.0

let paramDisplay (t:t) (parameter:parameter) : unit =
   match parameter.kind with
   | Encoder column  ->
      Primitives.writeLabel t parameter.name Left Top column;
      Primitives.writeLabel t (paramString parameter) Left Bottom column;
      Primitives.setRing t column (paramEncoder parameter)

   | Knob column ->
      Primitives.writeLabel t parameter.name Left Top column;
      Primitives.writeLabel t (paramString parameter) Left Bottom column;

   | Slider column ->
      Primitives.writeLabel t parameter.name Right Top column;
      Primitives.writeLabel t (paramString parameter) Right Bottom column;

   | Toggle (side, row, column) ->
      Primitives.writeLabel t parameter.name Left Top column;
      Primitives.writeLabel t (paramString parameter) Left Bottom column;
      Primitives.setButton t side row column (parameBool parameter)

   | Push (side, row, column) ->
      Primitives.writeLabel t parameter.name Left Top column;
      Primitives.writeLabel t (paramString parameter) Left Bottom column;
      Primitives.setButton t side row column (parameBool parameter)

   | Count (side, row, column) ->
      Primitives.writeLabel t parameter.name Left Top column;
      Primitives.writeLabel t (paramString parameter) Left Bottom column;
      Primitives.setButton t side row column parameter.pushed

   | PlusMinus ((side_p, row_p, column_p), (side_m, row_m, column_m)) ->
      Primitives.writeLabel t parameter.name Left Top column_p;
      Primitives.writeLabel t (paramString parameter) Left Bottom column_p;
      Primitives.setButton t side_p row_p column_p parameter.pushed;
      Primitives.setButton t side_m row_m column_m parameter.pushed

   | Radio elems ->
      let index = paramInt parameter in
      Array.iteri (fun i (side, row, column) ->
            Primitives.setButton t side row column (i = index))
         elems

   | Blank column ->
      Primitives.writeLabel t "" Left Top column;
      Primitives.writeLabel t "" Left Bottom column;
      Primitives.setRing t column 0

let subName (sub:sub) : string =
   match sub with
   | Parameter s -> s.name
   | Group g -> g.name

let subSubs (sub:sub) : sub array =
   match sub with
   | Parameter _ -> [||]
   | Group g -> g.subs

let array_find (f:'a -> bool) (a:'a array) : 'a option =
   let rec loop i f a =
      if i < Array.length a then
         let e = Array.get a i in
         if f e then Some e
         else loop (i+1) f a
      else None
   in loop 0 f a


let rec mapGroup (f:sub -> sub) (a:sub) : sub =
   match a with
   | Parameter _ -> f a
   | Group g ->
      let subs = Array.map (mapGroup f) g.subs in
      f (Group { g with subs })

let rec foldGroup (f:'a -> sub -> 'a) (a:'a) (e:sub) : 'a =
   match e with
   | Parameter _ -> f a e
   | Group g ->
      let a = Array.fold_left f a g.subs in
      foldGroup f a e

let find (group:group) (name:string list) : sub list option =
   let rec loop acc group name =
      match name with
      | [] -> None
      | [h] ->
         begin match array_find (fun a -> subName a = h) group.subs with
            | Some sub -> Some (sub :: acc)
            | None -> None
         end
      | h :: t ->
         match array_find (fun a -> subName a = h) group.subs with
         | Some ((Group g) as sub) -> loop (sub :: acc) g t
         | _ -> None
   in
   loop [] group name

let mapSingle (group:group) (f:sub -> sub) (name:string list) =
   match find group name with
   | Some (sub :: _) ->
      let update s = if compare s sub = 0 then f s else s in
      begin match mapGroup update (Group group) with
         | Group g -> Some g
         | _ -> None
      end
   | _ -> None

let groupFind (group:group) (name:string list) : sub list option =
   match find group name with
   | Some (Group _ :: _) as r -> r
   | _ -> None

let paramFind (group:group) (name:string list) : sub list option =
   match find group name with
   | Some (Parameter _ :: _ ) as r -> r
   | _ -> None

let groupSetActive (group:group) (name:string list) =
   let choke =
      match find group name with
      | Some (Group h :: _) -> h.choke
      | _ -> -1000
   in
   let deactivate a =
      match a with
      | Group p when p.choke = choke -> Group { p with active = false }
      | _ -> a
   in
   match mapGroup deactivate (Group group) with
   | Group g ->
      let res =
         mapSingle g
            (fun g ->
                match g with
                | Group p -> Group { p with active = true }
                | _ -> g)
            name
      in
      begin match res with
         | Some g -> g
         | None -> group
      end
   | _ -> group


let applyMessageToParam (msg:midi_message) (modified:parameter list) (parameter:parameter) : parameter list * parameter =
   match msg, parameter.kind with
   | IgnoredMsg _, _ -> modified, parameter

   | KnobMsg (msg_column, value), Knob column ->
      if msg_column = column  then
         let new_param = applyAbsValue parameter value in
         new_param :: modified, new_param
      else modified, parameter

   | KnobMsg _ , _ -> modified, parameter

   | SliderMsg (msg_column, value), Slider column ->
      if msg_column = column then
         let new_param = applyAbsValue parameter value in
         new_param :: modified, new_param
      else modified, parameter

   | SliderMsg _ , _ -> modified, parameter

   | EncoderMsg (msg_column, value), Encoder column ->
      if msg_column = column then
         let new_param = applyRelValue parameter value in
         new_param :: modified, new_param
      else modified, parameter

   | EncoderMsg _ , _ -> modified, parameter

   | BottonMsg (msg_position, value), Toggle (position) ->
      if msg_position = position then
         let new_param = applyToggleValue parameter value in
         new_param :: modified, new_param
      else modified, parameter

   | BottonMsg (msg_position, value), Push (position) ->
      if msg_position = position then
         let new_param = applyPushValue parameter value in
         new_param :: modified, new_param
      else modified, parameter

   | BottonMsg (msg_position, value), Count (position) ->
      if msg_position = position then
         let new_param = applyCountValue parameter value in
         new_param :: modified, new_param
      else modified, parameter

   | BottonMsg (msg_position, value), PlusMinus (plus,minus) ->
      if msg_position = plus then
         let new_param = applyPlusMinusValue parameter value in
         new_param :: modified, new_param
      else
      if msg_position = minus then
         let new_param = applyPlusMinusValue parameter (- value) in
         new_param :: modified, new_param
      else modified, parameter

   | BottonMsg (msg_position, _), Radio elems ->
      begin match array_find_index elems msg_position with
         | Some index ->
            let new_param = applyRadioValue parameter (index + 1) in
            new_param :: modified, new_param
         | None -> modified, parameter
      end

   | BottonMsg _, _ -> modified, parameter


let rec applyMessage (msg:midi_message) (modified:parameter list) (group:group) =
   if group.active then
      let modified, rev_parameters =
         Array.fold_left
            (fun (modified, acc) (p:sub) ->
                match p with
                | Parameter param ->
                   let modified, new_p = applyMessageToParam msg modified param in
                   modified, Parameter new_p :: acc
                | Group g ->
                   let modified, new_g = applyMessage msg modified g in
                   modified, Group new_g :: acc)
            (modified,[]) group.subs
      in
      modified, { group with subs = List.rev rev_parameters |> Array.of_list }
   else
      modified, group

(** ==== Updating ===== *)

let compare_value (v1:value) (v2:value) : int =
   match v1, v2 with
   | Float(_, min1, max1, f1), Float(_, min2, max2, f2) ->
      compare (min1, max1, f1) (min2, max2, f2)
   | _ -> compare v1 v2

let compare_parameter (p1:parameter) (p2:parameter) : int =
   match compare p1.name p2.name with
   | 0 ->
      begin match compare p1.kind p2.kind with
         | 0 -> compare_value p1.value p2.value
         | n -> n
      end
   | n -> n

let rec compare_group (g1:group) (g2:group) : int =
   match compare g1.name g2.name with
   | 0 ->
      let rec loop s1 s2 =
         match s1, s2 with
         | [], [] -> 0
         | h1 :: t1, h2 :: t2 ->
            let res =
               match h1, h2 with
               | Parameter p1, Parameter p2 -> compare_parameter p1 p2
               | Group g1, Group g2 -> compare_group g1 g2
               | Parameter _, _ -> -1
               | Group _, _ -> 1
            in
            if res = 0 then res else loop t1 t2
         | [], _ -> -1
         | _, [] -> 1
      in loop (Array.to_list g1.subs) (Array.to_list g2.subs)
   | n -> n

let rec forcedUpdateSub t sub =
   match sub with
   | Parameter p -> paramDisplay t p
   | Group g -> forceUpdateGroup t g

and forceUpdateGroup t group =
   if group.active then
      Array.iter (fun sub -> forcedUpdateSub t sub) group.subs

let rec updateGroup t before after =
   if not before.active && after.active || Array.length before.subs <> Array.length after.subs then
      forceUpdateGroup t after
   else if after.active then
      Array.iteri (fun i a -> updateSub t (Array.get before.subs i) a) after.subs

and updateSub t before after =
   match before, after with
   | Parameter b, Parameter a when compare_parameter b a = 0 -> ()
   | Parameter _, Parameter _ ->
      forcedUpdateSub t after
   | Group b, Group a -> updateGroup t b a
   | _, Group g -> forceUpdateGroup t g
   | _, Parameter _ -> forcedUpdateSub t after

(* ===  Predefined controls ==== *)

let numericKnob (name:string) (column:column) ~(min:float) ~(max:float) (start:float) : parameter =
   let start = (start -. min /. (max -. min)) *. 127.0 in
   { name; kind = Knob column; value = Float (Numeric ,min, max, start); pushed = false; group = None }

let numericEncoder (name:string) (column:column) ~(min:float) ~(max:float) (start:float) : parameter =
   let start = (start -. min /. (max -. min)) *. 127.0 in
   { name; kind = Encoder column; value = Float (Numeric, min, max, start); pushed = false; group = None }

let numericSlider (name:string) (column:column) ~(min:float) ~(max:float) (start:float) : parameter =
   let start = (start -. min /. (max -. min)) *. 127.0 in
   { name; kind = Slider column; value = Float (Numeric, min, max, start); pushed = false; group = None }

let percentualKnob (name:string) (column:column) ~(min:float) ~(max:float) (start:float) : parameter =
   let start = (start -. min /. (max -. min)) *. 127.0 in
   { name; kind = Knob column; value = Float (Percent, min, max, start); pushed = false; group = None }

let percentualEncoder (name:string) (column:column) ~(min:float) ~(max:float) (start:float) : parameter =
   let start = (start -. min /. (max -. min)) *. 127.0 in
   { name; kind = Encoder column; value = Float (Percent, min, max, start); pushed = false; group = None }

let percentualSlider (name:string) (column:column) ~(min:float) ~(max:float) (start:float) : parameter =
   let start = (start -. min /. (max -. min)) *. 127.0 in
   { name; kind = Slider column; value = Float (Percent, min, max, start); pushed = false; group = None }

let toggleButton (name:string) (side:side) (row:row) (column:column) (start:bool) : parameter =
   { name; kind = Toggle (side, row, column); value = Bool start; pushed = false; group = None }

let pushButton (name:string) (side:side) (row:row) (column:column) : parameter =
   { name; kind = Push (side, row, column); value = Bool false; pushed = false; group = None }

let countButton (name:string) (side:side) (row:row) (column:column) (elems:string array) (start:int) : parameter =
   let n = float_of_int (Array.length elems) in
   let start = float_of_int start -. 1.0 in
   { name; kind = Count (side, row, column); value = Enum (elems, n, start); pushed = false; group = None }

let plusMinusButton (name:string) (side_p:side) (row_p:row) (column_p:column) (side_m:side) (row_m:row) (column_m:column) (elems:string array) (start:int) : parameter =
   let n = float_of_int (Array.length elems) in
   let start = float_of_int start -. 1.0 in
   { name; kind = PlusMinus ((side_p, row_p, column_p),(side_m, row_m, column_m)); value = Enum (elems, n, start); pushed = false; group = None }

let radioButton (name:string) (buttons:(side * row * column) array) (elems:string array) (start:int) : parameter =
   let n = float_of_int (Array.length elems) in
   let start = (float_of_int start) -. 1.0 in
   { name; kind = Radio buttons; value = Enum (elems, n, start); pushed = false; group = None }

let blank (column:column) : parameter =
   { name =""; kind = Blank column; value = Bool false; pushed = false; group = None }

let enumEncoder (name:string) (column:column) (elems:string array) (start:int) : parameter =
   let n = float_of_int (Array.length elems) in
   let start = float_of_int start -. 1.0 in
   { name; kind = Encoder column; value = Enum (elems, n, start); pushed = false; group = None }

let enumKnob (name:string) (column:column) (elems:string array) (start:int) : parameter =
   let n = float_of_int (Array.length elems) in
   let start = float_of_int start -. 1.0 in
   { name; kind = Knob column; value = Enum (elems, n, start); pushed = false; group = None }

let newGroup (name:string) ~(choke:int) ~(active:bool) (parameters:parameters) : group =
   let subs = Array.map (fun p -> Parameter { p with group = Some name}) parameters in
   { name; active = active; subs; choke }

let makeTopGroup (groups: group array) : group =
   { name = ""; active = true; choke = -1; subs = Array.map (fun a -> Group a) groups }

let update (zero:t) (custom_handler: parameters -> group -> group) (message:midi_message) : t =
   let initial_groups = zero.groups in
   let modified, groups = applyMessage message [] initial_groups in
   let groups = custom_handler (Array.of_list modified) groups in
   updateGroup zero initial_groups groups;
   { zero with groups = groups }

let handle_messages (zero:t) custom_handler : float -> int array -> unit =
   let state = ref zero in
   fun _time data ->
      let message = decode data in
      (*print_endline (_show_midi_message message);*)
      state := update !state custom_handler message

let openPort (name:string) (groups:group) (custom_handler:parameters -> group -> group) : t option =
   match Remidi.openInput name, Remidi.openOutput name with
   | Some input, Some output ->
      let t = { input; output; groups } in
      Primitives.init t;
      Remidi.onMessage input (handle_messages t custom_handler);
      forceUpdateGroup t groups;
      Some t
   | Some input , None ->
      Remidi.closeInput input;
      None
   | None, Some output ->
      Remidi.closeOutput output;
      None
   | None, None -> None

