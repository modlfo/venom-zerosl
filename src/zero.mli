
type side =
   | Left
   | Right

type row =
   | Top
   | Bottom


type column = int


type position = side * row * column

type parameter

type parameters = parameter array

type group

type groups = group array

type t

module Primitives : sig

   val init : t -> unit

   val clear : t -> unit

   val writeLabel : t -> string -> side -> row -> column -> unit

   val setButton : t -> side -> row -> column -> bool -> unit

   val setRing : t -> int -> int -> unit

   val writeLabelRaw : t -> string -> int -> int -> unit

   val setButtonRaw : t -> int -> int -> unit

end


val paramName : parameter -> string
val paramGroup : parameter -> string
val paramPath : parameter -> string

val paramString : parameter -> string
val paramInt : parameter -> int
val paramFloat : parameter -> float


val paramFind : groups -> string -> string -> parameter option

val groupSetActive : groups -> string -> groups


val openPort : string -> groups -> (parameters -> groups -> groups) -> t option

val numericKnob : string -> column -> min:float -> max:float -> float -> parameter

val numericEncoder : string -> column -> min:float -> max:float -> float -> parameter

val numericSlider : string -> column -> min:float -> max:float -> float -> parameter

val percentualKnob : string -> column -> min:float -> max:float -> float -> parameter

val percentualEncoder : string -> column -> min:float -> max:float -> float -> parameter

val percentualSlider : string -> column -> min:float -> max:float -> float -> parameter

val toggleButton : string -> side -> row -> column -> bool -> parameter

val pushButton : string -> side -> row -> column -> parameter

val countButton : string -> side -> row -> column -> string array -> int -> parameter

val plusMinusButton : string -> side -> row -> column -> side -> row -> column -> string array -> int -> parameter

val radioButton : string -> position array -> string array -> int -> parameter

val enumEncoder : string -> column -> string array -> int -> parameter

val enumKnob : string -> column -> string array -> int -> parameter

val blank : column -> parameter

val newGroup : string -> choke:int -> active:bool -> parameters -> group

