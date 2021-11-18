type side =
  | Left
  | Right

type row =
  | Top
  | Bottom

type column = int

type position = side * row * column

type parameter

type group

type sub

type t

val paramName : parameter -> string

val paramGroup : parameter -> string

val paramPath : parameter -> string

val paramString : parameter -> string

val paramInt : parameter -> int

val paramFloat : parameter -> float

val paramFind : group -> string list -> sub list option

val groupSetActive : group -> string list -> group

val openPort : string -> group -> (parameter array -> group -> group) -> t option

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

val newGroup : string -> choke:int -> active:bool -> parameter array -> group

val makeTopGroup : group array -> group

val update : t -> unit
