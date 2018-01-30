(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral




(*--- bounded numbers : guaranteed within lower and upper bounds ---*)

module type BoundedIntSig =
sig
   type t = private int
   val create   : int -> t
   val create_o : int -> t option
   val scan_o   : string -> t option
end

module IntM12toP12 : BoundedIntSig
module Int1to5     : BoundedIntSig
module Int1to9     : BoundedIntSig
module Int0to8     : BoundedIntSig
module Int0to24    : BoundedIntSig
module Int1to99    : BoundedIntSig
module Int1to999   : BoundedIntSig
module Int0to99    : BoundedIntSig
module Int0to999   : BoundedIntSig

module Int0to11    : BoundedIntSig




(*--- constrained strings : sets of string 'tokens' ---*)

module type StringSectionSig =
sig
   type t    = private string
   type pos  = { idx:int ; chr:int; }
   type tpos = { str:t ; pos:pos; }

   val createFilter  : string -> t
   val createCheck   : string -> t option
   val createAnalyse : string -> tpos list
   val posPlus1      : pos -> pos
   val toStringp     : tpos -> string
   val toStringPos   : tpos -> string * int
end

module Token : StringSectionSig
module Line  : StringSectionSig




(*--- message string makers ---*)

val messageString : string -> int -> int -> string -> string -> string

val errorString : int -> int -> string -> string -> string

(* TODO: remove ? *)
val unrecognisedString : int -> int -> string -> string -> string

val render : string -> int -> int -> string -> 'a ress -> 'a ress




(*--- printers ---*)

val optPrint : ('a -> string) -> 'a option -> string
