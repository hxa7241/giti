(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Basics




(*--- types ---*)

type symbol =
   | Bar         of Int1to9.t option
   | Segment
   | TupletOpen  of Int1to9.t option
   | TupletClose

type t      = { symbol:symbol ; repeat:Int1to9.t }




(*--- functions ---*)

val is     : Token.tpos -> bool
val isBar  : Token.tpos -> bool
val create : Token.tpos -> int -> (t ress)
