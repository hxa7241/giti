(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




(*open HxaGeneral*)
(*open Basics*)




(*--- types ---*)

type formats = { fret:bool ; note:bool ; tab:bool }



(*--- functions ---*)

val print : formats -> Piece.t -> string
