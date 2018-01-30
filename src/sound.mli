(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Basics




(*--- types ---*)

type ordinary =
   { act   :Act.t option  ;
     pitch :Pitch.t       ;
     time  :Time.t option ; }
type continuation = Time.t option
type rest         = Time.t option

type t =
   | Ordinary     of ordinary
   | Continuation of continuation
   | Rest         of rest




(*--- functions ---*)

val create : Token.tpos -> int -> (t ress)
