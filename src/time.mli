(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Basics




(*--- types ---*)

type articulation = | ArticNorm | Legato | Staccato
type extension    =
   | Dots  of Int1to5.t
   | Fracs of Int1to999.t list1

type extra        = articulation
type basic        =
   { duration     : Int0to999.t ;
     extension    : extension option ;
     continuation : (bool * bool) ; }

type t            = { basic:basic ; extra:extra ; }




(*--- functions ---*)

val create : Token.t -> (t ress)
