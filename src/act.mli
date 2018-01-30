(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Basics




(*--- types ---*)

type volume    = | VolumeStill | VolumeUp | VolumeDown
type loudness  = | Medium | Quiet | Loud
type damping   = | Clear | Mute | Dead

type extra     =
   { damping:damping ; loudness:loudness ; volume:volume ; }
type basic     =
   | Pluck   | Hammer | Pulloff     | Sweep
   | Sustain | Tap    | TremoloPick
   | Finger  | Backed | EndString   | MidString

type t         = { basic:basic ; extra:extra ; }




(*--- functions ---*)

val isFirstChar : char -> bool
val wordToNote  : t option -> t option

val create      : Token.t -> (t ress)
