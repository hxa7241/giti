(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Basics




(*--- types ---*)

type fret       = Int0to24.t
type strng      = Int1to9.t
type bend       =
   | Micro
   | Macro of { semi :Int1to5.t ; micro :bool }

type harmonic   =
   | HarmonicNeck | HarmonicBridge
type vibrato    =
   | Vibrato
   | Trill
   | TrillSpec of
      { below :bool ;
        adjac :Int1to9.t ;
        repet :Int1to9.t option ; }
type portamento =
   | BendUp   of bend | SlideUp   of fret
   | BendDown of bend | SlideDown of fret

type octave     = Int0to8.t
type accident   = | Natural | Sharp | Flat
type letter     = | A | B | C | D | E | F | G

type note       =
   { letter   :letter ;
     accident :accident ;
     octave   :octave ; }
type word       =
   { strng :strng ;
     fret  :fret  ; }

type extra      =
   { portamento :portamento option ;
     vibrato    :vibrato option    ;
     harmonic   :harmonic option   ; }
type basic      =
   | Word of word
   | Note of note

type pitch      = { basic:basic ; extra:extra ; }
type t          = { pitch:pitch ; chord:pitch list ; }




(*--- functions ---*)

val strngCreate : string -> strng ress
val noteCreate  : string -> note ress
val wordCreate  : string -> word ress
val wordToNote  : word   -> note

val isFirstChar : char -> bool

val create      : Token.t -> (t ress)
