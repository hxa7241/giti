(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Basics




(*--- types ---*)

type tuningSpec =
   {  tuningSpecString :Pitch.strng ;
      tuningSpecNote   :Pitch.note ; }

type version =
   {  versionHead :Token.t ;
      versionBody :Token.t ; }
type tuning =
   | TuningStd   of Token.t
   | TuningShift of { semi :IntM12toP12.t ; micro :bool }
   | TuningSpec  of tuningSpec list1
type tempo =
   | TempoDuration of
      {  tempoDuraInt  :Int0to99.t ;
         tempoDuraFrac :Int0to999.t }
   | TempoRate of
      {  tempoRateFrac :Int1to99.t ;
         tempoRateBpm  :Int1to999.t }

type item =
   | Version of version | Tuning of tuning | Tempo of tempo

type t = item list




(*--- values ---*)

val default : t




(*--- functions ---*)

val create : Line.tpos -> (t ress)
