(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




(*open HxaGeneral*)
open Basics




(*--- types ---*)

type content =
   | Sound     of Sound.t
   | Indicator of Indicator.t

type metadata =
   {  name  :Token.t ;
      value :string }

type item =
   | Content    of (content list)
   | Annotation of Annotation.t
   | Metadata   of metadata
   | Comment    of Line.t
   | Blankline

type t = item list




(*--- functions ---*)

val create : string -> ((t, string list) result)
