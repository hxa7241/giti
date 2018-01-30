(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
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




(*--- precalculated values ---*)

let metadataRx = Rx.compile {|^!\([^=]+\)=\(.+\)$|}




(*--- functions ---*)

(* construct *)

(*
 * Grammar (SBNF):
 *
 * (content
 *    (, ( * blank)
 *       ( + (| sound
 *              indicator)
 *           (+ blank))
 *       ( * blank)
 *       newline))
 *)
let parseContent (line:Line.tpos) : ((content list) ress) =

   (* split text line into tokens, then map those through parsers *)
   let linePos = Line.((posPlus1 line.pos).idx) in

   (* : tpos list *)
   (Token.createAnalyse (Line.toStringp line))
   |>
   (* : (content ress) list *)
   (List.map
      (fun (tokenWithPos:Token.tpos) : (content ress) ->
         let parse create constr : (content ress) =
            (create tokenWithPos linePos) |> (okMap constr)
         in
         if (Indicator.is tokenWithPos)
         then parse Indicator.create (fun a -> Indicator a)
         else parse Sound.create     (fun a -> Sound a)))
   |>
   (* all or nothing item validity, and concat messages
      : ((content list) ress) *)
   (List.resAnd % (errorMap (String.concat "\n")))


(*
 * Grammar (SBNF):
 *
 * (metadata
 *    (, "!"
 *       ( * blank) name ( * blank)
 *       =
 *       ( * blank) value newline))
 *
 * (name
 *    (+ non-blank))
 *
 * (value
 *    (, non-blank
 *       ( * non-newline)))
 *)
let parseMetadata (line:Line.tpos) : (metadata ress) =

   let str = Line.toStringp line
   and pos = Line.((posPlus1 line.pos).idx)

   and notEmpty (label:string) (s:string) : string ress =
      let s = String.trim s in
      if s <> "" then Ok s else Error (label ^ " missing")
   in

   (Rx.apply metadataRx str "name/value separator missing")
   |>=
   (* : (Token.t * string) ress *)
   (ressAnd2p
      ", "
      (fun rxmatch : Token.t ress ->
         (optToRes "name" (Rx.groupFound rxmatch 1))
         |>=
         (notEmpty "name")
         |>=
         (Token.createCheck % (optToRes "name has spaces")) )
      (fun rxmatch : string ress ->
         (optToRes "value" (Rx.groupFound rxmatch 2))
         |>=
         (notEmpty "value")) )
   |>=-
   (fun (name , value) ->
      { name ; value } )
   |>
   (render "Metadata" pos 0 str)


(*
 * Grammar (SBNF):
 *
 * (piece
 *    ( * (| content
 *           annotation
 *           metadata
 *           comment
 *           blankline)))
 *)
let create (text:string) : ((t, string list) result) =

   (* split raw text into lines, then map those through parsers
      : Line.tpos list *)
   (Line.createAnalyse text)
   |>
   (* : (item ress) list *)
   (List.map
      (* construct value of root Giti sum-type *)
      (fun (lineWithPos:Line.tpos) : (item ress) ->
         let lineStr = (lineWithPos.Line.str :> string) in
         let firstChar = excToDefault ' ' (fun () -> lineStr.[0]) in
         match firstChar with
         (* comment *)
         | '#' ->
            (*
             * Grammar (SBNF):
             *
             * (comment
             *    (, "#"
             *       ( * non-newline)
             *       newline))
             *)
             Ok (Comment (Line.createFilter (String.trail lineStr 1)))
         (* metadata *)
         | '!' ->
            (parseMetadata lineWithPos)
            |>=-
            (fun m -> Metadata m)
         (* annotation *)
         | '@' ->
            (Annotation.create lineWithPos)
            |>=-
            (fun a -> Annotation a)
         (* blank line *)
         | _ when (String.check Char.isBlank lineStr) ->
            Ok Blankline
         (* content *)
         | _   ->
            (parseContent lineWithPos)
            |>=-
            (fun c -> Content c)
         ))
   |>
   (* all or nothing item validity : ((item list, string list) result) *)
   List.resAnd

   (* merge adjacent content elements *)
   (* ... on the other hand, why would this be worthwhile ? *)
   (*
   - split item list on content-other or other-content edges
      # yielding: list of lists of content | other
      # (list containing runs (in lists) of content and other)
   - flatten lists of other
   - flatten lists of content:
      from list of lists of Content.item to lists of Content.item
   *)


(*
split into bars


module Bar =
   Basics.StringSection (struct let f = Indicator.isBar let e = true end)

(Bar.createAnalyse text)


{|[|]+[0-9]?|}

type split_result =
   | Text of string
   | Delim of string
Str.full_split  : regexp -> string -> split_result list

Str.split       : regexp -> string -> string list
Str.split_delim : regexp -> string -> string list

*)
