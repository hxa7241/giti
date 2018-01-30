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




(*--- precalculated values ---*)

let indicatorRx = Rx.compile {|^\(\(.\)\2*\)\(.\)?$|}




(*--- functions ---*)

(* query *)

let is (token:Token.tpos) : bool =
   let token = Token.toStringp token in
   String.isFirstChar
      (function
         | '|' | '(' | ')' | '\'' -> true
         | _                      -> false)
      token


let isBar (token:Token.tpos) : bool =
   let token = Token.toStringp token in
   String.isFirstChar ((=) '|') token


(* construct *)

(*
 * Grammar (SBNF):
 *
 * (indicator
 *    (| bar
 *       tuplet
 *       segment))
 *
 * (bar
 *    (, (+ "|") (? int1-9)))
 *
 * (tuplet
 *    (| (, (+ "(") (? int1-9))
 *       (+ ")")))
 *
 * (segment
 *    (+ "'"))
 *)
let create (token:Token.tpos) (linePos:int) : (t ress) =
   let token , pos = Token.toStringPos token in
   let firstChar = token.[0] in
   (* overall rough check: all same char, or all same except last
      : Rx.rxmatch ress *)
   (Rx.apply indicatorRx token "unrecognised")
   |>=
   (* assemble product : (symbol * Int1to9.t) ress *)
   (ressAnd2p
      ", "
      (fun rxmatch : symbol ress ->
         let parseNumber (s:string) : Int1to9.t ress =
            (Int1to9.scan_o s) |> (optToRes "number")
         in
         let numbero = Rx.groupFound rxmatch 3 in
         (* discriminate sum, and construct *)
         match (firstChar , numbero) with
         | '|'  , None   -> Ok (Bar None)
         | '\'' , None   -> Ok Segment
         | '('  , None   -> Ok (TupletOpen None)
         | ')'  , None   -> Ok TupletClose
         | '|'  , Some n -> (parseNumber n) |>=- (fun n -> Bar (Some n))
         | '\'' , Some _ -> Error "number"
         | '('  , Some n -> (parseNumber n) |>=- (fun n -> TupletOpen (Some n))
         | ')'  , Some _ -> Error "number"
         | _             -> Error "unrecognised" )
      (fun rxmatch : Int1to9.t ress ->
         ((Rx.groupFound rxmatch 1) |> (optToRes "unrecognised"))
         |>=
         (fun (symbols:string) : Int1to9.t ress ->
            let len = String.length symbols in
            (Int1to9.create_o len) |> (optToRes "too many")) ) )
   |>=-
   (* : t ress *)
   (fun (symbol , repeat) ->
      { symbol ; repeat })
   |>
   (* add message : (t ress) *)
   (render "Indicator" linePos pos token)
