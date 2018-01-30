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

let version = "e-4.1"

let default : t =
   [  Version
         {  versionHead = (Token.createFilter "giti")
         ;  versionBody = (Token.createFilter version ) }
   ;  Tuning
         (TuningStd (Token.createFilter "std"))
   ;  Tempo
         (TempoRate
            {  tempoRateFrac = (Int1to99.create 4)
            ;  tempoRateBpm  = (Int1to999.create 90) } ) ]




(*--- precalculated values ---*)

let tuningSpecRx     = Rx.compile {|^\([^=]+\)\(=[^=]+\)*$|}
let tuningSpecItemRx = Rx.compile {|^\(.\)\(..?.\)$|}
let tempoRx          = Rx.compile {|^\([^.=]+\)[.=]\([^.=]+\)$|}




(*--- functions ---*)

(* construct *)

(*
 * Grammar (SBNF):
 *
 * (version
 *    (, "giti:"
 *       ( * non-blank)))
 *)
let parseVersion (name:string) (body:string) (linePos:int) (_:int)
   (bodyPos:int)
   : (item ress) =
   if body = version
   then
      Ok (Version {
         versionHead = Token.createFilter name ;
         versionBody = Token.createFilter body ; })
   else
      Error (errorString linePos bodyPos body
         "Annotation-version not "
         ^ version
         ^ " - not exact match for this parser")


(*
 * Grammar (SBNF):
 *
 * (tuning
 *    (, "tuning:"
 *       (| "std"
 *          (, (| "-" "+") int0-12 (? "?"))
 *          (+ (, int1-9
 *                (| "A" "B" "C" "D" "E" "F" "G")
 *                (? (| "#" "b"))
 *                int0-8)
 *             "="))))
 *)
let parseTuning (_:string) (body:string) (linePos:int) (_:int)
   (bodyPos:int)
   : (item ress) =
   (* discriminate sum, and construct : tuning ress *)
   begin if body = "std"
   (* std *)
   then
      (* extract token : Token.t *)
      Ok (TuningStd (Token.createFilter body))
   else if (String.notEmpty body) && (Char.isSign (body.[0]))
   (* uniform shift *)
   then
      (* extract product *)
      let micro = (String.last body) = '?' in
      let body =
         if not micro
         then body else String.lead body (String.lastPos body)
      in
      (* : IntM12toP12.t ress *)
      (optToRes "shift amount" (IntM12toP12.scan_o body))
      |>=-
      (fun semi -> TuningShift { semi ; micro })
   (* string spec *)
   else
      (* extract product : tuningSpec list *)
      (Rx.apply tuningSpecRx body "spec")
      |>=
      (fun _ : (tuningSpec list) ress ->
         (* parse spec items : (tuningSpec ress) list*)
         (String.split ((=) '=') body)
         |>
         (List.mapi
            (fun index item : tuningSpec ress ->
               (Rx.apply tuningSpecItemRx item "")
               |>=
               (fun rxmatch : (Pitch.strng * Pitch.note) ress ->
                  ressAnd2
                     ", "
                     ((optToRes "string" (Rx.groupFound rxmatch 1))
                        |>= Pitch.strngCreate)
                     ((optToRes "note" (Rx.groupFound rxmatch 2))
                        |>= Pitch.noteCreate) )
               |>
               (okMap (fun (strng , note) ->
                  { tuningSpecString = strng ;
                    tuningSpecNote   = note  ; }))
               |>
               (errorMap (fun message ->
                  "spec item " ^ (string_of_int (index + 1)) ^
                  (if String.notEmpty message then " " else "") ^ message))))
         (* transpose results : (tuningSpec list) ress *)
         (* (all or nothing, then merge errors) *)
         |>
         (List.resAnd %
            (errorMap (fun messages -> String.concat "; " messages))) )
      (* reject empty list *)
      |>=
      (fun specs ->
         match specs with
         | []     -> Error "spec"
         | h :: t -> Ok (TuningSpec (h , t)) )
   end
   (* wrap in item type, and message : (item ress) *)
   |>=-
   (fun t -> Tuning t)
   |>
   (render "Annotation-tuning" linePos bodyPos body)


(*
 * Grammar (SBNF):
 *
 * (tempo
 *    (, "tempo:"
 *       (| (, int0-99 "." int0-999)
 *          (, int1-99 "=" int1-999))))
 *)
let parseTempo (_:string) (body:string) (linePos:int) (_:int)
   (bodyPos:int)
   : (item ress) =
   (* apply regex : unit ress *)
   (Rx.apply tempoRx body "body")
   |>=
   (* discriminate sum, and construct : tempo ress *)
   (fun rxmatch : tempo ress ->
      begin if optToBool (String.index_o '=' body)
      (* note BPM *)
      then
         (* extract product matched by regex : ('a * 'b) ress *)
         (ressAnd2
            ", "
            (optToRes "rate frac"
               ((Rx.groupFound rxmatch 1) |>- Int1to99.scan_o))
            (optToRes "rate bpm"
               ((Rx.groupFound rxmatch 2) |>- Int1to999.scan_o)))
         |>=-
         (* assemble product into structure : tempo ress *)
         (fun (frac , bpm) ->
            TempoRate {
               tempoRateFrac = frac;
               tempoRateBpm  = bpm } )
      (* bar duration *)
      else
         (* extract product matched by regex : ('a * 'b) ress *)
         (ressAnd2
            ", "
            (optToRes "duration int"
               ((Rx.groupFound rxmatch 1) |>- Int0to99.scan_o))
            (optToRes "duration frac"
               ((Rx.groupFound rxmatch 2) |>- Int0to999.scan_o)))
         |>=-
         (* assemble product into structure : tempo ress *)
         (fun (inte , frac) ->
            TempoDuration {
               tempoDuraInt  = inte ;
               tempoDuraFrac = frac } )
      end)
   (* wrap in item type, and message : (item ress) *)
   |>=-
   (fun t -> Tempo t)
   |>
   (render "Annotation-tempo" linePos bodyPos body)


(*
 * Grammar (SBNF):
 *
 * (annotation
 *    (, "@"
 *       ( * blank)
 *       ( * (| version
 *              tuning
 *              tempo)
 *           (+ blank))
 *       newline))
 *)
let create (line:Line.tpos) : (t ress) =
   (* split text line into tokens, then map those through parsers *)
   let linePos = Line.((posPlus1 line.pos).idx)
   and (tokens:Token.tpos list) =
      (* discard leading annotation-line marker *)
      List.tl ( Token.createAnalyse (line.Line.str :> string) )
   in
   (* : (item ress) list *)
   (List.map
      (* construct value of item sum-type : (item ress) *)
      (fun (tokenWithPos:Token.tpos) : (item ress) ->
         let str = (tokenWithPos.Token.str :> string)
         and pos = Token.((posPlus1 tokenWithPos.pos).chr) in
         let name , body , bodyOffset =
            noneDefault (fun () -> (str , "" , String.length str))
               (String.halve ':' str)
         in
         let bodyPos = pos + bodyOffset + 1 in
         (* discriminate sum, and construct : item ress *)
         match name with
         | "giti"   -> parseVersion name body linePos pos bodyPos
         | "tuning" -> parseTuning  name body linePos pos bodyPos
         | "tempo"  -> parseTempo   name body linePos pos bodyPos
         | t        ->
            Error (unrecognisedString linePos pos "Annotation" t)
         )
      tokens)
   |>
   (* all or nothing item validity, and concat messages : (t ress) *)
   (List.resAnd % (errorMap (String.concat "\n")))
