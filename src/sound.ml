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




(*--- precalculated values ---*)

let ordinaryRx     =
   Rx.compile {|^\(\([a-z]?[^:0-9]*\):\)?\([^:]+\)\(:\([^:]+\)\)?$|}
let continuationRx = Rx.compile {|^\([-&]\)\(:\([^:]+\)\)?$|}
let restRx         = Rx.compile {|^\([.]\)\(:\([^:]+\)\)?$|}




(*--- functions ---*)

(* construct *)

(* (will not type-infer correctly when defined inside a function) *)
let optionalCreate (rxmatch:Rx.rxmatch) (index:int)
   (create:(Token.t -> ('t ress)))
   : ('t option) ress =
   (Rx.groupFound rxmatch index)
   |>-
   (fun objStr : 't ress option ->
      (Token.createFilter objStr) |> create |> fSome)
   |>
   resoptToOptres


let parseOrdinary (token:string) (linePos:int) (pos:int) : (t ress) =
   (* check overall form : Rx.rxmatch ress *)
   (Rx.apply ordinaryRx token "ordinary")
   |>=
   (* : ((Pitch.t * (Act.t option) * (Time.t option)) , string) result *)
   (ressAnd3p
      "; "
      (fun rxmatch -> optionalCreate rxmatch 2 Act.create)
      (fun rxmatch ->
         (* pitch is obligatory (although overall check should ensure its
            presence) *)
         match (optionalCreate rxmatch 3 Pitch.create) with
         | Ok None         -> Error "missing pitch"
         | Ok (Some pitch) -> Ok pitch
         | Error message   -> Error message)
      (fun rxmatch -> optionalCreate rxmatch 5 Time.create))
   |>=-
   (* : t ress *)
   (fun (act , pitch , time) ->
      Ordinary { act ; pitch ; time ; })
   |>
   (render "Sound" linePos pos token)


let parseSpecial (token:string) (rx:Rx.rx) (name:string)
   (wrap:(Time.t option -> t)) (linePos:int) (pos:int)
   : (t ress) =
   (* check overall form : Rx.rxmatch ress *)
   (Rx.apply rx token name)
   |>=
   (* : (Time.t option) , string) result *)
   (fun rxmatch -> optionalCreate rxmatch 3 Time.create)
   |>=-
   (* : t ress *)
   wrap
   |>
   (render ("Sound " ^ name) linePos pos token)


(*
 * Grammar (SBNF):
 *
 * (sound
 *    (| ordinary
 *       continuation
 *       rest))
 *
 * (ordinary
 *    (, (? (, act ":"))
 *       pitch
 *       (? (, ":" time))))
 *
 * (continuation
 *    (, "&" (? (, ":" time))))
 *
 * (rest
 *    (, "." (? (, ":" time))))
 *)
let create (token:Token.tpos) (linePos:int) : (t ress) =
   let str , pos = Token.toStringPos token in
   let firstChar = str.[0] in
   (* discriminate sum, and construct : (t ress) *)
   if (Act.isFirstChar   firstChar) ||
      (Pitch.isFirstChar firstChar)
   then
      (* (act:)?pitch(:act)? *)
      parseOrdinary str linePos pos
   else
      match firstChar with
      | '-' | '&'  ->
         parseSpecial
            str continuationRx "continuation"
            (fun time -> Continuation time) linePos pos
      | '.'  ->
         parseSpecial
            str restRx "rest"
            (fun time -> Rest time) linePos pos
      | _    ->
         (Error "unrecognised")
         |>
         (render "Sound" linePos pos str)
