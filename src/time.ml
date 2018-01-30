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




(*--- precalculated values ---*)

let timeRx =
   Rx.compile {|^\(\(-\)?\([0-9]+\)\(\(-[0-9]+\|\*\)*\)\(-\)?\)?\([^0-9]+\)?$|}




(*--- functions ---*)

(* construct *)

let parseBasic (rxmatch:Rx.rxmatch) : basic ress =
   match
      (Rx.groupFound rxmatch 2) ,
      (Rx.groupFound rxmatch 3) ,
      (Rx.groupFound rxmatch 4) ,
      (Rx.groupFound rxmatch 6)
   with
   | _             , None          , _          , _             ->
      Error "basic duration"
   | pretinuationo , Some duration , extensiono , continuationo ->
      let duration =
         optToRes "basic duration" (Int0to999.scan_o duration)
      and extension =
         (optUnify (fConst "") extensiono)
         |>
         (fun (ext:string) : (extension option) ress ->
            (* discriminate sum *)
            if String.isEmpty ext
            then
               Ok None
            else if String.isFirstChar ((=) '*') ext
            then
               (* dots *)
               if String.check ((=) '*') ext
               then Ok (Some (Dots (Int1to5.create (String.length ext))))
               else Error "basic dots"
            else
               (* fractions *)
               (* : string list *)
               (String.split ((=) '-') (String.trail ext 1))
               |>
               (List.map
                  (fun s : Int1to999.t ress ->
                     optToRes "" (Int1to999.scan_o s)))
               |>
               (* (Int1to999.t list , string list) result *)
               List.resAnd
               |>
               (resMap
                  (  (function
                        | []     -> None
                        | h :: t -> Some (Fracs (h , t))) ,
                     (fConst "basic extension"))) )
      and continuation =
         Ok ((optToBool pretinuationo) , (optToBool continuationo))
      in
      (ressAnd3 " && " duration extension continuation)
      |>=-
      (fun (duration , extension , continuation) ->
         { duration ; extension ; continuation ; })


let parseArticulation (rxmatch:Rx.rxmatch) : articulation ress =
   match (Rx.groupFound rxmatch 7) with
   | Some s ->
      begin match s with
      | "e" -> Ok Legato
      | "s" -> Ok Staccato
      | s   ->
         Error ("extra " ^
            (if (String.length s > 1)
            then "too many" else "unrecognised"))
      end
   | None -> Ok ArticNorm


(*
 * Grammar (SBNF):
 *
 * (time
 *    (, (? timebasic)
 *       (? timeextra)))
 *
 * (timebasic
 *    (, (? "-")
 *       int0-999
 *       (| ( * "*")
 *          ( * (, "-" int1-999)))
 *       (? "-")))
 *
 * (timeextra
 *    (| "e" "s"))
 *)
let create (token:Token.t) : (t ress) =
   let token = (token:>string) in
   (* check overall main form : Rx.rxmatch ress *)
   (Rx.apply timeRx token "unrecognised")
   |>=
   (* : (basic * articulation) ress *)
   (ressAnd2p " & " parseBasic parseArticulation)
   |>
   (errorMap (fun e -> "time " ^ e))
   |>=-
   (fun (basic , extra) : t ->
      { basic ; extra })
