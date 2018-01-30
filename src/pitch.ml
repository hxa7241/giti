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




(*--- precalculated values ---*)

let chordListRx  = Rx.compile {|^\([^=]+\)\(=[^=]+\)*$|}
let pitchRx      = Rx.compile {|^\(..?[0-9]\)\(.*\)$|}

let portamentoRx = Rx.compile {|\([><].\??\)\|\([/\].[0-9]?\)|}
let vibratoRx    = Rx.compile {|\(v\)\|\(r\(-?.[0-9]?\)?\)|}
let harmonicRx   = Rx.compile {|[hi]|}
let trillRx      = Rx.compile {|^\(v\)\|\(r\(\(-\)?\(.\)\(.\)?\)?\)$|}



(*--- functions ---*)

(* construct *)

let strngCreate (s:string) : strng ress =
   (optToRes "string" (Int1to9.scan_o s))


let noteCreate (s:string) : note ress =
   (* format: "^[A-G][#b]?[0-8]$" *)
   (let len = (String.length s) in
      if (len = 2) || (len = 3) then Ok len else Error "note")
   |>=
   (ressAnd3p
      ", "
      (fun _ ->
         match s.[0] with
         | 'A' -> Ok A
         | 'B' -> Ok B
         | 'C' -> Ok C
         | 'D' -> Ok D
         | 'E' -> Ok E
         | 'F' -> Ok F
         | 'G' -> Ok G
         | _   -> Error "letter" )
      (fun len ->
         match (len , s.[1]) with
         | (2 , _  ) -> Ok Natural
         | (3 , '#') -> Ok Sharp
         | (3 , 'b') -> Ok Flat
         | _         -> Error "accident" )
      (fun len ->
         let octaveo = Int0to8.scan_o (string_of_char s.[len - 1]) in
         match octaveo with
         | Some octave -> Ok octave
         | None        -> Error "octave" ))
   |>=-
   (fun (letter , accident , octave) ->
      { letter ; accident ; octave })


let wordCreate (s:string) : word ress =
   let (strng , fret) = String.leadTrail s 1 in
   (ressAnd2
      ", "
      (optToRes "string" (Int1to9.scan_o  strng))
      (optToRes "fret"   (Int0to24.scan_o fret )))
   |>=-
   (fun (strng , fret) ->
      { strng ; fret })


let wordToNote (word:word) : note =
   (* standard tuning, 7-string:
      E  n4  o4
      B  n11 o3
      G  n7  o3
      D  n2  o3
      A  n9  o2
      E  n4  o2 = (f + 4) % 12 , ((f + 4) / 12) + 2
      B  n11 o1 *)
   let (notes:(letter * accident) array) =
      [|
         (C , Natural) ; (C , Sharp) ;
         (D , Natural) ; (D , Sharp) ;
         (E , Natural) ;
         (F , Natural) ; (F , Sharp) ;
         (G , Natural) ; (G , Sharp) ;
         (A , Natural) ; (A , Sharp) ;
         (B , Natural) ;
      |]
   and offsets = [| 4 ; 11 ; 7 ; 2 ; 9 ; 4 ; 11 |]
   and octaves = [| 4 ;  3 ; 3 ; 3 ; 2 ; 2 ;  1 |] in
   let note   = (word.fret :> int) + offsets.((word.strng :> int) - 1)
   and octave = octaves.((word.strng :> int) - 1) in
   {
      letter   = fst notes.(note mod 12) ;
      accident = snd notes.(note mod 12) ;
      octave   = Int0to8.create((note / 12) + octave) ;
   }


let isFirstChar (c:char) : bool =
   match c with
   | '0'..'9' | 'A'..'Z' -> true
   | _                   -> false


let parseBasic (rxmatch:Rx.rxmatch) : basic ress =
   (optToRes "pitch basic" (Rx.groupFound rxmatch 1))
   |>=
   (fun (s:string) : basic ress ->
      (* discriminate basic sum, and construct *)
      if String.isFirstChar Char.isDigit s
      then okMap (fun w -> Word w) (wordCreate s)
      else okMap (fun n -> Note n) (noteCreate s) )


let extractOpt (rx:Rx.rx) (parser:string->(_ option) ress)
   (message:string) (content:string)
   : (_ option * int) ress =
   match Rx.allMatches rx content with
   | []  -> Ok (None , 0)
   | [e] -> okMap (fun o -> o , String.length e) (parser e)
   | _   -> Error ("too many " ^ message ^ "s")


let parsePortamento (s:string) : (portamento option * int) ress =
   let parser (s:string) : (portamento option) ress =
      let parseBend (s:string) (ctor:_ -> portamento)
         : (portamento option) ress =
         (if s = "?"
         then
            Ok Micro
         else
            (String.leadTrail s 1)
            |>
            (optAnd2p
               (fst % Int1to5.scan_o)
               (fun (_ , rest) ->
                  match rest with
                  | ""  -> Some false | "?" -> Some true | _ -> None) )
            |>
            (optToRes "bend")
            |>=-
            (fun (i , b) -> Macro { semi = i ; micro = b }))
         |>=-
         (fun i -> Some (ctor i))
      and parseSlide (s:string) (ctor:_ -> portamento)
         : (portamento option) ress =
         s
         |>
         (Int0to24.scan_o % (optToRes "slide"))
         |>=-
         (fun i -> Some (ctor i))
      and firstChar , restChars = String.leadTrail s 1 in
      match firstChar with
      | ">"  -> parseBend  restChars (fun b -> BendUp b)
      | "<"  -> parseBend  restChars (fun b -> BendDown b)
      | "/"  -> parseSlide restChars (fun i -> SlideUp i)
      | "\\" -> parseSlide restChars (fun i -> SlideDown i)
      | ""   -> Ok None
      | _    -> Error "portamento"
   in
   (extractOpt portamentoRx parser "portamento" s)


let parseVibrato (s:string) : (vibrato option * int) ress =
   let parser (s:string) : (vibrato option) ress =
      let parseDigit (i:int) (rx:Rx.rxmatch) : (Int1to9.t option) ress =
         (Rx.groupFound rx i)
         |>
         (optMatch
            (*(Int1to9.scan_o %> (optToRes ("pD2:" ^ g)) %> (okMap fSome))*)
            (Int1to9.scan_o %> (optToRes "trill") %> (okMap fSome))
            (fConst (Ok None)))
      in
      (Rx.apply trillRx s "vibrato")
      |>=
      (fun (rxmatch:Rx.rxmatch) : (vibrato option) ress ->
         if String.isFirstChar ((=) 'v') (Rx.wholeFound rxmatch)
         then
            Ok (Some Vibrato)
         else
            (ressAnd3
               ""
               (Ok (optToBool (Rx.groupFound rxmatch 4)))
               (parseDigit 5 rxmatch)
               (parseDigit 6 rxmatch))
            |>=
            (fun (below , adjac , repet) ->
               Ok (Some
                  (optMapUnify
                     (fun adjac -> TrillSpec { below ; adjac ; repet ; })
                     (fun ()    -> Trill)
                     adjac)))
            (*|>
            (errorMap (fConst "vibrato"))*))
   in
   (extractOpt vibratoRx parser "vibrato" s)


let parseHarmonic (s:string) : (harmonic option * int) ress =
   let parser (s:string) : (harmonic option) ress =
      match s with
      | "h" -> Ok (Some HarmonicNeck)
      | "i" -> Ok (Some HarmonicBridge)
      | ""  -> Ok None
      | _   -> Error "harmonic"
   in
   (extractOpt harmonicRx parser "harmonic" s)


let parseExtra (rxmatch:Rx.rxmatch) : extra ress =
   (optToRes "pitch extra" (Rx.groupFound rxmatch 2))
   |>=
   (fun (extra:string) ->
      extra
      |>=+
      (* : extra ress *)
      (ressAnd3p " & " parsePortamento parseVibrato parseHarmonic)
      |>=
      (fun ((portamento , pLen) , (vibrato , vLen) , (harmonic , hLen)) ->
         if (String.length extra) > (pLen + vLen + hLen)
         then Error "pitch extra"
         else Ok { portamento ; vibrato ; harmonic } ))
   (*|>
   (errorMap (fConst "pitch extra"))*)


(*
 * Grammar (SBNF):
 *
 * (pitch
 *    (+ (, pitchbasic
 *          pitchextra)
 *       "="))
 *
 * (pitchbasic
 *    (| stringfret
 *       scorenote))
 *
 * (stringfret
 *    (, int1-9 int0-24))
 *
 * (scorenote
 *    (, (| "A" "B" "C" "D" "E" "F" "G")
 *       (? (| "#" "b"))
 *       int0-8))
 *
 * (pitchextra
 *    (, (? (| (, ">" (| "?"
 *                       (, int1-5 (? "?"))))
 *             (, "<" (| "?"
 *                       (, int1-5 (? "?"))))
 *             (, "/" int0-24)
 *             (, "\\" int0-24)))
 *       (? (| "v"
 *             (, "r" (? (, (? "-")
 *                          int1-9
 *                          (? int1-9))))))
 *       (? (| "h"
 *             "i"))))
 *)
let create (token:Token.t) : (t ress) =
   (* check overall chord form : rxmatch ress *)
   (Rx.apply chordListRx (token:>string) "pitchs unrecognised")
   |>=
   (* extract chord product : (pitch list) ress *)
   (fun rxmatch : (pitch list) ress ->
      (String.split ((=) '=') (Rx.wholeFound rxmatch))
      |>
      (* parse pitch items : (pitch ress) list*)
      (List.mapi
         (fun (index:int) (item:string) : pitch ress ->
            (* : rxmatch ress *)
            (Rx.apply pitchRx item ("pitch " ^ (string_of_int index) ))
            |>=
            (* : (basic * extra) ress *)
            (ressAnd2p ", " parseBasic parseExtra)
            |>=-
            (fun (basic , extra) -> { basic ; extra }) ))
      |>
      (* transpose results : (pitch list) ress *)
      (* (all or nothing, then merge errors) *)
      List.resAnd
      |>
      (errorMap (fun messages -> String.concat "; " messages)) )
   |>=
   (fun pitchs : t ress ->
      (* check non-zero length *)
      match pitchs with
      | pitch :: chord -> Ok { pitch ; chord ; }
      | _              -> Error "pitchs unrecognised")
