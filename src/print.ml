(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Basics




(*--- values ---*)

let _TAB_KEY_COMMENT =
{|
Tab key
-------

### layout ###

   act:        h
          |----------------|
          |----------------|
   pitch: |----7-----------|
          |----------------|
          |----------------|
          |----------------|
   time:       4


### act ###

   p : pluck (default)
   h : hammer-on
   u : pull-off
   w : sweep-pick
   s : sustain from previous
   t : tap
   i : tremolo picking
   f : pluck with finger
   b : pluck with plectrum backed by finger (quasi-harmonic)
   e : pluck string end (very near bridge)
   m : pluck string middle
   $ : damp/mute
   % : dead-note
   + : loud
   - : quiet
   < : volume increase
   > : volume decrease
   & : tied from previous


### pitch ###

   lines  : are strings from, at top, 1 (thin E) to 6 (thick E)
   number : is fret
   .      : rest


### pitch suffix ###

   >2? : bend up, 2 frets plus microtone above
   <1  : bend down, from 1 frets above (i.e. pre-bend)
   /15 : slide up, to fret 15
   \8  : slide down, to fret 8
   v   : vibrato
   r   : trill
   r14 : trill, 1 fret above (- for below), with 4 sub-sounds
   h   : neck harmonic (touching over fret)
   i   : bridge harmonic (touching with picking thumb)


### time ###

   4   : quarter-note (denominator of fraction of bar)
   -4  : tied to the left
   4-  : tied to the right
   4*  : dotted - extended by half (one or more)
   4-8 : extended by eighth
   e   : legato
   s   : staccato|}



(*--- types ---*)

type formats = { fret:bool ; note:bool ; tab:bool }



(*--- functions ---*)

let printListGeneric (f:'a -> string) (separator:string) (l:'a list) : string =

   (List.map f l)
   |>
   (String.concat separator)


let optionPrint (f:'a -> string) (o:'a option) : string =

   match o with
   | Some a -> f a
   | None   -> ""


let boolPrint (s:string) (b:bool) : string =

   if b then s else ""


let intPrint (i:int) : string =

   string_of_int i


let notePrint (note:Pitch.note) : string =

   let open Pitch in

   let letter   =
      match note.letter with
      | A -> "A"
      | B -> "B"
      | C -> "C"
      | D -> "D"
      | E -> "E"
      | F -> "F"
      | G -> "G"

   and accident =
      match note.accident with
      | Natural -> ""
      | Sharp   -> "#"
      | Flat    -> "b"

   and octave   =
      intPrint (note.octave :> int)
   in

   letter ^ accident ^ octave


let actPrint (act:Act.t) : string =

   let open Act in

   let basicPrint (basic:basic) : string =
      match basic with
      | Pluck       -> ""
      | Hammer      -> "h"
      | Pulloff     -> "u"
      | Sweep       -> "w"
      | Sustain     -> "s"
      | Tap         -> "t"
      | TremoloPick -> "i"
      | Finger      -> "f"
      | Backed      -> "b"
      | EndString   -> "e"
      | MidString   -> "m"

   and extraPrint (extra:extra) : string =
      let dampingPrint (damping:damping) : string =
         match damping with
         | Clear -> ""
         | Mute  -> "$"
         | Dead  -> "%"
      and loudnessPrint (loudness:loudness) : string =
         match loudness with
         | Medium -> ""
         | Quiet  -> "-"
         | Loud   -> "+"
      and volumePrint (volume:volume) : string =
         match volume with
         | VolumeStill -> ""
         | VolumeUp    -> "<"
         | VolumeDown  -> ">"
      in
      let damping  = dampingPrint  extra.damping
      and loudness = loudnessPrint extra.loudness
      and volume   = volumePrint   extra.volume in
      damping ^ loudness ^ volume
   in

   let basic = basicPrint act.basic
   and extra = extraPrint act.extra in
   basic ^ extra


let pitchsPrint (asNote:bool) (pitch:Pitch.t) : string =

   let open Pitch in

   let pitchPrint (asNote:bool) (pitch:pitch) : string =

      let basicPrint (asNote:bool) (basic:basic) : string =
         let wordPrint (word:word) : string =
            let strng = intPrint (word.Pitch.strng :> int)
            and fret  = intPrint (word.Pitch.fret  :> int) in
            strng ^ fret
         in
         match basic with
         | Note note             -> notePrint note
         | Word word when asNote -> notePrint (Pitch.wordToNote word)
         | Word word             -> wordPrint word

      and extraPrint (extra:extra) : string =
         let portamentoPrint (portamentoo:portamento option) : string =
            let bendPrint (bend:bend) : string =
               match bend with
               | Micro -> "?"
               | Macro { semi ; micro } ->
                  (intPrint (semi :> int)) ^ (boolPrint "?" micro)
            in
            match portamentoo with
            | Some BendUp    b -> ">"  ^ bendPrint b
            | Some BendDown  b -> "<"  ^ bendPrint b
            | Some SlideUp   i -> "/"  ^ (intPrint (i :> int))
            | Some SlideDown i -> "\\" ^ (intPrint (i :> int))
            | None             -> ""
         and vibratoPrint (vibratoo:vibrato option) : string =
            match vibratoo with
            | Some vibrato ->
               begin match vibrato with
               | Vibrato -> "v"
               | Trill   -> "r"
               | TrillSpec { below ; adjac ; repet } ->
                  let below = boolPrint "-" below
                  and adjac = intPrint (adjac :> int)
                  and repet =
                     let i19Print (i:Int1to9.t) : string =
                        intPrint (i :> int)
                     in
                     optionPrint i19Print repet
                  in
                  "r" ^ below ^ adjac ^ repet
               end
            | None -> ""
         and harmonicPrint (harmonico:harmonic option) : string =
            match harmonico with
            | Some HarmonicNeck   -> "h"
            | Some HarmonicBridge -> "i"
            | None                -> ""
         in
         let portamento = portamentoPrint extra.portamento
         and vibrato    = vibratoPrint    extra.vibrato
         and harmonic   = harmonicPrint   extra.harmonic in
         portamento ^ vibrato ^ harmonic
      in

      let basic = basicPrint asNote pitch.basic
      and extra = extraPrint pitch.extra in
      basic ^ extra
   in

   (printListGeneric (pitchPrint asNote) "=" (pitch.pitch :: pitch.chord))


let timePrint (time:Time.t) : string =

   let open Time in

   let basicPrint (basic:Time.basic) : string =
      let extensionPrint (ext:Time.extension option) : string =
         let print (ext:Time.extension) : string =
            match ext with
            | Dots  dots    ->
               String.make (dots :> int) '*'
            | Fracs (h , t) ->
               let extension =
                  (printListGeneric
                     (fun (e:Int1to999.t) -> intPrint (e :> int)) "-"
                     (h :: t))
               in
               "-" ^ extension
         in
         optionPrint print ext
      and continuationPrint (cont:bool) : string =
         boolPrint "-" cont
      in
      match basic with
      | { duration ; extension ; continuation } ->
         let duration     = intPrint (duration :> int)
         and extension    = extensionPrint extension
         and pretinuation = continuationPrint (fst continuation)
         and continuation = continuationPrint (snd continuation) in
         pretinuation ^ duration ^ extension ^ continuation
   and extraPrint (extra:Time.extra) : string =

      match (extra :> Time.articulation) with
      | ArticNorm -> ""
      | Legato    -> "e"
      | Staccato  -> "s"
   in

   let { basic ; extra } = time in
   (basicPrint basic) ^ (extraPrint extra)


let soundPrint (asNote:bool) (sound:Sound.t) : string =

   let partPrint (ao:_ option) (prec:bool) (f:_ -> string) : string =
      (* if some, print it, and a separator before or after *)
      let partPrint (a:_) : string =
         let sep (prec:bool) = boolPrint ":" prec in
         (sep prec) ^ (f a) ^ (sep (not prec))
      in
      optionPrint partPrint ao
   in

   let ordinaryPrint (asNote:bool) (sound:Sound.ordinary) : string =
      let open Sound in
      let act   =
         let fAsNote = if asNote then Act.wordToNote else id in
         partPrint (fAsNote sound.act) false actPrint
      and pitch = pitchsPrint asNote sound.pitch
      and time  = partPrint sound.time true timePrint in
      act ^ pitch ^ time

   and specialPrint (symbol:string) (timeo:Time.t option) : string =
      let time = partPrint timeo true timePrint in
      symbol ^ time
   in

   let open Sound in
   match sound with
   | Ordinary     o -> ordinaryPrint asNote o
   | Continuation c -> specialPrint "&" (c :> Time.t option)
   | Rest         r -> specialPrint "." (r :> Time.t option)


let indicatorPrint (indicator:Indicator.t) : string =

   let open Indicator in

   let symbol , number =
      let numberPrint =
         optionPrint (fun (i:Int1to9.t) -> intPrint (i :> int))
      in
      match indicator.symbol with
      | Bar         io -> ("|" , numberPrint io)
      | Segment        -> ("'" , "")
      | TupletOpen  io -> ("(" , numberPrint io)
      | TupletClose    -> (")" , "")

   and repeat = (indicator.repeat :> int) in

   (String.repeat symbol repeat) ^ number


let annotationPrint (annotation:Annotation.t) : string =

   let itemPrint (item:Annotation.item) : string =
      let open Annotation in

      let versionPrint (version:version) : string =
         let head = (version.versionHead :> string)
         and body = (version.versionBody :> string) in
         head ^ ":" ^ body

      and tuningPrint (tuning:tuning) : string =
         let bodyPrint (tuning:tuning) : string =
            let specPrint (tuningspec:tuningSpec) : string =
               let strng = intPrint (tuningspec.tuningSpecString :> int)
               and note  = notePrint tuningspec.tuningSpecNote in
               strng ^ note
            in
            match tuning with
            | TuningStd t-> (t :> string)
            | TuningShift { semi ; micro } ->
               (Printf.sprintf "%+i" (semi :> int)) ^ (boolPrint "?" micro)
            | TuningSpec (h,t) -> printListGeneric specPrint "=" (h :: t)
         in
         let tuning = bodyPrint tuning in
         "tuning:" ^ tuning

      and tempoPrint (tempo:tempo) : string =
         let tempoPrint (symbol:string) (int0:int) (int1:int) : string =
            let int0 = intPrint int0
            and int1 = intPrint int1 in
            "tempo:" ^ int0 ^ symbol ^ int1
         in
         match tempo with
         | TempoDuration d ->
            tempoPrint "." (d.tempoDuraInt:>int) (d.tempoDuraFrac:>int)
         | TempoRate     r ->
            tempoPrint "=" (r.tempoRateFrac:>int) (r.tempoRateBpm:>int)
      in

      match item with
      | Version v -> versionPrint v
      | Tuning  t -> tuningPrint  t
      | Tempo   t -> tempoPrint   t
   in

   let itemListPrint (items:Annotation.t) : string =
      printListGeneric itemPrint " " (items :> Annotation.item list)
   in

   let annotations = itemListPrint annotation in
   "@ " ^ annotations


let metadataPrint (metadata:Piece.metadata) : string =

   let open Piece in
   let name , value = (metadata.name :> string) , metadata.value in

   "! " ^ name ^ " : " ^ value


module Tab :
sig

   val contentListPrint : Piece.content list -> string

end
=
struct

   let _MIN_STRING_COUNT = 6

   let transpose (grid:string list list) : string list list =
      (* assumes sub-lists are same length,
         so bare minimum checking/remediation *)

      let grid = List.rev grid
      and init =
         let len = try List.length (List.hd grid) with Failure _ -> 0 in
         List.unfoldl (fConst []) len
      and rowAppender (lls:string list list) (ls:string list) : 'a list list =
         (* short sub-lists are simply omitted *)
         try List.rev (List.rev_map2 List.cons ls lls) with
         | Invalid_argument _ -> lls
      in
      List.fold_left rowAppender init grid


   let makeColumn (noOfStrings:int)
      (head:string) (main:int -> string) (foot:string)
      : string list =

      let main = (succ % main) in
      head :: (List.unfoldl ~list:[foot] main noOfStrings)


   let soundColumnRender (noOfStrings:int) (sound:Sound.t) : string list =

      let (columnRagged :string list) =
         let open Sound in
         let head , main , foot =
            match sound with
            | Ordinary { act ; pitch ; time } ->
               let act  = optionPrint actPrint  act
               and time = optionPrint timePrint time
               and (pitchs :(int * string) list) =
                  (* TODO? refactor: use data-struct *)
                  let pitchs =
                     String.split ((=) '=') (pitchsPrint false pitch)
                  and getString (s:string) : (int * string) =
                     let h , t = String.leadTrail s 1 in
                     match (String.toInt h) with
                     | Some i -> (i , t)
                     (* put note-form pitches on line 1 *)
                     | None   -> (1 , s)
                  in
                  (List.map getString pitchs)
               in
               let pitchi (i:int) : string =
                  excToDefault "-" (fun () -> List.assoc i pitchs)
               in
               ( act , pitchi , time)
            | Continuation time ->
               let time = optionPrint timePrint (time :> Time.t option) in
               ( "&" , (fConst "-") , time )
            | Rest time ->
               let time = optionPrint timePrint (time :> Time.t option) in
               ( " " , (fConst ".") , time )
         in
         makeColumn noOfStrings head main foot
      in

      let (maxWidth:int) =
         let maxLen (maxSoFar:int) (s:string) : int =
            max maxSoFar (String.length s)
         in
         List.fold_left maxLen 0 columnRagged
      in

      let (columnPadded :string list) =
         let padding = makeColumn noOfStrings " " (fConst "-") " "
         and pad (s:string) (padding:string) : string =
            let amount = max 0 (maxWidth - (String.length s)) in
            s ^ (String.repeat padding amount)
         in
         List.map2 pad columnRagged padding
      in

      columnPadded


   let indicatorColumnRender (noOfStrings:int) (indicator:Indicator.t)
      : string list =

      let columnRepFirst (i:int) (c:string list) : string list =
         let repFirst (i:int) (s:string) : string =
            let lead , trail = String.leadTrail s 1 in
            (String.repeat lead i) ^ trail
         in
         List.map (repFirst i) c
      in

      let open Indicator in

      let column =
         let head , main , foot =
            match indicator.symbol with
            | Bar        io ->
               begin match io with
               | None   -> (" " , (fConst "|") , " ")
               | Some i ->
                  let foot = "|" ^ (intPrint (i:>int)) ^ ":" in
                  ("   " , (fConst "|--") , foot)
               end
            | TupletOpen io ->
               begin match io with
               | None   -> (" " , (fConst "-") , "(")
               | Some i ->
                  let foot = "(" ^ (intPrint (i:>int)) in
                  ("  " , (fConst "--") , foot)
               end
            | TupletClose   -> (" " , (fConst "-") , ")")
            | Segment       -> (" " , (fConst "-") , "'")
         in
         makeColumn noOfStrings head main foot
      in

      columnRepFirst (indicator.repeat :> int) column


   let getLargestStrng (contents:Piece.content list) : int =
      (* TODO? refacto: move parts into module accessors *)

      let getLargestStrng (maxSoFar:int) (item:Piece.content) : int =
         let (strng:int option) =
            let getStrng (sound:Sound.t) : int option =
               let getStrng (pitch:Pitch.t) : int =
                  let open Pitch in
                  let getLargestStrng (maxSoFar:int) (note:pitch) : int =
                     let (strng:int option) =
                        match note.basic with
                        | Word { strng ; _ } -> Some (strng :> int)
                        | Note _             -> None
                     in
                     match strng with
                     | Some i -> max maxSoFar i
                     | None   -> maxSoFar
                  and { pitch ; chord } = pitch in
                  List.fold_left getLargestStrng 0 (pitch :: chord)
               in
               let open Sound in
               match sound with
               | Ordinary {act= _ ; pitch ; time= _} -> Some (getStrng pitch)
               | Continuation _                      -> None
               | Rest         _                      -> None
            in
            match item with
            | Piece.Sound sound -> getStrng sound
            | Piece.Indicator _ -> None
         in
         match strng with
         | Some i -> max maxSoFar i
         | None   -> maxSoFar
      in

      List.fold_left getLargestStrng _MIN_STRING_COUNT contents


   let contentListPrint (contents:Piece.content list) : string =

      let noOfStrings = getLargestStrng contents in

      let gridRender (noOfStrings:int) (contents:Piece.content list)
         : (string list) list =
         let itemRender (item:Piece.content) : string list =
            let open Piece in
            match item with
            | Sound s     -> soundColumnRender noOfStrings s
            | Indicator i -> indicatorColumnRender noOfStrings i
         and maybePadEnds (grid:(string list) list) : (string list) list =
            let padder (eo:Piece.content option) f grid : (string list) list =
               let isBar (item:Piece.content option) : bool =
                  let open Piece in let open Indicator in
                  match item with
                  | Some (Indicator { symbol = Bar _ ; _ })           -> true
                  | Some (Indicator
                     { symbol = (Segment | TupletClose | TupletOpen _) ; _ })
                  | Some (Sound _)
                  | None                                              -> false
               and hyphens = makeColumn noOfStrings " " (fConst "-") " " in
               if not (isBar eo) then (f grid (hyphens :: [])) else grid
            in
            (padder (List.hdo contents) (swp (@)) grid)
            |>
            (padder (List.fto contents) (@))
         in
         (* expand to 2D: list of items to list of columns (of elems) *)
         (List.map itemRender contents)
         |>
         maybePadEnds

      and rowsPrint (noOfStrings:int) (grid:(string list) list) : string =
         let concatRow (index:int) (row:string list) : string =
            (* middle rows are main tab lines *)
            let sep = if (index > 0) && (index <= noOfStrings) then "-" else " "
            and pre = "#= " in
            pre ^ (String.concat sep row)
         in
         (* transpose: list of columns to list of rows (of elems) *)
         (transpose grid)
         |>
         (* reduce to 1D *)
         (List.mapi concatRow)
         |>
         (String.concat "\n")
         |>
         (Printf.sprintf "\n%s\n")
      in

      (gridRender noOfStrings contents)
      |>
      (rowsPrint noOfStrings)

end


let itemPrint (formats:formats) (item:Piece.item) : string =

   (* (each item is a line) *)

   let open Piece in

   let contentListPrint (formats:formats) (contents:content list) : string =
      let contentPrint (asNote:bool) (content:content) : string =
         match content with
         | Sound     s -> soundPrint asNote s
         | Indicator i -> indicatorPrint i
      in
      let fret =
         if formats.fret
         then Some (printListGeneric (contentPrint false) " " contents)
         else None
      and note =
         if formats.note
         then Some ("#^ " ^ (printListGeneric (contentPrint true) " " contents))
         else None
      and tab =
         if formats.tab
         then Some (Tab.contentListPrint contents)
         else None
      in
      let chosen  = List.filtmap id [tab ; fret ; note] in
      String.concat "\n" chosen
      (*let withGaps = List.map (fun s -> s ^ "\n") chosen in
      String.concat "" withGaps*)

   and commentPrint (comment:Line.t) : string =
      let comment = (comment :> string) in
      "#" ^ comment
   in

   match item with
   | Content c    -> contentListPrint formats c
   | Annotation a -> annotationPrint a
   | Metadata   m -> metadataPrint m
   | Comment line -> commentPrint line
   | Blankline    -> ""


let print (formats:formats) (piece:Piece.t) : string =

   let body =
      printListGeneric (itemPrint formats) "\n" (piece :> Piece.item list)
   and footer =
      if not formats.tab
      then ""
      else
         let prefixedTabKey =
            String.concat "\n# " (String.split Char.isNewline _TAB_KEY_COMMENT)
         in
         "\n\n\n" ^ prefixedTabKey ^ "\n"
   in

   body ^ footer
