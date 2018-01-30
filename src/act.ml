(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral
open Basics




(*--- types ---*)

type volume    = | VolumeStill | VolumeUp | VolumeDown
type loudness  = | Medium | Quiet | Loud
type damping   = | Clear | Mute | Dead

type extra     =
   { damping:damping ; loudness:loudness ; volume:volume ; }
type basic     =
   | Pluck   | Hammer | Pulloff     | Sweep
   | Sustain | Tap    | TremoloPick
   | Finger  | Backed | EndString | MidString

type t         = { basic:basic ; extra:extra ; }




(*--- functions ---*)

(* query *)

let isFirstChar (c:char) : bool =
   match c with
   | 'a'..'z' | '$' | '%' | '+' | '-' | '<' | '>' -> true
   | _                                            -> false


(* construct *)

let wordToNote  (acto:t option) : t option =
   acto
   |>-
   (* default basic (which prints blank),
      and if extra is defaults, disappear everything *)
   (fun act : t option ->
      match act.extra with
      | { damping = Clear ; loudness = Medium ; volume = VolumeStill } -> None
      | { damping = Clear ; loudness = Medium ; volume = VolumeUp    }
      | { damping = Clear ; loudness = Medium ; volume = VolumeDown  }
      | { damping = Clear ; loudness = Quiet  ; volume = VolumeStill }
      | { damping = Clear ; loudness = Quiet  ; volume = VolumeUp    }
      | { damping = Clear ; loudness = Quiet  ; volume = VolumeDown  }
      | { damping = Clear ; loudness = Loud   ; volume = VolumeStill }
      | { damping = Clear ; loudness = Loud   ; volume = VolumeUp    }
      | { damping = Clear ; loudness = Loud   ; volume = VolumeDown  }
      | { damping = Mute  ; loudness = Medium ; volume = VolumeStill }
      | { damping = Mute  ; loudness = Medium ; volume = VolumeUp    }
      | { damping = Mute  ; loudness = Medium ; volume = VolumeDown  }
      | { damping = Mute  ; loudness = Quiet  ; volume = VolumeStill }
      | { damping = Mute  ; loudness = Quiet  ; volume = VolumeUp    }
      | { damping = Mute  ; loudness = Quiet  ; volume = VolumeDown  }
      | { damping = Mute  ; loudness = Loud   ; volume = VolumeStill }
      | { damping = Mute  ; loudness = Loud   ; volume = VolumeUp    }
      | { damping = Mute  ; loudness = Loud   ; volume = VolumeDown  }
      | { damping = Dead  ; loudness = Medium ; volume = VolumeStill }
      | { damping = Dead  ; loudness = Medium ; volume = VolumeUp    }
      | { damping = Dead  ; loudness = Medium ; volume = VolumeDown  }
      | { damping = Dead  ; loudness = Quiet  ; volume = VolumeStill }
      | { damping = Dead  ; loudness = Quiet  ; volume = VolumeUp    }
      | { damping = Dead  ; loudness = Quiet  ; volume = VolumeDown  }
      | { damping = Dead  ; loudness = Loud   ; volume = VolumeStill }
      | { damping = Dead  ; loudness = Loud   ; volume = VolumeUp    }
      | { damping = Dead  ; loudness = Loud   ; volume = VolumeDown  } as extra
         -> Some { basic = Pluck ; extra } )


(*
 * Grammar (SBNF):
 *
 * (act
 *    (, (? actbasic)
 *       (? actextra)))
 *
 * (actbasic
 *    (| "p" "h" "u" "w" "s" "t" "i" "f" "b" "e" "m"))
 *
 * (actextra
 *    (, (? (| "$" "%"))
 *       (? (| "+" "-"))
 *       (? (| "<" ">"))))
 *)
let create (token:Token.t) : (t ress) =
   let parsePart chars defaulter discriminator message : _ ress =
      begin match String.length chars with
      | 0 -> Ok (defaulter ())
      | 1 -> discriminator chars.[0]
      | _ -> Error "too many"
      end
      |>
      (errorMap (fun e -> message ^ " " ^ e))
   and extraDiscrimator (c0,v0:(char * _)) (c1,v1:(char * _)) (c:char)
      : _ ress =
      if      c = c0 then Ok v0
      else if c = c1 then Ok v1
      else Error "unrecognised"
   in
   let either (c0:char) (c1:char) (c:char) : bool =
      (c = c0) || (c = c1)
   in
   let tokenStr = (token :> string) in
   let basics = String.filter Char.isAlpha tokenStr
   and damps  = String.filter (either '$' '%') tokenStr
   and louds  = String.filter (either '+' '-') tokenStr
   and volus  = String.filter (either '<' '>') tokenStr
   in
   let allRecognised =
      let recogniseds =
         String.concat "" [basics ; damps ; louds ; volus]
      in
      (String.length tokenStr) = (String.length recogniseds)
   in
   (* : unit ress *)
   (optToRes "extra unrecognised" (boolToOpt allRecognised))
   |>=
   (* : (basic * (extra1 * extra2 * extra3)) ress *)
   (ressAnd2p
      " & "
      (fun () : basic ress ->
         parsePart basics
            (fConst Pluck)
            (function
               | 'p' -> Ok Pluck
               | 'h' -> Ok Hammer
               | 'u' -> Ok Pulloff
               | 'w' -> Ok Sweep
               | 's' -> Ok Sustain
               | 't' -> Ok Tap
               | 'i' -> Ok TremoloPick
               | 'f' -> Ok Finger
               | 'b' -> Ok Backed
               | 'e' -> Ok EndString
               | 'm' -> Ok MidString
               | _   -> Error "basic unrecognised")
            "basic")
      (ressAnd3p
         " & "
         (fun () : damping ress ->
            parsePart damps
               (fConst Clear)
               (extraDiscrimator ('$' , Mute) ('%' , Dead))
               "extra-damping")
         (fun () : loudness ress ->
            parsePart louds
               (fConst Medium)
               (extraDiscrimator ('-' , Quiet) ('+' , Loud))
               "extra-loudness")
         (fun () : volume ress ->
            parsePart volus
               (fConst VolumeStill)
               (extraDiscrimator ('<' , VolumeUp) ('>' , VolumeDown))
               (*(function
                  | '<' -> (Ok VolumeUp) | '>' -> (Ok VolumeDown)
                  | _   -> Error "unrecognised")*)
               "extra-volume")))
   |>
   (errorMap (fun e -> "act " ^ e))
   |>=-
   (fun (basic , (damping , loudness , volume)) : t ->
      {  basic ;
         extra = { damping ; loudness ; volume } })
