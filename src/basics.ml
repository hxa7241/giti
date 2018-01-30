(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral




(*--- bounded numbers : guaranteed within lower and upper bounds ---*)

(*module Bounded
   (Type  : sig type t end)
   (Bound : sig val lower:Type.t val upper:Type.t end) :
sig
   type t       = private Type.t
   val create   : Type.t -> t
   val create_o : Type.t -> t option
end
=
struct
   type t = Type.t

   let create (i:Type.t) : t =
      min (max i Bound.lower) Bound.upper

   let create_o (i:Type.t) : t option =
      if (i >= Bound.lower) && (i <= Bound.upper)
      then Some i else None
end*)


module type BoundedIntSig =
sig
   type t       = private int
   val create   : int -> t
   val create_o : int -> t option
   val scan_o   : string -> t option
end


module BoundedInt
   (Bound : sig val lower:int val upper:int end) :
BoundedIntSig
=
struct
   type t = int

   let create (i:int) : t =
      min (max i Bound.lower) Bound.upper

   let create_o (i:int) : t option =
      if (i >= Bound.lower) && (i <= Bound.upper)
      then Some i else None

   let scan_o (input:string) : t option =
      (String.toInt
         ~signed:(Bound.lower < 0)
         ~widthMaxed:(Int.digitsDec Bound.upper)
         input)
      |>-
      create_o
end


module IntM12toP12 = BoundedInt (struct let lower = ~-12 let upper = ~+13 end)
module Int1to5     = BoundedInt (struct let lower = 1    let upper = 5    end)
module Int1to9     = BoundedInt (struct let lower = 1    let upper = 9    end)
module Int0to8     = BoundedInt (struct let lower = 0    let upper = 8    end)
module Int0to24    = BoundedInt (struct let lower = 0    let upper = 24   end)
module Int1to99    = BoundedInt (struct let lower = 1    let upper = 99   end)
module Int1to999   = BoundedInt (struct let lower = 1    let upper = 999  end)
module Int0to99    = BoundedInt (struct let lower = 0    let upper = 99   end)
module Int0to999   = BoundedInt (struct let lower = 0    let upper = 999  end)

module Int0to11    = BoundedInt (struct let lower = 0    let upper = 11   end)




(*--- constrained strings : sets of string 'tokens' ---*)

(*module StringAscii :
sig
   type t = private string

   val createFilter : string -> t
   val createCheck  : string -> t option
end
=
struct
   type t = string

   let createFilter (s:string) : t =
      filterAscii s

   let createCheck (s:string) : t option =
      if ( String.check Char.isAscii ) s
      then Some s else None
end*)


(*module StringNonEmpty :
sig
   type t = private string

   val createCheck : string -> t option
end
=
struct
   type t = string

   let create (s:string) : t option =
      if String.isEmpty s then None else Some s
end*)


module type StringSectionSig =
sig
   (*include module type of String*)

   type t    = private string
   type pos  = { idx:int ; chr:int }
   type tpos = { str:t ; pos:pos }

   (** Delete splitting-chars. *)
   val createFilter  : string -> t

   (** None if any splitting-chars. *)
   val createCheck   : string -> t option

   (** 'Tokenise' by splitting-chars, yielding the 'token's and their
    *  split-indexs (line-numbers, if split is \n) and char positions. *)
   val createAnalyse : string -> tpos list

   (** Pos incremented by one -- 1-indexed -- for UI output. *)
   val posPlus1 : pos -> pos

   val toStringp   : tpos -> string
   val toStringPos : tpos -> (string * int)
end


module StringSection
   (Splitter : sig val f : char -> bool val e : bool end) :
StringSectionSig
=
struct
   (*include String*)

   type t    = string
   type pos  = { idx:int ; chr:int }
   type tpos = { str:t ; pos:pos }

   let createFilter (s:string) : t =
      String.filter (fNot Splitter.f) s

   let createCheck (s:string) : t option =
      if String.check (fNot Splitter.f) s
      then Some s else None

   let createAnalyse (s:string) : tpos list =
      (* split on *each* true predicate, then delete empty parts *)
      (String.splitp Splitter.f s)
      |>
      (List.mapi (fun idx (str , chr) -> {str ; pos = {idx ; chr}}))
      |>
      (List.filter (fun {str ; _} -> Splitter.e || (String.notEmpty str)))

   let posPlus1 (pos:pos) : pos =
      {  idx = pos.idx + 1 ;
         chr = pos.chr + 1 ; }

   let toStringp (tpos:tpos) : string =
      tpos.str

   let toStringPos (tpos:tpos) : (string * int) =
      let token = tpos.str
      and pos   = (posPlus1 tpos.pos).chr in
      (token , pos)
end


module Token = StringSection (struct let f = Char.isBlank   let e = false end)
module Line  = StringSection (struct let f = Char.isNewline let e = true  end)

(*module Token : StringSectionSig =
   StringSection (struct let f = Char.isBlank end)
module Line  : StringSectionSig =
   StringSection (struct let f = Char.isNewline end)*)




(*--- message string makers ---*)

let messageString (level:string) (line:int) (char:int) (token:string)
   (concern:string) : string =
   Printf.sprintf "%s: line %i char %i : \"%s\" - %s."
      level line char token concern

(* TODO: remove ? *)
(*let warningString : (int -> int -> string -> string -> string) =
   (messageString "+++ warning")*)

let errorString (line:int) (char:int) (token:string) (concern:string) : string =
   messageString "*** error" line char token concern

(* TODO: remove ? *)
let unrecognisedString (line:int) (char:int) (context:string) (token:string)
   : string =
   messageString "*** error" line char token
      (context ^ ": unrecognised element")

(* TODO: remove ? *)
(*let faultString (line:int) (char:int) (token:string) (part:string) : string =
   messageString "### fault" line char token
      (part ^ " parse not implemented yet")*)

let render (subject:string) (linePos:int) (pos:int) (body:string) (is:'a ress)
   : ('a ress) =
   errorMap (fun s -> (errorString linePos pos body (subject ^ ": " ^ s))) is
(*
let render (subject:string) (linePos:int) (pos:int) (body:string)
   (is:('a,string) result)
   : ('a option * string) =
   is
   |> resDiverge
   (* :('i option * string option) *)
   |> hemap2
      (  id ,
         optMapUnify
            (fun s -> (errorString linePos pos body (subject ^ ": " ^ s)))
            (fun () -> "")  )
let render (construct:'i -> 'item) (subject:string)
   (linePos:int) (pos:int) (body:string)
   (is:('i,string) result)
   : ('item option * string) =
   is
   |> resDiverge
   (* :('i option * string option) *)
   |> hemap2
      (  optMap construct ,
         optMapUnify
            (fun s -> (errorString linePos pos body (subject ^ ": " ^ s)))
            (fun () -> "")  )
*)



(*--- printers ---*)

let optPrint (printer:'a -> string) (opt:'a option) : string =
   optMatch printer (fConst "") opt
