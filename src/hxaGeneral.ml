(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/tools/

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*)




(* uses:
 * - Unix
 * - Sys
 *)




(* ---- types ---- *)

type 'a ress  = ('a , string) result
type 'a resx  = ('a , exn)    result

type 'a list1 = ('a * 'a list)




(* ---- functions ---- *)

let ofList1 (l1:'a list1) : 'a list =
   (fst l1) :: (snd l1)


let print_string_flush (s:string) : unit =
   Printf.printf "%s%!" s


(* @exceptions: whatever can be raised by Printf.ksprintf *)
let assertLog_x (outputter:string->unit) (b:bool) (message:string) : bool =
   if not b then Printf.ksprintf outputter "%s\n%!" message ;
   b


let fail (message:string) : 'a =
   begin
      Printf.eprintf "*** Failed: %s\n%!" message ;
      (*prerr_endline ("*** Failed: " ^ message) ;*)
      exit 1
   end


let splitFileNameExt (nameExt:string) : (string * string) =
   try
      let extPos = (String.rindex nameExt '.') in
      let extLen = (String.length nameExt) - extPos in
      ( String.sub nameExt 0 extPos , String.sub nameExt extPos extLen )
   with
   | Not_found -> (nameExt , "")


let getFileNameMain (nameExt:string) : string =
   fst (splitFileNameExt nameExt)


let getFileNameExt (nameExt:string) : string =
   snd (splitFileNameExt nameExt)


let splitFilePathName (pathName:string) : (string * string) =
   try
      let namePos = (String.rindex pathName '/') + 1 in
      let nameLen = (String.length pathName) - namePos in
      ( String.sub pathName 0 namePos , String.sub pathName namePos nameLen )
   with
   | Not_found -> (pathName , "")


let getFilePath (pathName:string) : string =
   fst (splitFilePathName pathName)


let getFileName (pathName:string) : string =
   snd (splitFilePathName pathName)


let id (v:'a) : 'a =
   v


let fNot (f:'a -> bool) : ('a -> bool) =
   fun p -> not (f p)


let fConst (a:'a) (_:'b) : 'a =
   a


let ( % ) f g x =
   g (f x)

let ( %> ) = ( % )


let swp (f:'a -> 'b -> 'c) (a:'a) (b:'b) : 'c =
   f b a


let hemap2 (f0,f1:('a0 -> 'b0) * ('a1 -> 'b1)) (a0,a1:'a0 * 'a1) : ('b0 * 'b1) =
   ( f0 a0 , f1 a1 )


let fSome (a:'a) : 'a option =
   Some a


let fOk (o:'o) : ('o ,'e) result =
   Ok o


let resToOpt (r:('o,'e) result) : 'o option =
   match r with
   | Ok    o -> Some o
   | Error _ -> None


let optToRes (e:'e) (oo:'o option) : ('o,'e) result =
   match oo with
   | Some o -> Ok    o
   | None   -> Error e


let optToResU (oo:'o option) : ('o, unit) result =
   optToRes () oo


let resoptToOptres (resopt:(('o,'e) result) option)
   : (('o option),'e) result =

   match resopt with
   | Some res ->
      begin match res with
      | Ok o    -> Ok (Some o)
      | Error e -> Error e
      end
   | None -> Ok None

   (*
   o r -> ro
   ----+----
   s o |  os (value)
   s e |  e
   n - |  on
   n - |  on
   *)


let optresToResopt (optres:(('o option),'e) result)
   : (('o,'e) result) option =

   match optres with
   | Ok opt ->
      begin match opt with
      | Some s -> Some (Ok s)
      | None   -> None
      end
   | Error e -> Some (Error e)

   (*
   r o -> or
   ----+----
   o s |  so (value)
   o n |  n
   e - |  se
   e - |  se
   *)


let optMap (f:'a -> 'b) (o:'a option) : 'b option =
   match o with
   | Some v -> Some (f v)
   | None   -> None


let someMap = optMap


let optUnify (f:unit -> 'a) (o:'a option) : 'a =
   match o with
   | Some v -> v
   | None   -> f ()


let noneDefault = optUnify


let optDiverge (o:'a option) : ('a option * unit option) =
   match o with
   | Some a -> (Some a , None   )
   | None   -> (None   , Some ())


let optMatch (fs:'a -> 'b) (fn:unit -> 'b) (o:'a option) : 'b =
   (optMap fs o) |> (noneDefault fn)

let optMapUnify = optMatch


let optToBool (o:'a option) : bool =
   match o with
   | Some _ -> true
   | None   -> false


let bool_of_option = optToBool


let boolToOpt (b:bool) : unit option =
   if b then Some () else None


let option_of_bool = boolToOpt


let classifyToOpt (pred:'a -> bool) (a:'a) : 'a option =
   if pred a then Some a else None


(*let bool_of_result (r:('o,'e) result) : bool =
   match r with
   | Ok _    -> true
   | Error _ -> false


let result_of_bool (b:bool) : ('o,'e) result =
   if b then Ok () else Error ()*)


let resMap ((fo,fe):('o0 -> 'o1)*('e0 -> 'e1)) (r:('o0,'e0) result)
   : ('o1,'e1) result =
   match r with
   | Ok    o -> Ok    (fo o)
   | Error e -> Error (fe e)


(* won't compile to correct types -- id forces it to ('a -> 'a) *)
(*let resMap_ ?(ok:('a -> 'c) = id) ?(er:('b -> 'd) = id) (r:('a,'b) result)
   : ('c,'d) result =
   match r with
   | Ok    o -> Ok    (ok o)
   | Error e -> Error (er e)*)


let okMap    (f:('o0 -> 'o1)) (r:('o0,'e) result) : ('o1,'e) result =
   resMap (f , id) r
   (*resMap_ ~ok:f r*)


let errorMap (f:('e0 -> 'e1)) (r:('o,'e0) result) : ('o,'e1) result =
   resMap (id , f) r
   (*resMap ~er:f r*)


let resUnify (f:('e0 -> 'o)) (r:('o,'e0) result) : 'o =
   match r with
   | Ok o    -> o
   | Error e -> f e


let errorDefault = resUnify


let resDiverge (r:('o,'e) result) : ('o option * 'e option) =
   match r with
   | Ok    o -> (Some o , None)
   | Error e -> (None , Some e)


let resToExc_x (f:'e -> exn) (r:('o,'e) result) : 'o =
   match r with
   | Ok    o -> o
   | Error e -> raise (f e)


let excToRes (f:unit -> 'o) : ('o , exn) result =
   try Ok (f ()) with
   | Out_of_memory | Stack_overflow | Sys.Break as x -> raise x
   | x                                               -> Error x


let excToDefaultf ~(default:unit -> 'a) ~(f:unit -> 'a) : 'a =
   match excToRes f with
   | Ok o    -> o
   | Error _ -> default ()


let excToDefault (default:'a) (f:unit -> 'a) : 'a =
   excToDefaultf ~default:(fConst default) ~f

(*let excToDefault (default:'a) (f:unit -> 'a) : 'a =
   match excToRes f with
   | Ok o    -> o
   | Error _ -> default*)


let excToRes2 (e:'e) (f:unit -> 'o) : ('o,'e) result =
   excToDefault (Error e) (f % fOk)


let optToExc_x (f:unit -> exn) (o:'a option) : 'a =
   match o with
   | Some a -> a
   | None   -> raise (f ())


let excToOpt (f:unit -> 'a) : 'a option =
   excToDefault None (f % fSome)


(*let mergeOpt2 (nul:'a) (sum:'a -> 'a -> 'a) (e0oe1o:'a option * 'a option)
   : 'a =
   match e0oe1o with
   | Some e0 , Some e1 -> sum e0 e1
   | None    , Some e1 -> e1
   | Some e0 , None    -> e0
   | None    , None    -> nul*)


let optAnd2
   (o0:'o0 option)
   (o1:'o1 option)
   : ('o0 * 'o1) option =

   match o0 , o1 with
   | Some s0 , Some s1 -> Some (s0 , s1)
   | Some _  , None    -> None
   | None    , Some _  -> None
   | None    , None    -> None


let ressAnd2
   (joiner:string)
   (r0:('o0,string) result)
   (r1:('o1,string) result)
   : (('o0 * 'o1) , string) result =

   match (r0,r1) with
   | (Ok o0    , Ok o1   ) -> Ok (o0 , o1)
   | (Error e0 , Ok _    ) -> Error e0
   | (Ok _     , Error e1) -> Error e1
   | (Error e0 , Error e1) -> Error (e0 ^ joiner ^ e1)


let ressAnd3
   (joiner:string)
   (r0:('o0,string) result)
   (r1:('o1,string) result)
   (r2:('o2,string) result)
   : (('o0 * 'o1 * 'o2) , string) result =

   (ressAnd2 joiner
      (ressAnd2 joiner r0 r1)
      r2)
   |>
   (okMap (fun ((o1,o2),o3) -> (o1,o2,o3)))


(*let ( |>=& ) (r0:('o0,string) result) (r1:('o1,string) result)
   : (('o1 * 'o0) , string) result =

   match (r0, r1) with
   | (Ok r0    , Ok r1   ) -> Ok (r1, r0)
   | (Error r0 , Ok _    ) -> Error r0
   | (Ok _     , Error r1) -> Error r1
   | (Error r0 , Error r1) -> Error (r0 ^ ", " ^ r1)*)


(*let resAnd (ab:('o0,'e0) result * ('o1,'e1) result)
   : (('o0 * 'o1) , ('e0 option * 'e1 option)) result =

   match ab with
   | Ok o0    , Ok o1    -> Ok (o0 , o1)
   | Error e0 , Ok _     -> Error (Some e0 , None   )
   | Ok _     , Error e1 -> Error (None    , Some e1)
   | Error e0 , Error e1 -> Error (Some e0 , Some e1)*)


(*let ( &&= ) (a:('o0,'e) result) (b:('o1,'e) result)
   : (('o1 * 'o0) , 'e list) result =

   match (a,b) with
   | (Ok o0    , Ok o1   ) -> Ok (o1 , o0)
   | (Ok _     , Error e1) -> Error [e1]
   | (Error e0 , Ok _    ) -> Error [e0]
   | (Error e0 , Error e1) -> Error [e1 ; e0]*)


let ( ||> ) (o:'a option) (f:unit -> 'a option) : 'a option =
   match o with
   | Some _ as a -> a
   | None        -> f ()


let ( &&> ) (o:'a option) (f:unit -> 'a option) : 'a option =
   match o with
   | Some _ -> f ()
   | None   -> None


let ( |>- ) (o1:'o1 option) (f:'o1 -> 'o2 option)
   : 'o2 option =

   match o1 with
   | Some o -> f o
   | None   -> None

   (* alternatively, using |>=: *)
   (*o1
   |> optToResU
   |>= (f % optToResU)
   |> resToOpt*)


let ( |>= ) (r1:('o1,'e) result) (f:'o1 -> ('o2,'e) result)
   : ('o2,'e) result =

   match r1 with
   | Ok    o      -> f o
   | Error _ as e -> e


(*let ( |>>= ) (r1:('o1,'e) result) (f:'o1 -> ('o2,'e) result)
   : (('o1 * 'o2) , 'e) result =

   r1 |>= (fun o1 ->
      match f o1 with
      | Ok    o2     -> Ok (o1 , o2)
      | Error _ as e -> e)*)


(* dependent on List, so moved below that

let ( |^= ) (r:('o1,'e) result) (lf:('o1 -> ('o2,'e) result) list)
   : ('o2 list , 'e list) result =

   (* : (('o2,'e) result) list *)
   (List.map ((|>=) r) lf)
   |>
   List.resAnd
*)


let optAnd2p
   (f0:'s0 -> 'o1 option)
   (f1:'s0 -> 'o2 option)
   (s0:'s0)
   : ('o1 * 'o2) option =

   optAnd2 (f0 s0) (f1 s0)


let ressAnd2p
   (joiner:string)
   (f0:'o0 -> ('o1,string) result)
   (f1:'o0 -> ('o2,string) result)
   (o0:'o0)
   : (('o1 * 'o2) , string) result =

   ressAnd2 joiner (f0 o0) (f1 o0)


(*let ( |^^= )
   (r1:('o1,string) result)
   (  (f0:'o1 -> ('o2,string) result) ,
      (f1:'o1 -> ('o3,string) result) )
   : (('o2 * 'o3) , string) result =

   r1 |>= (fun o1 ->
      match ((f0 o1) , (f1 o1)) with
      | (Ok o2    , Ok o3   ) -> Ok (o2 , o3)
      | (Ok _     , Error e1) -> Error e1
      | (Error e0 , Ok _    ) -> Error e0
      | (Error e0 , Error e1) -> Error (e0 ^ " && " ^ e1))*)


let ressAnd3p
   (joiner:string)
   (f0:'o0 -> ('o1,string) result)
   (f1:'o0 -> ('o2,string) result)
   (f2:'o0 -> ('o3,string) result)
   (o0:'o0)
   : (('o1 * 'o2 * 'o3) , string) result =

   ressAnd3 joiner (f0 o0) (f1 o0) (f2 o0)

   (*
   (ressAnd2 joiner
      (ressAnd2 joiner
         (f0 o0)
         (f1 o0))
      (f2 o0))
   |>
   (okMap (fun ((o1,o2),o3) -> (o1,o2,o3)))

   (*let f12 = (fun o0 -> ressAnd2p joiner f1 f2 (Ok o0)) in
   match (ressAnd2p joiner f0 f12 r) with
   | Ok (o1 , (o2 , o3)) -> Ok (o1 , o2 , o3)
   | Error e             -> Error e*)

   (*let f12 = (fun o0 -> ressAnd2p joiner f1 f2 (Ok o0)) in
   match (ressAnd2p joiner f0 f12 r) with
   | Ok (o1 , (o2 , o3)) -> Ok (o1 , o2 , o3)
   | Error e             -> Error e*)
   *)


(*let ( |^^^= ) (r1:('o1,string) result)
   (  (f0:'o1 -> ('o2,string) result) ,
      (f1:'o1 -> ('o3,string) result) ,
      (f2:'o1 -> ('o4,string) result) )
   : (('o2 * 'o3 * 'o4) , string) result =

   let f12 = (fun o1 -> (Ok o1) |^^= (f1 , f2)) in
   match r1 |^^= (f0 , f12) with
   | Ok (o2 , (o3 , o4)) -> Ok (o2 , o3 , o4)
   | Error e             -> Error e

   (*r1 |>= (fun o1 ->
      match ((f0 o1) , (f1 o1) , (f2 o1)) with
      | (Ok o2 , Ok o3 , Ok o4)          -> Ok (o2 , o3 , o4)
      | (Error e0 , Ok _ , Ok _)         -> Error e0
      | (Ok _ , Error e1 , Ok _)         -> Error e1
      | (Ok _ , Ok _ , Error e2)         -> Error e2
      | (Error e0 , Error e1 , Ok _)     -> Error (e0 ^ " && " ^ e1)
      | (Error e0 , Ok _ , Error e2)     -> Error (e0 ^ " && " ^ e2)
      | (Ok _ , Error e1 , Error e2)     -> Error (e1 ^ " && " ^ e2)
      | (Error e0 , Error e1 , Error e2) ->
         Error (e0 ^ " && " ^ e1 ^ " && " ^ e2))*)*)


let ( |>=? ) (r1:('o,'e) result) (f:'e -> ('o,'e) result)
   : ('o,'e) result =

   match r1 with
   | Ok _ as o -> o
   | Error e   -> f e


let ( |>=+ ) (o1:'o1) (f:'o1 -> ('o2,'e) result) : ('o2,'e) result =
   (Ok o1) |>= f


let ( |>=- ) (r1:('o1,'e) result) (f:'o1 -> 'o2) : ('o2,'e) result =
   r1 |>= (fun o1 -> Ok (f o1))


let string_of_char (c:char) : string =
   String.make 1 c


let string_of_byte (b:int)  : string =
   String.make 1 (char_of_int (b land 0xFF))


let blankSpacyCtrlChars : (string -> string) =
   String.map
      (function
      | '\x09' | '\x0A' | '\x0B' | '\x0C' | '\x0D' -> ' '
      | c                                          -> c)


let blankNewlines : (string -> string) =
   String.map (function | '\n' | '\r' -> ' ' | c -> c)


(*let unifySpaces (s:string) : string =
   let rx = Str.regexp
      "\x09\\|\x0A\\|\x0B\\|\x0C\\|\x0D\\|\x20\\|\
      \xC2\x85\\|\xC2\xA0\\|\
      \xE1\x9A\x80\\|\xE1\xA0\x8E\\|\
      \xE2\x80\x80\\|\xE2\x80\x81\\|\xE2\x80\x82\\|\xE2\x80\x83\\|\
      \xE2\x80\x84\\|\xE2\x80\x85\\|\xE2\x80\x86\\|\
      \xE2\x80\x87\\|\xE2\x80\x88\\|\
      \xE2\x80\x89\\|\xE2\x80\x8A\\|\
      \xE2\x80\x8B\\|\xE2\x80\x8C\\|\xE2\x80\x8D\\|\
      \xE2\x80\xA8\\|\xE2\x80\xA9\\|\
      \xE2\x80\xAF\\|\
      \xE2\x81\x9F\\|\
      \xE2\x81\xA0\\|\
      \xE3\x80\x80\\|\
      \xEF\xBB\xBF"
   in
   Str.global_replace rx " " s*)


let clamp ~(lo:'a) ~(up:'a) (n:'a) : 'a =
   (min (max lo n) up)


let isNan (f:float) : bool =
   match classify_float f with
   | FP_nan                                           -> true
   | FP_normal | FP_subnormal | FP_zero | FP_infinite -> false


let minMaxMean (things:float list) : (float * float * float) =
   let min , max , sum = List.fold_left
      (fun (mini,maxi,sum) e -> ( (min e mini) , (max e maxi) , (e +. sum) ))
      ( max_float , min_float , 0.0 )
      things
   and count = float_of_int (List.length things) in
   (min , max , (sum /. count))


let ( @@ ) (l:'a list) (a:'a) : 'a list =
   l @ (a :: [])


let timerWall (f:'a -> 'b) (input:'a) : ('b * float) =
   let timeBegin = Unix.gettimeofday () in
   let output = f input in
   let timeEnd = Unix.gettimeofday () in
   ( output , timeEnd -. timeBegin )




(* ---- std lib module augmentations ---- *)

module Int :
sig
   (*include module type of Int*)

   val digitsDec : int -> int
   val modw      : int -> int -> int
end
=
struct
   (*include Int*)

   let digitsDec (i:int) : int =
      i  |> float_of_int |> abs_float |> (max 1.0)
         |> log10 |> floor |> int_of_float |> succ

   let modw (x:int) (y:int) : int =
      if x >= 0
      then x mod y
      else let y = abs y in (y - 1) - ((-x - 1) mod y)
end


module Char :
sig
   include module type of Char

   val isAlpha   : char -> bool
   val isDigit   : char -> bool
   val isSign    : char -> bool
   val isAscii   : char -> bool
   val isBlank   : char -> bool
   val isNewline : char -> bool
end
=
struct
   include Char

   let isAlpha (c:char) : bool =
      match c with | 'a'..'z' | 'A'..'Z' -> true | _ -> false

   let isDigit (c:char) : bool =
      match c with | '0'..'9' -> true | _ -> false

   let isSign (c:char) : bool =
      (c = '-') || (c = '+')

   let isAscii (c:char) : bool =
      (int_of_char c) <= 127

   let isBlank (c:char) : bool =
      match c with
      | ' ' | '\x09' | '\x0A' | '\x0B' | '\x0C' | '\x0D' -> true
      | _                                                -> false

   let isNewline (c:char) : bool =
      c = '\n'
end


module String :
sig
   include module type of String

   val isEmpty     : string -> bool
   val notEmpty    : string -> bool
   val lastPos     : string -> int
   val repeat      : string -> int -> string
   val lead        : string -> int -> string
   val trail       : string -> int -> string
   val leadTrail   : string -> int -> (string * string)
   val last        : string -> char
   val isFirstChar : (char -> bool) -> string -> bool
   val index_o     : char -> ?start:int -> string -> int option
   val indexp_o    : (char -> bool) -> ?start:int -> string -> int option
   val indexl      : char -> ?start:int -> string -> int
   val indexpl     : (char -> bool) -> ?start:int -> string -> int
   val rindexp_o   : (char -> bool) -> string -> int option
   val filter      : (char -> bool) -> string -> string
   val filterAscii : string -> string
   val check       : (char -> bool) -> string -> bool
   val halve       : char -> string -> (string * string * int) option
   val splitp      : ?ls:((string * int) list) -> (char -> bool) -> string ->
                     (string * int) list
   val split       : (char -> bool) -> string -> string list
   val trimTrunc   : (string * int) -> (string , string) result
   val truncate    : int -> string -> string
   val toInt    : ?zeroPadded:(bool * int) -> ?widthMaxed:int -> ?signed:bool ->
                  string -> int option
end
=
struct
   include String

   let isEmpty (s:string) : bool =
      (String.length s) = 0

   let notEmpty (s:string) : bool =
      not (isEmpty s)

   let lastPos (s:string) : int =
      (String.length s) - 1

   let repeat (s:string) (i:int) : string =
      let b = Buffer.create 16 in
      if i > 0 then for _ = 1 to i do Buffer.add_string b s done ;
      Buffer.contents b

   let lead (s:string) (pos:int) : string =
      let posc = clamp ~lo:0 ~up:(String.length s) pos in
      String.sub s 0 posc

   let trail (s:string) (pos:int) : string =
      let posc = clamp ~lo:0 ~up:(String.length s) pos in
      String.sub s posc ((String.length s) - posc)

   let leadTrail (s:string) (pos:int) : (string * string) =
      ( lead s pos , trail s pos )

   let last (s:string) : char =
      s.[(String.length s) - 1]

   let isFirstChar (pred:char -> bool) (s:string) : bool =
      ((String.length s) > 0) && pred s.[0]

   let index_o (c:char) ?(start:int = 0) (s:string) : int option =
      try Some (String.index_from s start c) with
      | Invalid_argument _ | Not_found -> None

   let indexp_o (pred: char -> bool) ?(start:int = 0) (s:string) : int option =
      let len = String.length s in
      let rec recur (i:int) (s:string) : int option =
         if i < len
         then if pred s.[i] then Some i else recur (i + 1) s
         else None
      in
      recur (if start < 0 then len else start) s

   let indexl (c:char) ?(start:int = 0) (s:string) : int =
      noneDefault (fun () -> String.length s) (index_o c ~start s)

   let indexpl (pred: char -> bool) ?(start:int = 0) (s:string) : int =
      noneDefault (fun () -> String.length s) (indexp_o pred ~start s)

   let rindexp_o (pred: char -> bool) (s:string) : int option =
      let rec recur (s:string) (i:int) : int option =
         let i = i - 1 in
         if i >= 0
         then if pred s.[i] then Some i else recur s i
         else None
      in
      recur s (String.length s)

   let filter (pred: char -> bool) (s:string) : string =
      let len = String.length s in
      let buf = Buffer.create len in
      for i = 0 to (len - 1) do
         if pred s.[i] then Buffer.add_char buf s.[i] ;
      done ;
      Buffer.contents buf

   let filterAscii : (string -> string) =
      filter Char.isAscii

   let check (pred: char -> bool) (s:string) : bool =
      match rindexp_o (fNot pred) s with
      | Some _ -> false
      | None   -> true

   let halve (div:char) (str:string) : (string * string * int) option =
      optMap
         (fun pos ->
            (  String.sub str 0 pos ,
               String.sub str (pos + 1) ((String.length str) - (pos + 1)) ,
               pos ))
         (index_o div str)

   let rec splitp ?(ls:(string * int) list = []) (pred: char -> bool)
      (s:string)
      : (string * int) list =
      match rindexp_o pred s with
      | Some pos ->
         let half1 = String.sub s 0 pos
         and half2 = String.sub s (pos + 1) ((String.length s) - (pos + 1)) in
         splitp ~ls:((half2 , (pos + 1)) :: ls) pred half1
      | None -> ((s , 0) :: ls)

   let split (pred: char -> bool) (s:string) : string list =
      let lsi = splitp pred s in
      fst (List.split lsi)

   let trimTrunc ((s:string) , (max:int)) : (string , string) result =
      let st = String.trim s in
      if (String.length st) <= max
      then Ok st
      else Error ("too long (> " ^ (string_of_int max) ^ ")")

   let truncate (max:int) (s:string) : string =
      if String.length s <= max then s else String.sub s 0 max

   let toInt ?(zeroPadded:(bool * int) option) ?(widthMaxed:int option)
      ?(signed:bool option) (input:string) : int option =

      (* input-string analysis:
         [prefix: non-digits] [digits: digits only] [suffix: any left] *)
      let digitsPos   = indexpl Char.isDigit input in
      let suffixPos   = (indexpl (fNot Char.isDigit) ~start:digitsPos input) in
      let digitsWidth = suffixPos - digitsPos in

      if
         (* is string basically viable ? *)
         (notEmpty input)
         &&
         (* is prefix (sign) valid: 0 or 1 width, and - or + ? *)
         (  let isSignedValid   = (digitsPos = 1) && (Char.isSign input.[0])
            and isUnsignedValid = digitsPos = 0
            in
            match signed with
            | Some false -> isUnsignedValid
            | Some true  -> isSignedValid
            | None       -> isUnsignedValid || isSignedValid )
         &&
         (* is digits width minimally valid ? *)
         ( digitsWidth >= 1 )
         &&
         (* is there no (non-digit) suffix ? *)
         ( suffixPos == String.length input )
         &&
         (* is digits width not too large ? *)
         (  match widthMaxed with
            | Some width -> digitsWidth <= (max 1 width)
            | None       -> true )
         &&
         (* is digits correctly zero-padded (to minimum width) ? *)
         (  match zeroPadded with
            | Some (false , _)     -> input.[digitsPos] <> '0'
            | Some (true  , width) -> digitsWidth = (max 1 width)
            | None                 -> true )
      then
         (* Scan %d handles decimal, optional sign, and leading zeros -- OK.
            Scan %d also allows embedded '_'s -- not OK, but these have been
            prohibited in the above checks. *)
         excToOpt (fun () -> Scanf.sscanf (lead input suffixPos) "%d" id)
      else
         None
end


module List :
sig
   include module type of List

   val isEmpty   : 'a list -> bool
   val hdo       : 'a list -> 'a option
   val fto       : 'a list -> 'a option
   val tlSafe    : 'a list -> 'a list
   val ntho      : int -> 'a list -> 'a option
   val bisect    : 'a list -> int -> ('a list * 'a list)
   val find_o    : ('a -> bool) -> 'a list -> 'a option
   val filtmap   : ('a -> 'b option) -> 'a list -> 'b list
   val partmap   : ('a -> ('o, 'e) result) -> 'a list -> ('o list * 'e list)
   val findmap_o : ('a -> 'b option) -> 'a list -> 'b option
   val optAnd    : ('a option) list -> ('a list) option
   val optOr     : ('a option) list -> ('a list) option
   val resAnd    : (('o,'e) result list) -> ('o list , 'e list) result
   val unfoldl   : ?list:('a list) -> (int->'a) -> int -> 'a list
   val unfoldo   : ?list:('a list) -> ?index:int -> (int->'a option) -> 'a list
   val ofStringAscii : string -> char list
   val toStringAscii : (char list) -> string
end
=
struct
   include List

   let isEmpty (l:'a list) : bool =
      (List.length l = 0)

   let hdo (l:'a list) : 'a option =
      match l with
      | hd :: _ -> Some hd
      | []      -> None

   let rec fto (l:'a list) : 'a option =
      match l with
      | ft :: [] -> Some ft
      | _  :: tl -> fto tl
      | []       -> None

   let tlSafe (l:'a list) : 'a list =
      match l with
      | _ :: tail -> tail
      | []        -> []

   let ntho (index:int) (l:'a list) : 'a option =
      try Some (List.nth l index) with Failure _ -> None

   let bisect (l:'a list) (m:int) : ('a list * 'a list) =
      let rec recur (i:int) (l:'a list) (body:'a list) : ('a list * 'a list) =
         if i > 0
         then recur (i - 1) (List.tl l) ((List.hd l) :: body)
         else ( (List.rev body) , l )
      in
      recur (min m (List.length l)) l []

   let find_o (f:'a -> bool) (l:'a list) : 'a option =
      try Some (List.find f l) with Not_found -> None

   (*
   ?

   let classify2 (preds:('a -> bool) list) (l:'a list) : 'a list list =
      List.fold_left
         (fun (classes:'a list list) (item:'a) : 'a list list ->
            List.rev_map2
               (fun (pred:('a -> bool)) (class_:'a list) : 'a list ->
                   if (pred item) then (item :: class_) else class_)
               preds classes)
         (List.unfoldl (fConst []) (List.length preds))
         l

   let classify (f:'out list array -> 'inp -> 'out list array) (l:'inp list)
      : 'out list array =
      List.fold_left f [||] (List.rev l)

   let clsf1 (last:string list array) (item:char) : string list array =
      let last = if last = [||] then [|[];[];[]|] else last in
      let index =
         match item with
         | 'a'..'z' -> 0
         | '0'..'9' -> 1
         | _        -> 2
      in
      Array.set last index ((string_of_char item) :: last.(index)) ;
      last
   *)

   let filtmap (f:'a -> 'b option) (l:'a list) : 'b list =
      List.fold_right (fun a out ->
         match f a with | Some b -> b :: out | None -> out) l []

   let partmap (f:'a -> ('ok,'er) result) (l:'a list) : ('ok list * 'er list) =
      List.fold_right
         (fun a (oOut , eOut) ->
            match f a with
            | Ok    o -> (o :: oOut ,      eOut)
            | Error e -> (     oOut , e :: eOut))
         l ([] , [])

   let rec findmap_o (predmap:'a -> 'b option) (l:'a list) : 'b option =
      match l with
      | a :: tail ->
         begin match predmap a with
         | None        -> findmap_o predmap tail
         | Some _ as b -> b
         end
      | [] -> None

   let optAnd (lo:('a option) list) : ('a list) option =
      let la    = filtmap id lo in
      let laLen = List.length la in
      (* Some if: all Some, or empty *)
      if (laLen = (List.length lo))
      then Some la else None

   let optOr (lo:('a option) list) : ('a list) option =
      match filtmap id lo with
      | [] -> None
      | la -> Some la

   let resAnd (listRes:(('ok,'er) result) list) : ('ok list , 'er list) result =
      let listOk , listErr = partmap id listRes in
      let listOkLen        = List.length listOk
      and listResLen       = List.length listRes in
      (* Ok if: all Ok, or empty *)
      if (listOkLen = listResLen)
      then (Ok listOk) else (Error listErr)

   let rec unfoldl ?(list = []) (f:int->'a) (size:int) : 'a list =
      if size > 0
      then unfoldl ~list:((f (size - 1)) :: list) f (size - 1)
      else list

   let rec unfoldo ?(list = []) ?(index = 0) (f:int->'a option) : 'a list =
      match (f index) with
      | Some element -> unfoldo ~list:(element :: list) ~index:(index + 1) f
      | None         -> List.rev list

   let ofStringAscii (s:string) : char list =
      unfoldo (fun i -> try Some s.[i] with | Invalid_argument _ -> None)

   (*let rec ofStringAscii ?(lc:char list = []) (s:string) : char list =
      let len = String.length s in
      if len > 0
      then ofStringAscii ~lc:(s.[len - 1] :: lc) (String.sub s 0 (len - 1))
      else lc*)

   let toStringAscii (lc:char list) : string =
      String.concat "" (List.map string_of_char lc)
end


module Array :
sig
   include module type of Array

   val isEmpty   : 'a array -> bool
   val lead      : int -> 'a array -> 'a array
   val trail     : int -> 'a array -> 'a array
   val leadTrail : int -> 'a array -> ('a array * 'a array)
   val forAll    : ('a -> bool) -> 'a array -> bool
   val bisect    : 'a array -> int -> ('a array * 'a array)
   val bisect_o  : 'a array -> int -> ('a array * 'a array) option
   val partition : ('a -> bool) -> 'a array -> ('a array * 'a array)
   val printc_x  : ('a -> out_channel -> unit) -> 'a array -> out_channel
      -> unit
   (*
   val printks_x : ('a -> unit -> string) -> 'a array -> unit -> string
   *)
end
=
struct
   include Array

   let isEmpty (a:'a array) : bool =
      Array.length a = 0

   let lead (pos:int) (a:'a array) : 'a array =
      let posc = clamp ~lo:0 ~up:(Array.length a) pos in
      Array.sub a 0 posc

   let trail (pos:int) (a:'a array) : 'a array =
      let posc = clamp ~lo:0 ~up:(Array.length a) pos in
      Array.sub a posc ((Array.length a) - posc)

   let leadTrail (pos:int) (a:'a array) : ('a array * 'a array) =
      ( lead pos a , trail pos a )

   let forAll (pred:'a -> bool) (a:'a array) : bool =
      Array.fold_left (fun b a -> b && pred a) (not (isEmpty a)) a

   (* exceptioning (same as Array.sub) *)
   (*let bisect (a:'a array) (i:int) : ('a array * 'a array) =
      let len = Array.length a in
      (Array.sub a 0 i) , (Array.sub a i (len - i))*)

   let bisect (a:'a array) (i:int) : ('a array * 'a array) =
      let len = Array.length a in
      let i   = min (max 0 i) len in
      (Array.sub a 0 i) , (Array.sub a i (len - i))

   let bisect_o (a:'a array) (i:int) : ('a array * 'a array) option =
      let len = Array.length a in
      try
         Some ( (Array.sub a 0 i) , (Array.sub a i (len - i)) )
      with Invalid_argument _ -> None

   let partition (pred:'a -> bool) (a:'a array) : ('a array * 'a array) =
      let l0      = Array.to_list a in
      (* (refman says List.partition preserves order) *)
      let l1 , l2 = List.partition pred l0 in
      ( Array.of_list l1 , Array.of_list l2 )

   let printc_x (printer:('a -> out_channel -> unit)) (a:'a array)
      (out:out_channel) : unit =
      Array.iter (fun e -> Printf.fprintf out "%t " (printer e)) a

   (*
   let printks_x (p:('a -> unit -> string)) (a:'a array) () : string =
      (* maybe *)
   *)
end




(* ---- modules ---- *)

module Rx :
sig
   type rx
   type rxmatch

   val compile    : string -> rx
   val apply      : rx -> string -> string -> ((rxmatch,string) result)
   val seekFirst  : rx -> string -> string -> ((rxmatch,string) result)
   val regex      : string -> string -> string -> ((rxmatch,string) result)
   val allMatches : rx -> string -> string list
   val wholeFound : rxmatch -> string
   val groupFound : rxmatch -> int -> (string option)
end
=
struct
   type rx      = Str.regexp
   type rxmatch =
      {  whole  : string ;
         groups : (string option) array ; }
   (*type rx      = Re.re
   type rxmatch = string array option*)

   let compile (rxs:string) : rx =
      Str.regexp rxs
      (*Re.compile (Re_perl.re rxs)*)

   let oneMatch (query:bool) (content:string) (message:string)
      : (rxmatch,string) result =
      if query
      then
         match
            (* should not fail, since the matching query succeeded
               (but if it does, abort the whole result) *)
            try Ok (Str.matched_string content) with
            | Not_found | Invalid_argument _ -> Error message
         with
         | Ok wholeMatch ->
            let groups : (string option) list =
               List.unfoldo
                  (fun index : (string option) option ->
                     try Some (Some (Str.matched_group index content)) with
                     | Not_found          -> Some None
                     | Invalid_argument _ -> None)
            in
            Ok { whole = wholeMatch ; groups = Array.of_list groups }
         | Error _ as e ->
            e
      else
         Error message
      (*try Ok (Re.get_all (Re.exec rx content)) with
      | Not_found -> Error message*)

   let apply (rx:rx) (content:string) (message:string)
      : (rxmatch,string) result =
      let query = Str.string_match rx content 0 in
      oneMatch query content message

   let seekFirst (rx:rx) (content:string) (message:string)
      : (rxmatch,string) result =
      let query =
         try ignore(Str.search_forward rx content 0) ; true with
         | Not_found -> false
      in
      oneMatch query content message

   let regex (rxs:string) (content:string) (message:string)
      : (rxmatch,string) result =
      apply (compile rxs) content message

   let allMatches (rx:rx) (content:string) : string list =
      (Str.full_split rx content)
      |>
      (List.filtmap (function
         | Str.Delim d -> Some d
         | Str.Text  _ -> None ))

   let wholeFound (rxmatch:rxmatch) : string =
      rxmatch.whole

   (* (group index starts at 1) *)
   let groupFound (rxmatch:rxmatch) (index:int) : string option =
      rxmatch.groups.(index)
end




(* ---- more functions -- dependent on module augmentations ---- *)

(*let ( |^= ) (r:('o1,'e) result) (lf:('o1 -> ('o2,'e) result) list)
   : ('o2 list , 'e list) result =

   (* : (('o2,'e) result) list *)
   (List.map ((|>=) r) lf)
   |>
   List.resAnd*)




(* ---- IO ---- *)

let scanExnUnify_x (f:unit -> 'a) : 'a =
   try f () with
   | Scanf.Scan_failure s | Failure s | Invalid_argument s ->
      raise (Scanf.Scan_failure s)
   | End_of_file ->
      raise (Scanf.Scan_failure "unexpected end-of-file")

   (*match excToRes (f ()) with
   | Ok o    -> o
   | Error e ->
      let s =
         match e with
         | Scanf.Scan_failure s | Failure s | Invalid_argument s -> s
         | End_of_file -> "unexpected end-of-file"
         | _           -> "unspecified exception"
      in
      raise (Scanf.Scan_failure s)*)


let (kscanfErrFn : Scanf.Scanning.scanbuf -> exn -> ('o , string) result) =
   function _ -> function
   | Scanf.Scan_failure s -> Error ("Scanf.Scan_failure: " ^ s)
   | Failure s            -> Error ("Number conversion failure: " ^ s)
   | Invalid_argument s   -> Error ("Invalid format string: " ^ s)
   | End_of_file          -> Error "Unexpected end-of-file"
   | _                    -> Error "Unknown exception"

let skipBlank (inBuffer:Scanf.Scanning.scanbuf) : unit =
   try
      Scanf.bscanf inBuffer " " ()
   with
   | Scanf.Scan_failure _ | Failure _ | Invalid_argument _
   | End_of_file                                           -> ()




let useFile (filePathname:string)
   (opener:string -> 'c) (closer:'c -> unit) (use:'c -> 'i)
   : ('i , exn) result =

   excToRes (fun () : 'c -> opener filePathname)
   |>=
   (fun (channel:'c) : ('i , exn) result ->
      let result = excToRes (fun () : 'i -> use channel) in
      closer channel ;
      result)


let fileRead ~(filePathname:string)
   : (string , exn) result =

   useFile filePathname open_in_bin close_in_noerr
      (fun c ->
         let len = in_channel_length c in
         really_input_string c len
         (*if len <= ...
         then really_input_string c len
         else failwith ("file too large (" ^ (string_of_int len) ^ ")")*))


let fileWrite ~(filePathname:string) ~(stuffToWrite:string)
   : (unit , exn) result =

   useFile filePathname open_out_bin close_out_noerr
      (fun c ->
         output_string c stuffToWrite ;
         flush c ;)
