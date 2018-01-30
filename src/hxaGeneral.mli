(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/tools/

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*)




(**
 * General useful little pieces to fill-in or extend the std lib.
 * No larger, overall structure or dependencies, merely disparate items, with
 * a few delegations here and there.
 *
 *
 * Contents
 * --------
 *
 * * types
 *    * specialised results
 *    * list > 0
 * * functions
 *    * print, assert, fail
 *    * filename
 *    * id, not, const, compose, swp
 *    * some fn, ok fn
 *    * result and option conversions and maps
 *    * error-handling pipeline/monad
 *    * string, numerical, etc
 * * std lib module augmentations
 *    * Int
 *    * Char
 *    * String
 *    * List
 *    * Array
 * * modules
 *    * regexp wrapper
 * * IO
 *    * scanning
 *    * skipping
 *    * file read/write
 *
 *)




(* ---- types ---- *)

type 'a ress  = ('a , string) result
type 'a resx  = ('a , exn)    result

(** List of at least one item. *)
type 'a list1 = ('a * 'a list)




(* ---- functions ---- *)

(** Convert list1 to list. *)
val ofList1 : 'a list1 -> 'a list


(** Like print_string, but with a flush. *)
val print_string_flush : string -> unit

(** If boolean false, call outputter with string.
    @exceptions: whatever can be raised by Printf.ksprintf *)
val assertLog_x : (string->unit) -> bool -> string -> bool

(** Print message and exit 1. *)
val fail : string -> 'a


(** Return "name" and ".ext". *)
val splitFileNameExt : string -> (string * string)

(** Return "name" but not ".ext". *)
val getFileNameMain : string -> string

(** Return ".ext" but not "name". *)
val getFileNameExt : string -> string

(** Return "dir/path/" and "filename". *)
val splitFilePathName : string -> (string * string)

(** Return "dir/path/" but not "filename". *)
val getFilePath : string -> string

(** Return "filename" but not "dir/path/". *)
val getFileName : string -> string


(** The identity function. *)
val id : 'a -> 'a

(** Return the inverted predicate. *)
val fNot : ('a -> bool) -> ('a -> bool)

(** Make a general constant unary lambda. *)
val fConst : 'a -> 'b -> 'a

(** Compose two functions: (f % g)(x) = g(f(x))
 * (be aware of value restriction). *)
val ( %  ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** Swap params for a binary op. *)
val swp : ('a -> 'a -> 'c) -> 'a -> 'a -> 'c


(** Heterogenous (product) map, arity 2. *)
val hemap2 : (('a0 -> 'b0) * ('a1 -> 'b1)) -> ('a0 * 'a1) -> ('b0 * 'b1)

(** Some as a function. *)
val fSome : 'a -> 'a option

(** Ok as a function. *)
val fOk : 'o -> ('o, 'e) result


(** Convert result to option (by discarding error). *)
val resToOpt : ('o,'e) result -> 'o option

(** Convert option to result (by replacing None with Error). *)
val optToRes : 'e -> 'o option -> ('o,'e) result

(** Convert option to result (by replacing None with unit Error). *)
val optToResU : 'o option -> ('o, unit) result

(** Transpose res-opt to opt-res. *)
val resoptToOptres : (('o, 'e) result) option -> ('o option, 'e) result

(** Transpose opt-res to res-opt. *)
val optresToResopt : ('o option, 'e) result -> (('o, 'e) result) option

(** Map Some a -> Some (f a). *)
val optMap  : ('a -> 'b) -> 'a option -> 'b option
val someMap : ('a -> 'b) -> 'a option -> 'b option

(** Unify an option with a default for None. *)
val optUnify    : (unit -> 'a) -> 'a option -> 'a
val noneDefault : (unit -> 'a) -> 'a option -> 'a

(** Diverge an option to a tuple of options (non-case -> None). *)
val optDiverge : 'a option -> ('a option * unit option)

(** Map and unify option -- map Some, and default None. *)
val optMatch    : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b
val optMapUnify : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b

(** Convert option to bool. *)
val optToBool      : 'a option -> bool
val bool_of_option : 'a option -> bool

(** Convert bool to option. *)
val boolToOpt      : bool -> unit option
val option_of_bool : bool -> unit option

(** Convert value to option, according to predicate. *)
val classifyToOpt : ('a -> bool) -> 'a -> 'a option

(** Map (o,e) result -> ((fo o),(fe e)) result. *)
val resMap : (('a -> 'c) * ('b -> 'd)) -> ('a,'b) result -> ('c,'d) result

(** Map (o,e) result -> ((fo o),(fe e)) result.
    (Type-inference implemtation seems to make default args not work.). *)
(*val resMap_ : ?ok:('a -> 'c) -> ?er:('b -> 'd) -> ('a,'b) result ->
   ('c,'d) result*)

(** Map (o,e) result -> ((fo o),e) result. *)
val okMap    : ('o0 -> 'o1) -> ('o0,'e) result -> ('o1,'e) result

(** Map (o,e) result -> (o,(fe e)) result. *)
val errorMap : ('e0 -> 'e1) -> ('o,'e0) result -> ('o,'e1) result

(** Unify a result with a default for Error. *)
val resUnify     : ('e0 -> 'o) -> ('o, 'e0) result -> 'o
val errorDefault : ('e0 -> 'o) -> ('o, 'e0) result -> 'o

(** Diverge a result to a tuple of options (non-case -> None). *)
val resDiverge : ('o,'e) result -> ('o option * 'e option)

(** Convert result to value or exception.
    @exceptions: exn *)
val resToExc_x : ('e -> exn) -> ('o,'e) result -> 'o

(** Convert exception (by catching) to result.
    (not caught: Out_of_memory, Stack_overflow, Sys.Break). *)
val excToRes : (unit -> 'o) -> ('o , exn) result

(** Convert (by catching) exception to defaulter function
    (except: Out_of_memory, Stack_overflow, Sys.Break). *)
val excToDefaultf : default:(unit -> 'a) -> f:(unit -> 'a) -> 'a

(** Convert (by catching) exception to default value
    (except: Out_of_memory, Stack_overflow, Sys.Break). *)
val excToDefault : 'a -> (unit -> 'a) -> 'a

(** Convert exception (by catching) to result.
    (not caught: Out_of_memory, Stack_overflow, Sys.Break). *)
val excToRes2 : 'e -> (unit -> 'o) -> ('o,'e) result

(** Convert option to value or exception.
    @exceptions: exn *)
val optToExc_x : (unit -> exn) -> 'a option -> 'a

(** Convert exception (by catching) to option.
    (not caught: Out_of_memory, Stack_overflow, Sys.Break). *)
val excToOpt : (unit -> 'a) -> 'a option

(*(**
 * Not sure about this ... .
 * @param null-form of type
 * @param sum function for type
 *)
val mergeOpt2 : 'a -> ('a -> 'a -> 'a) -> ('a option * 'a option) -> 'a*)

(** And-merge double heterogeneous options. *)
val optAnd2 : ('a option) -> ('b option) -> (('a * 'b) option)

(** And-merge double heterogeneous. *)
val ressAnd2 :
   string ->
   (('o0,string) result) ->
   (('o1,string) result) ->
   ((('o0 * 'o1) , string) result)

(** And-merge triple heterogeneous. *)
val ressAnd3 :
   string ->
   (('o0,string) result) ->
   (('o1,string) result) ->
   (('o2,string) result) ->
   ((('o0 * 'o1 * 'o2) , string) result)


(** Augmented or: short-circuiting 'or' that returns more than a bool. *)
val ( ||> ) : 'a option -> (unit -> 'a option) -> 'a option

(** Augmented and: short-circuiting 'and' that returns more than a bool. *)
val ( &&> ) : 'a option -> (unit -> 'a option) -> 'a option


(** Error-handling pipeline (option version). *)
val ( |>- ) : 'o1 option -> ('o1 -> 'o2 option) -> 'o2 option

(** Error-handling pipeline. *)
val ( |>= ) : ('o1,'e) result -> ('o1 -> ('o2,'e) result) -> ('o2,'e) result

(** Error-handling pipeline, output tuple concatenating.
  * Deprecated -- not useful. *)
(*val ( |>>= ) : ('o1,'e) result -> ('o1 -> ('o2,'e) result) ->
   (('o1 * 'o2) , 'e) result*)

(** Error-handling pipeline, parallel and-merge homogeneous. *)
(*val ( |^= ) : ('o1,'e) result -> (('o1 -> ('o2,'e) result) list) ->
   ('o2 list,'e list) result*)

val optAnd2p :
   ('s0 -> 'o1 option) ->
   ('s0 -> 'o2 option) ->
   's0 ->
   ('o1 * 'o2) option

(** Error-handling pipeline block, parallel and-merge double heterogeneous. *)
val ressAnd2p :
   string ->
   ('o0 -> ('o1,string) result) ->
   ('o0 -> ('o2,string) result) ->
   'o0 ->
   ((('o1 * 'o2) , string) result)

(** Error-handling pipeline block, parallel and-merge triple heterogeneous. *)
val ressAnd3p :
   string ->
   ('o0 -> ('o1,string) result) ->
   ('o0 -> ('o2,string) result) ->
   ('o0 -> ('o3,string) result) ->
   'o0 ->
   ((('o1 * 'o2 * 'o3) , string) result)

(** Error-handling pipeline, else-if structure.
    Like |>=, but instead of mapping Ok, maps Error. *)
val ( |>=? ) : ('o,'e) result -> ('e -> ('o,'e) result) -> ('o,'e) result

(** Error-handling pipeline, no-error input form. *)
val ( |>=+ ) : 'o1 -> ('o1 -> ('o2,'e) result) -> ('o2,'e) result

(** Error-handling pipeline, no-error output form. *)
val ( |>=- ) : ('o1,'e) result -> ('o1 -> 'o2) -> ('o2,'e) result


(** Convert a char to a string. *)
val string_of_char : char -> string

(** Convert a char-code (clamped to 0-FF) to a string. *)
val string_of_byte : int -> string

(** Replace \n, \r, \t, \v, \f with space. *)
val blankSpacyCtrlChars : string -> string

(** Replace \n, \r with space. *)
val blankNewlines : string -> string

(**
 * Replace all UTF-8 blank chars with spaces.
 *
 * [ ["0x09" ],                 "U+0009  HT horizontal tab"         ],
 * [ ["0x0A" ],                 "U+000A  LF line feed"              ],
 * [ ["0x0B" ],                 "U+000B  VT vertical tab"           ],
 * [ ["0x0C" ],                 "U+000C  FF form feed"              ],
 * [ ["0x0D" ],                 "U+000D  CR carriage return"        ],
 * [ ["0x20" ],                 "U+0020  SP space"                  ],
 * [ ["0xC2", "0x85" ],         "U+0085  NEL next line"             ],
 * [ ["0xC2", "0xA0" ],         "U+00A0  no-break space"            ],
 * [ ["0xE1", "0x9A", "0x80"],  "U+1680  ogham space mark"          ],
 * [ ["0xE1", "0xA0", "0x8E"],  "U+180E  mongolian vowel separator" ],
 * [ ["0xE2", "0x80", "0x80"],  "U+2000  en quad"                   ],
 * [ ["0xE2", "0x80", "0x81"],  "U+2001  em quad"                   ],
 * [ ["0xE2", "0x80", "0x82"],  "U+2002  en space"                  ],
 * [ ["0xE2", "0x80", "0x83"],  "U+2003  em space"                  ],
 * [ ["0xE2", "0x80", "0x84"],  "U+2004  three-per-em space"        ],
 * [ ["0xE2", "0x80", "0x85"],  "U+2005  four-per-em space"         ],
 * [ ["0xE2", "0x80", "0x86"],  "U+2006  six-per-em space"          ],
 * [ ["0xE2", "0x80", "0x87"],  "U+2007  figure space"              ],
 * [ ["0xE2", "0x80", "0x88"],  "U+2008  punctuation space"         ],
 * [ ["0xE2", "0x80", "0x89"],  "U+2009  thin space"                ],
 * [ ["0xE2", "0x80", "0x8A"],  "U+200A  hair space"                ],
 * [ ["0xE2", "0x80", "0x8B"],  "U+200B  zero-width space"          ],
 * [ ["0xE2", "0x80", "0x8C"],  "U+200C  zero-width non-joiner"     ],
 * [ ["0xE2", "0x80", "0x8D"],  "U+200D  zero-width joiner"         ],
 * [ ["0xE2", "0x80", "0xA8"],  "U+2028  line separator"            ],
 * [ ["0xE2", "0x80", "0xA9"],  "U+2029  paragraph separator"       ],
 * [ ["0xE2", "0x80", "0xAF"],  "U+202F  narrow no-break space"     ],
 * [ ["0xE2", "0x81", "0x9F"],  "U+205F  medium mathematical space" ],
 * [ ["0xE2", "0x81", "0xA0"],  "U+2060  word joiner"               ],
 * [ ["0xE3", "0x80", "0x80"],  "U+3000  ideographic space"         ],
 * [ ["0xEF", "0xBB", "0xBF"]   "U+FEFF  zero width no-break space" ]
 *)
(*val unifySpaces : string -> string*)


(** Clamp number to: lower <= num <= upper. *)
val clamp : lo:'a -> up:'a -> 'a -> 'a

(** The nan aspect of Pervasives.classify_float. *)
val isNan : float -> bool

(** Basic stats of a float list. *)
val minMaxMean : float list -> (float * float * float)


(** Reverse cons - append at foot. *)
val ( @@ ) : 'a list -> 'a -> 'a list


(** Wrap a function in a timer (wall-clock, sub-second resolution). *)
val timerWall : ('a -> 'b) -> 'a -> ('b * float)




(* ---- std lib module augmentations ---- *)

module Int :
sig
   (*include module type of Int*)

   (** Number of decimal digits a number has. *)
   val digitsDec : int -> int

   (**
    * Wrap-around modulo.
    *
    * E.g.:
    * * usual mod:  [-3 -2 -1 0 1 2 3] mod  3  =  -0 -2 -1 0 1 2 0
    * * this modw:  [-3 -2 -1 0 1 2 3] modw 3  =   0  1  2 0 1 2 0
    *)
   val modw : int -> int -> int
end


module Char :
sig
   include module type of Char

   (** Is a-z or A-Z ?. *)
   val isAlpha : char -> bool

   (** Is 0-9 ?. *)
   val isDigit : char -> bool

   (** Is - or + ?. *)
   val isSign  : char -> bool

   (** Is char-code <= 127 ?. *)
   val isAscii : char -> bool

   (** Is any of: space \n \r \t \v \f ?. *)
   val isBlank : char -> bool

   (** Is \n ?. *)
   val isNewline : char -> bool
end


module String :
sig
   include module type of String

   (** Is string empty?. *)
   val isEmpty  : string -> bool
   val notEmpty : string -> bool

   (* Length - 1 (will be -1 if empty). *)
   val lastPos : string -> int

   (** Concatenation of n copies of a string. *)
   val repeat : string -> int -> string

   (** First n chars, with clamped n. *)
   val lead : string -> int -> string

   (** Rest of chars from pos, with clamped pos. *)
   val trail : string -> int -> string

   (** First n chars, and rest of chars from pos, with clamped pos. *)
   val leadTrail : string -> int -> (string * string)

   (** Last char. *)
   val last : string -> char

   (** Test first char. *)
   val isFirstChar : (char -> bool) -> string -> bool

   (** Like index_from, but return option instead of exception. *)
   val index_o : char -> ?start:int -> string -> int option

   (** Like index_from, but with predicate instead of char, and return option
       instead of exception. *)
   val indexp_o : (char -> bool) -> ?start:int -> string -> int option

   (** Like index_from, but return length instead of exception. *)
   val indexl : char -> ?start:int -> string -> int

   (** Like index_from, but with predicate instead of char, and return length
       instead of exception. *)
   val indexpl : (char -> bool) -> ?start:int -> string -> int

   (** Like rindex, but with predicate instead of char, and option instead of
       exception. *)
   val rindexp_o : (char -> bool) -> string -> int option

   (** Remove false-predicate chars. *)
   val filter : (char -> bool) -> string -> string

   (** Remove non-Ascii (> 127) chars. *)
   val filterAscii : string -> string

   (** Check predicate is true for all chars (like a kind of &&).
       (empty string returns true) *)
   val check : (char -> bool) -> string -> bool

   (** Break into two fragments by finding some first char, also output
       breaking-char position. *)
   val halve : char -> string -> (string * string * int) option

   (** Break into fragments by char predicate, also output positions. *)
   val splitp : ?ls:((string * int) list) -> (char -> bool) -> string ->
      (string * int) list

   (** Break into fragments by char predicate. *)
   val split : (char -> bool) -> string -> string list

   (** Trim, and check if too long (bytewise length). *)
   val trimTrunc : (string * int) -> (string , string) result

   (** Truncate to max length. *)
   val truncate : int -> string -> string

   (** Parse int, with some optional restrictions.
       (optional bools express forbidden or required (or allowed if absent)). *)
   val toInt : ?zeroPadded:(bool * int) -> ?widthMaxed:int -> ?signed:bool ->
      string -> int option
end


module List :
sig
   include module type of List

   (** Is list empty?. *)
   val isEmpty : 'a list -> bool

   (** Like hd, but with option instead of exception. *)
   val hdo : 'a list -> 'a option

   (** Last element (foot) - opposite end to head. *)
   val fto : 'a list -> 'a option

   (** Like tl, but return empty list if list is empty. *)
   val tlSafe : 'a list -> 'a list

   (** Like nth, but with option instead of exception. *)
   val ntho : int -> 'a list -> 'a option

   (** Break into two: first n and last (len - n) elements.
       (out-of-range bisect point is clamped) *)
   val bisect : 'a list -> int -> ('a list * 'a list)

   (** Like find, but with option instead of exception. *)
   val find_o : ('a -> bool) -> 'a list -> 'a option

   (** Filter to a different type -- by predicate returning option. *)
   val filtmap : ('a -> 'b option) -> 'a list -> 'b list

   (** Partition to different types -- by predicate returning result. *)
   val partmap : ('a -> ('ok, 'er) result) -> 'a list -> ('ok list * 'er list)

   (** Some kind of find ... *)
   val findmap_o : ('a -> 'b option) -> 'a list -> 'b option

   (** Gather options with an 'and': yield Some when all Some, else None.
       (Or call it a 'transpose': list of options -> option of a list.).
       (Empty list yields Some.). *)
   val optAnd : ('a option) list -> ('a list) option

   (** Gather options with an 'or': yield Some when >=1 Some, else None.
       (Or call it a 'transpose': list of options -> option of a list.).
       (Empty list yields None.). *)
   val optOr : ('a option) list -> ('a list) option

   (** Gather results with an 'and': yield Ok when all Ok, else Error.
       (Or call it a 'transpose': result list -> list result.).
       (Empty list yields Ok.). *)
   val resAnd : (('ok,'er) result) list -> ('ok list , 'er list) result

   (** Make a list by calling a function on each index; explicit length.
       Length is specified up-front as a parameter. *)
   val unfoldl : ?list:('a list) -> (int->'a) -> int -> 'a list

   (** Make a list by calling a function on each index; implicit length.
       Length is extended until generator function returns None. *)
   val unfoldo : ?list:('a list) -> ?index:int -> (int->'a option) -> 'a list

   (** Convert string (of bytes) to list-of-chars. *)
   val ofStringAscii : string -> char list

   (** Convert list-of-chars to string (of bytes). *)
   val toStringAscii : (char list) -> string
end


module Array :
sig
   include module type of Array

   (** Is array empty?. *)
   val isEmpty : 'a array -> bool

   (** First n elements, with clamped n. *)
   val lead      : int -> 'a array -> 'a array

   (** Rest of elements from pos, with clamped pos. *)
   val trail     : int -> 'a array -> 'a array

   (** First n elements, and rest of elements from pos, with clamped pos. *)
   val leadTrail : int -> 'a array -> ('a array * 'a array)

   (** Is predicate true for all elements? (like List.for_all). *)
   val forAll : ('a -> bool) -> 'a array -> bool

   (** Break into two: first n and last (len - n) elements.
       (out-of-range bisect point is clamped) *)
   val bisect   : 'a array -> int -> ('a array * 'a array)

   (** Break into two: first n and last (len - n) elements. *)
   val bisect_o : 'a array -> int -> ('a array * 'a array) option

   (** Filter into two parts (like List.partition), order preservingly. *)
   val partition : ('a -> bool) -> 'a array -> ('a array * 'a array)

   (** For use (curried) with _printf %t .
       @exceptions: whatever can be raised by Printf._printf *)
   val printc_x  : ('a -> out_channel -> unit) -> 'a array -> out_channel
      -> unit

   (*
   (** For use (curried) with ksprintf %t .
       @exceptions: whatever can be raised by Printf.sprintf *)
   val printks_x : ('a -> unit -> string) -> 'a array -> unit -> string
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




(* ---- IO ---- *)

val scanExnUnify_x : (unit -> 'a) -> 'a

val kscanfErrFn : Scanf.Scanning.scanbuf -> exn -> ('o , string) result

val skipBlank : Scanf.Scanning.scanbuf -> unit


(**
 * Open and close a file (from name) around the given callback.
 *
 * @param filePathname
 * @param opener
 * @param closer
 * @param action
 *)
val useFile : string -> (string -> 'c) -> ('c -> unit) -> ('c -> 'i) ->
   ('i , exn) result

(** Read a file (from name) into a string. *)
val fileRead  : filePathname:string ->
   (string , exn) result

(** Write a file (from name) from a string. *)
val fileWrite : filePathname:string -> stuffToWrite:string ->
   (unit , exn) result
