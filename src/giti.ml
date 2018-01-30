(*------------------------------------------------------------------------------

   GITI tool (OCaml 4.06)
   Harrison Ainsworth / HXA7241 : 2017

   http://www.hxa.name/giti

   License: AGPL -- http://www.gnu.org/licenses/agpl.html

------------------------------------------------------------------------------*)




open HxaGeneral




(* user messages ------------------------------------------------------------ *)

let _TITLE  = "GITI tool (OCaml 4.06)"
and _AUTHOR = "Harrison Ainsworth / HXA7241 : 2017."
and _URL    = "http://www.hxa.name/giti"
and _DATE   = "2017-12-11" ;;

let _BANNER_MESSAGE =
{|
  |} ^ _TITLE ^ {| - |} ^ _URL ^ {|

|}
and _HELP_MESSAGE   =
{|
----------------------------------------------------------------------
  |} ^ _TITLE ^ {|

  |} ^ _AUTHOR ^ {|
  |} ^ _URL ^ {|

  |} ^ _DATE ^ {|
----------------------------------------------------------------------

GITI tool translates GITI content to other styles or formats.

usage:
  giti-o [-val]            gitiFilePathName
  giti-o -{fret|note|tab}+ gitiFilePathName

options:
  -val              : validate (default command)
  -{fret|note|tab}+ : transcribe to combination of forms requested:
                      fret &| note &| tab
                      (limitation: 'note' only works for std tuning)

|} ;;




(* entry point -------------------------------------------------------------- *)

try

   match List.tl (Array.to_list Sys.argv) with

   (* print help *)
   | []
   | ["-?"] | ["--help"] -> print_string _HELP_MESSAGE

   (* execute *)
   | _ as _argv ->

      print_string_flush _BANNER_MESSAGE ;

      (* functions *)
      let eprintf                 = Printf.eprintf
      and fileRead_x filePathname = resToExc_x id (fileRead ~filePathname)
      and fileWrite ~(filePathname:string) ~(stuffToWrite:string)
         : (unit , string) result =
         (fileWrite ~filePathname ~stuffToWrite)
         |>
         (errorMap (fun exn : string ->
            "*** File-write exception: " ^
               (match exn with | Sys_error s -> s | _ -> "")))
      in
      let warnForExtraneous (ls:string list) : unit =
         if ls != []
         then eprintf "+++ Warning: unrecognised options: %s.\n%!"
            (String.concat " " ls) ;
      in

      (* values *)
      let commands , filePathname =
         let half0 , half1 = List.bisect _argv ((List.length _argv) - 1) in
         ( half0 , List.hd half1 )
         (*let r = List.rev l in
         ( List.rev (List.tl r) , List.hd r )*)
      in

      (* decision *)
      let outputMessages =
         match commands with

         (* default: validate *)
         | [] as tail
         | "-val" :: tail ->
            warnForExtraneous tail ;
            begin
               let gitiText = fileRead_x filePathname in
               match Piece.create gitiText with
               | Ok _           -> ["--- OK: that is valid."]
               | Error messages -> "*** That is invalid.\n" :: messages
            end

         (* translations *)
         | "-fret" :: _
         | "-note" :: _
         | "-tab"  :: _ as transcribes ->
            (* regularise choices *)
            (* too many (> 3) ? *)
            if (List.length transcribes) > 3
            then eprintf "+++ Warning: excess transcribe options.\n%!" ;
            (* any unrecognised ? *)
            warnForExtraneous
               (snd (List.partition
                  (function
                     | "-fret" | "-note" | "-tab" -> true
                     | _                          -> false)
                  transcribes)) ;
            (* (tolerate duplicates) *)
            let fret , note , tab =
               let has (s:string) : bool = List.exists ((=) s) transcribes in
               ( has "-fret" , has "-note" , has "-tab" )
            in

            (* process: read, parse/translate, write, messages *)
            (* : string *)
            (fileRead_x filePathname)
            |>
            (* : (Piece.t , string list) result *)
            Piece.create
            |>=
            (* : (unit , string list) result *)
            (  (fun (giti:Piece.t) : (unit , string) result ->
                  fileWrite
                     ~filePathname:(filePathname ^ ".giti_")
                     ~stuffToWrite:(Print.(print {fret ; note ; tab} giti)))
               %>
               (* : (unit , string list) result *)
               (errorMap (String.split Char.isNewline)) )
            |>
            (function | Ok _ -> [] | Error ls -> ls)

         (* fail: unrecognised *)
         | _ :: _  as unrecognised  ->
            fail ("unrecognised command/options: " ^
               (String.concat " " unrecognised)) ;
      in

      List.iter prerr_endline outputMessages ;

with
| e -> prerr_string "*** General failure: " ; raise e
