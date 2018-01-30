
GITI tool : Introductory Documentation
======================================================================


"GITI tool" ; hxa7241 ; 2017 / OCaml / software .

Harrison Ainsworth / HXA7241 : 2017  
http://www.hxa.name/giti

2018-01-26T11:23Z




Contents
--------

* Description
* Installation
* Usage

* Future intentions
* Contributions guide
* Project status

* Acknowledgements




Description
-----------

This is a command-line tool for validation and minor processing of
GITI files. GITI is an electric guitar music notation text format, the
description and definition (spec) of which is at
http://www.hxa.name/giti .

GITI tool can validate, can translate to tab form, and can translate
from string/fret to note (limitedly).




Installation
------------

### Build ###

Requirements:
* OCaml 4.06 -- installed normally
* make -- installed normally

Procedure:
1. have command-line at giti root dir
2. run make (which will use the supplied `makefile` file)

... which yields the `giti-o` executable.


### Config ###

None.


### Install ###

Move the `giti-o` executable file to wherever convenient.




Usage
-----

### Commands ###

There are two usage forms, validation and translation:
    giti-o [-val]            gitiFilePathName
    giti-o -{fret|note|tab}+ gitiFilePathName

(`[...]` meaning optional, `{...|...}` meaning any of, `+` meaning one
or more)

The options are:
    -val              : validate (default command)
    -{fret|note|tab}+ : transcribe to combination of forms requested:
                        fret &| note &| tab
                        (limitation: 'note' only works for std tuning)

Results output:
* for `-val`: invocation acknowledgement to stdout, either:
  * `--- OK: that is valid.`
  * `*** That is invalid.`
* for `-{fret|note|tab}+`: translation into file
  `gitiFilePathName.giti_` (adding the `.giti_` suffix)
* for all: error messages to stderr


### Errors ###

Each validation error is a line of the form:
    *** error: line <num> char <num> : "<snippet>" - <section>:
      <(subsection) reason>

'snippet' being a quote of the part containing the invalidity.
'section' being one of the main sections of the spec at
http://www.hxa.name/giti -- sound, indicator, annotation, metadata.

Eg:
    *** error: line 48 char 1 : "p~:62" - Sound: act extra
      unrecognised.
    *** error: line 50 char 45 : "62:1234~" - Sound: time basic
      duration & extra unrecognised.

The error messages are not very precise or clear, merely enough to
locate the part of the spec to consult.




Future intentions
-----------------

* improve tab output translation (tweak toward conventions)
* make note output translation work for non-std tuning

* translators to:
   * abc -- http://abcnotation.com
   * SuperCollider -- http://supercollider.github.io
   * ? OSC -- http://opensoundcontrol.org
   * ? MIDI
      * midi-text -- http://www.archduke.org/midi/
      * midi-csv  -- http://www.fourmilab.ch/webtools/midicsv/
* validate times against bars/measures
* parse tab-form
* random sequence generator




Contributions guide
-------------------

...




Project status
--------------

Dormant/quiescent.

(And dependent on http://www.hxa.name/giti .)


### Modification request response time ###

Could be weeks ...




Acknowledgements
----------------

* Ibanez guitars, Aria guitars --
  http://www.ibanez.com/  http://www.ariaguitars.com/
* the idea and design of guitar tabs --
  https://en.wikipedia.org/wiki/Tablature#Guitar_tablature
* the event-packet idea of MIDI --
  https://www.midi.org/articles/tutorials
* OCaml language --
  https://ocaml.org/

