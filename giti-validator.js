/*------------------------------------------------------------------------------

   GITI Validator
   Harrison Ainsworth / HXA7241 : 2013-06

   http://www.hxa.name/tools

   License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/

------------------------------------------------------------------------------*/




var GitiValidator = function()
{

   /**
    * Validate a GITI piece, directly to and from element ids.
    *
    * @param inputId  [string] id of element containing giti piece [string]
    * @param outputId [string] id of element to receive warnings [string]
    */
   var validateHtml = function( inputId, outputId )
   {
      var piece = document.getElementById(inputId).value;

      var warnings       = doPiece( piece );
      var warningsString = stringifyWarnings( warnings );

      document.getElementById(outputId).textContent = warningsString;
   }


   /**
    * Validate a GITI piece.
    *
    * @param piece [string] of lines separated by \n
    * @return [array] of objects:
    *    [ { "line":<number>, "detail":<string>, "token":<string>,
    *        "message":<string> } ... ]
    */
   var doPiece = function( piece )
   {
      var lines = (piece.length > 0) && piece.split( "\n" ) || [];
      var warns = [];

      // handle each kind of line differently
      for( var i = 0, len = lines.length; i < len; i++ )
      {
         var line = lines[i];
         var warn;
         switch( line[0] )
         {
            case undefined : warn = doEmpty( line, i );          break;
            case "#"       : warn = doComment( line, i );        break;
            case "@"       : warn = doAnnotation( line, i );     break;
            default        : warn = doSoundIndicator( line, i ); break;
         }
         warns = concat( warns, warn );
      }

      return warns;
   };


   /**
    * Serialise warnings into a string.
    *
    * @param warnings [array] of objects:
    *    [ { "line":<number>, "detail":<string>, "token":<string>,
    *        "message":<string> } ... ]
    * @return [string] each warning on a separate line
    */
   var stringifyWarnings = function( warnings )
   {
      var warningsString = "";
      for( var i = 0, len = warnings.length; i < len; i++ )
      {
         var warning = warnings[i];
         warningsString += i + " : line " + (warning.line + 1) +
            " \"" + (warning.detail && (warning.detail + "\" in \"") || "") +
            warning.token + "\" -- " + warning.message + "\n";
      }
      return warningsString;
   }


   var doEmpty = function( line, lineNumber )
   {
      // ignore
   };


   var doComment = function( line, lineNumber )
   {
      // ignore
   };


   var doAnnotation = function( line, lineNumber )
   {
      var warns = [];

      var notFirst = line.slice( 1 );
      var tokens   = tokenise( notFirst );

      // handle each token
      for( var i = 0, len = tokens.length; i < len; i++ )
      {
         var token = tokens[i];
         var warn;

         var parse = /^([^:]*):(.*)/.exec( token ) || ["","",""];
         var name  = parse[1];
         var val   = parse[2];

         switch( name )
         {
            case "giti"   : warn = doVersion( val, token, lineNumber ); break;
            case "tuning" : warn = doTuning( val, token, lineNumber );  break;
            case "tempo"  : warn = doTempo( val, token, lineNumber );   break;
            default       :
               warn = makeWarn( "invalid annotation part", name, token,
                  lineNumber ); break;
         }
         warns = concat( warns, warn );
      }

      return warns;
   };


   var doVersion = function( value, token, lineNumber )
   {
      var warn;
      if( value != "e-3.2" )
      {
         warn = makeWarn( "non-expected annotation version", value, token,
            lineNumber );
      }

      return warn;
   };


   var doTuning = function( value, token, lineNumber )
   {
      var warn;

      // offset
      if( /^[-+]/.test( value ) )
      {
         if( !( /^[-+]\d{1,2}$/.test( value ) ) ||
            (parseInt( value.slice(1), 10 ) > 12) )
         {
            warn = makeWarn( "invalid annotation tuning, offset", value, token,
               lineNumber );
         }
      }
      // pitches
      else if( /^\d/.test( value ) )
      {
         // handle pitches as a list, recursively

         var doPitch = function( pitches )
         {
            var warns = [];

            var parse = /^([^=]+)(=(.*))?$/.exec( pitches ) || ["",""];
            var first = parse[1];
            var rest  = parse[2] || "";
            var next  = parse[3] || "";

            if( !( /^[1-8](C|C#|D|Eb|E|F|F#|G|G#|A|Bb|B)[0-8]$/.test( first )) )
            //if( !( /^[1-8][A-G][b#]?[0-8]$/.test( first ) ) )
            {
               warns = concat( warns, makeWarn(
                  "invalid annotation tuning: string-pitch", first || pitches,
                  token, lineNumber ) );
            }

            if( rest ) warns = concat( warns, doPitch( next ) );

            return warns;
         };

         warn = doPitch( value );
      }
      // std
      else if( value != "std" )
      {
         warn = makeWarn( "invalid annotation tuning, unrecognised", value,
            token, lineNumber );
      }

      return warn;
   };


   var doTempo = function( value, token, lineNumber )
   {
      var warn;
      if( !( /^\d{1,2}\.\d{1,3}$/.test( value ) ) )
      {
         warn = makeWarn( "invalid annotation tempo", value, token,
            lineNumber );
      }

      return warn;
   }


   var doSoundIndicator = function( line, lineNumber )
   {
      var tokens = tokenise( line );
      var warns  = [];

      // handle each token
      for( var i = 0, len = tokens.length; i < len; i++ )
      {
         var token = tokens[i];

         // indicator
         if( /^[|'()]/.test( token ) )
         {
            if( !( /^((\|+[1-9]?)|(\(+[1-9]?)|(\)+)|('+))$/.test( token ) ) )
            {
               warns = concat( warns,
                  makeWarn( "invalid indicator", "", token, lineNumber ) );
            }
         }
         // sound
         else
         {
            var parse = /^([a-z][^:]*:)?([^:]+)(:[^:]+)?$/.exec( token ) ||
               ["","","",""];
            var act   = parse[1] || "";
            var pitch = parse[2] || "";
            var time  = parse[3] || "";

            if( parse[0] != "" )
            {
               warns = concat( warns, doAct( act, pitch, token, lineNumber ) );
               warns = concat( warns, doPitch( pitch, token, lineNumber ) );
               warns = concat( warns, doTime( time, token, lineNumber ) );
            }
            else if( !( /^[-.]:$/.test( token ) ) )
            {
               warns = concat( warns,
                  makeWarn( "invalid sound", "", token, lineNumber ) );
            }
         }
      }

      return warns;
   };


   var doAct = function( act, pitch, token, lineNumber )
   {
      var warns = [];

      if( act )
      {
         if( /^[-.]$/.test( pitch ) )
         {
            warns = concat( warns,
               makeWarn( "invalid sound act, rests/continuations have no act", 
               act, token, lineNumber ) );
         }
         else
         {
            if( !( /^[pdhusti].*$/.test( act ) ) )
            {
               warns = concat( warns,
                  makeWarn( "invalid sound act, invalid basic",
                  act, token, lineNumber ) );
            }
            if( !( /^.\$?[+-]?[<>]?:$/.test( act ) ) )
            {
               warns = concat( warns,
                  makeWarn( "invalid sound act, invalid extra",
                  act, token, lineNumber ) );
            }
         }
      }

      return warns;
   };


   var doPitch = function( pitch, token, lineNumber )
   {
      var warns = [];

      if( !( /^[-.]$/.test( pitch ) ) )
      {
         // handle chord parts as a list, recursively

         var doChordPart = function( parts )
         {
            var warns = [];

            var parse = /^([^=]+)(=(.+))?$/.exec( parts );
            var first = parse[1];
            var rest  = parse[3] || "";

            {
               var parse = /^(\*|[1-8](\d{1,2})|[,^][0-8][><](\d{1,2}))(.*)$/.
                  exec( first ) || ["","","","",""];
               var basic = parse[1] || "";
               var fret0 = parse[2] || "";
               var fret1 = parse[3] || "";
               var extra = parse[4] || "";

               if( !basic || (fret0 > 24) || (fret1 > 24) )
               {
                  warns = concat( warns,
                     makeWarn( "invalid sound pitch, invalid basic",
                     (first != pitch) && first, pitch, lineNumber ) );
               }

               var extraParse =
                  /^([+-][1-5]|[/\\](\d{1,2}))?(v|r(-?[1-9][1-9]?)?)?[hi]?$/.
                  exec( extra ) || [null,"",""];
               var extra = extraParse[0];
               var fret2 = extraParse[2] || "";

               if( (extra == null) || (fret2 > 24) )
               {
                  warns = concat( warns,
                     makeWarn( "invalid sound pitch, invalid extra",
                     (first != pitch) && first, pitch, lineNumber ) );
               }
            }

            if( rest ) warns = concat( warns, doChordPart( rest ) );

            return warns;
         };

         warns = doChordPart( pitch );
      }

      return warns;
   };


   var doTime = function( time, token, lineNumber )
   {
      var warns = [];

      if( time )
      {
         var parse = /^:(\d{1,3})(.*)$/.exec( time ) || ["","",""];
         var basic = parse[1] || "";
         var extra = parse[2] || "";

         if( !basic )
         {
            warns = concat( warns,
               makeWarn( "invalid sound time, invalid basic",
               time, token, lineNumber ) );
         }
         if( extra && !( /^(-[1-9]\d{0,2})*[es]?-?$/.test( extra ) ) )
         {
            warns = concat( warns,
               makeWarn( "invalid sound time, invalid extra",
               time, token, lineNumber ) );
         }
      }

      return warns;
   };


   var makeWarn = function( message, detail, token, lineNumber )
   {
      return { "line" : lineNumber, "detail" : detail, "token" : token,
         "message" : message };
   };


   var tokenise = function( str )
   {
      var trimmed = str.replace( /^\s+|\s+$/g, "" );
      return (trimmed.length > 0) && trimmed.split( /\s+/ ) || [];
   };


   var concat = function( arr, v )
   {
      if( v ) arr = arr.concat( v );
      return arr;
   };


   return {
      validateHtml:      validateHtml,
      validate:          doPiece,
      stringifyWarnings: stringifyWarnings
   };
}
