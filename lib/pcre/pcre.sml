structure Pcre = struct
  type iopt = word

  (* polymorphic variants represented with polymorphic records *)
  type 'a compile_option =
       {ANCHORED: unit -> 'a,
        AUTO_CALLOUT: unit -> 'a,
        BSR_ANYCRLF: unit -> 'a,
        BSR_UNICODE: unit -> 'a,
        CASELESS: unit -> 'a,
        DOLLAR_ENDONLY: unit -> 'a,
        DOTALL: unit -> 'a,
        DUPNAMES: unit -> 'a,
        EXTENDED: unit -> 'a,
        EXTRA: unit -> 'a,
        FIRSTLINE: unit -> 'a,
        JAVASCRIPT_COMPAT: unit -> 'a,
        MULTILINE: unit -> 'a,
        NEVER_UTF: unit -> 'a,
        NEWLINE_ANY: unit -> 'a,
        NEWLINE_ANYCRLF: unit -> 'a,
        NEWLINE_CR: unit -> 'a,
        NEWLINE_CRLF: unit -> 'a,
        NEWLINE_LF: unit -> 'a,
        NO_AUTO_CAPTURE: unit -> 'a,
        NO_AUTO_POSSESS: unit -> 'a,
        NO_START_OPTIMISE: unit -> 'a,
        NO_START_OPTIMIZE: unit -> 'a,
        NO_UTF16_CHECK: unit -> 'a,
        NO_UTF32_CHECK: unit -> 'a,
        NO_UTF8_CHECK: unit -> 'a,
        UCP: unit -> 'a,
        UNGREEDY: unit -> 'a,
        UTF16: unit -> 'a,
        UTF32: unit -> 'a,
        UTF8: unit -> 'a} -> 'a

  type 'a dfa_exec_option =
       {ANCHORED: unit -> 'a,
        BSR_ANYCRLF: unit -> 'a,
        BSR_UNICODE: unit -> 'a,
        DFA_RESTART: unit -> 'a,
        DFA_SHORTEST: unit -> 'a,
        NEWLINE_ANY: unit -> 'a,
        NEWLINE_ANYCRLF: unit -> 'a,
        NEWLINE_CR: unit -> 'a,
        NEWLINE_CRLF: unit -> 'a,
        NEWLINE_LF: unit -> 'a,
        NOTBOL: unit -> 'a,
        NOTEMPTY: unit -> 'a,
        NOTEMPTY_ATSTART: unit -> 'a,
        NOTEOL: unit -> 'a,
        NO_START_OPTIMISE: unit -> 'a,
        NO_START_OPTIMIZE: unit -> 'a,
        NO_UTF16_CHECK: unit -> 'a,
        NO_UTF32_CHECK: unit -> 'a,
        NO_UTF8_CHECK: unit -> 'a,
        PARTIAL: unit -> 'a,
        PARTIAL_HARD: unit -> 'a,
        PARTIAL_SOFT: unit -> 'a} -> 'a

  type 'a exec_option =
       {ANCHORED: unit -> 'a,
        BSR_ANYCRLF: unit -> 'a,
        BSR_UNICODE: unit -> 'a,
        NEWLINE_ANY: unit -> 'a,
        NEWLINE_ANYCRLF: unit -> 'a,
        NEWLINE_CR: unit -> 'a,
        NEWLINE_CRLF: unit -> 'a,
        NEWLINE_LF: unit -> 'a,
        NOTBOL: unit -> 'a,
        NOTEMPTY: unit -> 'a,
        NOTEMPTY_ATSTART: unit -> 'a,
        NOTEOL: unit -> 'a,
        NO_START_OPTIMISE: unit -> 'a,
        NO_START_OPTIMIZE: unit -> 'a,
        NO_UTF16_CHECK: unit -> 'a,
        NO_UTF32_CHECK: unit -> 'a,
        NO_UTF8_CHECK: unit -> 'a,
        PARTIAL: unit -> 'a,
        PARTIAL_HARD: unit -> 'a,
        PARTIAL_SOFT: unit -> 'a} -> 'a

  type 'a jit_exec_option =
       {NOTBOL: unit -> 'a,
        NOTEMPTY: unit -> 'a,
        NOTEMPTY_ATSTART: unit -> 'a,
        NOTEOL: unit -> 'a,
        NO_UTF16_CHECK: unit -> 'a,
        NO_UTF32_CHECK: unit -> 'a,
        NO_UTF8_CHECK: unit -> 'a,
        PARTIAL: unit -> 'a,
        PARTIAL_HARD: unit -> 'a,
        PARTIAL_SOFT: unit -> 'a} -> 'a

  fun compile_opts opts cases =
      List.foldl (fn (opt, r) => Word.orb (opt cases, r)) 0w0 opts

  val optVals =
      { CASELESS = 0wx00000001
      , MULTILINE = 0wx00000002
      , DOTALL = 0wx00000004
      , EXTENDED = 0wx00000008
      , ANCHORED = 0wx00000010
      , DOLLAR_ENDONLY = 0wx00000020
      , EXTRA = 0wx00000040
      , NOTBOL = 0wx00000080
      , NOTEOL = 0wx00000100
      , UNGREEDY = 0wx00000200
      , NOTEMPTY = 0wx00000400
      , UTF8 = 0wx00000800
      , UTF16 = 0wx00000800
      , UTF32 = 0wx00000800
      , NO_AUTO_CAPTURE = 0wx00001000
      , NO_UTF8_CHECK = 0wx00002000
      , NO_UTF16_CHECK = 0wx00002000
      , NO_UTF32_CHECK = 0wx00002000
      , AUTO_CALLOUT = 0wx00004000
      , PARTIAL_SOFT = 0wx00008000
      , PARTIAL = 0wx00008000
      , NEVER_UTF = 0wx00010000
      , DFA_SHORTEST = 0wx00010000
      , NO_AUTO_POSSESS = 0wx00020000
      , DFA_RESTART = 0wx00020000
      , FIRSTLINE = 0wx00040000
      , DUPNAMES = 0wx00080000
      , NEWLINE_CR = 0wx00100000
      , NEWLINE_LF = 0wx00200000
      , NEWLINE_CRLF = 0wx00300000
      , NEWLINE_ANY = 0wx00400000
      , NEWLINE_ANYCRLF = 0wx00500000
      , BSR_ANYCRLF = 0wx00800000
      , BSR_UNICODE = 0wx01000000
      , JAVASCRIPT_COMPAT = 0wx02000000
      , NO_START_OPTIMIZE = 0wx04000000
      , NO_START_OPTIMISE = 0wx04000000
      , PARTIAL_HARD = 0wx08000000
      , NOTEMPTY_ATSTART = 0wx10000000
      , UCP = 0wx20000000
      }

  fun CASELESS cases = #CASELESS cases ()
  fun MULTILINE cases = #MULTILINE cases ()
  fun DOTALL cases = #DOTALL cases ()
  fun EXTENDED cases = #EXTENDED cases ()
  fun ANCHORED cases = #ANCHORED cases ()
  fun DOLLAR_ENDONLY cases = #DOLLAR_ENDONLY cases ()
  fun EXTRA cases = #EXTRA cases ()
  fun NOTBOL cases = #NOTBOL cases ()
  fun NOTEOL cases = #NOTEOL cases ()
  fun UNGREEDY cases = #UNGREEDY cases ()
  fun NOTEMPTY cases = #NOTEMPTY cases ()
  fun UTF8 cases = #UTF8 cases ()
  fun UTF16 cases = #UTF16 cases ()
  fun UTF32 cases = #UTF32 cases ()
  fun NO_AUTO_CAPTURE cases = #NO_AUTO_CAPTURE cases ()
  fun NO_UTF8_CHECK cases = #NO_UTF8_CHECK cases ()
  fun NO_UTF16_CHECK cases = #NO_UTF16_CHECK cases ()
  fun NO_UTF32_CHECK cases = #NO_UTF32_CHECK cases ()
  fun AUTO_CALLOUT cases = #AUTO_CALLOUT cases ()
  fun PARTIAL_SOFT cases = #PARTIAL_SOFT cases ()
  fun PARTIAL cases = #PARTIAL cases ()
  fun NEVER_UTF cases = #NEVER_UTF cases ()
  fun DFA_SHORTEST cases = #DFA_SHORTEST cases ()
  fun NO_AUTO_POSSESS cases = #NO_AUTO_POSSESS cases ()
  fun DFA_RESTART cases = #DFA_RESTART cases ()
  fun FIRSTLINE cases = #FIRSTLINE cases ()
  fun DUPNAMES cases = #DUPNAMES cases ()
  fun NEWLINE_CR cases = #NEWLINE_CR cases ()
  fun NEWLINE_LF cases = #NEWLINE_LF cases ()
  fun NEWLINE_CRLF cases = #NEWLINE_CRLF cases ()
  fun NEWLINE_ANY cases = #NEWLINE_ANY cases ()
  fun NEWLINE_ANYCRLF cases = #NEWLINE_ANYCRLF cases ()
  fun BSR_ANYCRLF cases = #BSR_ANYCRLF cases ()
  fun BSR_UNICODE cases = #BSR_UNICODE cases ()
  fun JAVASCRIPT_COMPAT cases = #JAVASCRIPT_COMPAT cases ()
  fun NO_START_OPTIMIZE cases = #NO_START_OPTIMIZE cases ()
  fun NO_START_OPTIMISE cases = #NO_START_OPTIMISE cases ()
  fun PARTIAL_HARD cases = #PARTIAL_HARD cases ()
  fun NOTEMPTY_ATSTART cases = #NOTEMPTY_ATSTART cases ()
  fun UCP cases = #UCP cases ()

  fun compile_opts_to_iopt opts =
      compile_opts opts
                   { CASELESS = fn () => #CASELESS optVals
                   , MULTILINE = fn () => #MULTILINE optVals
                   , DOTALL = fn () => #DOTALL optVals
                   , EXTENDED = fn () => #EXTENDED optVals
                   , ANCHORED = fn () => #ANCHORED optVals
                   , DOLLAR_ENDONLY = fn () => #DOLLAR_ENDONLY optVals
                   , EXTRA = fn () => #EXTRA optVals
                   , UNGREEDY = fn () => #UNGREEDY optVals
                   , UTF8 = fn () => #UTF8 optVals
                   , UTF16 = fn () => #UTF16 optVals
                   , UTF32 = fn () => #UTF32 optVals
                   , NO_AUTO_CAPTURE = fn () => #NO_AUTO_CAPTURE optVals
                   , NO_UTF8_CHECK = fn () => #NO_UTF8_CHECK optVals
                   , NO_UTF16_CHECK = fn () => #NO_UTF16_CHECK optVals
                   , NO_UTF32_CHECK = fn () => #NO_UTF32_CHECK optVals
                   , AUTO_CALLOUT = fn () => #AUTO_CALLOUT optVals
                   , NEVER_UTF = fn () => #NEVER_UTF optVals
                   , NO_AUTO_POSSESS = fn () => #NO_AUTO_POSSESS optVals
                   , FIRSTLINE = fn () => #FIRSTLINE optVals
                   , DUPNAMES = fn () => #DUPNAMES optVals
                   , NEWLINE_CR = fn () => #NEWLINE_CR optVals
                   , NEWLINE_LF = fn () => #NEWLINE_LF optVals
                   , NEWLINE_CRLF = fn () => #NEWLINE_CRLF optVals
                   , NEWLINE_ANY = fn () => #NEWLINE_ANY optVals
                   , NEWLINE_ANYCRLF = fn () => #NEWLINE_ANYCRLF optVals
                   , BSR_ANYCRLF = fn () => #BSR_ANYCRLF optVals
                   , BSR_UNICODE = fn () => #BSR_UNICODE optVals
                   , JAVASCRIPT_COMPAT = fn () => #JAVASCRIPT_COMPAT optVals
                   , NO_START_OPTIMIZE = fn () => #NO_START_OPTIMIZE optVals
                   , NO_START_OPTIMISE = fn () => #NO_START_OPTIMISE optVals
                   , UCP = fn () => #UCP optVals
                   }

  fun exec_opts_to_iopt opts =
      compile_opts opts
                   { ANCHORED = fn () => #ANCHORED optVals
                   , NOTBOL = fn () => #NOTBOL optVals
                   , NOTEOL = fn () => #NOTEOL optVals
                   , NOTEMPTY = fn () => #NOTEMPTY optVals
                   , NO_UTF8_CHECK = fn () => #NO_UTF8_CHECK optVals
                   , NO_UTF16_CHECK = fn () => #NO_UTF16_CHECK optVals
                   , NO_UTF32_CHECK = fn () => #NO_UTF32_CHECK optVals
                   , PARTIAL_SOFT = fn () => #PARTIAL_SOFT optVals
                   , PARTIAL = fn () => #PARTIAL optVals
                   , NEWLINE_CR = fn () => #NEWLINE_CR optVals
                   , NEWLINE_LF = fn () => #NEWLINE_LF optVals
                   , NEWLINE_CRLF = fn () => #NEWLINE_CRLF optVals
                   , NEWLINE_ANY = fn () => #NEWLINE_ANY optVals
                   , NEWLINE_ANYCRLF = fn () => #NEWLINE_ANYCRLF optVals
                   , BSR_ANYCRLF = fn () => #BSR_ANYCRLF optVals
                   , BSR_UNICODE = fn () => #BSR_UNICODE optVals
                   , NO_START_OPTIMIZE = fn () => #NO_START_OPTIMIZE optVals
                   , NO_START_OPTIMISE = fn () => #NO_START_OPTIMISE optVals
                   , PARTIAL_HARD = fn () => #PARTIAL_HARD optVals
                   , NOTEMPTY_ATSTART = fn () => #NOTEMPTY_ATSTART optVals
                   }

  fun dfa_exec_opts_to_iopt opts =
      compile_opts opts
                   { ANCHORED = fn () => #ANCHORED optVals
                   , NOTBOL = fn () => #NOTBOL optVals
                   , NOTEOL = fn () => #NOTEOL optVals
                   , NOTEMPTY = fn () => #NOTEMPTY optVals
                   , NO_UTF8_CHECK = fn () => #NO_UTF8_CHECK optVals
                   , NO_UTF16_CHECK = fn () => #NO_UTF16_CHECK optVals
                   , NO_UTF32_CHECK = fn () => #NO_UTF32_CHECK optVals
                   , PARTIAL_SOFT = fn () => #PARTIAL_SOFT optVals
                   , PARTIAL = fn () => #PARTIAL optVals
                   , DFA_SHORTEST = fn () => #DFA_SHORTEST optVals
                   , DFA_RESTART = fn () => #DFA_RESTART optVals
                   , NEWLINE_CR = fn () => #NEWLINE_CR optVals
                   , NEWLINE_LF = fn () => #NEWLINE_LF optVals
                   , NEWLINE_CRLF = fn () => #NEWLINE_CRLF optVals
                   , NEWLINE_ANY = fn () => #NEWLINE_ANY optVals
                   , NEWLINE_ANYCRLF = fn () => #NEWLINE_ANYCRLF optVals
                   , BSR_ANYCRLF = fn () => #BSR_ANYCRLF optVals
                   , BSR_UNICODE = fn () => #BSR_UNICODE optVals
                   , NO_START_OPTIMIZE = fn () => #NO_START_OPTIMIZE optVals
                   , NO_START_OPTIMISE = fn () => #NO_START_OPTIMISE optVals
                   , PARTIAL_HARD = fn () => #PARTIAL_HARD optVals
                   , NOTEMPTY_ATSTART = fn () => #NOTEMPTY_ATSTART optVals
                   }

  fun jit_exec_opts_to_iopt opts =
      compile_opts opts
                   { NOTBOL = fn () => #NOTBOL optVals
                   , NOTEOL = fn () => #NOTEOL optVals
                   , NOTEMPTY = fn () => #NOTEMPTY optVals
                   , NO_UTF8_CHECK = fn () => #NO_UTF8_CHECK optVals
                   , NO_UTF16_CHECK = fn () => #NO_UTF16_CHECK optVals
                   , NO_UTF32_CHECK = fn () => #NO_UTF32_CHECK optVals
                   , PARTIAL_SOFT = fn () => #PARTIAL_SOFT optVals
                   , PARTIAL = fn () => #PARTIAL optVals
                   , PARTIAL_HARD = fn () => #PARTIAL_HARD optVals
                   , NOTEMPTY_ATSTART = fn () => #NOTEMPTY_ATSTART optVals
                   }

  type t = unit ptr

  type match_result = t * string * int array

  type extra = unit ptr

  type jit_stack = unit ptr

  type pcre_jit_callback = unit ptr

  fun compile pattern options =
      let
        val errorRef = ref (Pointer.NULL ())
        val errorOffsetRef = ref 0
        val pcre = _ffiapply
                     _import "pcre_compile"
                     ( pattern : string
                     , compile_opts_to_iopt options : word
                     , errorRef : char ptr ref
                     , errorOffsetRef : int ref
                     , Pointer.NULL () : char ptr ) : t
      in
        if Pointer.isNull pcre then
          (* FIXME: custom exception *)
          raise Fail (Pointer.importString (!errorRef))
        else
          pcre
      end

  (* FIXME: use variants, not raw integers *)
  fun intInfo v n =
      let
        val r = ref 0
        (* FIXME: error check *)
        val res = _ffiapply _import "pcre_fullinfo"
                            ( v : t
                            , Pointer.NULL () : extra
                            , n : int
                            , r : int ref ) : int
      in
        !r
      end

  fun exec_from_pos v subj pos opts =
      let
        (* 2 for PCRE_INFO_CAPTURECOUNT *)
        val sz = 3 * (intInfo v 2 + 1)
        val ovec = Array.array (sz, ~1)
        (* FIXME: error check *)
        val res = _ffiapply _import "pcre_exec"
                            ( v : t
                            , Pointer.NULL () : extra
                            , subj : string
                            , String.size subj : int
                            , pos : int
                            , exec_opts_to_iopt opts : word
                            , ovec : int array
                            , sz : int ) : int
      in
        (* FIXME: memory management *)
        SOME (v, subj, ovec)
      end

  fun exec v subj opts =
      exec_from_pos v subj 0 opts

  fun get_stringnumber v str =
      let
        val res = _ffiapply _import "pcre_get_stringnumber"
                            ( v : t
                            , str : string ) : int
      in
        if res > 0 then
          SOME res
        else
          NONE
      end

  fun match_is_safe_index (result as (_, _, ovector)) i =
      i * 3 < Array.length ovector

  fun unsafe_match_start (result as (_, _, ovector)) n =
      Array.sub (ovector, n * 2)

  fun unsafe_match_end (result as (_, _, ovector)) n =
      Array.sub (ovector, n * 2 + 1)

  fun match_start result n =
      if match_is_safe_index result n then
        SOME (unsafe_match_start result n)
      else
        NONE

  fun match_end result n =
      if match_is_safe_index result n then
        SOME (unsafe_match_end result n)
      else
        NONE

  fun unsafe_substring (result as (_, subj, _)) n =
      let
        val s = unsafe_match_start result n
        val t = unsafe_match_end result n - s
      in
        String.substring (subj, s, t)
      end

  fun substring result n =
      if match_is_safe_index result n then
        SOME (unsafe_substring result n)
      else
        NONE

  fun unsafe_match_before (result as (_, subj, _)) n =
      let
        val e = Int.max (0, unsafe_match_start result n - 1)
      in
        String.substring (subj, 0, e)
      end

  fun unsafe_match_after (result as (_, subj, _)) n =
      let
        val s = Int.min (unsafe_match_end result n + 1,
                         String.size subj - 1)
      in
        String.extract (subj, s, NONE)
      end

  fun match_before result n =
      if match_is_safe_index result n then
        SOME (unsafe_match_before result n)
      else
        NONE

  fun match_after result n =
      if match_is_safe_index result n then
        SOME (unsafe_match_after result n)
      else
        NONE

  fun named_match_start (result as (v, _, _)) name =
      Option.map (fn i => unsafe_match_start result i)
                 (get_stringnumber v name)

  fun named_match_end (result as (v, _, _)) name =
      Option.map (fn i => unsafe_match_end result i)
                 (get_stringnumber v name)

  fun named_substring (result as (v, _, _)) name =
      Option.map (fn i => unsafe_substring result i)
                 (get_stringnumber v name)

  fun named_match_before (result as (v, _, _)) name =
      Option.map (fn i => unsafe_match_before result i)
                 (get_stringnumber v name)

  fun named_match_after (result as (v, _, _)) name =
      Option.map (fn i => unsafe_match_after result i)
                 (get_stringnumber v name)

  fun free v =
      _ffiapply
        _import "pcre_free" (v : t) : ()

  (*
  fun compile2 pattern options errorcodeptr errptr erroroffset tableptr = ()

  fun study code optionsn errptr = ()

  fun free_study extra = ()

  fun dfa_exec code extra
               subject length startoffset
               options ovector ovecsize workspace wscount = ()

  fun get_stringtable_entries
        code name first last = ()

  fun jit_exec
        code extra subject length startoffset
        options ovector ovecsize
        jstack = ()

  fun jit_stack_alloc startsize maxsize = ()

  fun jit_stack_free stack = ()

  fun assign_jit_stack extra callback data = ()

  fun maketables () = ()

  fun fullinfo code extra what where_ = ()

  fun refcount code adjust = ()

  fun config what where_ = ()

  fun pattern_to_host_byte_order code extra tables = ()
  *)

  fun version () =
      Pointer.importString
        (_ffiapply _import "pcre_version" () : char ptr)
end
