.\"**************************************************************************
.\"*                                                                        *
.\"*                                 OCaml                                  *
.\"*                                                                        *
.\"*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *
.\"*                                                                        *
.\"*   Copyright 1996 Institut National de Recherche en Informatique et     *
.\"*     en Automatique.                                                    *
.\"*                                                                        *
.\"*   All rights reserved.  This file is distributed under the terms of    *
.\"*   the GNU Lesser General Public License version 2.1, with the          *
.\"*   special exception on linking described in the file LICENSE.          *
.\"*                                                                        *
.\"**************************************************************************
.\"
.TH OCAMLC 1

.SH NAME
ocamlc \- The OCaml bytecode compiler

.SH SYNOPSIS
.B ocamlc
[
.I options
]
.I filename ...

.B ocamlc.opt
[
.I options
]
.I filename ...

.SH DESCRIPTION

The OCaml bytecode compiler
.BR ocamlc (1)
compiles OCaml source files to bytecode object files and links
these object files to produce standalone bytecode executable files.
These executable files are then run by the bytecode interpreter
.BR ocamlrun (1).

The
.BR ocamlc (1)
command has a command-line interface similar to the one of
most C compilers. It accepts several types of arguments and processes them
sequentially, after all options have been processed:

Arguments ending in .mli are taken to be source files for
compilation unit interfaces. Interfaces specify the names exported by
compilation units: they declare value names with their types, define
public data types, declare abstract data types, and so on. From the
file
.IR x \&.mli,
the
.BR ocamlc (1)
compiler produces a compiled interface
in the file
.IR x \&.cmi.

Arguments ending in .ml are taken to be source files for compilation
unit implementations. Implementations provide definitions for the
names exported by the unit, and also contain expressions to be
evaluated for their side-effects.  From the file
.IR x \&.ml,
the
.BR ocamlc (1)
compiler produces compiled object bytecode in the file
.IR x \&.cmo.

If the interface file
.IR x \&.mli
exists, the implementation
.IR x \&.ml
is checked against the corresponding compiled interface
.IR x \&.cmi,
which is assumed to exist. If no interface
.IR x \&.mli
is provided, the compilation of
.IR x \&.ml
produces a compiled interface file
.IR x \&.cmi
in addition to the compiled object code file
.IR x \&.cmo.
The file
.IR x \&.cmi
produced
corresponds to an interface that exports everything that is defined in
the implementation
.IR x \&.ml.

Arguments ending in .cmo are taken to be compiled object bytecode.  These
files are linked together, along with the object files obtained
by compiling .ml arguments (if any), and the OCaml standard
library, to produce a standalone executable program. The order in
which .cmo and.ml arguments are presented on the command line is
relevant: compilation units are initialized in that order at
run-time, and it is a link-time error to use a component of a unit
before having initialized it. Hence, a given
.IR x \&.cmo
file must come before all .cmo files that refer to the unit
.IR x .

Arguments ending in .cma are taken to be libraries of object bytecode.
A library of object bytecode packs in a single file a set of object
bytecode files (.cmo files). Libraries are built with
.B ocamlc\ \-a
(see the description of the
.B \-a
option below). The object files
contained in the library are linked as regular .cmo files (see above),
in the order specified when the .cma file was built. The only
difference is that if an object file
contained in a library is not referenced anywhere in the program, then
it is not linked in.

Arguments ending in .c are passed to the C compiler, which generates
a .o object file. This object file is linked with the program if the
.B \-custom
flag is set (see the description of
.B \-custom
below).

Arguments ending in .o or .a are assumed to be C object files and
libraries. They are passed to the C linker when linking in
.B \-custom
mode (see the description of
.B \-custom
below).

Arguments ending in .so
are assumed to be C shared libraries (DLLs).  During linking, they are
searched for external C functions referenced from the OCaml code,
and their names are written in the generated bytecode executable.
The run-time system
.BR ocamlrun (1)
then loads them dynamically at program start-up time.

The output of the linking phase is a file containing compiled bytecode
that can be executed by the OCaml bytecode interpreter:
the command
.BR ocamlrun (1).
If
.B caml.out
is the name of the file produced by the linking phase, the command
.B ocamlrun caml.out
.IR arg1 \  \ arg2 \ ... \ argn
executes the compiled code contained in
.BR caml.out ,
passing it as arguments the character strings
.I arg1
to
.IR argn .
(See
.BR ocamlrun (1)
for more details.)

On most systems, the file produced by the linking
phase can be run directly, as in:
.B ./caml.out
.IR arg1 \  \ arg2 \ ... \ argn .
The produced file has the executable bit set, and it manages to launch
the bytecode interpreter by itself.

.B ocamlc.opt
is the same compiler as
.BR ocamlc ,
but compiled with the native-code compiler
.BR ocamlopt (1).
Thus, it behaves exactly like
.BR ocamlc ,
but compiles faster.
.B ocamlc.opt
may not be available in all installations of OCaml.

.SH OPTIONS

The following command-line options are recognized by
.BR ocamlc (1).
.TP
.B \-a
Build a library (.cma file) with the object files (.cmo files) given
on the command line, instead of linking them into an executable
file. The name of the library must be set with the
.B \-o
option.
.IP
If
.BR \-custom , \ \-cclib \ or \ \-ccopt
options are passed on the command
line, these options are stored in the resulting .cma library.  Then,
linking with this library automatically adds back the
.BR \-custom , \ \-cclib \ and \ \-ccopt
options as if they had been provided on the
command line, unless the
.B \-noautolink
option is given. Additionally, a substring
.B $CAMLORIGIN
inside a
.BR \ \-ccopt
options will be replaced by the full path to the .cma library,
excluding the filename.
.B \-absname
Show absolute filenames in error messages.
.TP
.B \-annot
Deprecated since 4.11. Please use
.BR \-bin-annot
instead.
.TP
.B \-annot\-shapes
Dump information about the definitions of the current compilation unit in binary
format. This information is represented as shapes, an abstraction of modules.
The information for file
.IR src .ml
is put into file
.IR src .cms.
The files
produced by
.B \-annot\-shapes
are more lightweight than the ones produced by
.B \-bin\-annot
but contain enough information for most external tooling needs.
.TP
.B \-bin\-annot
Dump detailed information about the compilation (types, bindings,
tail-calls, etc) in binary format. The information for file
.IR src .ml
is put into file
.IR src .cmt.
In case of a type error, dump
all the information inferred by the type-checker before the error.
The annotation files produced by
.B \-bin\-annot
contain more information
and are much more compact than the files produced by
.BR \-annot .
.TP
.B \-c
Compile only. Suppress the linking phase of the
compilation. Source code files are turned into compiled files, but no
executable file is produced. This option is useful to
compile modules separately.
.TP
.BI \-cc \ ccomp
Use
.I ccomp
as the C linker when linking in "custom runtime" mode (see the
.B \-custom
option) and as the C compiler for compiling .c source files.
.TP
.BI \-cclib\ -l libname
Pass the
.BI \-l libname
option to the C linker when linking in "custom runtime" mode (see the
.B \-custom
option). This causes the given C library to be linked with the program.
.TP
.BI \-ccopt \ option
Pass the given
.I option
to the C compiler and linker, when linking in
"custom runtime" mode (see the
.B \-custom
option). For instance,
.BI \-ccopt\ \-L dir
causes the C linker to search for C libraries in
directory
.IR dir .
.TP
.BI \-color \ mode
Enable or disable colors in compiler messages (especially warnings and errors).
The following modes are supported:

.B auto
use heuristics to enable colors only if the output supports them (an
ANSI-compatible tty terminal);

.B always
enable colors unconditionally;

.B never
disable color output.

The environment variable "OCAML_COLOR" is considered if \-color is not
provided. Its values are auto/always/never as above.

If \-color is not provided, "OCAML_COLOR" is not set and the environment
variable "NO_COLOR" is set, then color output is disabled. Otherwise,
the default setting is
.B auto,
and the current heuristic
checks that the "TERM" environment variable exists and is
not empty or "dumb", and that isatty(stderr) holds.

.TP
.BI \-error\-style \ mode
Control the way error messages and warnings are printed.
The following modes are supported:

.B short
only print the error and its location;

.B contextual
like "short", but also display the source code snippet corresponding
to the location of the error.

The default setting is
.B contextual.

The environment variable "OCAML_ERROR_STYLE" is considered if
\-error\-style is not provided. Its values are short/contextual as
above.

.TP
.B \-compat\-32
Check that the generated bytecode executable can run on 32-bit
platforms and signal an error if it cannot. This is useful when
compiling bytecode on a 64-bit machine.
.TP
.B \-config
Print the version number of
.BR ocamlc (1)
and a detailed summary of its configuration, then exit.
.TP
.BI \-config-var
Print the value of a specific configuration variable
from the
.B \-config
output, then exit. If the variable does not exist,
the exit code is non-zero.
.TP
.B \-custom
Link in "custom runtime" mode. In the default linking mode, the
linker produces bytecode that is intended to be executed with the
shared runtime system,
.BR ocamlrun (1).
In the custom runtime mode, the
linker produces an output file that contains both the runtime system
and the bytecode for the program. The resulting file is larger, but it
can be executed directly, even if the
.BR ocamlrun (1)
command is not
installed. Moreover, the "custom runtime" mode enables linking OCaml
code with user-defined C functions.

Never use the
.BR strip (1)
command on executables produced by
.BR ocamlc\ \-custom ,
this would remove the bytecode part of the executable.

Security warning: never set the "setuid" or "setgid" bits on
executables produced by
.BR ocamlc\ \-custom ,
this would make them vulnerable to attacks.
.TP
.BI \-depend\ ocamldep-args
Compute dependencies, as ocamldep would do.
.TP
.BI \-dllib\ \-l libname
Arrange for the C shared library
.BI dll libname .so
to be loaded dynamically by the run-time system
.BR ocamlrun (1)
at program start-up time.
.TP
.BI \-dllpath \ dir
Adds the directory
.I dir
to the run-time search path for shared
C libraries.  At link-time, shared libraries are searched in the
standard search path (the one corresponding to the
.B \-I
option).
The
.B \-dllpath
option simply stores
.I dir
in the produced
executable file, where
.BR ocamlrun (1)
can find it and use it.
.TP
.BI \-for\-pack \ module\-path
Generate an object file (.cmo file) that can later be included
as a sub-module (with the given access path) of a compilation unit
constructed with
.BR \-pack .
For instance,
.B ocamlc\ \-for\-pack\ P\ \-c\ A.ml
will generate a.cmo that can later be used with
.BR "ocamlc -pack -o P.cmo a.cmo" .
Note: you can still pack a module that was compiled without
.B \-for\-pack
but in this case exceptions will be printed with the wrong names.
.TP
.B \-g
Add debugging information while compiling and linking. This option is
required in order to be able to debug the program with
.BR ocamldebug (1)
and to produce stack backtraces when
the program terminates on an uncaught exception.
.TP
.B \-i
Cause the compiler to print all defined names (with their inferred
types or their definitions) when compiling an implementation (.ml
file). No compiled files (.cmo and .cmi files) are produced.
This can be useful to check the types inferred by the
compiler. Also, since the output follows the syntax of interfaces, it
can help in writing an explicit interface (.mli file) for a file: just
redirect the standard output of the compiler to a .mli file, and edit
that file to remove all declarations of unexported names.
.TP
.BI \-I \ directory
Add the given directory to the list of directories searched for
compiled interface files (.cmi), compiled object code files
(.cmo), libraries (.cma), and C libraries specified with
.BI \-cclib\ \-l xxx
.RB .
By default, the current directory is searched first, then the
standard library directory. Directories added with
.B \-I
are searched
after the current directory, in the order in which they were given on
the command line, but before the standard library directory. See also
option
.BR \-nostdlib .

If the given directory starts with
.BR + ,
it is taken relative to the
standard library directory. For instance,
.B \-I\ +compiler-libs
adds the subdirectory
.B compiler-libs
of the standard library to the search path.
.TP
.BI \-impl \ filename
Compile the file
.I filename
as an implementation file, even if its extension is not .ml.
.TP
.BI \-intf \ filename
Compile the file
.I filename
as an interface file, even if its extension is not .mli.
.TP
.BI \-intf\-suffix \ string
Recognize file names ending with
.I string
as interface files (instead of the default .mli).
.TP
.B \-keep-docs
Keep documentation strings in generated .cmi files.
.TP
.B \-keep-locs
Keep locations in generated .cmi files.
.TP
.B \-labels
Labels are not ignored in types, labels may be used in applications,
and labelled parameters can be given in any order.  This is the default.
.TP
.B \-linkall
Force all modules contained in libraries to be linked in. If this
flag is not given, unreferenced modules are not linked in. When
building a library (option
.BR \-a ),
setting the
.B \-linkall
option forces all subsequent links of programs involving that library
to link all the modules contained in the library.
When compiling a module (option
.BR \-c ),
setting the
.B \-linkall
option ensures that this module will
always be linked if it is put in a library and this library is linked.
.TP
.B \-make\-runtime
Build a custom runtime system (in the file specified by option
.BR \-o )
incorporating the C object files and libraries given on the command
line.  This custom runtime system can be used later to execute
bytecode executables produced with the option
.B ocamlc\ \-use\-runtime
.IR runtime-name .
.TP
.B \-match\-context\-rows
Set number of rows of context used during pattern matching
compilation. Lower values cause faster compilation, but
less optimized code. The default value is 32.
.TP
.B \-no-alias-deps
Do not record dependencies for module aliases.
.TP
.B \-no\-app\-funct
Deactivates the applicative behaviour of functors. With this option,
each functor application generates new types in its result and
applying the same functor twice to the same argument yields two
incompatible structures.
.TP
.B \-noassert
Do not compile assertion checks.  Note that the special form
.B assert\ false
is always compiled because it is typed specially.
This flag has no effect when linking already-compiled files.
.TP
.B \-noautolink
When linking .cma libraries, ignore
.BR \-custom , \ \-cclib \ and \ \-ccopt
options potentially contained in the libraries (if these options were
given when building the libraries).  This can be useful if a library
contains incorrect specifications of C libraries or C options; in this
case, during linking, set
.B \-noautolink
and pass the correct C libraries and options on the command line.
.TP
.B \-nolabels
Ignore non-optional labels in types. Labels cannot be used in
applications, and parameter order becomes strict.
.TP
.B \-nostdlib
Do not automatically add the standard library directory to the list of
directories searched for compiled interface files (.cmi), compiled
object code files (.cmo), libraries (.cma), and C libraries specified
with
.BI \-cclib\ \-l xxx
.RB .
See also option
.BR \-I .
.TP
.BI \-o \ exec\-file
Specify the name of the output file produced by the linker. The
default output name is
.BR a.out ,
in keeping with the Unix tradition. If the
.B \-a
option is given, specify the name of the library
produced.  If the
.B \-pack
option is given, specify the name of the
packed object file produced.  If the
.B \-output\-obj
or
.B \-output\-complete\-obj
option is given,
specify the name of the output file produced.
This can also be used when compiling an interface or implementation
file, without linking, in which case it sets the name of the cmi or
cmo file, and also sets the module name to the file name up to the
first dot.
.TP
.B \-opaque
Interface file compiled with this option are marked so that other
compilation units depending on it will not rely on any implementation
details of the compiled implementation. The native compiler will not
access the .cmx file of this unit -- nor warn if it is absent. This can
improve speed of compilation, for both initial and incremental builds,
at the expense of performance of the generated code.
.TP
.BI \-open \ module
Opens the given module before processing the interface or
implementation files. If several
.B \-open
options are given, they are processed in order, just as if
the statements open! module1;; ... open! moduleN;; were added
at the top of each file.
.TP
.B \-output\-obj
Cause the linker to produce a C object file instead of a bytecode
executable file. This is useful to wrap OCaml code as a C library,
callable from any C program. The name of the output object file
must be set with the
.B \-o
option. This
option can also be used to produce a C source file (.c extension) or
a compiled shared/dynamic library (.so extension).
.TP
.B \-output\-complete\-obj
Same as
.B \-output\-obj
except when creating an object file where it includes the runtime and
autolink libraries.
.TP
.B \-pack
Build a bytecode object file (.cmo file) and its associated compiled
interface (.cmi) that combines the object
files given on the command line, making them appear as sub-modules of
the output .cmo file.  The name of the output .cmo file must be
given with the
.B \-o
option.  For instance,
.B ocamlc\ \-pack\ \-o\ p.cmo\ a.cmo\ b.cmo\ c.cmo
generates compiled files p.cmo and p.cmi describing a compilation
unit having three sub-modules A, B and C, corresponding to the
contents of the object files a.cmo, b.cmo and c.cmo.  These
contents can be referenced as P.A, P.B and P.C in the remainder
of the program.
.TP
.BI \-pp \ command
Cause the compiler to call the given
.I command
as a preprocessor for each source file. The output of
.I command
is redirected to
an intermediate file, which is compiled. If there are no compilation
errors, the intermediate file is deleted afterwards. The name of this
file is built from the basename of the source file with the
extension .ppi for an interface (.mli) file and .ppo for an
implementation (.ml) file.
.TP
.BI \-ppx \ command
After parsing, pipe the abstract syntax tree through the preprocessor
.IR command .
The module
.BR Ast_mapper (3)
implements the external interface of a preprocessor.
.TP
.B \-principal
Check information path during type-checking, to make sure that all
types are derived in a principal way.  When using labelled arguments
and/or polymorphic methods, this flag is required to ensure future
versions of the compiler will be able to infer types correctly, even
if internal algorithms change.
All programs accepted in
.B \-principal
mode are also accepted in the
default mode with equivalent types, but different binary signatures,
and this may slow down type checking; yet it is a good idea to
use it once before publishing source code.
.TP
.B \-rectypes
Allow arbitrary recursive types during type-checking.  By default,
only recursive types where the recursion goes through an object type
are supported. Note that once you have created an interface using this
flag, you must use it again for all dependencies.
.TP
.BI \-runtime\-variant \ suffix
Add
.I suffix
to the name of the runtime library that will be used by the program.
If OCaml was configured with option
.BR \-with\-debug\-runtime ,
then the
.B d
suffix is supported and gives a debug version of the runtime.
.TP
.BI \-stop\-after \ pass
Stop compilation after the given compilation pass. The currently
supported passes are:
.BR parsing ,
.BR typing .
.TP
.B \-safe\-string
Enforce the separation between types
.BR string \ and\  bytes ,
thereby making strings read-only. This is the default.
.TP
.B \-short\-paths
When a type is visible under several module-paths, use the shortest
one when printing the type's name in inferred interfaces and error and
warning messages.
.TP
.B \-strict\-sequence
Force the left-hand part of each sequence to have type unit.
.TP
.B \-unboxed\-types
When a type is unboxable (i.e. a record with a single argument or a
concrete datatype with a single constructor of one argument) it will
be unboxed unless annotated with
.BR [@@ocaml.boxed] .
.TP
.B \-no-unboxed\-types
When a type is unboxable  it will be boxed unless annotated with
.BR [@@ocaml.unboxed] .
This is the default.
.TP
.B \-unsafe
Turn bound checking off for array and string accesses (the
.BR v.(i) and s.[i]
constructs). Programs compiled with
.B \-unsafe
are therefore
slightly faster, but unsafe: anything can happen if the program
accesses an array or string outside of its bounds.
.TP
.B \-unsafe\-string
Identify the types
.BR string \ and\  bytes ,
thereby making strings writable.
This is intended for compatibility with old source code and should not
be used with new software.
.TP
.BI \-use\-runtime \ runtime\-name
Generate a bytecode executable file that can be executed on the custom
runtime system
.IR runtime\-name ,
built earlier with
.B ocamlc\ \-make\-runtime
.IR runtime\-name .
.TP
.B \-v
Print the version number of the compiler and the location of the
standard library directory, then exit.
.TP
.B \-verbose
Print all external commands before they are executed, in particular
invocations of the C compiler and linker in
.B \-custom
mode.  Useful to debug C library problems.
.TP
.BR \-vnum \ or\  \-version
Print the version number of the compiler in short form (e.g. "3.11.0"),
then exit.
.TP
.BI \-w \ warning\-list
Enable, disable, or mark as fatal the warnings specified by the argument
.IR warning\-list .

Each warning can be
.IR enabled \ or\  disabled ,
and each warning can be
.IR fatal \ or
.IR non-fatal .
If a warning is disabled, it isn't displayed and doesn't affect
compilation in any way (even if it is fatal).  If a warning is enabled,
it is displayed normally by the compiler whenever the source code
triggers it.  If it is enabled and fatal, the compiler will also stop
with an error after displaying it.

The
.I warning\-list
argument is a sequence of warning specifiers, with no separators
between them.  A warning specifier is one of the following:

.BI + num
\ \ Enable warning number
.IR num .

.BI \- num
\ \ Disable warning number
.IR num .

.BI @ num
\ \ Enable and mark as fatal warning number
.IR num .

.BI + num1 .. num2
\ \ Enable all warnings between
.I num1
and
.I num2
(inclusive).

.BI \- num1 .. num2
\ \ Disable all warnings between
.I num1
and
.I num2
(inclusive).

.BI @ num1 .. num2
\ \ Enable and mark as fatal all warnings between
.I num1
and
.I num2
(inclusive).

.BI + letter
\ \ Enable the set of warnings corresponding to
.IR letter .
The letter may be uppercase or lowercase.

.BI \- letter
\ \ Disable the set of warnings corresponding to
.IR letter .
The letter may be uppercase or lowercase.

.BI @ letter
\ \ Enable and mark as fatal the set of warnings corresponding to
.IR letter .
The letter may be uppercase or lowercase.

.I uppercase\-letter
\ \ Enable the set of warnings corresponding to
.IR uppercase\-letter .

.I lowercase\-letter
\ \ Disable the set of warnings corresponding to
.IR lowercase\-letter .

The warning numbers are as follows.

1
\ \ \ Suspicious-looking start-of-comment mark.

2
\ \ \ Suspicious-looking end-of-comment mark.

3
\ \ \ Deprecated feature.

4
\ \ \ Fragile pattern matching: matching that will remain
complete even if additional constructors are added to one of the
variant types matched.

5
\ \ \ Partially applied function: expression whose result has
function type and is ignored.

6
\ \ \ Label omitted in function application.

7
\ \ \ Method overridden without using the "method!" keyword.

8
\ \ \ Partial match: missing cases in pattern-matching.

9
\ \ \ Missing fields in a record pattern.

10
\ \ Expression on the left-hand side of a sequence that doesn't
have type
.B unit
(and that is not a function, see warning number 5).

11
\ \ Redundant case in a pattern matching (unused match case).

12
\ \ Redundant sub-pattern in a pattern-matching.

13
\ \ Override of an instance variable.

14
\ \ Illegal backslash escape in a string constant.

15
\ \ Private method made public implicitly.

16
\ \ Unerasable optional argument.

17
\ \ Undeclared virtual method.

18
\ \ Non-principal type.

19
\ \ Type without principality.

20
\ \ Unused function argument.

21
\ \ Non-returning statement.

22
\ \ Preprocessor warning.

23
\ \ Useless record
.B with
clause.

24
\ \ Bad module name: the source file name is not a valid OCaml module name.

25
\ \ Deprecated: now part of warning 8.

26
\ \ Suspicious unused variable: unused variable that is bound with
.BR let \ or \ as ,
and doesn't start with an underscore (_) character.

27
\ \ Innocuous unused variable: unused variable that is not bound with
.BR let \ nor \ as ,
and doesn't start with an underscore (_) character.

28
\ \ A pattern contains a constant constructor applied to the underscore (_)
pattern.

29
\ \ A non-escaped end-of-line was found in a string constant.  This may
cause portability problems between Unix and Windows.

30
\ \ Two labels or constructors of the same name are defined in two
mutually recursive types.

31
\ \ A module is linked twice in the same executable.

32
\ \ Unused value declaration.

33
\ \ Unused open statement.

34
\ \ Unused type declaration.

35
\ \ Unused for-loop index.

36
\ \ Unused ancestor variable.

37
\ \ Unused constructor.

38
\ \ Unused extension constructor.

39
\ \ Unused rec flag.

40
\ \ Constructor or label name used out of scope.

41
\ \ Ambiguous constructor or label name.

42
\ \ Disambiguated constructor or label name.

43
\ \ Nonoptional label applied as optional.

44
\ \ Open statement shadows an already defined identifier.

45
\ \ Open statement shadows an already defined label or constructor.

46
\ \ Error in environment variable.

47
\ \ Illegal attribute payload.

48
\ \ Implicit elimination of optional arguments.

49
\ \ Missing cmi file when looking up module alias.

50
\ \ Unexpected documentation comment.

59
\ \ Assignment on non-mutable value.

60
\ \ Unused module declaration.

61
\ \ Unannotated unboxable type in primitive declaration.

62
\ \ Type constraint on GADT type declaration.

63
\ \ Erroneous printed signature.

64
\ \ -unsafe used with a preprocessor returning a syntax tree.

65
\ \ Type declaration defining a new '()' constructor.

66
\ \ Unused open! statement.

67
\ \ Unused functor parameter.

68
\ \ Pattern-matching depending on mutable state prevents the remaining
arguments from being uncurried.

The letters stand for the following sets of warnings.  Any letter not
mentioned here corresponds to the empty set.

.B A
\ all warnings

.B C
\ 1, 2

.B D
\ 3

.B E
\ 4

.B F
\ 5

.B K
\ 32, 33, 34, 35, 36, 37, 38, 39

.B L
\ 6

.B M
\ 7

.B P
\ 8

.B R
\ 9

.B S
\ 10

.B U
\ 11, 12

.B V
\ 13

.B X
\ 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 30

.B Y
\ 26

.B Z
\ 27

.IP
The default setting is
.BR \-w\ +a\-4\-7\-9\-27\-29\-30\-32..42\-44\-45\-48\-50\-60\-66..70 .
Note that warnings
.BR 5 \ and \ 10
are not always triggered, depending on the internals of the type checker.
.TP
.BI \-warn\-error \ warning\-list
Mark as errors the warnings specified in the argument
.IR warning\-list .
The compiler will stop with an error when one of these
warnings is emitted.  The
.I warning\-list
has the same meaning as for
the
.B \-w
option: a
.B +
sign (or an uppercase letter) marks the corresponding warnings as fatal, a
.B \-
sign (or a lowercase letter) turns them back into non-fatal warnings, and a
.B @
sign both enables and marks as fatal the corresponding warnings.

Note: it is not recommended to use the
.B \-warn\-error
option in production code, because it will almost certainly prevent
compiling your program with later versions of OCaml when they add new
warnings or modify existing warnings.

The default setting is
.B \-warn\-error \-a+31
(only warning 31 is fatal).
.TP
.B \-warn\-help
Show the description of all available warning numbers.
.TP
.B \-where
Print the location of the standard library, then exit.
.TP
.B \-with-runtime
Include the runtime system in the generated program. This is the default.
.TP
.B \-without-runtime
The compiler does not include the runtime system (nor a reference to it) in the
generated program; it must be supplied separately.
.TP
.BI \- \ file
Process
.I file
as a file name, even if it starts with a dash (-) character.
.TP
.BR \-help \ or \ \-\-help
Display a short usage summary and exit.

.SH SEE ALSO
.BR ocamlopt (1), \ ocamlrun (1), \ ocaml (1).
.br
.IR "The OCaml user's manual" ,
chapter "Batch compilation".
