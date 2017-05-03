gitrev
=======

Tool to output git revision info to a file, similar to SVN `$keywords$`.

Usage
-----

```
gitrev [commands]
Commands: (executed in order specified on commandline)
  --path
      Output current git repo root path
  -f|--format "formatstring"
      Specify format string to use for output
  -e|--echo
      Evaluate current format and output to stdout
  -d|--delimiter "begin:end"
      Specify colon-separated begin and end markers for file insert mode
  -i|--insert "filename"
      Evaluate current format and replace part between delimiters in file
```

Formatting Keywords
-------------------

* `$root$`: Repository root path
* `$branch$`: current branch, i.e. `heads/master`
* `$hash$`: full hash of current commit
* `$hash6$`: short hash of current commit
* `$mod$`: repository modified state, either empty or `(modified)`
* `$time$`: ISO-8601 timestamp (local time)
* `$timeutc$`: ISO-8601 timestamp (UTC)

Default format string is:
```
$branch$ $hash6$ $mod$ $timeutc$
```

Delimiter
---------

Inserting text into files needs a delimiter. Start and end-marker are delimited by a `:`. If there is no `:` in the string given, both are assumed to be the same. The default delimiter for `git_rev.pas.sample` is:
```
{REV}':'{/REV}
```
Everything between those two strings on the same line is replaced with the evaluated format string.

buildutil
=========

Tool to automate build/release processes, similar to what could be achieved with makefiles, but more integrated.

Usage
-----

```
buildutil.exe [options] [TASK]
Task: begin building at [TASK], otherwise starts at [*MAIN*]
Options:
  -f|--file "FILENAME"
      Read Buildfile from FILENAME, defaults to Buildfile
  -s|--suppress "TASK[,TASK]"
      Do not execute TASKs, assume succeeded
```

File Structure
--------------
Buildfiles are structured in INI-style sections. Each section describes a single Task to be performed by a Tool, or a series of Task names to invoke.
Tasks are executed in the order they are specified. If any Task fails (this includes nonzero exit codes of external programs), the entire process will be aborted.

Variables
---------
Before a Task is executed, variables in the section content (including CMD scripts) are replaced with their current values. Variables are written like so: `${VARIABLE}`. VARIABLE may be:

1. A valid variable name (any alphanumeric character as well as the underscore):
   1. if the variable is defined, return the value of the variable
   2. otherwise, return the empty string
2. Any other content will be examined as a function call `${FUNC ARGV}`
   1. If the function and arguments are valid, return result of the function
   2. otherwise, return unparsed VARIABLE

Functions syntax is heavily inspired by [GNU Make](https://www.gnu.org/software/make/manual/html_node/Functions.html), although only a subset is implemented here. Functions are always a one-word function name followed by a space character followed by one or more comma-separated arguments. Arguments may contain variable and function references again, but only if not more than the top contain comma-delimited arguments. Example:
```
Valid:
  ${subst dir,dir2,${realpath ${TOOLPATH}}}
Not Valid:
  ${subst dir,dir2,${subst my,other,${TOOLPATH}}}
```

Functions are:
* Text Functions

  | Function | Description |
  | --- | --- |
  | `${upper TEXT}` | Returns TEXT in uppercase |
  | `${lower TEXT}` | Returns TEXT in lowercase |
  | `${subst FROM,TO,TEXT}` | Replace all occurences of FROM in TEXT with TO |

* Variable Functions

  | Function | Description |
  | --- | --- |
  | `${defined NAME}` | return 1 if NAME is defined and nonempty, 0 otherwise |

* File Name Functions

  | Function | Description |
  | --- | --- |
  | `${dir FNAME}` | Returns the directory part of FNAME, including backslash, or `./` if FNAME contains no directory |
  | `${notdir FNAME}` | Returns only the filename part of FNAME, without any directory |
  | `${suffix FNAME}` | Returns only the extension part of FNAME, or the empty string if FNAME has no extension |
  | `${basename FNAME}` | Returns FNAME without extension, but including path names |
  | `${realpath FNAME}` | Returns FNAME expanded relative to the current directory |

* Shell Function

  | Function | Description |
  | --- | --- |
  | `${shell COMMAND}` | Executes COMMAND in a subshell and returns the output, lines concatenated by spaces. COMMAND may include arbitrary spaces and commas. The result code is stored in the _SHELLSTATUS variable. |

Task Lists
----------
Task lists are Tasks that only invoke other Tasks. Usually, the main Task is a task list. Task lists are defined by having a section contain no Tool specifier and a single `tasks` item:
```
[*MAIN*]
tasks=build,makearchive
```
All items except `TASKS=` will be treated as if they were passed to the `env` Tool, that is, set/clear environment variables. The original environment will be restored after executing all referenced tasks.

Tasks
-----
If a Task contains a Tool specifier (which must be given as the first item), this tool is invoked.

* `cmd`: Run a shell script

   All remaining lines in the task are executed as a shell script

* `powershell`: Run a Windows PowerShell script

   All remaining lines in the task are executed as a PowerShell script (as if dot-sourced) with ExecutionPolicy RemoteSigned (so you may call further scripts)

* `lazbuild`: invoke LazBuild (global: `PROGRAM_LAZBUILD`)

   Call Lazbuild to build a Lazarus project.
   ```
   PROJECT=
     Required, .lpi file name
   MODE=
     Optional, build mode
   BUILD=default
     Optional, build depth. Allowed values:
     DEFAULT: only build what is needed
     CLEAN  : re-build project
     ALL    : re-build project and all dependencies
   OPTS=
     Optional, additional call arguments
   ```

* `lazversion`: Modify version information

   Modifies version information in Lazarus project file (LPI). Version information must be active (UseVersionInfo=true) in the PROJECT given. Backups of original files will be created before saving.
   ```
   PROJECT=
     Required, .lpi file name
   INCREMENT=
     Optional, increment version number by `Major.Minor.Revision.Build`. Trailing zeroes may be left out.
   SETENV=
     Optional, store new version number into a variable after other operations are done.
   ADDFLAG=
   REMOVEFLAG=
     Optional, add/remove version flags. Specify comma-separated flags, those not explicitly modified remain unchanged. Allowed values:
       pvaDebug
       pvaPreRelease
       pvaPatched
       pvaPrivateBuild
       pvaSpecialBuild
   COPYTO=
     Optional, copy version information to other LPI file(s) after other operations are done. Specify comma-separated file names here.
   ```

* `zip`: create a zip file from file list (global: `PROGRAM_7ZIP` or `PROGRAM_ZIP`)

   Create a Zip-File from files specified in a listing section. Can use 7z or Info-ZIP style utils.
   ```
   FILENAME=
     Required, output file name
   FILES=
     Required, section containing file definitions
   LEVEL=5
     Optional, compression level (0..9)
   EXCLUDE=
     Optional, file mask to exclude
   ```
   Files list format is `FROM=TO`. `FROM` may be a file or file mask. If it is a file, `TO` may be a directory inside the archive or the new file name. To signal a directory name, use a path delimiter as last character, in this case, the file will keep the basename. If `FROM` is a file mask, `TO` will be treated as a directory name. If `TO` is empty, it is the same as `/`, meaning the archive root.

* `env`: Set environment variables

   Lines contain `Name=Value` pairs of variables. Use `Name=` to clear/undefine a variable.


Global Configuration
--------------------
Some global parameters are required to specify for example program paths. These are searched in (in order):

1. Variables set via the `env` Tool
2. Environment variables
3. the section `*BUILDUTIL*` of the current Buildfile
4. the section `Globals` of an INI-filed called `buildutil.ini` located in the same place as the buildutil binary (more precisely, the buildutil binary name with `.ini` extension).