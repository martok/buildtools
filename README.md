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
Task: begin building at [TASK], otherwise starts at [*buildutil*]
Options:
  -f|--file "FILENAME"
      Read Buildfile from FILENAME, defaults to Buildfile
```

File Structure
--------------
Buildfiles are structured in INI-style sections. Each section describes a single Task to be performed by a Tool, or a series of Task names to invoke.
Tasks are executed in the order they are specified. If any Task fails (this includes nonzero exit codes of external programs), the entire process will be aborted.

Variables
---------
Before a Task is executed, variables in the section content are replaced with their current values. Variables are written like so: `${NAME}`. Names may contain any alphanumeric character as well as the underscore. If a NAME is not a valid variable name, it will be kept as-is. If a variable name is valid, but no such variable is defined, it will be replaced by the empty string.

Task Lists
----------
Task lists are Tasks that only invoke other Tasks. Usually, the main Task is a task list. Task lists are defined by having a section contain no Tool specifier and a single `tasks` item:
```
[*buildutil*]
tasks=build,makearchive
```

Tasks
-----
If a Task contains a Tool specifier (which must be given as the first item), this tool is invoked.

* `cmd`: Run a shell script

   All lines except the first are executed as a shell script

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
     required, section containing file definitions
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