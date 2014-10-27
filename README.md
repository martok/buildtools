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
