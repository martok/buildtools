program gitrev;

uses
  Classes, SysUtils, getopts, uGitCalls, strutils, dateutils;

const
  OptionsLong: array[1..7] of TOption = (
    (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
    (Name: 'path'; Has_Arg: No_Argument; Flag: nil; Value: #0),
    (Name: 'format'; Has_Arg: Required_Argument; Flag: nil; Value: 'f'),
    (Name: 'echo'; Has_Arg: No_Argument; Flag: nil; Value: 'e'),
    (Name: 'delimiter'; Has_Arg: Required_Argument; Flag: nil; Value: 'd'),
    (Name: 'insert'; Has_Arg: Required_Argument; Flag: nil; Value: 'i'),
    (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = 'h?f:ed:i:';


var
  optDescriptFormatString: string = '$branch$ $hash6$ $mod$ $timeutc$';
  optInsertDelimiter: string = '{REV}'':''{/REV}';

function Timestamp(dt: TDateTime): string;
begin
  DateTimeToString(Result, 'yyyy-mm-dd"T"hh:nn:ss', dt);
end;

function GetDescriptor: string;
begin
  Result:= optDescriptFormatString;
  Result:= StringReplace(Result, '$root$', GitRootPath, [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '$branch$', GitBranch, [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '$hash$', GitHash, [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '$hash6$', Copy(GitHash, 1, 6), [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '$mod$', IfThen(GitIsModified, '(modified)',''), [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '$time$', Timestamp(Now), [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '$timeutc$', Timestamp(LocalTimeToUniversal(Now))+'Z', [rfReplaceAll, rfIgnoreCase]);
end;

procedure InsertInto(FName: string);
var
  fl: TStringList;
  i,s,e: integer;
  modified: boolean;
  mstart, mend, l: string;
begin
  fl:= TStringList.Create;
  try
    fl.LoadFromFile(FName);
    modified:= false;

    i:= pos(':', optInsertDelimiter);
    if i > 0 then begin
      mstart:= Copy(optInsertDelimiter, 1, i-1);
      mend:= Copy(optInsertDelimiter, i+1, MaxInt);
    end else begin
      mstart:= optInsertDelimiter;
      mend:= optInsertDelimiter;
    end;

    for i:= 0 to fl.Count - 1 do begin
      l:= fl[i];
      s:= Pos(mstart, l);
      if s=0 then Continue;
      e:= PosEx(mend, l, s + Length(mstart));
      if e=0 then Continue;

      l:= Copy(l, 1, s-1) + mstart + GetDescriptor + mend + Copy(l, e + Length(mend), MaxInt);
      fl[i]:= l;
      modified:= true;
    end;
    if modified then
      fl.SaveToFile(FName);
  finally
    FreeAndNil(fl);
  end;
end;

procedure ProcessOption(const opt: string; const OptArg: string);
begin
  case opt of
    'path': begin
      WriteLn(GitRootPath);
    end;
    'f': begin
      optDescriptFormatString:= OptArg;
    end;
    'e': begin
      WriteLn(GetDescriptor);
    end;
    'd': begin
      optInsertDelimiter:= OptArg;
    end;
    'i': begin
      InsertInto(OptArg);
    end;
    'h',
    '?': begin
      WriteLn(ExtractFileName(ParamStr(0)),' [commands]');
      WriteLn('Commands: (executed in order specified on commandline)');
      WriteLn('  --path');
      WriteLn('      Output current git repo root path');
      WriteLn('  -f|--format "formatstring"');
      WriteLn('      Specify format string to use for output');
      WriteLn('  -e|--echo ');
      WriteLn('      Evaluate current format and output to stdout');
      WriteLn('  -d|--delimiter "begin:end" ');
      WriteLn('      Specify colon-separated begin and end markers for file insert mode');
      WriteLn('  -i|--insert "filename" ');
      WriteLn('      Evaluate current format and replace part between delimiters in file');
      Halt(0);
    end;
  else
    WriteLn(ErrOutput, 'Unknown option: ', opt);
  end;
end;

var
  optIndex: integer;
  opt: string;
begin
  optIndex:= 0;
  while True do begin
    opt:= GetLongOpts(OptionShort, @OptionsLong[1], optIndex);
    if opt = EndOfOptions then
      break;
    if opt = #0 then
      opt:= OptionsLong[optIndex].Name;
    ProcessOption(opt, optArg);
  end;
end.

