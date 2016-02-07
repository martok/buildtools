program buildutil;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  SysUtils,
  Classes,
  IniFiles,
  processutils,
  fileutil, LazFileUtils,
  uGetOpt, getopts, uBuildFile;

const
  OptionsLong: array[1..3] of TOption = (
    (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
    (Name: 'file'; Has_Arg: Required_Argument; Flag: nil; Value: 'f'),
    (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = 'h?f:';

var
  Buildfile: string = 'Buildfile';
  Buildtask: string = '*buildutil*';

procedure ProcessOption(const opt: string; const OptArg: string);
begin
  case opt of
    'h',
    '?': begin
      WriteLn(ExtractFileName(ParamStr(0)),' [options] [TASK]');
      WriteLn('Task: begin building at [TASK], otherwise starts at [*buildutil*] ');
      WriteLn('Options:');
      WriteLn('  -f|--file "FILENAME"');
      WriteLn('      Read Buildfile from FILENAME, defaults to Buildfile');
      Halt(0);
    end;
  else
    WriteLn(ErrOutput, 'Unknown option: ', opt);
  end;
end;

var
  lastopt: integer;
  bf: TBuildFile;

begin
  Randomize;

  lastopt:= HandleAllOptions(OptionShort, @OptionsLong[1], @ProcessOption);

  if lastopt <= ParamCount then begin
    Buildtask:= ParamStr(lastopt);
  end;

  Buildfile:= ExpandFileName(Buildfile);
  if not FileExists(Buildfile) then begin
    WriteLn(ErrOutput, 'Buildfile ',Buildfile,' not found!');
    Halt(ERROR_BUILD_FILE);
  end;

  bf:= TBuildFile.Create(Buildfile);
  try
    ExitCode:= bf.BuildTask(Buildtask);
  finally
    FreeAndNil(bf);
  end;
end.
