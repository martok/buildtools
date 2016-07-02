program buildutil;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  SysUtils,
  Classes,
  IniFiles,
  fileutil, LazFileUtils,
  uGetOpt, getopts, uBuildFile;

const
  OptionsLong: array[1..4] of TOption = (
    (Name: 'help'; Has_Arg: No_Argument; Flag: nil; Value: 'h'),
    (Name: 'file'; Has_Arg: Required_Argument; Flag: nil; Value: 'f'),
    (Name: 'suppress'; Has_Arg: Required_Argument; Flag: nil; Value: 's'),
    (Name: ''; Has_Arg: 0; Flag: nil; Value: #0)
  );
  OptionShort = 'h?f:s:';

const
  TASK_DEFAULT = '*MAIN*';

var
  Buildfile: string = 'Buildfile';
  Buildtask: string = TASK_DEFAULT;
  SuppressedTasks: string = '';

procedure ProcessOption(const opt: string; const OptArg: string);
begin
  case opt of
    'h',
    '?': begin
      WriteLn(ExtractFileName(ParamStr(0)),' [options] [TASK]');
      WriteLn('Task: begin building at [TASK], otherwise starts at ',TASK_DEFAULT,' ');
      WriteLn('Options:');
      WriteLn('  -f|--file "FILENAME"');
      WriteLn('      Read Buildfile from FILENAME, defaults to Buildfile');
      WriteLn('  -s|--suppress "TASK[,TASK]"');
      WriteLn('      Do not execute TASKs, assume succeeded');
      Halt(0);
    end;
    'f': begin
      Buildfile:= OptArg;
    end;
    's': begin
      if SuppressedTasks > '' then
        SuppressedTasks += '';
      SuppressedTasks += OptArg;
    end
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
    bf.SetSuppressed(SuppressedTasks);
    ExitCode:= bf.BuildTask(Buildtask);
  finally
    FreeAndNil(bf);
  end;
end.
