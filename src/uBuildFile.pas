unit uBuildFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TBuildFile = class(TMemIniFile)
  private
    fCurrentTask: string;
  protected
    function RunMetatask(const aOrder: TStringList): Integer;
    function RunTooltask(const aOrder: TStringList): Integer;
  public
    function BuildTask(const aTask: string): Integer;
    function GetSection(const aSection: string): TStringList;
    property CurrentTask: string read fCurrentTask;
    function TryGetGlobal(Name: string; out Value: string): boolean;
    function GetGlobal(Name: string): string;
  public
    class function GetTempName: string;
  end;

  TBuildToolClass = class of TBuildTool;
  TBuildTool = class
  private
    fOwner: TBuildFile;
  public
    constructor Create(aOwner: TBuildFile); virtual;
    property Owner: TBuildFile read fOwner;
    function Execute(const aOrder: TStringList): Integer; virtual; abstract;
  end;

const
  ERROR_BUILD_BASE     = 100;
  ERROR_BUILD_FILE     = ERROR_BUILD_BASE + 1;
  ERROR_TASK_NOT_FOUND = ERROR_BUILD_BASE + 2;
  ERROR_TASK_INVALID   = ERROR_BUILD_BASE + 3;
  ERROR_TASK_TOOL      = ERROR_BUILD_BASE + 4;
  ERROR_GLOBAL_CONFIG  = ERROR_BUILD_BASE + 5;
  ERROR_TASK_PARAMETER = ERROR_BUILD_BASE + 6;
  ERROR_TASK_PROCESS   = ERROR_BUILD_BASE + 7;

implementation

uses
  uToolCmd, uToolLazbuild, uToolZip;

{ TBuildFile }

function TBuildFile.BuildTask(const aTask: string): Integer;
var
  task: TStringList;
begin
  if SectionExists(aTask) then begin
    task:= GetSection(aTask);
    try
      WriteLn('Begin Task : ', aTask);
      fCurrentTask:= aTask;
      if (task.Values['TOOL'] = '') and (task.IndexOfName('TASKS')>=0) then begin
        Result:= RunMetatask(task);
      end else
      if (task.IndexOfName('TOOL') >= 0) then begin
        Result:= RunTooltask(task);
      end else begin
        WriteLn(ErrOutput, 'Invalid task format for ',aTask);
        Exit(ERROR_TASK_INVALID);
      end;
      if Result <> 0 then begin
        WriteLn(ErrOutput, 'Task failed, return code ', Result);
        Halt(Result);
      end;
    finally
      FreeAndNil(task);
    end;
  end else begin
    WriteLn(ErrOutput, 'Task ',aTask,' not found');
    Exit(ERROR_TASK_NOT_FOUND);
  end;
end;

function TBuildFile.GetSection(const aSection: string): TStringList;
begin
  Result:= TStringList.Create;
  ReadSectionRaw(aSection, Result);
  Result.CaseSensitive:= false;
end;

class function TBuildFile.GetTempName: string;
begin
  Result := ConcatPaths([GetTempDir(True), 'pack' + IntToStr(Random(1000))]);
end;

function TBuildFile.TryGetGlobal(Name: string; out Value: string): boolean;
var
  ini: TIniFile;
begin
  Result:= false;
  Value := GetEnvironmentVariable(Name);
  if Value > '' then
    Exit(true);
  Value := ReadString('*BUILDUTIL*', Name, '');
  if Value > '' then
    Exit(true);
  ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Value := ini.ReadString('GLOBALS', Name, '');
  finally
    FreeAndNil(ini);
  end;
  if Value > '' then
    Exit(true);
end;

function TBuildFile.GetGlobal(Name: string): string;
begin
  if not TryGetGlobal(Name, Result) then begin
    WriteLn(ErrOutput, 'Error locating global config field ', UpperCase(Name), '.');
    halt(ERROR_GLOBAL_CONFIG);
  end;
end;

function TBuildFile.RunMetatask(const aOrder: TStringList): Integer;
var
  tasks: TStringList;
  task: string;
begin
  tasks:= TStringList.Create;
  try
    tasks.Delimiter:= ',';
    tasks.DelimitedText:= aOrder.Values['TASKS'];
    if tasks.Count = 0 then begin
      WriteLn('No Tasks given, nothing to do.');
      Exit(0);
    end;

    for task in tasks do begin
      Result:= BuildTask(task);
      if Result <> 0 then
        Exit;
    end;
  finally
    FreeAndNil(tasks);
  end;
end;

function TBuildFile.RunTooltask(const aOrder: TStringList): Integer;
var
  toolc: TBuildToolClass;
  tool: TBuildTool;
begin
  if (aOrder.Count < 1) or (aOrder.IndexOfName('TOOL') <> 0) then begin
    WriteLn(ErrOutput, 'Task ', CurrentTask, ': Tool specifier must be first line of definition.');
    Exit(ERROR_TASK_TOOL);
  end;

  case UpperCase(aOrder.ValueFromIndex[0]) of
    'CMD': toolc := TBuildToolCmd;
    'LAZBUILD': toolc := TBuildToolLazbuild;
    'ZIP': toolc := TBuildToolZip;
    else begin
      WriteLn(ErrOutput, 'Task ', CurrentTask, ': Tool ', aOrder.ValueFromIndex[0], ' is unknown.');
      Exit(ERROR_TASK_TOOL);
    end;
  end;
  tool:= toolc.Create(Self);
  try
    Result := Tool.Execute(aOrder);
  finally
    FreeAndNil(tool);
  end;
end;

{ TBuildTool }

constructor TBuildTool.Create(aOwner: TBuildFile);
begin
  inherited Create;
  fOwner:= aOwner;
end;

end.


