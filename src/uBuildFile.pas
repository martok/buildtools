unit uBuildFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TBuildFile = class(TMemIniFile)
  private
    fCurrentTask: string;
    fGlobals: TStringList;
    fSuppressed: TStringList;
  protected
    function RunTasklist(const aOrder: TStringList): Integer;
    function RunTask(const aOrder: TStringList): Integer;
    function ValidGlobalName(Name: string): boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function BuildTask(const aTask: string): Integer;
    function GetSection(const aSection: string): TStringList;
    property CurrentTask: string read fCurrentTask;
    function TryGetGlobal(Name: string; out Value: string): boolean;
    function GetGlobal(Name: string): string;
    function SetGlobal(Name, Value: string): Boolean;
    procedure ProcessVariables(const aOrder: TStringList);
    function SubstituteVariables(Line: String): String;
    procedure SetSuppressed(Tasks: string);
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
  uToolCmd, uToolLazbuild, uToolZip, uToolEnv, uToolLazVersion,
  strutils, processutils;

{ TBuildFile }

class function TBuildFile.GetTempName: string;
begin
  Result := ConcatPaths([GetTempDir(True), 'pack' + IntToStr(Random(1000))]);
end;

procedure TBuildFile.AfterConstruction;
begin
  inherited AfterConstruction;
  fGlobals:= TStringList.Create;
  fGlobals.CaseSensitive:= false;
  fSuppressed:= TStringList.Create;
  fSuppressed.Delimiter:= ',';
  fSuppressed.CaseSensitive:= false;
end;

procedure TBuildFile.BeforeDestruction;
begin
  FreeAndNil(fSuppressed);
  FreeAndNil(fGlobals);
  inherited BeforeDestruction;
end;

function TBuildFile.BuildTask(const aTask: string): Integer;
var
  task: TStringList;
begin
  if SectionExists(aTask) then begin
    if fSuppressed.IndexOf(aTask) >= 0 then begin
      WriteLn('Skipping Task : ', aTask, ' (because on Suppressed list)');
      Exit(0);
    end;
    task:= GetSection(aTask);
    try
      WriteLn('Begin Task : ', aTask);
      fCurrentTask:= aTask;
      ProcessVariables(task);
      if (task.Values['TOOL'] = '') and (task.IndexOfName('TASKS')>=0) then begin
        Result:= RunTasklist(task);
      end else
      if (task.IndexOfName('TOOL') >= 0) then begin
        Result:= RunTask(task);
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

function TBuildFile.TryGetGlobal(Name: string; out Value: string): boolean;
var
  ini: TIniFile;
begin
  Result:= false;
  Value:= fGlobals.Values[Name];
  if Value > '' then
    Exit(true);
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

function TBuildFile.SetGlobal(Name, Value: string): Boolean;
var
  i: integer;
begin
  Result:= true;
  if Value = '' then begin
    i:= fGlobals.IndexOfName(Name);
    if i >= 0 then
      fGlobals.Delete(i);
  end else begin
    if not ValidGlobalName(Name) then
      Exit(False)
    else
      fGlobals.Values[Name]:= Value;
  end;
end;

function TBuildFile.RunTasklist(const aOrder: TStringList): Integer;
var
  tasks: TStringList;
  task: string;
  saveEnv: TStringList;
  i: Integer;
begin
  tasks:= TStringList.Create;
  try
    tasks.Delimiter:= ',';
    tasks.DelimitedText:= aOrder.Values['TASKS'];
    if tasks.Count = 0 then begin
      WriteLn('No Tasks given, nothing to do.');
      Exit(0);
    end;

    saveEnv:= TStringList.Create;
    try
      saveEnv.Assign(fGlobals);
      for i:= 0 to aOrder.Count - 1 do begin
        if not AnsiSameText(aOrder[i], 'TASKS') then
          if not SetGlobal(aOrder.Names[i], aOrder.ValueFromIndex[i]) then begin
            WriteLn(ErrOutput, 'Task ', CurrentTask, ': Cannot set env var ', aOrder.Names[i], '.');
            Exit(ERROR_TASK_PROCESS);
          end;
      end;
      for task in tasks do begin
        Result:= BuildTask(task);
        if Result <> 0 then
          Exit;
      end;
    finally
      fGlobals.Assign(saveEnv);
      FreeAndNil(saveEnv);
    end;
  finally
    FreeAndNil(tasks);
  end;
end;

function TBuildFile.RunTask(const aOrder: TStringList): Integer;
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
    'LAZVERSION': toolc := TBuildToolLazVersion;
    'ZIP': toolc := TBuildToolZip;
    'ENV': toolc := TBuildToolEnv;
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

function TBuildFile.ValidGlobalName(Name: string): boolean;
const
  Forbidden: set of Char = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'] - ['_'];
begin
  Result:= PosSet(Forbidden, Name) = 0;
end;

procedure TBuildFile.ProcessVariables(const aOrder: TStringList);
var
  i: integer;
begin
  for i:= 0 to aOrder.Count - 1 do begin
    aOrder[i]:= SubstituteVariables(aOrder[i]);
  end;
end;

function TBuildFile.SubstituteVariables(Line: String): String;
const
  VAR_START = '${';
  VAR_END   = '}';
  VAR_FUNC_DELIM = ' ';
  VAR_PARAM_DELIM = ',';

  function InterpretFunction(func: string; args: TStringArray; out Value: string): boolean;
  begin
    Result:= true;
    case UpperCase(func) of
      // Text Functions
      'UPPER': begin
        if Length(Args) <> 1 then
          Exit(false);
        Value:= args[0].ToUpper;
      end;
      'LOWER': begin
        if Length(Args) <> 1 then
          Exit(false);
        Value:= args[0].ToLower;
      end;
      'SUBST': begin
        if Length(Args) <> 3 then
          Exit(false);
        Value:= StringReplace(args[2], args[0], args[1], [rfReplaceAll]);
      end;
      // File Name Functions
      'DIR': begin
        if Length(Args) <> 1 then
          Exit(false);
        Value:= ExtractFilePath(args[0]);
        if Value = '' then
          Value:= '.'+DirectorySeparator;
      end;
      'NOTDIR': begin
        if Length(Args) <> 1 then
          Exit(false);
        Value:= ExtractFileName(args[0]);
      end;
      'SUFFIX': begin
        if Length(Args) <> 1 then
          Exit(false);
        Value:= ExtractFileExt(args[0]);
      end;
      'BASENAME': begin
        if Length(Args) <> 1 then
          Exit(false);
        Value:= ChangeFileExt(args[0], '');
      end;
      'REALPATH': begin
        if Length(Args) <> 1 then
          Exit(false);
        Value:= ExpandFileName(args[0]);
      end;
      // Shell Function
      'SHELL': begin
        if Length(Args) < 1 then
          Exit(false);
        Value:= '';
        SetGlobal('_SHELLSTATUS', IntToStr(ExecuteCommand(AnsiString.Join(VAR_PARAM_DELIM, args), Value, false)));
        Value:= Value.Replace(#13,'').Replace(#10,' ').TrimRight;
      end;
    else
      Exit(False);
    end;
  end;

  function InterpretVariable(Name: String; out Value: String): boolean;
  var
    fc,args: TStringArray;
    i: Integer;
  begin
    Result:= false;
    // Variables may be (in order:)
    // 1. simple variables ${FOO}, return '' if undefined
    // 2. function calls ${FUNC ARGV}, return unparsed if FUNC is undefined
    if ValidGlobalName(Name) then begin
      Result:= true;
      if not TryGetGlobal(Name, Value) then
        Value:= '';
    end else begin
      fc:= Name.Split(VAR_FUNC_DELIM);
      if Length(FC) = 0 then
        Exit;
      Result:= true;
      if Length(fc) > 1 then begin
        args:= AnsiString.Join(VAR_FUNC_DELIM, FC, 1, Maxint).Split(VAR_PARAM_DELIM);
        for i:= 0 to high(args) do
          args[i]:= SubstituteVariables(Trim(args[i]));
      end
      else
        SetLength(args, 0);
      Result:= InterpretFunction(fc[0], args, Value);
    end;
  end;
var
  ps, pe, tok: integer;
  vn, vv: string;
  nesting: integer;
begin
  Result:= '';
  nesting:= 0;
  ps:= Pos(VAR_START, Line);
  while ps > 0 do begin
    Result += Copy(Line, 1, ps-1);
    Delete(Line, 1, ps-1);
    inc(nesting);

    pe:= Length(VAR_START) + 1;
    repeat
      pe:= Line.IndexOfAny([VAR_START, VAR_END], pe-1, MaxInt, tok) + 1;
      if pe = 0 then begin
        // no end, cannot have any more variables, skip to collection at end
        break;
      end;
      // identify token
      case tok of
        0: begin
          inc(nesting);
          inc(pe, length(VAR_START));
        end;
        1: begin
          dec(nesting);
          inc(pe, length(VAR_END));
        end;
      end;
    until (nesting = 0) or (pe > length(Line));

    if (pe = 0) then
      break;

    if nesting = 0 then begin
      vn:= Copy(Line, Length(VAR_START) + 1, pe - Length(VAR_END) - Length(VAR_START) - 1);
      if InterpretVariable(vn, vv) then begin
        Result += vv;
        Delete(Line, 1, pe-1);
      end else begin
        Result += VAR_START;
        Delete(Line, 1, Length(VAR_START));
      end;
    end;

    ps:= Pos(VAR_START, Line);
  end;
  Result:= Result + Line;
end;

procedure TBuildFile.SetSuppressed(Tasks: string);
begin
  fSuppressed.DelimitedText:= Tasks;
end;

{ TBuildTool }

constructor TBuildTool.Create(aOwner: TBuildFile);
begin
  inherited Create;
  fOwner:= aOwner;
end;

end.


