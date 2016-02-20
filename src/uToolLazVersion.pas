unit uToolLazVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes, uBuildFile, sysutils;

type
  TBuildToolLazVersion = class(TBuildTool)
    function Execute(const aOrder: TStringList): Integer; override;
  end;

implementation

uses
  Laz2_XMLCfg, fileinfo, strutils, FileUtil;

function TryStrToVersionQuadOpt(S: String; out Quad: TVersionQuad): Boolean;
var
  i, k: integer;
  p: string;
begin
  Result:= false;
  FillChar({%H-}Quad[1], sizeof(TVersionQuad), 0);
  for i:= 1 to 4 do begin
    p:= Copy2SymbDel(S, '.');
    if TryStrToInt(p, k) then
      Quad[i]:= k
    else
      Exit;
    if S = '' then
      break;
  end;
  Result:= (S = '');
end;


Operator + (a, b : TVersionQuad) : TVersionQuad;
begin
  Result[1]:= a[1] + b[1];
  Result[2]:= a[2] + b[2];
  Result[3]:= a[3] + b[3];
  Result[4]:= a[4] + b[4];
end;

function BackupFile(const fn: TFilename): boolean;
begin
  Result:= CopyFile(fn, fn + '.bak', [cffOverwriteFile, cffPreserveTime], true);
end;

{ TBuildToolLazVersion }

const
  VPATH = 'ProjectOptions/VersionInfo/';

function TBuildToolLazVersion.Execute(const aOrder: TStringList): Integer;

  function CopyVersionInfo(From: TXMLConfig; Target: TFilename): boolean;
  var
    trg: TXMLConfig;
  begin
    Result:= false;

    trg:= TXMLConfig.Create(Target);
    try
      trg.SetDeleteValue(VPATH+'UseVersionInfo/Value', From.GetValue(VPATH+'UseVersionInfo/Value', False), False);

      trg.SetDeleteValue(VPATH+'MajorVersionNr/Value', From.GetValue(VPATH+'MajorVersionNr/Value', 0), 0);
      trg.SetDeleteValue(VPATH+'MinorVersionNr/Value', From.GetValue(VPATH+'MinorVersionNr/Value', 0), 0);
      trg.SetDeleteValue(VPATH+'RevisionNr/Value', From.GetValue(VPATH+'RevisionNr/Value', 0), 0);
      trg.SetDeleteValue(VPATH+'BuildNr/Value', From.GetValue(VPATH+'BuildNr/Value', 0), 0);

      trg.SetDeleteValue(VPATH+'Attributes/pvaDebug', From.GetValue(VPATH+'Attributes/pvaDebug', false), false);
      trg.SetDeleteValue(VPATH+'Attributes/pvaPreRelease', From.GetValue(VPATH+'Attributes/pvaPreRelease', false), false);
      trg.SetDeleteValue(VPATH+'Attributes/pvaPatched', From.GetValue(VPATH+'Attributes/pvaPatched', false), false);
      trg.SetDeleteValue(VPATH+'Attributes/pvaPrivateBuild', From.GetValue(VPATH+'Attributes/pvaPrivateBuild', false), false);
      trg.SetDeleteValue(VPATH+'Attributes/pvaSpecialBuild', From.GetValue(VPATH+'Attributes/pvaSpecialBuild', false), false);

      BackupFile(trg.Filename);
      trg.Flush;
      WriteLn('Copied version info to LPI file ', Target);
    finally
      FreeAndNil(trg);
    end;
  end;

var
  proj, f, cf: string;
  lpi: TXMLConfig;
  currentver, vi: TVersionQuad;
begin
  Result := 0;
  proj := ExpandFileName(aOrder.Values['PROJECT']);
  if proj = '' then begin
    WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Project specifier missing.');
    Exit(ERROR_TASK_PARAMETER);
  end;

  lpi:= TXMLConfig.Create(proj);
  try
    WriteLn('Loaded LPI file ', proj);

    if lpi.GetValue(VPATH+'UseVersionInfo/Value', '') = '' then begin
      WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Project file does not contain version information (or it is disabled).');
      Exit(ERROR_TASK_PROCESS);
    end;

    currentver[1]:= lpi.GetValue(VPATH+'MajorVersionNr/Value', 0);
    currentver[2]:= lpi.GetValue(VPATH+'MinorVersionNr/Value', 0);
    currentver[3]:= lpi.GetValue(VPATH+'RevisionNr/Value', 0);
    currentver[4]:= lpi.GetValue(VPATH+'BuildNr/Value', 0);

    if aOrder.Values['INCREMENT'] > '' then begin
      if not TryStrToVersionQuadOpt(aOrder.Values['INCREMENT'], vi) then begin
        WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Increment definition is not of the form "u.v.w.x".');
        Exit(ERROR_TASK_PARAMETER);
      end;
      currentver:= currentver + vi;
      lpi.SetDeleteValue(VPATH+'MajorVersionNr/Value', currentver[1], 0);
      lpi.SetDeleteValue(VPATH+'MinorVersionNr/Value', currentver[2], 0);
      lpi.SetDeleteValue(VPATH+'RevisionNr/Value', currentver[3], 0);
      lpi.SetDeleteValue(VPATH+'BuildNr/Value', currentver[4], 0);
    end;

    WriteLn('Version number ', VersionQuadToStr(currentver));

    f:= aOrder.Values['ADDFLAG'];
    while f > '' do begin
      f:= TrimSet(f, [' ', ',']);
      cf:= Copy2SymbDel(f, ',');
      lpi.SetDeleteValue(VPATH+'Attributes/'+cf, True, False);
    end;

    f:= aOrder.Values['REMOVEFLAG'];
    while f > '' do begin
      f:= TrimSet(f, [' ', ',']);
      cf:= Copy2SymbDel(f, ',');
      lpi.SetDeleteValue(VPATH+'Attributes/'+cf, False, False);
    end;

    f:= aOrder.Values['COPYTO'];
    while f > '' do begin
      f:= TrimSet(f, [' ', ',']);
      cf:= Copy2SymbDel(f, ',');
      CopyVersionInfo(lpi, cf);
    end;

    BackupFile(lpi.Filename);
    lpi.Flush;
  finally
    FreeAndNil(lpi);
  end;
end;

end.

