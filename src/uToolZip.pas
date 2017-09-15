unit uToolZip;

{$mode objfpc}{$H+}

interface

uses
  Classes, uBuildFile, processutils, sysutils, FileUtil, LazFileUtils;

type
  TBuildToolZip = class(TBuildTool)
    function Execute(const aOrder: TStringList): Integer; override;
  end;

implementation

uses
  dynlibs;

type
  TpfnCreateHardlinkW = function (lpFileName, lpExistingFileName: Pwidechar;
  lpSecurityAttributes: Pointer): LongBool; stdcall;
var
  pfnCreateHardLinkW: TpfnCreateHardlinkW = nil;

const
  FileCopyFlags = [cffPreserveTime];

{ TCopyDirTree for CopyDirTree function }
type
TCopyDirTree = class(TFileSearcher)
private
  FSourceDir: string;
  FTargetDir: string;
  FFlags: TCopyFileFlags;
  FCopyFailedCount: integer;
  FCopyAsHardlinks: boolean;
protected
  procedure DoFileFound; override;
  procedure DoDirectoryFound; override;
end;

procedure TCopyDirTree.DoFileFound;
var
  NewLoc, NewF: string;
begin
  // ToDo: make sure StringReplace works in all situations !
  NewLoc := StringReplace(FileName, FSourceDir, FTargetDir, []);
  NewF := StringReplace(FileName, FSourceDir, '', []);
  WriteLn('  ',newf);
  if FCopyAsHardlinks then begin
    if pfnCreateHardLinkW(PWideChar(UnicodeString(NewLoc)), PWideChar(UnicodeString(FileName)), nil) then
      exit
    else
      FCopyAsHardlinks:= false;
  end;
  if not CopyFile(FileName, NewLoc, FFlags) then
    Inc(FCopyFailedCount);
end;

procedure TCopyDirTree.DoDirectoryFound;
var
  NewPath: string;
begin
  NewPath := StringReplace(FileName, FSourceDir, FTargetDir, []);
  // ToDo: make directories also respect cffPreserveTime flag.
  if not LazFileUtils.DirectoryExistsUTF8(NewPath) then
    if not LazFileUtils.ForceDirectoriesUTF8(NewPath) then
      Inc(FCopyFailedCount);
end;

function DeepCopy(const SourceFind, TargetDir: string; Flags: TCopyFileFlags = []): boolean;
var
  Searcher: TCopyDirTree;
  RelPath, SourceDir, SourceMask: string;
  B: boolean;
begin
  Result := False;
  Searcher := TCopyDirTree.Create;
  try
    SourceDir:= ExtractFilePath(SourceFind);
    SourceMask:= ExtractFileName(SourceFind);

    // Destination directories are always created. User setting has no effect!
    Flags := Flags + [cffCreateDestDirectory];
    Searcher.FFlags := Flags;
    Searcher.FCopyFailedCount := 0;
    Searcher.FSourceDir := LazFileUtils.TrimFilename(SetDirSeparators(SourceDir));
    Searcher.FTargetDir := LazFileUtils.TrimFilename(SetDirSeparators(TargetDir));
    Searcher.FCopyAsHardlinks:= Assigned(pfnCreateHardLinkW);

    // Don't even try to copy to a subdirectory of SourceDir.
    B := TryCreateRelativePath(LazFileUtils.ExpandFilenameUtf8(Searcher.FSourceDir),
      LazFileUtils.ExpandFilenameUtf8(Searcher.FTargetDir), False, True, RelPath);
    if B and ((Copy(RelPath, 1, 2) = '..') or (RelPath = '')) then
      Exit;

    Searcher.Search(SourceDir, SourceMask);
    Result := Searcher.FCopyFailedCount = 0;
  finally
    Searcher.Free;
  end;
end;

{ TBuildToolZip }

function TBuildToolZip.Execute(const aOrder: TStringList): Integer;
  function JoinExcludes(delim: string): string;
  var
    i: integer;
  begin
    Result:= '';
    for i:= 0 to aOrder.Count-1 do
      if AnsiSameText('EXCLUDE', aOrder.Names[i]) then begin
        if Result > '' then Result += ' ';
        Result+= delim+aOrder.ValueFromIndex[i];
      end;
  end;

var
  ofn, fls, td, tn, sn, tdn, zip: string;
  files: TStringList;
  i: integer;
  level: integer;
begin
  Result := 0;
  ofn := ExpandFileName(aOrder.Values['FILENAME']);
  if ofn = '' then begin
    WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': FileName specifier missing.');
    Exit(ERROR_TASK_PARAMETER);
  end;

  fls := aOrder.Values['FILES'];
  if fls = '' then begin
    WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Files specifier missing.');
    Exit(ERROR_TASK_PARAMETER);
  end;

  level:= StrToIntDef(aOrder.Values['LEVEL'], 5);
  if not (level in [0..9]) then begin
    WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Invalid compression level ',level,': not in 0..9.');
    Exit(ERROR_TASK_PARAMETER);
  end;

  files:= Owner.GetSection(fls);
  try
    Owner.ProcessVariables(files);
    td := Owner.GetTempName;
    ForceDirectories(td);
    try
      for i := 0 to files.Count - 1 do begin
        sn := files.Names[i];
        sn := ExpandFileName(sn);
        if Pos('*', ExtractFileName(sn)) > 0 then begin
          // copy with mask, target is always a directory
          tn := ConcatPaths([files.ValueFromIndex[i], '']);
          tdn := ConcatPaths([td, tn]);
          WriteLn(sn, ' -> ', tn);
          if not DeepCopy(sn, tdn, FileCopyFlags) then begin
            WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Copy failed in ', sn, '.');
            Exit(ERROR_TASK_PROCESS);
          end;
        end
        else
        begin
          // copy single file, check if we keep the name or change it
          if ExtractFileName(files.ValueFromIndex[i]) = '' then
            tn := ConcatPaths([files.ValueFromIndex[i], ExtractFileName(sn)])
          else
            tn := files.ValueFromIndex[i];
          tdn := ConcatPaths([td, tn]);
          WriteLn(sn, ' -> ', tn);
          if not CopyFile(sn, tdn, [cffCreateDestDirectory] + FileCopyFlags) then begin
            WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Cannot copy file ', sn, '.');
            Exit(ERROR_TASK_PROCESS);
          end;
        end;
      end;
      if FileExists(ofn) then
        DeleteFile(ofn);
      if Owner.TryGetGlobal('PROGRAM_7ZIP',zip) then
        Result := ExecuteCommandInDir(format('"%s" a "%s" %s * -r -mx=%d', [zip, ofn, JoinExcludes('-xr!'), level]), td, True)
      else
        Result := ExecuteCommandInDir(format('"%s" -o -%d -r -S "%s" %s *', [Owner.GetGlobal('PROGRAM_ZIP'), level, ofn, JoinExcludes('-x ')]), td, True);
    finally
      DeleteDirectory(td, False);
    end;
  finally
    FreeAndNil(files);
  end;
end;

initialization
{$IFDEF MSWINDOWS}
  Pointer(pfnCreateHardLinkW):= GetProcAddress(SafeLoadLibrary(KernelDLL), 'CreateHardLinkW');
{$ENDIF}
end.

