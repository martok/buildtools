unit uGitCalls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, StrUtils;

function GitRootPath: string;
function GitBranch: string;
function GitHash: string;
function GitIsModified: boolean;
function GitSvnLatest: string;

implementation

var
  fGitRootPath: String;
  fGitBranch: string;
  fGitHash: string;
  fGitModified: integer = -1;
  fGitSvnLatest: string;

function RunGit(SubCmd: array of String; out output: string): boolean;
begin
  Result:= RunCommand('git', SubCmd, output);
  if Result then
    output:= Trim(output);
end;

function GitRootPath: string;
begin
  if fGitRootPath = '' then begin
    if RunGit(['rev-parse', '--show-cdup'], fGitRootPath) then begin
      fGitRootPath:= ExpandFileName(ConcatPaths([GetCurrentDir, fGitRootPath]));
    end;
  end;
  Result:= fGitRootPath;
end;

function GitBranch: string;
begin
  if fGitBranch = '' then begin
    if not RunGit(['describe','--all','HEAD'], fGitBranch) then
      fGitBranch:= '';
  end;
  Result:= fGitBranch;
end;

function GitHash: string;
begin
  if fGitHash = '' then begin
    if not RunGit(['rev-parse','HEAD'], fGitHash) then
      fGitHash:= '';
  end;
  Result:= fGitHash;
end;

function GitIsModified: boolean;
var
  cmdres: string;
  st: TStringList;
begin
  if fGitModified = -1 then begin
    if RunGit(['status','--short','-uno'], cmdres) then begin
      st:= TStringList.Create;
      try
        st.Text:= cmdres;
        fGitModified:= st.Count;
      finally
        FreeAndNil(st);
      end;
    end;
  end;
  Result:= fGitModified > 0;
end;


function GitSvnLatest: string;
const
  STARTMARK = 'git-svn-id: http';

  function ExtractSvnRev(A: String): string;
  var
    i: integer;
    spl: TStringArray;
  begin
    Result:= '';
    i:= LastDelimiter(#13#10, A);
    if i = 0 then Exit;

    Delete(A, 1, i);
    A:= Trim(A);
    i:= Pos(STARTMARK, A);
    if i <> 1 then Exit;

    spl:= A.Split([' '], TStringSplitOptions.None);
    if Length(spl) <> 3 then Exit;
    spl:= spl[1].Split('@', TStringSplitOptions.None);
    if Length(spl) <> 2 then Exit;
    Result:= spl[1];
  end;

var
  tmp: string;
begin
  if fGitSvnLatest = '' then begin
    if RunGit(['log','--grep='+STARTMARK,'-F','--topo-order','-n1'], tmp) then begin
      fGitSvnLatest:= ExtractSvnRev(tmp);
    end;
  end;
  Result:= fGitSvnLatest;
end;

end.

