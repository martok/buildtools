unit uGitCalls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

function GitRootPath: string;
function GitBranch: string;
function GitHash: string;
function GitIsModified: boolean;

implementation

var
  fGitRootPath: String;
  fGitBranch: string;
  fGitHash: string;
  fGitModified: integer = -1;

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
end.

