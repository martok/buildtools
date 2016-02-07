unit uToolEnv;

{$mode objfpc}{$H+}

interface

uses
  Classes, uBuildFile, sysutils, FileUtil, LazFileUtils;

type
  TBuildToolEnv = class(TBuildTool)
    function Execute(const aOrder: TStringList): Integer; override;
  end;

implementation

{ TBuildToolEnv }

function TBuildToolEnv.Execute(const aOrder: TStringList): Integer;
var
  i: integer;
begin
  Result := 0;
  for i:= 1 to aOrder.Count - 1 do begin
    if not Owner.SetGlobal(aOrder.Names[i], aOrder.ValueFromIndex[i]) then begin
      WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Cannot set env var ', aOrder.Names[i], '.');
      Exit(ERROR_TASK_PROCESS);
    end;
  end;
end;

end.

