unit uToolPsh;

{$mode objfpc}{$H+}

interface

uses
  Classes, uBuildFile, processutils, sysutils;

type
  TBuildToolPowershell = class(TBuildTool)
    function Execute(const aOrder: TStringList): Integer; override;
  end;

implementation

{ TBuildToolPowershell }

function TBuildToolPowershell.Execute(const aOrder: TStringList): Integer;
var
  tfn: string;
begin
  tfn := Owner.GetTempName + '.ps1';
  aOrder.Delete(0);
  aOrder.SaveToFile(tfn);
  try
    Result := ExecuteCommand(format('powershell -NoProfile -ExecutionPolicy RemoteSigned -File "%s"', [tfn]), True);
  finally
    DeleteFile(tfn);
  end;
end;

end.

