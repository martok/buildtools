unit uToolCmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, uBuildFile, processutils, sysutils;

type
  TBuildToolCmd = class(TBuildTool)
    function Execute(const aOrder: TStringList): Integer; override;
  end;

implementation

{ TBuildToolCmd }

function TBuildToolCmd.Execute(const aOrder: TStringList): Integer;
var
  tfn: string;
begin
  tfn := Owner.GetTempName + '.cmd';
  aOrder.Delete(0);
  aOrder.Insert(0, '@ECHO OFF');
  aOrder.SaveToFile(tfn);
  try
    Result := ExecuteCommand(tfn, True);
  finally
    DeleteFile(tfn);
  end;
end;

end.

