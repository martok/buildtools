unit uToolLazbuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, uBuildFile, processutils, sysutils;

type
  TBuildToolLazbuild = class(TBuildTool)
    function Execute(const aOrder: TStringList): Integer; override;
  end;

implementation

{ TBuildToolLazbuild }

function TBuildToolLazbuild.Execute(const aOrder: TStringList): Integer;
var
  proj, opts, useropts: string;
begin
  Result := 0;
  proj := ExpandFileName(aOrder.Values['PROJECT']);
  if proj = '' then begin
    WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Project specifier missing.');
    Exit(ERROR_TASK_PARAMETER);
  end;
  opts := '';
  if aOrder.Values['MODE']<>'' then
    opts += ' --build-mode='+aOrder.Values['MODE'];
  useropts := aOrder.Values['OPTS'];
  Result := ExecuteCommand(format('"%s" %s %s "%s"', [Owner.GetGlobal('PROGRAM_LAZBUILD'), opts, useropts, proj]), True);
end;

end.

