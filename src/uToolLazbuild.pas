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
    opts += ' --build-mode='+QuotedStr(aOrder.Values['MODE']);
  case UpperCase(aOrder.Values['BUILD']) of
    '',
    'DEFAULT': ;
    'CLEAN': opts += ' -B';
    'ALL': opts += ' -B -r';
    else begin
      WriteLn(ErrOutput, 'Task ', Owner.CurrentTask, ': Invalid Build type: ',aOrder.Values['BUILD']);
      Exit(ERROR_TASK_PARAMETER);
    end;
  end;
  useropts := aOrder.Values['OPTS'];
  Result := ExecuteCommand(format('"%s" %s %s "%s"', [Owner.GetGlobal('PROGRAM_LAZBUILD'), opts, useropts, proj]), True);
end;

end.

