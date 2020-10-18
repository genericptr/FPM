{$mode objfpc}
{$H+}
{$scopedenums on}

unit FPMScript;
interface
uses
  TOML, FGL, Classes;

type
  
  TScriptKind = (Build, Run);

  { TFPMScript }

  TFPMScript = class
    private
      command: string;
      args: string;
      kind: TScriptKind;
    public

      { Constructors }
      constructor Create(data: TTOMLTable); overload;
      constructor Create(_command: string); overload;

      { Methods }
      function Execute: integer; virtual;
  end;
  TFPMScriptList = specialize TFPGList<TFPMScript>;

implementation
uses
  SysUtils;

constructor TFPMScript.Create(data: TTOMLTable);
var
  value: TTOMLData;
begin
  command := string(data['command']);
  if data.Contains('args') then
    args := string(data['args']);
end;

constructor TFPMScript.Create(_command: string);
begin
  command := _command;
end;

function TFPMScript.Execute: integer;
begin
  result := ExecuteProcess(command, args, []);
end;

end.