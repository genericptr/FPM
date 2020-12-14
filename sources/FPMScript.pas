{$mode objfpc}
{$H+}
{$scopedenums on}

unit FPMScript;
interface
uses
  TOML, FGL, Classes, SysUtils,
  FPMUtils;

type
  
  TScriptKind = (Build, // execute script before build
                 Run    // execute script after build
                 );

  { TFPMScript }

  TFPMScript = class
    private
      command: string;
      args: TStringArray;
      kind: TScriptKind;
    public

      { Constructors }
      constructor Create(table: TTOMLTable); overload;
      constructor Create(_command: string); overload;

      { Methods }
      function Execute: integer; virtual;
  end;
  TFPMScriptList = specialize TFPGList<TFPMScript>;

implementation

constructor TFPMScript.Create(table: TTOMLTable);
begin
  command := string(table['command']);
  if table.Contains('args', TTOMLArray) then
    args := TTOMLArray(table['args']).AsArray;
end;

constructor TFPMScript.Create(_command: string);
begin
  {$ifdef darwin}
  args := ['-c', _command];
  command := '/bin/sh';
  {$else}
  command := _command;
  // TODO: on windows we need to parse the command/args
  //command := StrScan(pchar(_command), ' ');
  //writeln('got ', command);
  {$endif}
end;

function TFPMScript.Execute: integer;
begin  
  command := ReplaceVariables(command);
  ReplaceVariables(args);
  PrintColor(ANSI_FORE_MAGENTA, '► '+command+' '+args.Join(' '));
  result := RunCommand(command, args);
end;

end.