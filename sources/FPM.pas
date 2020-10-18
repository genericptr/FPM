{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

{
	TODO:

	- Parse command line options and have -r flag to run the executable
	- Use resource strings (https://freepascal.org/docs-html/ref/refse11.html) for error messages
	- Give warnings for undefined keys so users can't clutter the config
}

program FPM;
uses
	FPMConfig, FPMUtils,
	TOML, 
	SysUtils, Classes, DateUtils;

function GetInput: string;
var
	input: string;
begin
	input := ParamStr(1);
	input := ExpandFileName(input);
	
	if input = '' then
		begin
			writeln('FPM expects a valid directory or fpconfig.toml file.');
			halt;
		end;

	if not FileExists(input) and not DirectoryExists(input) then
		begin
			writeln('"'+input+'" does not exist. FPM expects a valid directory or fpconfig.toml file.');
			halt;
		end;

	result := input;
end;

var
  config: TFPMConfig;
  exitCode: integer;
begin
  config := TFPMConfig.Create(GetInput);
  exitCode := config.Execute;
  if exitCode = 0 then
  	begin
  		// todo: this must be an option to FPM -r
  		//ExecuteProcess(builder.executable, '', []);
  	end
  else
	  halt(exitCode);
end.