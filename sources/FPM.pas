{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

program FPM;
uses
	FPMConfig, FPMUtils,
	TOML, 
	SysUtils, Classes, DateUtils;

var
  config: TFPMConfig;
  exitCode: integer;
  flag: string;
begin

	// set configuration overrides
	if GetCommandLineArgument('target', flag) then
		GlobalSettings.target := flag;

	if GetCommandLineArgument('configuration', flag) then
		GlobalSettings.configuration := flag;

	if GetCommandLineArgument('run') then
		GlobalSettings.run := true;

	try
	  config := TFPMConfig.Create(GetInput);
	  exitCode := config.Execute(config.GetCommandLine);
		halt(exitCode);
	except
	  on E: Exception do
	    PrintColor(ANSI_BACK_RED, 'Error: '+E.Message);
	end;
end.