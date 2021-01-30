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

  config := TFPMConfig.Create(GetInput);
  exitCode := config.Execute(config.GetCommandLine);
	halt(exitCode);
end.