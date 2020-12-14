{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

{
	TODO:

	- Parse command line options and have -r flag to run the executable
	- Use resource strings (https://freepascal.org/docs-html/ref/refse11.html) for error messages
	- Give warnings for undefined keys so users can't clutter the config


	Library targets:

		https://wiki.freepascal.org/macOS_Static_Libraries
		libtool -static -o libtest.a test.o
	
		dynamic?
		https://wiki.lazarus.freepascal.org/macOS_Dynamic_Libraries

		framework?

	Better variables:
	
		ARCH = FPM_HOST_SYSTEM_PROCESSOR
		LATEST = FPM_COMPILER_LATEST
	
		FPM_HOST_SYSTEM_NAME = uname

		PMAKE_HOST_SYSTEM_PROCESSOR
		PMAKE_HOST_SYSTEM_NAME

}

program FPM;
uses
	FPMConfig, FPMUtils, FPMResources,
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