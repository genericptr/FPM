{
    Copyright (c) 2020 by Ryan Joseph

    Program file for the FPM command line utility

    The file is part of the FPM package (https://github.com/genericptr/fpm)

    Command line options:

      -target             Override target name.
      -configuration      Override configuration name.
      -run                Run the executable after building.
      -exec               Run the executable without building (used for make files).
      -bw                 Disable color output (black and white mode).
}
{$mode objfpc}
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
  help: TStringArray;
begin
  
  // show help
  if GetCommandLineArgument('help') or GetCommandLineArgument('h') then
    begin
      help := ['FPM supports the following options:',
               '',
               '  -target             Override target name.',
               '  -configuration      Override configuration name.',
               '  -run                Run the executable after building.',
               '  -exec               Run the executable without building (used for make files).',
               '  -bw                 Disable color output (black and white mode).'];

      writeln(help.Join(#10));
      halt(0);
    end;

  // set configuration overrides
  if GetCommandLineArgument('target', flag) then
    GlobalSettings.target := flag;

  if GetCommandLineArgument('configuration', flag) then
    GlobalSettings.configuration := flag;

  if GetCommandLineArgument('run') then
    GlobalSettings.run := true;

  // black and white color mode
  if GetCommandLineArgument('bw') then
    FPMOptions += [TFPMOption.NoColor];

  try
    config := TFPMConfig.Create(GetInput);

    // return the executable path (used for makefiles)
    if GetCommandLineArgument('exec') then
      begin
        writeln(config.Executable);
        Halt;
      end;

    exitCode := config.Execute(config.GetCommandLine);
    Halt(exitCode);
  except
    on E: Exception do
      begin
        PrintColor(ANSI_BACK_RED, 'Error: '+E.Message);
        Halt(1);
      end;
  end;
end.