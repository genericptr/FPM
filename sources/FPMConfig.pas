{
    Copyright (c) 2020 by Ryan Joseph

    This unit implements FPM config class

    The file is part of the FPM package (https://github.com/genericptr/fpm)
}
{$mode objfpc}
{$scopedenums on}
{$modeswitch arrayoperators}
{$H+}

unit FPMConfig;
interface
uses
  SysUtils, Classes, RegExpr,
  FPMUtils, FPMTable, FPMTarget,
  TOML;

const
  kConfigFileName = 'fpconfig.toml';

type
  TConfigOption = (DryRrun, RunProgram, ANSIColors);
  TConfigOptions = set of TConfigOption;

  TGlobalSettings = record
    options: TConfigOptions;
    target: string;
    configuration: string;
    run: boolean;
  end;

var
  GlobalSettings: TGlobalSettings;

type
  TFPMConfig = class
    private
      document: TTOMLDocument;
      settings: TFPMTable;
      target: TFPMTarget;
      configuration: TFPMTable;
      output: string;
      m_executable: string;

      function GetExecutable: string;
      function GetTarget: string;
      function GetConfiguration: string;
      function GetCommandLineForTable(table: TFPMTable): TStringArray;
      function GetPathFlags(table: TFPMTable): TStringArray;

      procedure Load;
      procedure PrintHeader;
    public
      constructor Create(path: string);
      destructor Destroy; override;

      { Methods }
      function Execute(commandLine: TStringArray): integer;
      function GetCommandLine: TStringArray;
      function GetProgramFile: string;
      function GetCodeToolOptions: TStringArray;

      { Properties }
      property Executable: string read GetExecutable write m_executable;
  end;

implementation

{$include target.inc}

function TFPMConfig.GetPathFlags(table: TFPMTable): TStringArray;
begin
  result := [];
  result.AddValues(ArrayToFlags('-Fu', table['unitPaths'] as TTOMLArray));
  result.AddValues(ArrayToFlags('-Fi', table['includePaths'] as TTOMLArray));
  result.AddValues(ArrayToFlags('-Fl', table['libraryPaths'] as TTOMLArray));
  result.AddValues(ArrayToFlags('-Ff', table['frameworkPaths'] as TTOMLArray));
end;

function TFPMConfig.GetCommandLineForTable(table: TFPMTable): TStringArray;
begin
  result := [];
  
  if table = nil then
    exit;

  // paths
  result.AddValues(GetPathFlags(table));

  // general options
  result.AddValues(table.MergedArray('options'));

  // symbols
  result.AddValues(ArrayToFlags('-d', table.MergedArray('symbols'), false));

  // linker flags
  result.AddValues(ArrayToFlags('-k', table.MergedArray('linkerFlags'), false));
end;

function TFPMConfig.GetProgramFile: string;
begin
  result := ExpandPath(target.table['program']);
end;

{ Flags which can be passed to Lazarus CodeTools (for Pascal Language Server) }
function TFPMConfig.GetCodeToolOptions: TStringArray;
begin
  result := [];

  result.AddValues(GetPathFlags(target.table));
  result.AddValues(GetPathFlags(configuration));

  result.AddValues(ArrayToFlags('-d', target.table.MergedArray('symbols'), false));
  result.AddValues(ArrayToFlags('-d', configuration.MergedArray('symbols'), false));
end;

function TFPMConfig.GetCommandLine: TStringArray;
var
  path: string;
begin

  // prepare the current target
  target.Prepare;

  result := [];

  result.AddValues(GetCommandLineForTable(target.table));
  result.AddValues(GetCommandLineForTable(configuration));

  output := ExpandPath(target.table['output']);
  if output <> '' then
    result += ['-FU'+output];

  path := ExpandPath(target.table['executable']);
  if path <> '' then
    result += ['-o'+path];
end;

procedure TFPMConfig.PrintHeader; 
begin
  if settings.Contains('target') then
    PrintColor(ANSI_FORE_GREEN, 'Target: '+GetTarget);
  if settings.Contains('configuration') then
    PrintColor(ANSI_FORE_GREEN, 'Configuration: '+GetConfiguration);
  if (target <> nil) and (target.Product <> '') then
    PrintColor(ANSI_FORE_GREEN, 'Product: '+target.Product);
end;

function TFPMConfig.Execute(commandLine: TStringArray): integer;
var
  path: string;
  table: TFPMTable;
  compiler,
  &program: string;
  units: TStringArray;
begin
  // default compiler paths
  {$if defined(DARWIN)}
  compiler := '/usr/local/bin/fpc';
  {$elseif defined(WINDOWS)}
  // TODO: default path?
  compiler := 'fpc';
  {$elseif defined(LINUX)}
  // TODO: default path?
  compiler := 'fpc';
  {$endif}

  // get table for the active target
  table := target.table;

  if table.Contains('compiler') then
    compiler := ExpandPath(table['compiler']);

  &program := ExpandPath(table['program']);
  output := ExpandPath(table['output']);

  // units
  units := table.MergedArray('units');

  // create output directories
  if output <> '' then
    begin
      if not DirectoryExists(output) then
        ForceDirectories(output);
      FPMAssert(DirectoryExists(output), 'Output directory '+output+' doesn''t exist');
      AddVariable('output', output);
    end;


  // add main program to units
  if Length(units) = 0 then
    begin
      FPMAssert(FileExists(&program), 'Program file "'+&program+'" doesn''t exist');
      units := [&program];
    end;

  // print the header
  PrintHeader;

  // execute command for each unit
  for path in units do
    begin
      FPMAssert(FileExists(path), 'File "'+path+'" doesn''t exist');
      PrintColor(ANSI_FORE_CYAN, compiler+' '+commandLine.Join(' ')+' '+path);
      result := RunCommand(compiler, commandLine + [path]);
      if result <> 0 then
        exit;
    end;

  target.Finalize;

  // run the executable
  if GlobalSettings.run and FileExists(executable) then
    ExecuteProcess(executable, '', []);
end;

function TFPMConfig.GetExecutable: string;
var
  path, prog: string;
begin

  if target.table.Contains('executable') then
    begin
      path := ExpandPath(target.table['executable']);
      if FileExists(path) then
        exit(path);
    end;

  // if no executable was specified assume from the program name
  prog := ExpandPath(target.table['program']);
  path := prog.DirectoryPath.AddComponent(prog.FileNameOnly);
  
  result := path;
end;

function TFPMConfig.GetTarget: string;
begin
  if GlobalSettings.target <> '' then
    result := GlobalSettings.target
  else if settings.Contains('target') then
    result := ExpandValue(settings['target'])
  else
    result := '';
end;

function TFPMConfig.GetConfiguration: string;
begin
  if GlobalSettings.configuration <> '' then
    result := GlobalSettings.configuration
  else if settings.Contains('configuration') then
    result := ExpandValue(settings['configuration'])
  else
    result := '';
end;

procedure TFPMConfig.Load;
var
  name, value: string;
  i: integer;
begin
  settings := TFPMTable.Create(document['settings'] as TTOMLTable);

  // create the default target
  // we need this so that the target can inherit from the
  // [settings] table, which is like the "implicit" target
  // if no target was defined

  if settings.Contains('parent') then
    target := TFPMTarget.Find(string(settings['parent'])).Create('$default', settings.data, nil)
  else
    target := TFPMTarget.Create('$default', settings.data, nil);

  // load global variables
  GlobalVariables := document['variables'] as TTOMLTable;

  if GlobalVariables = nil then
    GlobalVariables := TTOMLTable.Create;

  // define built-in global variables
  with GlobalVariables do
    begin
      {$if defined(DARWIN)}
      Add('platform', 'darwin');
      {$elseif defined(WINDOWS)}
      Add('platform', 'windows');
      {$elseif defined(LINUX)}
      Add('platform', 'linux');
      {$endif}

      {$ifdef darwin}
      // TODO: this needs to be the target kind which is only macos for now
      Add('sdk', GetSDK(TPlatform.MacOSX));

      {$if defined(CPUX86_64)}
      Add('arch', 'ppcx64');
      {$elseif defined(CPUI386)}
      Add('arch', 'ppc386');
      {$endif}

      {$endif}
      Add('latest', GetLatestCompiler);
    end;

  // replace variables recursively
  for i := 0 to GlobalVariables.Count - 1 do
    begin
      value := ReplaceVariables(String(GlobalVariables.Values[i]));
      GlobalVariables[GlobalVariables.Keys[i]] := Variant(value);
    end;

  // get active target
  name := GetTarget;
  if name <> '' then
    begin
      FPMAssert(document.Contains('target'), 'Target table doesn''t exist.');
      FPMAssert(TTOMLTable(document['target']).Contains(name), 'Target "'+name+'" table doesn''t exist in targets.');
      AddVariable('target', name);
      target := TFPMTarget.InheritedFrom(name, document['target'][name] as TTOMLTable, target);
    end;

  // get active configuration
  name := GetConfiguration;
  if name <> '' then
    begin
      AddVariable('configuration', name);
      configuration := TFPMTable.Create(document['configuration'][name] as TTOMLTable);
    end;
end;

destructor TFPMConfig.Destroy; 
begin
  FreeAndNil(document);
  inherited;
end;

constructor TFPMConfig.Create(path: string);
begin

  // if the path is a directory search for config file
  if DirectoryExists(path) then
    path := path+DirectorySeparator+kConfigFileName;

  if (ExtractFileName(path) <> kConfigFileName) or not FileExists(path) then
    FPMAssert('Config file "'+path+'" is invalid.');

  try
    document := GetTOML(GetFileAsString(path));
  except
    on E: Exception do
      FPMAssert('Failed to parse TOML config file ('+E.ClassName+' '+E.Message+')');
  end;
  
  Load;
end;


end.