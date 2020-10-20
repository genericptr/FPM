{$mode objfpc}
{$H+}

unit FPMConfig;
interface
uses
  FPMUtils, FPMTarget,
  TOML, 
  SysUtils, Classes, RegExpr;

type
  TFPMConfig = class
    private const
      kConfigFileName = 'fpconfig.toml';
    private type
      TOptions = record
        compiler: string;     // path to compiler
        main: string;         // path to program
        executable: string;   // -o
        output: string;       // -FU
      end;
    private
      config: TTOMLDocument;
      settings: TTOMLTable;
      variables: TTOMLTable;
      options: TOptions;
      m_target: TFPMTarget;
      function GetTarget: TFPMTarget;
      function GetCommandLine: string;
      function GetCommandLineForTable(table: TTOMLTable): string;
      function ExpandValue(data: TTOMLData): string; inline;
      function ExpandPath(data: TTOMLData): string; inline;
      function ReplaceVariables(const s: string): string;
      function ArrayToFlags(flag: ShortString; data: TTOMLArray): string;
      procedure FatalError(message: string);
      procedure PrintStats;
    public
      constructor Create(path: string);
      destructor Destroy; override;
      function Execute: integer;
      property Target: TFPMTarget read GetTarget;
  end;

implementation

{$include target.inc}

// TODO: we need this for packages also. maybe make a TFPMVariables class
function TFPMConfig.ReplaceVariables(const s: string): string;
var
  i: Integer;
begin
  result := s;
  if variables <> nil then
    for i := 0 to variables.Count - 1 do
      result := StringReplace(result, '${'+variables.Keys[I]+'}', variables[I].ToString, [rfReplaceAll, rfIgnoreCase]);
end;

function TFPMConfig.ExpandValue(data: TTOMLData): string;
begin
  result := data.ToString;
  result := ReplaceVariables(result);
end;

function TFPMConfig.ExpandPath(data: TTOMLData): string;
begin
  result := ExpandValue(data);
  result := ExpandFileName(result);
end;

function TFPMConfig.ArrayToFlags(flag: ShortString; data: TTOMLArray): string;
var
  value: TTOMLData;
  path: string;
begin
  if data = nil then
    exit('');
  result := '';
  for value in data do
    begin
      path := Trim(ExpandPath(value));
      if not DirectoryExists(path) then
        FatalError('Directory "'+path+'" for flag '+flag+' doesn''t exist');
      result += flag+path+' ';
    end;
end;

function TFPMConfig.GetCommandLineForTable(table: TTOMLTable): string;
var
  value: TTOMLData;
  list: TTOMLArray;
  path: string;
begin
  result := '';

  // paths
  result += ArrayToFlags('-Fu', table.Find('unitPaths') as TTOMLArray);
  result += ArrayToFlags('-Fi', table.Find('includePaths') as TTOMLArray);
  result += ArrayToFlags('-Fl', table.Find('libraryPaths') as TTOMLArray);
  result += ArrayToFlags('-Ff', table.Find('frameworkPaths') as TTOMLArray);

  // general options
  list := table.Find('options') as TTOMLArray;
  if list <> nil then
    for value in list do
      result += Trim(ExpandValue(value))+' ';

  // output
  if table.Contains('output') then
    options.output := ExpandPath(table['output']);

  // executable
  if table.Contains('executable') then
    options.executable := ExpandPath(table['executable']);

  // compiler
  if table.Contains('compiler') then
    options.compiler := ExpandPath(table['compiler']);

  // program
  if table.Contains('program') then
    options.main := ExpandPath(table['program']);
end;

function TFPMConfig.GetCommandLine: string;
var
  table: TTOMLTable;
begin
  result := '';

  // base
  result += GetCommandLineForTable(settings);

  // target
  if target <> nil then
    result += GetCommandLineForTable(target.data);

  // configuration
  if settings.Contains('configuration') then
    begin
      table := config['configuration'][string(settings['configuration'])] as TTOMLTable;
      result += GetCommandLineForTable(table);
    end;

  result += '-FU'+options.output+' ';
  result += '-o'+options.executable+' ';
  result += options.main+' ';
end;

function TFPMConfig.GetTarget: TFPMTarget;
var
  table: TTOMLTable;
  name: string;
begin
  if not settings.Contains('target') then
    exit(nil);
  if m_target = nil then
    begin
      name := string(settings['target']);
      table := config['targets'][name] as TTOMLTable;
      m_target := TFPMTarget.InheritedFrom(name, table)
    end;
  result := m_target;
end;

procedure TFPMConfig.PrintStats; 
begin
  // settings.GetAsString('target', 'none')
  if settings.Contains('target') then
    PrintColor(ANSI_FORE_GREEN, 'Target: '+string(settings['target']));
  if settings.Contains('configuration') then
    PrintColor(ANSI_FORE_GREEN, 'Configuration: '+string(settings['configuration']));
  //GetTarget
end;

function TFPMConfig.Execute: integer;
var
  commandLine: string;
begin

  // default compiler paths
  {$if defined(DARWIN)}
  options.compiler := '/usr/local/bin/fpc';
  {$elseif defined(WINDOWS)}
  // TODO: default path?
  options.compiler := 'fpc';
  {$elseif defined(LINUX)}
  // TODO: default path?
  options.compiler := 'fpc';
  {$endif}

  commandLine := GetCommandLine;
  PrintStats;
  writeln(options.compiler, ' ', commandLine);

  // create output directories
  if not DirectoryExists(options.output) then
    ForceDirectories(options.output);
  if not DirectoryExists(options.output) then
    FatalError('Output directory '+options.output+' doesn''t exist');

  if target <> nil then
    target.RunScripts;

  result := ExecuteProcess(options.compiler, commandLine, []);
end;

procedure TFPMConfig.FatalError(message: string);
begin
  writeln(message);
  halt(-1);
end;

destructor TFPMConfig.Destroy; 
begin
  FreeAndNil(config);
  inherited;
end;

constructor TFPMConfig.Create(path: string);
begin
  // if the path is a directory search for config file
  if DirectoryExists(path) then
    path := path+DirectorySeparator+kConfigFileName;

  if (ExtractFileName(path) <> kConfigFileName) or not FileExists(path) then
    FatalError('Config file "'+path+'" is invalid.');

  try
    config := GetTOML(GetFileAsString(path));
  except
    on E: Exception do
      FatalError('Failed to parse TOML config file ('+E.ClassName+' '+E.Message+')');
  end;

  //writeln(config.AsJSON.FormatJSON);

  settings := config['settings'] as TTOMLTable;
  variables := config['variables'] as TTOMLTable;

  if variables = nil then
    variables := TTOMLTable.Create;

  {$ifdef darwin}
  // TODO: this needs to be the target kind which is only macos for now
  variables.Add('sdk', GetSDK(TPlatform.MacOSX));

  {$if defined(CPUX86_64)}
  variables.Add('arch', 'ppcx64');
  {$elseif defined(CPUI386)}
  variables.Add('arch', 'ppc386');
  {$endif}

  {$endif}
  variables.Add('latest', GetLatestCompiler);
end;

end.