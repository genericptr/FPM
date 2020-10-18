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
    private
      config: TTOMLDocument;
      settings: TTOMLTable;
      variables: TTOMLTable;
      executable: string;
      m_target: TFPMTarget;
      function GetTarget: TFPMTarget;
      function GetCommandLine: string;
      function GetCommandLineForTable(table: TTOMLTable): string;
      function GetCompiler: string;
      function ExpandValue(data: TTOMLData): string; inline;
      function ExpandPath(data: TTOMLData): string; inline;
      function ReplaceVariables(const s: string): string;
      function ArrayToFlags(flag: ShortString; data: TTOMLArray; expandPaths: boolean = true): string;
      procedure FatalError(message: string);
      procedure PrintStats;
    public
      constructor Create(path: string);
      destructor Destroy; override;
      function Execute: integer;
      property Target: TFPMTarget read GetTarget;
  end;

implementation

{
  Free Pascal target is an Intel 80x86 or compatible (16 and 32 bit).
  CPU87 Free Pascal target is an Intel 80x86 or compatible (16 and 32 bit).
  CPU386  Free Pascal target is an Intel 80386 or later.
  CPUI386 Free Pascal target is an Intel 80386 or later.
  CPU68K  Free Pascal target is a Motorola 680x0 or compatible.
  CPUM68K Free Pascal target is a Motorola 680x0 or compatible.
  CPUM68020 Free Pascal target is a Motorola 68020 or later.
  CPU68 Free Pascal target is a Motorola 680x0 or compatible.
  CPUSPARC32  Free Pascal target is a SPARC v7 or compatible.
  CPUSPARC  Free Pascal target is a SPARC v7 or compatible.
  CPUALPHA  Free Pascal target is an Alpha AXP or compatible.
  CPUPOWERPC  Free Pascal target is a 32-bit or 64-bit PowerPC or compatible.
  CPUPOWERPC32  Free Pascal target is a 32-bit PowerPC or compatible.
  CPUPOWERPC64  Free Pascal target is a 64-bit PowerPC or compatible.
  CPUX86_64 Free Pascal target is a AMD64 or Intel 64-bit processor.
  CPUAMD64  Free Pascal target is a AMD64 or Intel 64-bit processor.
  CPUX64  Free Pascal target is a AMD64 or Intel 64-bit processor
  CPUIA64 Free Pascal target is a Intel itanium 64-bit processor.
  CPUARM  Free Pascal target is an ARM 32-bit processor.
  CPUAVR  Free Pascal target is an AVR 16-bit processor.
  CPU16 Free Pascal target is a 16-bit CPU.
  CPU32 Free Pascal target is a 32-bit CPU.
  CPU64 Free Pascal target is a 64-bit CPU.
}

{$if defined(CPUPOWERPC32)}
  {$define TARGET_CPU_PPC}
  {$undef TARGET_CPU_PPC64}
  {$undef TARGET_CPU_X86}
  {$undef TARGET_CPU_X86_64}
  {$undef TARGET_CPU_ARM}
  {$define TARGET_OS_MAC}
  {$undef TARGET_OS_IPHONE}
  {$undef TARGET_IPHONE_SIMULATOR}
  {$undef TARGET_RT_64_BIT}
{$elseif defined(CPUPOWERPC64)}
  {$undef TARGET_CPU_PPC}
  {$define TARGET_CPU_PPC64}
  {$undef TARGET_CPU_X86}
  {$undef TARGET_CPU_X86_64}
  {$undef TARGET_CPU_ARM}
  {$define TARGET_OS_MAC}
  {$undef TARGET_OS_IPHONE}
  {$undef TARGET_IPHONE_SIMULATOR}
  {$define TARGET_RT_64_BIT}
{$elseif defined(CPUI386)}
  {$undef TARGET_CPU_PPC}
  {$undef TARGET_CPU_PPC64}
  {$define TARGET_CPU_X86}
  {$undef TARGET_CPU_X86_64}
  {$undef TARGET_CPU_ARM}
  {$undef TARGET_RT_64_BIT}
  {$if defined(IPHONESIM)}
    {$undef TARGET_OS_MAC}
    {$define TARGET_OS_IPHONE}
    {$define TARGET_IPHONE_SIMULATOR}
  {$else}
    {$define TARGET_OS_MAC}
    {$undef TARGET_OS_IPHONE}
    {$undef TARGET_IPHONE_SIMULATOR}
  {$endif}
{$elseif defined(CPUX86_64)}
  {$undef TARGET_CPU_PPC}
  {$undef TARGET_CPU_PPC64}
  {$undef TARGET_CPU_X86}
  {$define TARGET_CPU_X86_64}
  {$undef TARGET_CPU_ARM}
  {$define TARGET_OS_MAC}
  {$undef TARGET_OS_IPHONE}
  {$undef TARGET_IPHONE_SIMULATOR}
  {$define TARGET_RT_64_BIT}
{$elseif defined(CPUARM)}
  {$undef TARGET_CPU_PPC}
  {$undef TARGET_CPU_PPC64}
  {$undef TARGET_CPU_X86}
  {$undef TARGET_CPU_X86_64}
  {$define TARGET_CPU_ARM}
  {$undef TARGET_OS_MAC}
  {$define TARGET_OS_IPHONE}
  {$undef TARGET_IPHONE_SIMULATOR}
  {$undef TARGET_RT_64_BIT}
{$endif}

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

function TFPMConfig.ArrayToFlags(flag: ShortString; data: TTOMLArray; expandPaths: boolean = true): string;
var
  value: TTOMLData;
  path: string;
begin
  if data = nil then
    exit('');
  result := '';
  for value in data do
    begin
      if expandPaths then
        begin
          path := Trim(ExpandPath(value));
          if not DirectoryExists(path) then
            FatalError('Directory "'+path+'" for flag '+flag+' doesn''t exist');
          result += flag+path;
        end
      else
        result += flag+Trim(ExpandValue(value));
      result += ' ';
    end;
end;

function TFPMConfig.GetCommandLineForTable(table: TTOMLTable): string;
var
  path: string;
begin
  // TODO: how do we deal with duplicates now? we need flag struct I guess
  result := '';

  // paths
  result += ArrayToFlags('-Fu', table.Find('unitPaths') as TTOMLArray);
  result += ArrayToFlags('-Fi', table.Find('includePaths') as TTOMLArray);
  result += ArrayToFlags('-Fl', table.Find('libraryPaths') as TTOMLArray);
  result += ArrayToFlags('-Ff', table.Find('frameworkPaths') as TTOMLArray);

  // general options
  result += ArrayToFlags('', table.Find('options') as TTOMLArray, false);

  // output
  if table.Contains('output') then
    begin
      path := ExpandPath(table['output']);
      if not DirectoryExists(path) then
        ForceDirectories(path);
      if not DirectoryExists(path) then
        FatalError('Output directory '+path+' doesn''t exist');
      result += '-FU'+path+' ';
    end;

  // executable
  if table.Contains('executable') then
    begin
      executable := ExpandPath(table['executable']);
      result += '-o'+executable+' ';
    end;
end;

function TFPMConfig.GetCommandLine: string;
var
  table: TTOMLTable;
begin
  result := ExpandPath(settings['program']);
  result += ' ';

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

function TFPMConfig.GetCompiler: string;
begin
  if settings.Contains('compiler') then
    result := ExpandPath(settings['compiler'])
  else
    begin
      {$if defined(DARWIN)}
      result := '/usr/local/bin/fpc';
      {$elseif defined(WINDOWS)}
      // TODO: default path?
      result := 'fpc';
      {$elseif defined(LINUX)}
      // TODO: default path?
      result := 'fpc';
      {$endif}
    end;
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
  path, commandLine: string;
begin
  path := GetCompiler;
  commandLine := GetCommandLine;
  PrintStats;
  writeln(path, ' ', commandLine);
  if target <> nil then
    target.RunScripts;
  //halt;
  result := ExecuteProcess(path, commandLine, []);
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