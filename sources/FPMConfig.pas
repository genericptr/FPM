{
    Copyright (c) 2020 by Ryan Joseph

    This unit implements FPM config class

    The file is part of the FPM package (https://github.com/genericptr/fpm)
}
{$mode objfpc}
{$scopedenums on}
{$modeswitch arrayoperators}
{$interfaces corba}
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
  TConfigOption = (
    DryRrun, 
    RunProgram, 
    ANSIColors
  );
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
  TFPMConfig = class(IFPMConfig)
    private
      document: TTOMLDocument;
      settings: TFPMTable;
      m_target: TFPMTarget;
      defaultTarget: TFPMTarget;
      configuration: TFPMTable;
      targets: TFPMTargetMap;

      function GetTarget: string;
      function GetConfiguration: string;
      function GetActiveConfiguration: TFPMTable;

      function FindTarget(name: String): TFPMTarget;
      procedure Load;
    public
      { Constructors }
      constructor Create(path: string);
      destructor Destroy; override;

      { Methods }
      function Execute(target: TFPMTarget = nil): integer;
      function GetCodeToolOptions: TStringArray;

      { Properties }
      property Target: TFPMTarget read m_target;
  end;

implementation

{$include target.inc}

{ Flags which can be passed to Lazarus CodeTools (for Pascal Language Server) }
function TFPMConfig.GetCodeToolOptions: TStringArray;
begin
  result := [];

  result.AddValues(GetPathFlags(target.table));
  result.AddValues(GetPathFlags(configuration));

  result.AddValues(ArrayToFlags('-d', target.table.MergedArray('symbols'), false));
  result.AddValues(ArrayToFlags('-d', configuration.MergedArray('symbols'), false));
end;

function TFPMConfig.Execute(target: TFPMTarget): integer;
  
  procedure PrintHeader(target: TFPMTarget); 
  begin
    if settings.Contains('target') then
      PrintColor(ANSI_FORE_GREEN, 'Target: '+GetTarget);
    if settings.Contains('configuration') then
      PrintColor(ANSI_FORE_GREEN, 'Configuration: '+GetConfiguration);
    if (target <> nil) and (target.Product <> '') then
      PrintColor(ANSI_FORE_GREEN, 'Product: '+target.Product);
  end;

begin
  // use default timer
  if target = nil then
    target := self.target;

  PrintHeader(target);

  result := target.Start(self);

  // run the executable
  if GlobalSettings.run and FileExists(target.executable) then
    ExecuteProcess(target.executable, '');
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

function TFPMConfig.GetActiveConfiguration: TFPMTable;
begin
  result := configuration;
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

function TFPMConfig.FindTarget(name: String): TFPMTarget;
begin
  if name = '' then
    exit(defaultTarget);

  if targets.TryGetData(name, result) then
    exit;

  FPMAssert(document.Contains('target'), 'Target table doesn''t exist.');
  FPMAssert(TTOMLTable(document['target']).Contains(name), 'Target "'+name+'" table doesn''t exist in targets.');
  AddVariable('target', name);

  result := TFPMTarget.InheritedFrom(name, document['target'][name] as TTOMLTable, defaultTarget);

  targets.Add(name, result);
end;

procedure TFPMConfig.Load;
var
  name, value: string;
  i: integer;
begin
  settings := TFPMTable.Create(document['settings'] as TTOMLTable);

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

      // compiler install path
      {$if defined(DARWIN)}
      Add('fpc_root', '/usr/local/bin');
      {$elseif defined(WINDOWS)}
      Add('fpc_root', 'C:\FPC');
      {$elseif defined(LINUX)}
      Add('fpc_root', '/usr/local/bin');
      {$endif}

      // Compiler defines during compilation
      // https://www.freepascal.org/docs-html/prog/progap7.html

      {$if defined(CPUX86_64)}
      Add('ppc', 'ppcx64');
      Add('arch', 'x86_64');
      {$elseif defined(CPUI386)}
      Add('ppc', 'ppc386');
      Add('arch', 'i386');
      {$elseif defined(CPUAARCH64)}
      Add('ppc', 'ppca64');
      Add('arch', 'aarch64');
      {$else}
      Add('ppc', 'undefined');
      Add('arch', 'undefined');
      {$endif}

      {$if defined(DARWIN)}
      // TODO: this needs to be the target kind which is only macos for now
      Add('sdk', GetSDK(TPlatform.MacOSX));
      // TODO: once we have these targets working only include these if the target is used
      Add('ios-sdk', GetSDK(TPlatform.IPhoneOS));
      Add('iphonesim-sdk', GetSDK(TPlatform.IPhoneSimulator));
      {$endif}

      Add('latest', GetLatestCompiler);
    end;

  // replace variables recursively
  for i := 0 to GlobalVariables.Count - 1 do
    begin
      value := ReplaceVariables(String(GlobalVariables.Values[i]));
      GlobalVariables[GlobalVariables.Keys[i]] := Variant(value);
    end;

  // create the default target
  // we need this so that the target can inherit from the
  // [settings] table, which is like the "implicit" target
  // if no target was defined

  if settings.Contains('parent') then
    defaultTarget := TFPMTarget.Find(string(settings['parent']))
                               .Create('$default', settings.data, nil)
  else
    defaultTarget := TFPMTarget.Create('$default', settings.data, nil);

  // get active target
  m_target := FindTarget(GetTarget);

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
  targets := TFPMTargetMap.Create;

  // if the path is a directory search for config file
  if DirectoryExists(path) then
    path := path+DirectorySeparator+kConfigFileName;

  if (ExtractFileName(path) <> kConfigFileName) or not FileExists(path) then
    raise EFPMNotFound.Create('Config file "'+path+'" is invalid.');

  try
    document := GetTOML(GetFileAsString(path));
  except
    on E: Exception do
      FPMAssert('Failed to parse TOML config file ('+E.ClassName+' '+E.Message+')');
  end;
  
  Load;
end;

end.