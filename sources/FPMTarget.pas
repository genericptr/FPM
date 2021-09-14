{
    Copyright (c) 2020 by Ryan Joseph

    This unit implements the target class

    The file is part of the FPM package (https://github.com/genericptr/fpm)
}
{$mode objfpc}
{$modeswitch typehelpers}
{$H+}

unit FPMTarget;
interface
uses
  SysUtils, FGL, Process,
  TOML, FPMTable, FPMScript, FPMUtils;

type
  
  { TFPMTarget }
  TFPMTargetClass = class of TFPMTarget;
  TFPMTarget = class
    private
      m_name: ShortString;
      m_table: TFPMTable;
      scripts: TFPMScriptList;
      parent: TFPMTarget;
      procedure RegisterScript(script: TFPMScript);
      procedure LoadScripts;
      function GetProduct: string; virtual;
      procedure RunScripts;
      procedure CopyResources;
    public

      { Constructors }
      class function Find(_name: string): TFPMTargetClass;
      class function InheritedFrom(_name: ShortString; _data: TTOMLTable; _parent: TFPMTarget): TFPMTarget;
      constructor Create(_name: ShortString; _data: TTOMLTable; _parent: TFPMTarget);
      destructor Destroy; override;

      { Methods }
      procedure Prepare; virtual;
      procedure Finalize; virtual;

      { Properties }
      property Table: TFPMTable read m_table;
      property Name: ShortString read m_name;
      property Product: string read GetProduct;
  end;
  TFPMTargetMap = specialize TFPGMap<ShortString, TFPMTargetClass>;

  { TFPMTargetConsole }

  TFPMTargetConsole = class(TFPMTarget)
    function GetProduct: string; override;
  end;

  { TFPMTargetDarwin }

  TFPMTargetDarwin = class(TFPMTarget)
    function GetProduct: string; override;
    procedure Prepare; override;
    procedure Finalize; override;
  end;

  { TFPMTargetLibrary }

  TFPMTargetLibrary = class(TFPMTarget)
    procedure Finalize; override;
    function GetProduct: string; override;
  end;

implementation
uses
  FPMResources;

var
  TargetClasses: TFPMTargetMap;

{ TFPMTargetLibrary }

function TFPMTargetLibrary.GetProduct: string;
begin
  result := ExpandPath(table['product']);
end;

procedure TFPMTargetLibrary.Finalize;
var
  args: TStringArray;
begin
  inherited;

  PrintColor(ANSI_FORE_MAGENTA, 'Build library: '+Product);

  args := ['libtool', '-static', '-o', Product, ReplaceVariables('${OUTPUT}/*.o')];
  RunCommand('/bin/sh', ['-c', args.Join(' ')]);
end;

{ TFPMTargetDarwin }

{
  @rpath
  https://wincent.com/wiki/@executable_path,_@load_path_and_@rpath
  https://www.mikeash.com/pyblog/friday-qa-2009-11-06-linking-and-install-names.html
}

function TFPMTargetDarwin.GetProduct: string;
begin
  result := ExpandPath(table['product']);
end;

procedure TFPMTargetDarwin.Prepare;
begin
  inherited;

  FPMAssert(table.Contains('product'), 'Target '+Name+' must contain "product" key.');

  // TODO: resources paths could force create directories also
  // ${PRODUCT}/Contents/MacOS/${PROJECT_NAME} could create the /Contents and /MacOS folders
  ForceDirectories(Product);
  ForceDirectories(Product.AddComponent('Contents'));
  ForceDirectories(Product.AddComponents(['Contents', 'MacOS']));
  ForceDirectories(Product.AddComponents(['Contents', 'Resources']));

  FPMAssert(Product.ExistsAtPath, 'product "'+Product+'" for target doesn''t exist');

  AddVariable('product', Product);
end;

procedure TFPMTargetDarwin.Finalize; 
var
  path: string;
  output: string;
  build: integer;
  oldAge: longint;
begin
  inherited;

  if table.Contains('infoPlist') then
    begin
      path := ExpandPath(table['infoPlist']);
      FPMAssert(path.ExistsAtPath, 'Info.plist "'+path+'" doesn''t exist');
      if Process.RunCommand('/usr/libexec/PlistBuddy', ['-c', 'Print :CFBundleVersion', path], output, []) then
        begin
          build := StrToInt(Trim(output)) + 1;
          PrintColor(ANSI_FORE_MAGENTA, 'Build Number: '+build.ToString);
          // update the bundle file date but keep the original date
          oldAge := FileAge(path);
          Process.RunCommand('/usr/libexec/PlistBuddy', ['-c', 'Set :CFBundleVersion '+IntToStr(build), path], output, []);
          FileSetDate(path, oldAge);
        end;
    end;
end;

{ TFPMTargetConsole }

function TFPMTargetConsole.GetProduct: string;
begin
  result := ExpandPath(table['executable']);
end;

{ TFPMTarget }

function TFPMTarget.GetProduct: string;
begin
  result := '';
end;

procedure TFPMTarget.RegisterScript(script: TFPMScript);
begin
  if scripts = nil then
    scripts := TFPMScriptList.Create;
  scripts.Add(script);
end;

procedure TFPMTarget.RunScripts;
var
  script: TFPMScript;
  exitCode: integer;
begin
  if scripts <> nil then
    for script in scripts do
      begin
        exitCode := script.Execute;
        FPMAssert(exitCode = 0, 'Failed to run script. Error: '+exitCode.ToString);
      end;
end;

procedure TFPMTarget.CopyResources;
var
  value: TTOMLData;
  dict: TTOMLTable;
  source: string;
  i: integer;
begin
  if table.Contains('resources') then
    for value in table['resources'] do
      begin
        dict := value as TTOMLTable;
        for i := 0 to dict.Count - 1 do
          begin
            source := ExpandPath(dict.Keys[i]);
            FPMAssert(source.ExistsAtPath, 'Resource path "'+source+'"" doesn''t exist');
            //PrintColor(ANSI_FORE_MAGENTA, 'Copy "'+source+'" to "'+ExpandPath(dict.Values[i])+'"');
            FPMResources.CopyResources(source, ExpandPath(dict.Values[i]));
          end;
      end;
end;

procedure TFPMTarget.LoadScripts;
var
  script: TTOMLData;
begin
  for script in table['scripts'] do
    if script is TTOMLTable then
      RegisterScript(TFPMScript.Create(TTOMLTable(script)))
    else
      RegisterScript(TFPMScript.Create(string(script)));
end;

class function TFPMTarget.Find(_name: string): TFPMTargetClass;
begin
  result := TargetClasses[_name];
end;

class function TFPMTarget.InheritedFrom(_name: ShortString; _data: TTOMLTable; _parent: TFPMTarget): TFPMTarget;
var
  targetClass: TFPMTargetClass;
  parentName: ShortString = '';
begin
  
  // get the taret parget
  targetClass := TFPMTarget;
  if _data.Contains('parent') then
    parentName := LowerCase(ShortString(_data['parent']));

  if parentName <> '' then
    begin
      FPMAssert(TargetClasses.IndexOf(parentName) <> -1, 'The parent target "'+parentName+'" for target "'+_name+'" does not exist.');
      result := TargetClasses[parentName].Create(_name, _data, _parent);
    end
  else
    result := TFPMTarget.Create(_name, _data, _parent);
end;

procedure TFPMTarget.Prepare;
begin
end;

procedure TFPMTarget.Finalize;
begin
  CopyResources;
  RunScripts;
end;

destructor TFPMTarget.Destroy;
begin
  FreeAndNil(scripts);
  FreeAndNil(m_table);
  inherited;
end;

constructor TFPMTarget.Create(_name: ShortString; _data: TTOMLTable; _parent: TFPMTarget);
begin
  m_name := _name;
  parent := _parent;
  if parent <> nil then
    m_table := TFPMTable.Create(_data, parent.table)
  else
    m_table := TFPMTable.Create(_data, nil);
  if table.Contains('scripts') then
    LoadScripts;
end;

begin
  TargetClasses := TFPMTargetMap.Create;
  TargetClasses.Add('console', TFPMTargetConsole);
  TargetClasses.Add('darwin', TFPMTargetDarwin);
  TargetClasses.Add('library', TFPMTargetLibrary);
end.