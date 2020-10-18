{$mode objfpc}
{$H+}

unit FPMTarget;
interface
uses
  TOML, FPMScript, FPMUtils,
  SysUtils, FGL;

type

  { TFPMTarget }

  TFPMTarget = class
    private
      m_name: ShortString;
      m_data: TTOMLTable;
      scripts: TFPMScriptList;
      function GetProduct: string;
      procedure RegisterScript(script: TFPMScript);
      procedure LoadScripts;
    public

      { Constructors }
      class function InheritedFrom(_name: ShortString; _data: TTOMLTable): TFPMTarget;
      constructor Create(_name: ShortString; _data: TTOMLTable);
      destructor Destroy; override;

      { Methods }
      procedure RunScripts;

      { Properties }
      property Name: ShortString read m_name;
      property Data: TTOMLTable read m_data;
  end;
  TFPMTargetClass = class of TFPMTarget;
  TFPMTargetMap = specialize TFPGMap<ShortString, TFPMTargetClass>;

  // TODO: darwin scripts unit

  { TFPMTargetDarwin }

  TFPMTargetDarwin = class(TFPMTarget)
    public
      procedure AfterConstruction; override;
  end;

  TCodeSignScript = class(TFPMScript)
    public
      function Execute: integer; override;
  end;

implementation

var
  TargetClasses: TFPMTargetMap;

  function TCodeSignScript.Execute: integer;
  begin
    writeln('RUN CODE SIGN');
    result := 1;
  end;


{ TFPMTargetDarwin }

procedure TFPMTargetDarwin.AfterConstruction;
begin
  inherited;

  // todo: pass in target to resolve variables
  //RegisterScript(TCodeSignScript.Create);
end;


{ TFPMTarget }

procedure TFPMTarget.RegisterScript(script: TFPMScript);
begin
  if scripts = nil then
    scripts := TFPMScriptList.Create;

  scripts.Add(script);
end;

function TFPMTarget.GetProduct: string;
begin
  result := string(data['product']);
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
        if exitCode <> 0 then
          begin
            PrintColor(ANSI_BACK_RED, 'Failed to run script. Error: '+exitCode.ToString);
            break;
          end;
      end;
end;

procedure TFPMTarget.LoadScripts;
var
  script: TTOMLData;
begin
  for script in data['scripts'] do
    if script is TTOMLTable then
      RegisterScript(TFPMScript.Create(TTOMLTable(script)))
    else
      RegisterScript(TFPMScript.Create(string(script)));
end;

class function TFPMTarget.InheritedFrom(_name: ShortString; _data: TTOMLTable): TFPMTarget;
var
  targetClass: TFPMTargetClass;
  parent: ShortString = '';
begin
  targetClass := TFPMTarget;
  if _data.Contains('parent') then
    parent := LowerCase(ShortString(_data['parent']));

  if parent <> '' then
    result := TargetClasses[parent].Create(_name, _data)
  else
    result := TFPMTarget.Create(_name, _data);
end;

destructor TFPMTarget.Destroy;
begin
  FreeAndNil(scripts);
  inherited;
end;

constructor TFPMTarget.Create(_name: ShortString; _data: TTOMLTable);
begin
  m_name := _name;
  m_data := _data;
  if data.Contains('scripts') then
    LoadScripts;
end;

begin
  TargetClasses := TFPMTargetMap.Create;
  TargetClasses.Add('darwin', TFPMTargetDarwin);
end.