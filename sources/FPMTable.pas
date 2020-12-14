{$mode objfpc}
{$modeswitch arrayoperators}
{$scopedenums on}

unit FPMTable;
interface
uses
  TOML, SysUtils;


type
  TMergeOption = (Expand, IsPath);
  TMergeOptions = set of TMergeOption;
  
type
  TFPMTable = class
    private
      parent: TFPMTable;
      function GetArray(const key: TTOMLKeyType): TTOMLArray;
      function GetValue(const key: TTOMLKeyType): TTOMLData;
    public
      data: TTOMLTable;

      { Constructors }
      constructor Create(_data: TTOMLTable; _parent: TFPMTable = nil);

      { Methods }
      function Contains(const key: TTOMLKeyType): boolean;
      function MergedArray(const key: TTOMLKeyType): TStringArray;
      function JoinArray(const key: TTOMLKeyType; delimiter: string = ''): string;

      { Properties }
      property Values[const key: TTOMLKeyType]: TTOMLData read GetValue; default;
      property Arrays[const key: TTOMLKeyType]: TTOMLArray read GetArray;
  end;

implementation

function TFPMTable.JoinArray(const key: TTOMLKeyType; delimiter: string): string;
var
  value: string;
begin
  result := '';
  for value in MergedArray(key) do
    result += value+delimiter;
end;

function TFPMTable.MergedArray(const key: TTOMLKeyType): TStringArray;
var
  value: TTOMLData;
  table: TFPMTable;
begin
  result := [];
  table := self;
  while table <> nil do
    begin
      if table.data.Contains(key) then
        for value in table.data[key] as TTOMLArray do
        result += [string(value)];
      table := table.parent;
    end;
end;

function TFPMTable.GetArray(const key: TTOMLKeyType): TTOMLArray;
begin
  result := Values[key] as TTOMLArray;
end;

function TFPMTable.GetValue(const key: TTOMLKeyType): TTOMLData;
begin
  if data.Contains(key) then
    result := data[key]
  else if parent <> nil then
    result := parent.GetValue(key)
  else
    result := nil;
end;

function TFPMTable.Contains(const key: TTOMLKeyType): boolean;
begin
  if data.Contains(key) then
    exit(true)
  else if parent <> nil then
    result := parent.Contains(key)
  else
    result := false;
end;

constructor TFPMTable.Create(_data: TTOMLTable; _parent: TFPMTable);
begin
  data := _data;
  parent := _parent;
end;

end.