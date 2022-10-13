{
    Copyright (c) 2020 by Ryan Joseph

    Various utilities

    The file is part of the FPM package (https://github.com/genericptr/fpm)
}
{$mode objfpc}
{$scopedenums on}
{$modeswitch arrayoperators}
{$modeswitch typehelpers}
{$H+}

unit FPMUtils;
interface
uses
  SysUtils, Classes, Process, RegExpr,
  TOML;

type
  EFPMError = class(Exception);
  EFPMNotFound = class(Exception);

type
  TPlatform = (MacOSX,
               IPhoneSimulator,
               IPhoneOS);
  
type
  TFPMOption = (NoColor);
  TFPMOptions = set of TFPMOption;

var
  FPMOptions: TFPMOptions = [];

function GetLatestCompiler: ShortString;
function GetSDK(platform: TPlatform): string;

// ANSI Color Codes
const
  ANSI_FORE_BLACK           = 30;
  ANSI_FORE_RED             = 31;
  ANSI_FORE_GREEN           = 32;
  ANSI_FORE_YELLOW          = 33;
  ANSI_FORE_BLUE            = 34;
  ANSI_FORE_MAGENTA         = 35;
  ANSI_FORE_CYAN            = 36;
  ANSI_FORE_WHITE           = 37;
  ANSI_FORE_RESET           = 39;

  ANSI_BACK_BLACK           = 40;
  ANSI_BACK_RED             = 41;
  ANSI_BACK_GREEN           = 42;
  ANSI_BACK_YELLOW          = 43;
  ANSI_BACK_BLUE            = 44;
  ANSI_BACK_MAGENTA         = 45;
  ANSI_BACK_CYAN            = 46;
  ANSI_BACK_WHITE           = 47;
  ANSI_BACK_RESET           = 49;

  ANSI_STYLE_BOLD           = 1;
  ANSI_STYLE_ITALIC         = 3;
  ANSI_STYLE_UNDERLINE      = 4;
  ANSI_STYLE_BLINK          = 5;

{ Assertions }

procedure FPMAssert(condition: boolean; message: string);
procedure FPMAssert(message: string);

{ Variables }

procedure AddVariable(name: ShortString; value: string);
function ReplaceVariables(const s: string): string; overload;
procedure ReplaceVariables(var arr: TStringArray); overload;
function ExpandValue(data: TTOMLData): string;
function ExpandPath(data: TTOMLData): string; overload;
function ExpandPath(data: string): string; overload;

{ Command Lines Tools }

function ArrayToFlags(flag: ShortString; strings: TStringArray; isPath: boolean = true): TStringArray;
function ArrayToFlags(flag: ShortString; data: TTOMLArray; isPath: boolean = true): TStringArray;

{ General Utilities }

procedure PrintColor(code: byte; str: ansistring);
procedure PrintColor(codes: array of byte; str: ansistring);
function GetFileAsString(const path: Ansistring): Ansistring;
procedure PutFileContents(const path: Ansistring; contents: Ansistring);
function FindAllFiles(path: string): TStringArray;

{ Command Line }

function GetInput: string;
function GetCommandLineArgument(name: string; out value: string): boolean; overload;
function GetCommandLineArgument(name: string): boolean; overload;

{ Commands }

function RunCommand(Executable: String; Parameters: TStringArray; silent: Boolean = false): integer; overload;
function ClearOption(options: TStringArray; name: ShortString): TStringArray;

{ TStringArrayHelper }

type
  TStringArrayHelper = type helper for TStringArray
    procedure Add(value: String);
    procedure AddValues(other: TStringArray);
    function Join(delimiter: ShortString = ' '): String;
  end;

{ TStringHelper }
  
  TStringHelper = type helper for String
    function Length: integer; inline;
    function ToInteger: integer; inline;
    function ToSingle: single; inline;
    function ToDouble: double; inline;
    function ToFloat: single; inline;
    function Replace(src: string; dest: string = ''): string;
    function StartsWith(prefix: string): boolean;
    function EndsWith(suffix: string): boolean;
    function Contains(character: Char): boolean;
    function Match(pattern: ShortString): boolean;

    { TStringFileHelper }

    function ExpandTilde: String;
    function Extension: ShortString;
    function ExistsAtPath: boolean;
    function FileExists: boolean;
    function FileName: ShortString;
    function FileNameOnly: ShortString;
    function TrailingPathDelimiter: boolean;
    function DirectoryPath: String;
    function DirectoryExists: boolean;
    function IsHidden: boolean;
    function IsLink: boolean;
    function ChangeExtension(newExtension: ShortString): String;
    function DeleteExtension: String;
    function AddComponent(const component: String): String;
    function AddComponents(const components: array of String): String;
    function AddExtension(const name: String): String;
  end;

{ Global variables for the active config }
var
  GlobalVariables: TTOMLTable;

implementation
uses
  {$ifdef DARWIN}
  CocoaAll, MacOSAll,
  {$endif}
  {$ifdef UNIX}
  BaseUnix,
  {$endif}
  StrUtils;

var
  LatestCompiler: ShortString = '';

{*****************************************************************************
 *                                 TStringFileHelpers
 *****************************************************************************}

{$ifdef DARWIN}

function GetFSRef(path: String): FSRef;
begin
  path += #0;
  if FSPathMakeRef(@path[1], result, nil) <> noErr then
    FillChar(result, sizeof(FSRef), 0);
end;

function FSIsPackage(fileRef: FSRef): boolean;
var
  info: LSItemInfoRecord;
begin
  if LSCopyItemInfoForRef(fileRef, kLSRequestBasicFlagsOnly, info) = noErr then
    result := info.flags = (info.flags or kLSItemInfoIsPackage)
  else
    result := false;
end;

function FSIsHidden(fileRef: FSRef): boolean;
var
  info: LSItemInfoRecord;
begin
  if LSCopyItemInfoForRef(fileRef, kLSRequestBasicFlagsOnly, info) = noErr then
    result := info.flags = (info.flags or kLSItemInfoIsInvisible)
  else
    result := false;
end;

{$endif}

function TStringHelper.ChangeExtension(newExtension: ShortString): String;
begin
  result := self.DeleteExtension.AddExtension(newExtension);
end;

function TStringHelper.DeleteExtension: String;
begin
  result := ExtractFilePath(self)+FileNameOnly;
end;

function TStringHelper.DirectoryPath: String;
begin
  result := ExtractFilePath(self);
end;

function TStringHelper.TrailingPathDelimiter: boolean;
begin
  result := self[High(self)] = DirectorySeparator;
end;

function TStringHelper.Extension: ShortString;
begin
  result := LowerCase(ExtractFileExt(self));
  if result <> '' then
    result := System.Copy(result, 2, System.Length(result));
end;

function TStringHelper.FileName: ShortString;
begin
  result := ExtractFileName(self);
end;

function TStringHelper.FileNameOnly: ShortString;
var
  ext: ShortString;
begin
  result := ExtractFileName(self);
  ext := ExtractFileExt(result);
  if ext <> '' then
    result := System.Copy(result, 1, Pos(ext, result) - 1);
end;

function TStringHelper.ExpandTilde: String;
begin
  {$ifdef DARWIN}
  result := StringReplace('~', GetUserDir, self, []);
  {$else}
  result := self;
  {$endif}
end;

function TStringHelper.IsHidden: boolean;
var
  attrs: LongInt;
begin
  attrs := FileGetAttr(self);
  result := (attrs and faHidden) <> 0;
end;

function TStringHelper.IsLink: boolean;
var
  attrs: LongInt;
begin
  attrs := FileGetAttr(self);
  result := (attrs and faSymLink) <> 0;
end;

function TStringHelper.ExistsAtPath: boolean;
begin
  result := DirectoryExists;
  if result then
    exit;
  result := FileExists;
end;

function TStringHelper.FileExists: boolean;
begin
  result := SysUtils.FileExists(self);
end;

function TStringHelper.DirectoryExists: boolean;
begin
  result := SysUtils.DirectoryExists(self);
  {$ifdef DARWIN}
  if not result then
    result := FSIsPackage(GetFSRef(self));
  {$endif}
end;

function TStringHelper.AddComponent(const component: String): String;
begin
  if TrailingPathDelimiter then
    result := self+component
  else
    result := self+DirectorySeparator+component;
end;

function TStringHelper.AddComponents(const components: array of String): String;
var
  component: string;
begin
  result := self;
  for component in components do
    result := result.AddComponent(component);
end;

function TStringHelper.AddExtension(const name: String): String;
begin
  result := self+ExtensionSeparator+name;
end;

{*****************************************************************************
 *                            TStringArrayHelper
 *****************************************************************************}

function TStringArrayHelper.Join(delimiter: ShortString ): String;
var
  value: String;
begin
  result := '';
  for value in self do
    begin
      if result <> '' then
        result += delimiter;
      result += value;
    end;
end;

procedure TStringArrayHelper.AddValues(other: TStringArray);
var
  value: String;
begin
  for value in other do
    Add(value);
end;

procedure TStringArrayHelper.Add(value: String);
begin
  self := Concat(self, [value]);
end;

{*****************************************************************************
 *                                TStringHelper
 *****************************************************************************}

function TStringHelper.Length: integer;
begin
  result := System.Length(self);
end;

function TStringHelper.ToInteger: integer;
begin
  if System.Length(self) > 0 then
    result := StrToInt(self)
  else
    result := 0;
end;

function TStringHelper.ToSingle: single;
begin
  result := StrToFloat(self);
end;

function TStringHelper.ToDouble: double;
begin
  result := StrToFloat(self);
end;

function TStringHelper.ToFloat: Single;
begin
  result := StrToFloat(self);
end;
  
function TStringHelper.Replace (src: string; dest: string = ''): string;
begin
  result := StringReplace(self, src, dest, [rfReplaceAll]);
end;

function TStringHelper.StartsWith(prefix: string): boolean;
var
  i: longint;
begin
  result := true;
  for i := 1 to prefix.Length do
    if self[i] <> prefix[i] then
      exit(false);
end;

function TStringHelper.EndsWith(suffix: string): boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to suffix.Length do
  if self[(Length+1) - i] <> suffix[(suffix.Length+1) - i] then
    exit(false);
end;

function TStringHelper.Contains(character: Char): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to High(self) do
    if self[i] = character then
      exit(true);
end;

threadvar
  SharedRegExpr: TRegExpr;

function TStringHelper.Match(pattern: ShortString): boolean;
begin
  if (SharedRegExpr <> nil) and (SharedRegExpr.Expression <> pattern) then
    begin
      SharedRegExpr.Free;
      SharedRegExpr := nil;
    end;
  if SharedRegExpr = nil then
    SharedRegExpr := TRegExpr.Create(pattern);
  result := SharedRegExpr.Exec(self);
end;

{*****************************************************************************
 *                              Utilities
 *****************************************************************************}

{ Removes command line option by name }
function ClearOption(options: TStringArray; name: ShortString): TStringArray;
var
  i: integer;
begin
  result := options;
  for i := 0 to high(options) do
    if options[i].StartsWith('-'+name) then
      begin
        Delete(result, i, 1);
        break;
      end;
end;

function RunCommand(Executable: String; Parameters: TStringArray; silent: Boolean): integer;
var
  parameter: string;
  process: TProcess;
  i: integer;
begin
  process := TProcess.Create(nil);
  process.Executable := Executable;
  for parameter in Parameters do
    process.Parameters.Add(parameter);
  for i := 1 to GetEnvironmentVariableCount do
    Process.Environment.Add(GetEnvironmentString(i));
  // pipe output to nowhere in silent mode
  if silent then
    process.Options := process.Options + [poWaitOnExit, poUsePipes]
  else
    process.Options := process.Options + [poWaitOnExit];
  process.Execute;
  result := process.ExitStatus;
  process.Free;
end;

function GetInput: string;
var
  input: string;
  i: integer;
begin
  
  // defaut to CWD as the input
  GetDir(0, input);

  // first param is a flag which means the input file was omitted
  if (ParamCount > 1) and (ParamStr(1)[1] = '-') then
    exit(input);

  // use first param as the input
  if ParamCount > 1 then
    input := ExpandFileName(ParamStr(1));
  
  if input = '' then
    FPMAssert('FPM expects a valid directory or fpconfig.toml file.');

  if not FileExists(input) and not DirectoryExists(input) then
    FPMAssert('"'+input+'" does not exist. FPM expects a valid directory or fpconfig.toml file.');

  result := input;
end;

function GetCommandLineArgument(name: string; out value: string): boolean;

  function GetCmdLineArg(const Switch: string; SwitchChars: TSysCharSet; out value: string): boolean;
  var
    i: Integer;
    S: string;
  begin
    i:=1;
    value:='';
    result := false;
    while (value='') and (i<=ParamCount) do
      begin
        S:=ParamStr(i);
        if (SwitchChars=[]) or ((S[1] in SwitchChars) and (Length(S) > 1)) and
           (AnsiCompareText(Copy(S,2,Length(S)-1),Switch)=0) then
          begin
            inc(i);
            result := true;
            if i<=ParamCount then
              value:=ParamStr(i);
          end;
        inc(i);
      end;
  end;

var
  i: integer;
begin
  result := false;
  result:= GetCmdLineArg(name, StdSwitchChars, value);
  if result then
    exit;
  result := GetCmdLineArg(name[1], StdSwitchChars, value);
  if result then
    exit;
end;

function GetCommandLineArgument(name: string): boolean;
var
  value: string;
begin
  result := GetCommandLineArgument(name, value);
end;

// TODO: GetFileAsString is part of the RTL in 3.3.1 trunk so we can remove it
// when the next post 3.2 release is available

function GetFileAsString(const path: Ansistring): Ansistring;
var
  f: File;
begin
  Assert(FileExists(path), 'file '+path+' doesnt''t exist');
  try
    AssignFile(f, path);
    FileMode := fmOpenRead;
    Reset(f, 1);
    SetLength(result, FileSize(f));
    BlockRead(f, pointer(result)^, FileSize(f));
    CloseFile(f);
  except
    on E:Exception do
      writeln(path+': ', E.Message);
  end;
end;

procedure PutFileContents(const path: Ansistring; contents: Ansistring);
var
  f: TextFile;
begin
  AssignFile(f, path);
  Rewrite(f);
  Write(f, contents);
  CloseFile(f);
end;

function ArrayToFlags(flag: ShortString; strings: TStringArray; isPath: boolean): TStringArray;
var
  value: string;
  path: string;
begin
  result := [];
  for value in strings do
    begin
      if isPath then
        begin
          path := ReplaceVariables(value);
          path := ExpandFileName(path);
          // ignore this warning for directories with * patterns
          if not path.Contains('*') then
            FPMAssert(path.ExistsAtPath, 'Path "'+path+'" for flag '+flag+' doesn''t exist');
          result += [flag+path];
        end
      else
        result += [flag+value];
    end;
end;

function ArrayToFlags(flag: ShortString; data: TTOMLArray; isPath: boolean): TStringArray;
var
  value: TTOMLData;
  path: string;
begin
  result := [];
  if data = nil then
    exit;
  for value in data do
    begin
      if isPath then
        begin
          path := Trim(ExpandPath(value));
          FPMAssert(path.ExistsAtPath, 'Path "'+path+'" for flag '+flag+' doesn''t exist');
          result += [flag+path];
        end
      else
        result += [flag+string(value)];
    end;
end;

procedure AddVariable(name: ShortString; value: String); 
begin
  if not GlobalVariables.Contains(name) then
    GlobalVariables.Add(name, value)
  else
    GlobalVariables.Put(name, value);
end;

function ExpandValue(data: TTOMLData): string;
begin
  if data = nil then
    exit('');
  result := data.ToString;
  result := ReplaceVariables(result);
end;

function ExpandPath(data: TTOMLData): string;
begin
  if data = nil then
    exit('');
  result := ExpandValue(data);
  result := ExpandFileName(result);
end;

function ExpandPath(data: string): string;
begin
  result := ReplaceVariables(data);
  result := ExpandFileName(data);
end;

function ReplaceVariables(const s: string): string;
var
  i: Integer;
begin
  result := s;
  if GlobalVariables <> nil then
    for i := 0 to GlobalVariables.Count - 1 do
      result := StringReplace(result, '${'+GlobalVariables.Keys[i]+'}', GlobalVariables[i].ToString, [rfReplaceAll, rfIgnoreCase]);
end;

procedure ReplaceVariables(var arr: TStringArray);
var
  i: integer;
begin
  if GlobalVariables = nil then
    exit;
  for i := 0 to length(arr) - 1 do
    arr[i] := ReplaceVariables(arr[i]);
end;

procedure FPMAssert(condition: boolean; message: string);
begin
  if condition then
    exit;
  raise EFPMError.Create(message);
end;

procedure FPMAssert(message: string);
begin
  FPMAssert(false, message);
end;

procedure PrintColor(code: byte; str: ansistring);
begin
  if TFPMOption.NoColor in FPMOptions then
    writeln(str)
  else
    PrintColor([code], str);
end;

procedure PrintColor(codes: array of byte; str: ansistring);
var
  attrs: string;
  code: byte;
begin
  attrs := '';
  for code in codes do
    begin
      if attrs <> '' then
        attrs += ';';
      attrs += IntToStr(code);
    end;
  writeln(#&033,'[',attrs,'m',str,#&033,'[',0,'m');
end;

function FindAllFiles(path: string): TStringArray;
var
  info: TSearchRec;
begin
  if not DirectoryExists(path) then
    raise Exception.Create('Directory "'+path+'"" doesn''t exist');
  path := path+DirectorySeparator+'*';
  result := [];
  if FindFirst(path, faAnyFile, info) = 0 then
    begin
      repeat
        result += [info.name];
      until FindNext(info) <> 0;
      FindClose(info);
    end;
end;

{ Returns the latest version of compiler }

function GetLatestCompiler: ShortString;
var
  path, name, compiler, version: string;
  paths: array of string;
  re: TRegExpr;
  list: TStringList;
begin
  if LatestCompiler = '' then
    begin
      {$ifdef darwin}
      paths := ['/usr/local/lib/fpc'];
      {$else}
      // TODO: find latest compiler for other platforms
      exit('');
      {$endif}
      re := TRegExpr.Create('^\d+\.\d+\.\d+$');
      for path in paths do
        begin
          list := TStringList.Create;
          for name in FindAllFiles(path) do
            if re.Exec(name) then
              list.Add(name);
          list.Sort;
          version := list[list.Count - 1];
          compiler := path+'/'+version;
          list.Free;
          if DirectoryExists(compiler) then
            begin
              LatestCompiler := version;
              break;
            end;
        end;
      re.Free;
    end;
  result := LatestCompiler;
end;

{ Returns SDK path for the specified platform }

function GetSDK(platform: TPlatform): string;
const
  TSDKPlatformStrings: array[TPlatform] of string[16] = (
      'macosx',
      'iphonesimulator',
      'iphoneos'
    );
begin
  Process.RunCommand('/usr/bin/xcrun', ['--sdk', TSDKPlatformStrings[platform], '--show-sdk-path'], result);
end;

end.