{$mode objfpc}
{$scopedenums on}
{$modeswitch arrayoperators}
{$H+}

unit FPMUtils;
interface
uses
  SysUtils, Classes, Process, RegExpr;

type
 TPlatform = (MacOSX,        
              IPhoneSimulator,             
              IPhoneOS);

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

procedure PrintColor(code: byte; str: ansistring);
procedure PrintColor(codes: array of byte; str: ansistring);

implementation

var
  LatestCompiler: ShortString = '';
  PlatformSDK: string = '';

procedure PrintColor(code: byte; str: ansistring);
begin
  writeln(#&033,'[',code,'m',str,#&033,'[',0,'m');
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
  if PlatformSDK = '' then
    RunCommand('/usr/bin/xcrun', ['--sdk', TSDKPlatformStrings[platform], '--show-sdk-path'], PlatformSDK);
  result := PlatformSDK;
end;

end.