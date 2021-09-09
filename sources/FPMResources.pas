{
    Copyright (c) 2020 by Ryan Joseph

    Resource loading utilities

    The file is part of the FPM package (https://github.com/genericptr/fpm)
}
{$mode objfpc}
{$modeswitch typehelpers}
{$modeswitch objectivec2}
{$H+}

unit FPMResources;
interface
uses
  {$ifdef DARWIN}
  CocoaAll, MacOSAll,
  {$endif}
  SysUtils, BaseUnix, Process, Classes,
  FPMUtils;

function CopyResources(source, destination: string): boolean;
function CompareFileDates(oldPath, newPath: string): boolean;
function CompileNIB(oldPath, newPath: string): boolean;

implementation

function FileCopy(Source, Target: string): boolean;
var
  MemBuffer: TMemoryStream;
  {$ifdef DARWIN}
  error: NSError;
  {$endif}
begin
  {$ifdef DARWIN}
  // use Cocoa so we copy any extended resource files
  if Target.FileExists then
    NSFileManager.defaultManager.removeItemAtPath_error(NSSTR(Target), nil);
  if not NSFileManager.defaultManager.copyItemAtPath_toPath_error(NSSTR(Source), NSSTR(Target), @error) then
    begin
      //PrintColor(ANSI_FORE_RED, error.localizedDescription.UTF8String);
      FPMAssert(error.localizedDescription.UTF8String);
      exit(false);
    end;
  {$else}
  // TODO: is this a valid method on Windows/Linux? not sure until I test
  result := false;
  MemBuffer := TMemoryStream.Create;
  try
    MemBuffer.LoadFromFile(Source);
    MemBuffer.SaveToFile(Target); 
    result := true
  except
  end;
  MemBuffer.Free;
  {$endif}
end;

(*
  function compile_metal ($src, $dest) {
    // xcrun -sdk macosx metal AAPLShaders.metal -o AAPLShaders.air
    // xcrun -sdk macosx metallib AAPLShaders.air -o AAPLShaders.metallib
    
    $src_dir = dirname($dest);
    $dest_dir = dirname($dest);
    $name = basename_no_ext($src);
    $air = "$src_dir/$name.air";
    $metal_error_pattern = '/^(.*\.metal):(\d+):(\d+):\s*(error):\s*(.*$/i';
    // $1 = path
    // $2 = line
    // $3 = column
    // $5 = message
    $metal_error_replacement = '$1:$2: error: $3: $5';

    $command = SYSTEM_XCRUN." -sdk macosx metal \"$src\" -o \"$air\"";
    // passthru($command, $err);
    $err = passthru_pattern($command, $metal_error_pattern, $metal_error_replacement);
    if ($err != 0) fatal("failed to compile metal .air '$src'.");

    $command = SYSTEM_XCRUN." -sdk macosx metallib \"$air\" -o \"$dest\"";
    // passthru($command, $err);
    $err = passthru_pattern($command, $metal_error_pattern, $metal_error_replacement);
    if ($err != 0) fatal("failed to compile metal .metallib '$src'.");

    // delete temporary .air file
    if (file_exists($air)) unlink($air);
  }
}
*)

function CompareFileDates(oldPath, newPath: string): boolean;
begin
  if not newPath.FileExists then
    exit(false);
  result := FileAge(oldPath) = FileAge(newPath);
end;


function CompileMetalShader(oldPath, newPath: string): boolean;
begin
  // NOTE: Xcode usually compiles the .metal shader files to a .metallib file
  // and places it at Contents/Resources/default.metallib ut we need to do this manually. 

  // https://developer.apple.com/library/archive/documentation/Miscellaneous/Conceptual/MetalProgrammingGuide/Dev-Technique/Dev-Technique.html#//apple_ref/doc/uid/TP40014221-CH8-SW10
  // xcrun -sdk macosx metal AAPLShaders.metal -o AAPLShaders.air
  // xcrun -sdk macosx metallib AAPLShaders.air -o AAPLShaders.metallib
end;

function CompileNIB(oldPath, newPath: string): boolean;
var
  output: string;
begin

  // file dates are the same
  if CompareFileDates(oldPath, newPath) then
    exit(false);

  PrintColor(ANSI_FORE_MAGENTA, 'Compile IB file from "'+oldPath+'" to "'+newPath+'"');

  result := Process.RunCommand('/usr/bin/ibtool', [
                '--errors',
                '--warnings',
                '--notices',
                '--output-format', 'human-readable-text',
                '--compile', newPath, oldPath,
                '--flatten', 'YES'
              ], output);
  if result then
    FileSetDate(newPath, FileAge(oldPath));
end;

function CopyFile(oldPath, newPath: string): boolean;
var
  extension: string;
begin
  extension := oldPath.Extension;
  result := true;

  {$ifdef DARWIN}
  if extension = 'metal' then
    begin
      // TODO: compile metal shaders
      //result := CompileMetalShader(oldPath, newPath);
      //exit;
    end
  else if extension = 'xib' then
    begin
      result := CompileNIB(oldPath, newPath.ChangeExtension('nib'));
      exit;
    end
  else if extension = 'nib' then
    begin
      result := CompileNIB(oldPath, newPath);
      exit;
    end
  else if extension = 'storyboard' then
    begin
      result := CompileNIB(oldPath, newPath.ChangeExtension('storyboardc'));
      exit;
    end;
  {$endif}

  // file dates are different so copy the file
  if not CompareFileDates(oldPath, newPath) then
    begin
      PrintColor(ANSI_FORE_MAGENTA, 'Copy "'+oldPath+'" to "'+newPath+'"');
      if not FileCopy(oldPath, newPath) then
        exit(false);
      FileSetDate(newPath, FileAge(oldPath));
    end;

end;

function CopyLink(oldPath, newPath: string): boolean;
var
  originalPath: string;
begin
  {$ifdef UNIX}
  originalPath := fpReadLink(oldPath);
  result := fpSymlink(@originalPath, @newPath) = 0;
  {$else}
  assert(false, 'copy link');
  {$endif}
end;

function CopyResources(source, destination: string): boolean;
var
  files: TStringArray;
  path, name, newPath: string;
begin
  if not destination.DirectoryExists then
    ForceDirectories(destination);

  if not source.DirectoryExists then
    begin
      if destination.DirectoryExists then
        result := CopyFile(source, destination+DirectorySeparator+ExtractFileName(source))
      else if destination.FileExists then
        result := CopyFile(source, destination)
      else
        exit(false);
    end
  else
    begin
      // if source folder has trailing / than copy entire directory structure
      if source.TrailingPathDelimiter then
        ForceDirectories(destination.AddComponent(source.FileName));

      files := FindAllFiles(source);
      for name in files do
        begin
          if (name = '.') or (name = '..') then
            continue;
          path := source.AddComponent(name);
          newPath := destination.AddComponent(name);
          if path.IsLink then
            CopyLink(path, newPath)
          else if path.DirectoryExists then
            CopyResources(path, newPath)
          else if path.FileExists then
            CopyFile(path, newPath);
        end;
    end;
end;

end.