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

implementation

function FileCopy(Source, Target: string): boolean;
var
  MemBuffer: TMemoryStream;
  {$ifdef DARWIN}
  error: NSError;
  {$endif}
begin
  {$ifdef DARWIN}
  if not NSFileManager.defaultManager.copyItemAtPath_toPath_error(NSSTR(Source), NSSTR(Target), @error) then
    writeln(error.localizedDescription.UTF8String);
  {$else}
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
  //assert(false, 'compile metal shader not implemented');
  FileSetDate(newPath, FileAge(oldPath));
end;

function CompileNIB(oldPath, newPath: string): boolean;
var
  output: string;
begin

  // file dates are the same
  if CompareFileDates(oldPath, newPath) then
    exit(false);

  writeln('compile IB file from ', oldPath, ' to ', newPath);

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
      result := CompileMetalShader(oldPath, newPath);
      exit;
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

  // file dates haven't changed
  if not CompareFileDates(oldPath, newPath) then
    begin
      writeln('copy ', oldPath, ' to ', newPath);
      FileCopy(oldPath, newPath);
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
    result := CopyFile(source, destination+DirectorySeparator+ExtractFileName(source))
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