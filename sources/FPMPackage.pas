{$mode objfpc}

unit FPMPackage;
interface

type
  TFPMPackage = class
    
  end;


  { TFPMDarwin }

  TFPMDarwin = class(TFPMPackage)
    {
      1) 
    }
  end;

implementation


// TODO: replace_file_macros

procedure SaveStringToFile(const s: AnsiString; const path: AnsiString);
var
  f: TextFile;
begin
  try
    AssignFile(f, path);
    Rewrite(f);
    Write(f, s);
    CloseFile(f);
  except
    on E:Exception do
      writeln(path, ': ', E.Message);
  end;
end;


end.