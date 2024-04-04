unit Service.Logger;

interface

uses
  System.SyncObjs;

type
  TLogger = class
  strict private
    class var FInstance: TLogger;
  private
    FCritical: TCriticalSection;

    FOpen: Boolean;
    FTextFile: TextFile;
    FActive: Boolean;
    FFilePath: String;
    FFileName: String;
  public
    { Public declarations }
    class function GetInstance: TLogger;
    class destructor DestroyClass;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Open;
    procedure Add(const AValue: String);
    property Active: Boolean read FActive write FActive;
    property FilePath: String read FFilePath write FFilePath;
    property FileName: String read FFileName write FFileName;
  end;

procedure AssertErrorHandlerLogger(const AMessage, AFileName: String; ALineNumber: Integer; AErrorAddr: Pointer);

implementation

uses
  System.SysUtils,
  System.IOUtils;

{$ASSERTIONS ON}

{ TLogger }

procedure DoMessage(AMessage, AFileName: String; ALineNumber: Integer; AErrorAddr: Pointer);
var
  LMessageFull: String;
begin
  LMessageFull := Format('%s (%s, line %d, address $%x)', [AMessage, AFileName, ALineNumber, Pred(Integer(AErrorAddr))]);
  TLogger.GetInstance.Add(LMessageFull);
end;

procedure AssertErrorHandlerLogger(const AMessage, AFileName: String; ALineNumber: Integer; AErrorAddr: Pointer);
begin
  DoMessage(AMessage, AFileName, ALineNumber, AErrorAddr);
end;

procedure TLogger.Add(const AValue: String);
begin
  if FActive then
  begin
    FCritical.Enter;
    try
      if not FOpen then
         Open;
      try
        Append(FTextFile);
        try
          Writeln(FTextFile, FormatDateTime('HH:MM:SS', Time) + ' : (' + AValue + ')');
        finally
          CloseFile(FTextFile);
        end;
      except
      end;
    finally
      FCritical.Release;
    end;
  end;
end;

procedure TLogger.AfterConstruction;
begin
  inherited;
  FCritical := TCriticalSection.Create;
end;

procedure TLogger.BeforeDestruction;
begin
  inherited;
  FCritical.Free;
end;

class destructor TLogger.DestroyClass;
begin
  if Assigned(FInstance) then
    FInstance.Free;
end;

class function TLogger.GetInstance: TLogger;
begin
  if not Assigned(FInstance) then
    FInstance := TLogger.Create;

  Result := FInstance;
end;

procedure TLogger.Open;
var
  LFileName: String;
begin
  FOpen := True;

  if Length(FFilePath) = 0 then
     FFilePath := TPath.Combine(GetCurrentDir, 'logs');

  if not DirectoryExists(FFilePath) then
    CreateDir(FFilePath);

  if Length(FFileName) = 0 then
     FFileName := FormatDateTime('YYYY.MM.DD', Date) + '.log';  //ChangeFileExt(ExtractFileName(ParamStr(0)), '.log');

  LFileName := TPath.Combine(FFilePath, FFileName);

  FCritical.Enter;
  try
    AssignFile(FTextFile, LFileName);

    if not FileExists(LFileName) then
    begin
      ReWrite(FTextFile);
      Writeln(FTextFile, '------ ' + FormatDateTime('DD/MM/YYYY', Date) + ' ------');
      CloseFile(FTextFile);
    end;
  finally
    FCritical.Release;
  end;
end;

initialization
  System.AssertErrorProc := @AssertErrorHandlerLogger;
  TLogger.GetInstance.Active  :=  True;

end.
