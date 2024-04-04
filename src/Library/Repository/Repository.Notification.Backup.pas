unit Repository.Notification.Backup;

interface

uses
  System.SyncObjs,

  Repository.Interfaces,
  Model.Entity.Notification.Backup;

type
  TRepositoryNotificationBackup = class(TInterfacedObject, IRepositoryNotificationBackup)
  private
    FFileStorageUser: String;
    FCritical: TCriticalSection;

    function GetDataNotificationBackup(AFileStorage: String): TNotificationBackup;
    procedure SaveDataNotificationBackup(AUser: TNotificationBackup; AFileStorage: String);
    procedure CheckDataNotificationBackup;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IRepositoryNotificationBackup;

    function Get: TNotificationBackup;
    function Update(ANotificationBackupNew: TNotificationBackup): IRepositoryNotificationBackup;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Commons;

{ TRepositoryNotificationBackup }

procedure TRepositoryNotificationBackup.CheckDataNotificationBackup;
var
  LNotificationBackup: TNotificationBackup;
begin
  if not FileExists(FFileStorageUser) then
  begin
    LNotificationBackup :=  TNotificationBackup.Create;
    try
      SaveDataNotificationBackup(LNotificationBackup, FFileStorageUser);
    finally
      LNotificationBackup.Free;
    end;
  end;
end;

constructor TRepositoryNotificationBackup.Create();
begin
  FFileStorageUser  := TPath.Combine(ExtractFilePath(ParamStr(0)), 'StorageNotificationBackup.json');
  FCritical :=  TCriticalSection.Create;
end;

destructor TRepositoryNotificationBackup.Destroy;
begin
  FCritical.Free;

  inherited;
end;

function TRepositoryNotificationBackup.Get: TNotificationBackup;
begin
  try
    CheckDataNotificationBackup;

    Result  :=  GetDataNotificationBackup(FFileStorageUser);
  except
    on Exc: Exception do
      raise ERepositoryException.Create('Error when trying to fetch backup notification',
                                        Format('[%s.Get] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TRepositoryNotificationBackup.GetDataNotificationBackup(AFileStorage: String): TNotificationBackup;
var
  LFile: TStringList;
begin
  Result  :=  TNotificationBackup.Create;

  LFile :=  TStringList.Create;
  try
    FCritical.Enter;
    try
      LFile.LoadFromFile(FFileStorageUser);
    finally
      FCritical.Release;
    end;

    Result.LoadJsonString(LFile.Text);
  finally
    LFile.Free;
  end;
end;

class function TRepositoryNotificationBackup.New: IRepositoryNotificationBackup;
begin
  Result  :=  Self.Create;
end;

procedure TRepositoryNotificationBackup.SaveDataNotificationBackup(AUser: TNotificationBackup; AFileStorage: String);
var
  LFile: TStringList;
begin
  LFile :=  TStringList.Create;
  try
    LFile.Text  :=  AUser.ToJsonString(False);
    FCritical.Enter;
    try
      LFile.SaveToFile(AFileStorage);
    finally
      FCritical.Release;
    end;
  finally
    LFile.Free;
  end;
end;

function TRepositoryNotificationBackup.Update(ANotificationBackupNew: TNotificationBackup): IRepositoryNotificationBackup;
begin
  Result  :=  Self;

  try
    SaveDataNotificationBackup(ANotificationBackupNew, FFileStorageUser);
  except
    on Exc: Exception do
      raise ERepositoryException.Create('Error when trying to update backup notification',
                                        Format('[%s.Update] %s', [Self.ToString, Exc.Message]));
  end;
end;

end.
