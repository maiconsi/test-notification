unit Repository.Notification.TaskDue;

interface

uses
  System.SyncObjs,

  Repository.Interfaces,
  Model.Entity.Notification.TaskDue;

type
  TRepositoryNotificationTaskDue = class(TInterfacedObject, IRepositoryNotificationTaskDue)
  private
    FFileStorageUser: String;
    FCritical: TCriticalSection;

    function GetDataNotificationTaskDue(AFileStorage: String): TNotificationTaskDue;
    procedure SaveDataNotificationTaskDue(AUser: TNotificationTaskDue; AFileStorage: String);
    procedure CheckDataNotificationTaskDue;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IRepositoryNotificationTaskDue;

    function Get: TNotificationTaskDue;
    function Update(ANotificationTaskDueNew: TNotificationTaskDue): IRepositoryNotificationTaskDue;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Commons;

{ TRepositoryNotificationTaskDue }

procedure TRepositoryNotificationTaskDue.CheckDataNotificationTaskDue;
var
  LNotificationTaskDue: TNotificationTaskDue;
begin
  if not FileExists(FFileStorageUser) then
  begin
    LNotificationTaskDue :=  TNotificationTaskDue.Create;
    try
      SaveDataNotificationTaskDue(LNotificationTaskDue, FFileStorageUser);
    finally
      LNotificationTaskDue.Free;
    end;
  end;
end;

constructor TRepositoryNotificationTaskDue.Create();
begin
  FFileStorageUser  := TPath.Combine(ExtractFilePath(ParamStr(0)), 'StorageNotificationTaskDue.json');
  FCritical :=  TCriticalSection.Create;
end;

destructor TRepositoryNotificationTaskDue.Destroy;
begin
  FCritical.Free;

  inherited;
end;

function TRepositoryNotificationTaskDue.Get: TNotificationTaskDue;
begin
  try
    CheckDataNotificationTaskDue;

    Result  :=  GetDataNotificationTaskDue(FFileStorageUser);
  except
    on Exc: Exception do
      raise ERepositoryException.Create('Error when trying to fetch the backup notification',
                                        Format('[%s.Get] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TRepositoryNotificationTaskDue.GetDataNotificationTaskDue(AFileStorage: String): TNotificationTaskDue;
var
  LFile: TStringList;
begin
  Result  :=  TNotificationTaskDue.Create;

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

class function TRepositoryNotificationTaskDue.New: IRepositoryNotificationTaskDue;
begin
  Result  :=  Self.Create;
end;

procedure TRepositoryNotificationTaskDue.SaveDataNotificationTaskDue(AUser: TNotificationTaskDue; AFileStorage: String);
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

function TRepositoryNotificationTaskDue.Update(ANotificationTaskDueNew: TNotificationTaskDue): IRepositoryNotificationTaskDue;
begin
  Result  :=  Self;

  try
    SaveDataNotificationTaskDue(ANotificationTaskDueNew, FFileStorageUser);
  except
    on Exc: Exception do
      raise ERepositoryException.Create('Error when trying to update due task notification',
                                        Format('[%s.Update] %s', [Self.ToString, Exc.Message]));
  end;
end;

end.
