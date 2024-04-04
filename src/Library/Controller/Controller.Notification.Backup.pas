unit Controller.Notification.Backup;

interface

uses
  Controller.Interfaces,
  Model.Entity.Notification.Backup,
  Model.Interfaces;

type
  TControllerNotificationBackup = class(TInterfacedObject, IControllerNotificationBackup)
  private
    FModel: IModelNotificationBackup;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IControllerNotificationBackup;

    function Get: TNotificationBackup;
    function Entity: TNotificationBackup;
    function SetBackupExecutedAt(AExecutedAt: TDateTime): IControllerNotificationBackup;
    function SetNotifiedAt(ANotifiedAt: TDateTime): IControllerNotificationBackup;
    function SendNotificationNow: IControllerNotificationBackup;
  end;

implementation

uses
  System.SysUtils,

  Model.Factory;

{ TControllerNotificationBackup }

constructor TControllerNotificationBackup.Create();
begin
  FModel  :=  TModelFactory.New.NotificationBackup;
end;

destructor TControllerNotificationBackup.Destroy;
begin

  inherited;
end;

function TControllerNotificationBackup.Entity: TNotificationBackup;
begin
  Result  :=  FModel.Entity;
end;

function TControllerNotificationBackup.Get: TNotificationBackup;
begin
  Result  :=  FModel.Get;
end;

class function TControllerNotificationBackup.New: IControllerNotificationBackup;
begin
  Result  :=  Self.Create;
end;

function TControllerNotificationBackup.SendNotificationNow: IControllerNotificationBackup;
begin
  Result  :=  Self;

  FModel.Entity.BackupExecutedAt:=  0;
  FModel.Entity.NotifiedAt      :=  0;
  FModel.Update;
end;

function TControllerNotificationBackup.SetBackupExecutedAt(AExecutedAt: TDateTime): IControllerNotificationBackup;
begin
  Result  :=  Self;

  FModel.Entity.BackupExecutedAt  :=  AExecutedAt;
  FModel.Update;
end;

function TControllerNotificationBackup.SetNotifiedAt(ANotifiedAt: TDateTime): IControllerNotificationBackup;
begin
  Result  :=  Self;

  FModel.Entity.NotifiedAt  :=  ANotifiedAt;
  FModel.Update;
end;

end.
