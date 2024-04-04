unit Controller.Notification;

interface

uses
  Controller.Interfaces,
  Model.Interfaces,
  Model.Entity.User,
  Model.Entity.Notification.Settings;

type
  TControllerNotification = class(TInterfacedObject, IControllerNotification)
  private
    FNotification: IModelNotification;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IControllerNotification;

    function Initialize: Boolean;
    function Terminate: Boolean;
    function GetListOfChannels: TArray<String>;
    function GetListOfFrequency: TArray<String>;
    function GetUserNotificationBackup(AIDUser: Integer): TNoticationSettings;
    function SaveUserNotificationBackup(AIDUser: Integer; ASettings: TNoticationSettings): Boolean;
    function GetUserNotificationTaskDue(AIDUser: Integer): TNoticationSettings;
    function SaveUserNotificationTaskDue(AIDUser: Integer; ASettings: TNoticationSettings): Boolean;
    function SendNotificationBackupNow: Boolean;
    function SendNotificationTaskDueNow: Boolean;
    function SetNotificationBackupExecutedAt(AExecutedAt: TDateTime): Boolean;
    function SetNotificationTaskDueAt(ATaskDueAt: TDateTime): Boolean;
  end;

implementation

uses
  Model.Factory;

{ TControllerNotification }

constructor TControllerNotification.Create();
begin
  FNotification :=  TModelFactory.New.Notification;
end;

destructor TControllerNotification.Destroy;
begin

  inherited;
end;

function TControllerNotification.GetListOfChannels: TArray<String>;
begin
  Result  :=  FNotification.GetListOfChannels;
end;

function TControllerNotification.GetListOfFrequency: TArray<String>;
begin
  Result  :=  FNotification.GetListOfFrequency;
end;

function TControllerNotification.GetUserNotificationBackup(
  AIDUser: Integer): TNoticationSettings;
begin
  Result  :=  FNotification.GetUserNotificationBackup(AIDUser);
end;

function TControllerNotification.GetUserNotificationTaskDue(
  AIDUser: Integer): TNoticationSettings;
begin
  Result  :=  FNotification.GetUserNotificationTaskDue(AIDUser);
end;

function TControllerNotification.Initialize: Boolean;
begin
  Result  :=  FNotification.Initialize;
end;

class function TControllerNotification.New: IControllerNotification;
begin
  Result  :=  Self.Create;
end;

function TControllerNotification.SaveUserNotificationBackup(AIDUser: Integer;
  ASettings: TNoticationSettings): Boolean;
begin
  Result  :=  FNotification.SaveUserNotificationBackup(AIDUser, ASettings);
end;

function TControllerNotification.SaveUserNotificationTaskDue(AIDUser: Integer;
  ASettings: TNoticationSettings): Boolean;
begin
  Result  :=  FNotification.SaveUserNotificationTaskDue(AIDUser, ASettings);
end;

function TControllerNotification.SendNotificationBackupNow: Boolean;
begin
  Result  :=  FNotification.SendNotificationBackupNow;
end;

function TControllerNotification.SendNotificationTaskDueNow: Boolean;
begin
  Result  :=  FNotification.SendNotificationTaskDueNow;
end;

function TControllerNotification.SetNotificationBackupExecutedAt(
  AExecutedAt: TDateTime): Boolean;
begin
  Result  :=  FNotification.SetNotificationBackupExecutedAt(AExecutedAt);
end;

function TControllerNotification.SetNotificationTaskDueAt(
  ATaskDueAt: TDateTime): Boolean;
begin
  Result  :=  FNotification.SetNotificationTaskDueAt(ATaskDueAt);
end;

function TControllerNotification.Terminate: Boolean;
begin
  Result  :=  FNotification.Terminate;
end;

end.
