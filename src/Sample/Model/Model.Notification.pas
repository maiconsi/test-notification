unit Model.Notification;

interface

uses
  Model.Interfaces,
  Model.Entity.User,
  Model.Entity.Notification.Settings;

type
  TModelNotification = class(TInterfacedObject, IModelNotification)
  private
    FDLLMapping: IModelNotificationDLLMapping;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelNotification;

    function Initialize: Boolean;
    function Terminate: Boolean;
    function GetUser(AIDUser: Integer): TUser;
    function SaveUser(AUser: TUser): Boolean;
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

{ TModelNotification }

uses Model.Factory;

constructor TModelNotification.Create();
begin
  FDLLMapping :=  TModelFactory.New.NotificationDLLMapping;
end;

destructor TModelNotification.Destroy;
begin

  inherited;
end;

function TModelNotification.GetListOfChannels: TArray<String>;
begin
  Result  :=  FDLLMapping.GetListOfChannels;
end;

function TModelNotification.GetListOfFrequency: TArray<String>;
begin
  Result  :=  FDLLMapping.GetListOfFrequency;
end;

function TModelNotification.GetUser(AIDUser: Integer): TUser;
var
  LLogin: String;
  LName: String;
  LEmail: String;
  LCellphone: String;
begin
  FDLLMapping.GetUser(AIDUser, LLogin, LName, LEmail, LCellphone);

  Result        :=  TUser.Create;
  Result.Id     :=  AIDUser;
  Result.Login  :=  LLogin;
  Result.Name   :=  LName;
  Result.Email  :=  LEmail;
  Result.Cellphone  :=  LCellphone;
end;

function TModelNotification.GetUserNotificationBackup(AIDUser: Integer): TNoticationSettings;
var
  LActive: Boolean;
  LFrequency: String;
  LChannels: TArray<String>;
begin
  FDLLMapping.GetUserNotificationBackup(AIDUser, LActive, LFrequency, LChannels);

  Result  :=  TNoticationSettings.Create;
  Result.Active   :=  LActive;
  Result.Frequency:=  LFrequency;
  Result.Channels :=  LChannels;
end;

function TModelNotification.GetUserNotificationTaskDue(AIDUser: Integer): TNoticationSettings;
var
  LActive: Boolean;
  LFrequency: String;
  LChannels: TArray<String>;
begin
  FDLLMapping.GetUserNotificationTaskDue(AIDUser, LActive, LFrequency, LChannels);

  Result  :=  TNoticationSettings.Create;
  Result.Active   :=  LActive;
  Result.Frequency:=  LFrequency;
  Result.Channels :=  LChannels;
end;

function TModelNotification.Initialize: Boolean;
begin
  Result  :=  FDLLMapping.Initialize;
end;

class function TModelNotification.New: IModelNotification;
begin
  Result  :=  Self.Create;
end;

function TModelNotification.SaveUser(AUser: TUser): Boolean;
begin
  Result  :=  FDLLMapping.SaveUser(AUser.Id, AUser.Login, AUser.Name, AUser.Email, AUser.Cellphone);
end;

function TModelNotification.SaveUserNotificationBackup(AIDUser: Integer; ASettings: TNoticationSettings): Boolean;
begin
  Result  :=  FDLLMapping.SaveUserNotificationBackup(AIDUser, ASettings.Active, ASettings.Frequency, ASettings.Channels);
end;

function TModelNotification.SaveUserNotificationTaskDue(AIDUser: Integer; ASettings: TNoticationSettings): Boolean;
begin
  Result  :=  FDLLMapping.SaveUserNotificationTaskDue(AIDUser, ASettings.Active, ASettings.Frequency, ASettings.Channels);
end;

function TModelNotification.SendNotificationBackupNow: Boolean;
begin
  Result  :=  FDLLMapping.SendNotificationBackupNow;
end;

function TModelNotification.SendNotificationTaskDueNow: Boolean;
begin
  Result  :=  FDLLMapping.SendNotificationTaskDueNow;
end;

function TModelNotification.SetNotificationBackupExecutedAt(AExecutedAt: TDateTime): Boolean;
begin
  Result  :=  FDLLMapping.SetNotificationBackupExecutedAt(AExecutedAt);
end;

function TModelNotification.SetNotificationTaskDueAt(ATaskDueAt: TDateTime): Boolean;
begin
  Result  :=  FDLLMapping.SetNotificationTaskDueAt(ATaskDueAt);
end;

function TModelNotification.Terminate: Boolean;
begin
  Result  :=  FDLLMapping.Terminate;
end;

end.
