unit Controller.Interfaces;

interface

uses
  Model.Entity.User,
  Model.Interfaces,
  Model.Entity.Notification.Settings;

type
  IControllerUser = interface
    ['{DA399904-853B-41C4-9951-EABE91EBCA68}']
    function GetCurrentUser: IControllerUser;
    function Entity: TUser;
    function Photo: IModelUserPhoto;
    function Save: IControllerUser;
  end;

  IControllerNotification = interface
    ['{A99C76F7-9973-45F2-AAB3-CEF72A564036}']
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

end.
