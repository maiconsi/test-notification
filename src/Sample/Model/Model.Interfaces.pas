unit Model.Interfaces;

interface

uses
  System.Classes,
  System.Generics.Collections,

  Commons,
  Model.Entity.User, Model.Entity.Notification.Settings;

type
  IModelUserPhoto = interface;

  IModelUser = interface
    ['{7B467CEA-41F8-41A4-9971-DAE18863D57A}']
    function Entity: Tuser; overload;
    function Entity(AValue: Tuser): IModelUser; overload;
    function GetById(const AID: Integer): Tuser;
    function Update(const AID: Integer): IModelUser;
    function Photo: IModelUserPhoto;
  end;

  IModelUserConfig = interface
    ['{A06A8C14-C3A8-4D68-B258-55E069138794}']
    function This: IModelUser;
  end;

  IModelUserPhoto = interface
    ['{2D8E1B97-0F71-4526-BA5F-4FF146619FAD}']
    function Get: TStream; overload;
    function Get(const AID: Integer): TStream; overload;
    function Save(APhoto: TStream): IModelUserPhoto; overload;
    function Save(const AID: Integer; APhoto: TStream): IModelUserPhoto; overload;
    function Delete: IModelUserPhoto; overload;
    function Delete(const AID: Integer): IModelUserPhoto; overload;
    function &End: IModelUser;
  end;


  IModelNotificationDLLMapping = interface
    ['{46E83399-17F8-4563-9EA2-65028AAEAE01}']
    function Initialize: Boolean;
    function Terminate: Boolean;
    function GetUser(AIDUser: Integer; out ALogin: String; out AName: String; out AEmail: String; out ACellphone: String): Boolean;
    function SaveUser(AIDUser: Integer; ALogin: String; AName: String; AEmail: String; ACellphone: String): Boolean;
    function GetListOfChannels: TArray<String>;
    function GetListOfFrequency: TArray<String>;
    function GetUserNotificationBackup(AIDUser: Integer; out AActive: Boolean; out AFrequency: String; out AChannels: TArray<String>): Boolean;
    function SaveUserNotificationBackup(AIDUser: Integer; AActive: Boolean; AFrequency: String; AChannels: TArray<String>): Boolean;
    function GetUserNotificationTaskDue(AIDUser: Integer; out AActive: Boolean; out AFrequency: String; out AChannels: TArray<String>): Boolean;
    function SaveUserNotificationTaskDue(AIDUser: Integer; AActive: Boolean; AFrequency: String; AChannels: TArray<String>): Boolean;
    function SendNotificationBackupNow: Boolean;
    function SendNotificationTaskDueNow: Boolean;
    function SetNotificationBackupExecutedAt(AExecutedAt: TDateTime): Boolean;
    function SetNotificationTaskDueAt(ATaskDueAt: TDateTime): Boolean;
  end;

  IModelNotification = interface
    ['{32C89923-D723-4692-8305-6C8B6C1D1033}']
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

end.
