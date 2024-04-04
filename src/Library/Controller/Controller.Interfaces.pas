unit Controller.Interfaces;

interface

uses
  System.SysUtils,

  Model.Interfaces,
  Model.Entity.Notification.Backup,
  Model.Entity.Notification.TaskDue,
  Model.Entity.User,
  Model.Entity.User.Subscription;

type
  IControllerNotificationBackup = interface;
  IControllerNotificationTaskDue = interface;
  IControllerUserSubscription = interface;

  IControllerUser = interface
    ['{9B2A7C0A-9492-4C6E-91D1-28730B5A4CB6}']
    function Entity: TUser; overload;
    function Entity(AValue: TUser): IControllerUser; overload;
    function FindById(const AID: Integer): TUser;
    function Update(const AID: Integer): IControllerUser;
    function Subscription: IControllerUserSubscription;
  end;

  IControllerUserConfig = interface
    ['{F291EC69-E619-4088-834D-8822FA2F6750}']
    function This: IControllerUser;
    function User: IModelUser;
  end;

  IControllerUserSubscription = interface
    ['{E1A539E5-9ADE-4E02-A395-5B85A50EFC98}']
    function Entity: TUserSubscription; overload;
    function Entity(AValue: TUserSubscription): IControllerUserSubscription; overload;
    function FindById(const AID: Integer): TUserSubscription;
    function FindByIDUser(const AIDUser: Integer): TUserSubscription;
    function Update(const AID: Integer): IControllerUserSubscription;
  end;

  IControllerNotificationManager = interface
    ['{2E6ED145-D70F-4D27-AFF4-E29E2C38649D}']
    function Initialize: IControllerNotificationManager;
    function Terminate: IControllerNotificationManager;
    function ListOfChannels: TArray<String>;
    function ListOfFrequency: TArray<String>;
  end;

  IControllerNotificationBackup = interface
    ['{5BE01F14-46F1-4F73-8125-28AE9A9A5245}']
    function Get: TNotificationBackup;
    function Entity: TNotificationBackup;
    function SetBackupExecutedAt(AExecutedAt: TDateTime): IControllerNotificationBackup;
    function SetNotifiedAt(ANotifiedAt: TDateTime): IControllerNotificationBackup;
    function SendNotificationNow: IControllerNotificationBackup;
  end;

  IControllerNotificationTaskDue = interface
    ['{3EA20409-8B89-4B34-8C5B-92EF34F6E720}']
    function Get: TNotificationTaskDue;
    function Entity: TNotificationTaskDue;
    function SetTaskDueAt(ATaskDueAt: TDateTime): IControllerNotificationTaskDue;
    function SetNotifiedAt(ANotifiedAt: TDateTime): IControllerNotificationTaskDue;
    function SendNotificationNow: IControllerNotificationTaskDue;
  end;

implementation

end.
