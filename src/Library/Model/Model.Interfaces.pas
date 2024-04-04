unit Model.Interfaces;

interface

uses
  System.Generics.Collections,

  Commons,
  Model.Entity.User,
  Model.Entity.User.Subscription,
  Model.Entity.Notification,
  Model.Entity.Notification.Backup,
  Model.Entity.Notification.TaskDue;

type
  TEventOnSending = procedure(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass) of object;
  TEventOnSendingError = procedure(const AError: String; const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass) of object;
  TEventOnSendingSuccess = procedure(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass) of object;

  IModelNotificationBuild = interface;
  IModelNotificationSend = interface;
  IModelUserSubscription = interface;

  IModelUser = interface
    ['{7B467CEA-41F8-41A4-9971-DAE18863D57A}']
    function Entity: TUser; overload;
    function Entity(AValue: TUser): IModelUser; overload;
    function FindById(const AID: Integer): TUser;
    function Update(const AID: Integer): IModelUser;
    function Subscription: IModelUserSubscription;
  end;

  IModelUserSubscription = interface
    ['{A4DE4D92-5ABD-43BC-93D4-02712847B982}']
    function Entity: TUserSubscription; overload;
    function Entity(AValue: TUserSubscription): IModelUserSubscription; overload;
    function FindById(const AID: Integer): TUserSubscription;
    function FindByIDUser(const AIDUser: Integer): TUserSubscription;
    function Update(const AID: Integer): IModelUserSubscription;
  end;


  IModelNotification = interface
    ['{68EDFB24-3032-4453-9A2F-0946CA357955}']
    function Entity(AValue: Tnotification): IModelNotification; overload;
    function Entity: Tnotification; overload;
    function Build: IModelNotificationBuild;
    function Send: IModelNotificationSend;
    function ListOfChannels: TArray<String>;
    function ListOfFrequency: TArray<String>;
    function EventOnSending(AValue: TEventOnSending): IModelNotification;
    function EventOnSendingError(AValue: TEventOnSendingError): IModelNotification;
    function EventOnSendingSuccess(AValue: TEventOnSendingSuccess): IModelNotification;
  end;

  IModelNotificationConfig = interface
    ['{BD5E3C33-C46A-4720-B582-1046038C894E}']
    function This: IModelNotification;
    function Entity: Tnotification;
    function EventOnSending(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass): IModelNotification;
    function EventOnSendingError(const AError: String; const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass): IModelNotification;
    function EventOnSendingSuccess(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass): IModelNotification;
  end;

  IModelNotificationBuild = interface
    ['{691B8FE9-804E-4F50-82CD-9C0FACA20A77}']
    function Title(AValue: String): IModelNotificationBuild;
    function &Message(AValue: String): IModelNotificationBuild;
    function UserName(AValue: String): IModelNotificationBuild;
    function UserEmail(AValue: String): IModelNotificationBuild;
    function UserCellphone(AValue: String): IModelNotificationBuild;
    function Channels(AValue: TArray<String>): IModelNotificationBuild;
    function &End: IModelNotification;
  end;

  IModelNotificationChannel = interface
    ['{6FA5AB6E-F1F2-46A4-9517-6A86A761B10A}']
    function Channel: String;
    function Send: IModelNotificationChannel;
  end;

  IModelNotificationSend = interface
    ['{42519609-F56C-4354-873C-6963118E712E}']
    function Execute: Boolean;
  end;

  IModelNotificationSendInvoker = interface
    ['{26DEEBBB-D10A-4899-B6FF-E66F5C1770A5}']
    function Add(AValue : IModelNotificationChannel) : IModelNotificationSendInvoker;
    function Execute : IModelNotificationSendInvoker;
  end;

  IModelNotificationBackup = interface
    ['{F7A6B2DE-69E5-4DCE-8279-28F6251D9950}']
    function Entity: TNotificationBackup; overload;
    function Entity(AValue: TNotificationBackup): IModelNotificationBackup; overload;
    function Get: TNotificationBackup;
    function Update: IModelNotificationBackup;
  end;

  IModelNotificationTaskDue = interface
    ['{C2F9E725-350D-40E8-BA20-491DDB91CB9E}']
    function Entity: TNotificationTaskDue; overload;
    function Entity(AValue: TNotificationTaskDue): IModelNotificationTaskDue; overload;
    function Get: TNotificationTaskDue;
    function Update: IModelNotificationTaskDue;
  end;

implementation

end.
