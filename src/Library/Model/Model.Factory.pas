unit Model.Factory;

interface

uses
  Model.Factory.Interfaces,
  Model.Interfaces;

type
  TModelFactory = class(TInterfacedObject, IModelFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelFactory;

    function User: IModelUser;
    function UserSubscription: IModelUserSubscription;
    function Notification: IModelNotification;
    function NotificationBackup: IModelNotificationBackup;
    function NotificationTaskDue: IModelNotificationTaskDue;
  end;

implementation

uses
  Model.Notification,
  Model.User,
  Model.Notification.Backup,
  Model.Notification.TaskDue,
  Model.User.Subscription;

{ TModelFactory }

constructor TModelFactory.Create();
begin

end;

destructor TModelFactory.Destroy;
begin

  inherited;
end;

class function TModelFactory.New: IModelFactory;
begin
  Result  :=  Self.Create;
end;

function TModelFactory.Notification: IModelNotification;
begin
  Result  :=  TModelNotification.New;
end;

function TModelFactory.NotificationBackup: IModelNotificationBackup;
begin
  Result  :=  TModelNotificationBackup.New;
end;

function TModelFactory.NotificationTaskDue: IModelNotificationTaskDue;
begin
  Result  :=  TModelNotificationTaskDue.New;
end;

function TModelFactory.User: IModelUser;
begin
  Result  :=  TModelUser.New;
end;

function TModelFactory.UserSubscription: IModelUserSubscription;
begin
  Result  :=  TModelUserSubscription.New;
end;

end.
