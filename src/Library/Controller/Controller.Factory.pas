unit Controller.Factory;

interface

uses
  Controller.Factory.Interfaces,
  Controller.Interfaces;

type
  TControllerFactory = class(TInterfacedObject, IControllerFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IControllerFactory;

    function User: IControllerUser;
    function UserSubscription(AParent: IControllerUserConfig): IControllerUserSubscription;
    function NotificationManager: IControllerNotificationManager;
    function NotificationBackup: IControllerNotificationBackup;
    function NotificationTaskDue: IControllerNotificationTaskDue;
  end;

implementation

uses
  Controller.Notification.Manager,
  Controller.Notification.Backup,
  Controller.Notification.TaskDue,
  Controller.User,
  Controller.User.Subscription;

{ TControllerFactory }

constructor TControllerFactory.Create();
begin

end;

destructor TControllerFactory.Destroy;
begin

  inherited;
end;

class function TControllerFactory.New: IControllerFactory;
begin
  Result  :=  Self.Create;
end;

function TControllerFactory.NotificationBackup: IControllerNotificationBackup;
begin
  Result  :=  TControllerNotificationBackup.New;
end;

function TControllerFactory.NotificationManager: IControllerNotificationManager;
begin
  Result  :=  TControllerNotificationManager.New;
end;

function TControllerFactory.NotificationTaskDue: IControllerNotificationTaskDue;
begin
  Result  :=  TControllerNotificationTaskDue.New;
end;

function TControllerFactory.User: IControllerUser;
begin
  Result  :=  TControllerUser.New;
end;

function TControllerFactory.UserSubscription(AParent: IControllerUserConfig): IControllerUserSubscription;
begin
  Result  :=  TControllerUserSubscription.New(AParent);
end;

end.
