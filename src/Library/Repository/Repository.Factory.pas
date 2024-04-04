unit Repository.Factory;

interface

uses
  Repository.Factory.Interfaces,
  Repository.Interfaces;

type
  TRepositoryFactory = class(TInterfacedObject, IRepositoryFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IRepositoryFactory;

    function User: IRepositoryUser;
    function UserSubscription: IRepositoryUserSubscription;
    function NotificationBackup: IRepositoryNotificationBackup;
    function NotificationTaskDue: IRepositoryNotificationTaskDue;
  end;

implementation

uses
  Repository.User,
  Repository.Notification.TaskDue,
  Repository.Notification.Backup,
  Repository.User.Subscription;

{ TRepositoryFactory }

constructor TRepositoryFactory.Create();
begin

end;

destructor TRepositoryFactory.Destroy;
begin

  inherited;
end;

class function TRepositoryFactory.New: IRepositoryFactory;
begin
  Result  :=  Self.Create;
end;

function TRepositoryFactory.NotificationBackup: IRepositoryNotificationBackup;
begin
  Result  :=  TRepositoryNotificationBackup.New;
end;

function TRepositoryFactory.NotificationTaskDue: IRepositoryNotificationTaskDue;
begin
  Result  :=  TRepositoryNotificationTaskDue.New;
end;

function TRepositoryFactory.User: IRepositoryUser;
begin
  Result  :=  TRepositoryUser.New;
end;

function TRepositoryFactory.UserSubscription: IRepositoryUserSubscription;
begin
  Result  :=  TRepositoryUserSubscription.New;
end;

end.
