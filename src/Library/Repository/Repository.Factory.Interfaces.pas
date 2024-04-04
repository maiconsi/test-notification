unit Repository.Factory.Interfaces;

interface

uses
  Repository.Interfaces;

type
  IRepositoryFactory = interface
    ['{9D5201FD-6CF3-479B-86BC-6270EB74FF30}']
    function User: IRepositoryUser;
    function UserSubscription: IRepositoryUserSubscription;
    function NotificationBackup: IRepositoryNotificationBackup;
    function NotificationTaskDue: IRepositoryNotificationTaskDue;
  end;

implementation

end.

