unit Controller.Factory.Interfaces;

interface

uses
  Controller.Interfaces;

type
  IControllerFactory = interface
    ['{2E0E1959-BC3A-4508-B911-FED855B2894E}']
    function User: IControllerUser;
    function UserSubscription(AParent: IControllerUserConfig): IControllerUserSubscription;
    function NotificationManager: IControllerNotificationManager;
    function NotificationBackup: IControllerNotificationBackup;
    function NotificationTaskDue: IControllerNotificationTaskDue;
  end;

implementation

end.
