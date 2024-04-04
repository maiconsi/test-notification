unit Model.Factory.Interfaces;

interface

uses
  Model.Interfaces;

type
  IModelFactory = interface
    ['{DF0CD881-97D1-48EE-A204-05DB56BC24EC}']
    function User: IModelUser;
    function Notification: IModelNotification;
    function UserSubscription: IModelUserSubscription;
    function NotificationBackup: IModelNotificationBackup;
    function NotificationTaskDue: IModelNotificationTaskDue;
  end;

implementation

end.
