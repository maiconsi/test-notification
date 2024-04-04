unit Model.Factory.Interfaces;

interface

uses
  Model.Interfaces;

type
  IModelFactory = interface
    ['{4C88332C-0EE4-4AE6-922C-48F986FCE93D}']
    function User: IModelUser;
    function Notification: IModelNotification;
    function NotificationDLLMapping: IModelNotificationDLLMapping;
  end;
implementation

end.
