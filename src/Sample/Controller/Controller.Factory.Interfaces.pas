unit Controller.Factory.Interfaces;

interface

uses
  Controller.Interfaces;

type
  IControllerFactory = interface
    ['{EEDD11A1-91D9-4020-9510-DBA8CED678E2}']
    function User: IControllerUser;
    function Notification: IControllerNotification;
  end;

implementation

end.
