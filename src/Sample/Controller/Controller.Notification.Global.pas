unit Controller.Notification.Global;

interface

uses
  Controller.Interfaces;

var
  _ControllerNotification: IControllerNotification;

implementation

uses
  Controller.Factory;

initialization
  _ControllerNotification :=  TControllerFactory.New.Notification;
  _ControllerNotification.Initialize;

finalization
  _ControllerNotification.Terminate;

end.
