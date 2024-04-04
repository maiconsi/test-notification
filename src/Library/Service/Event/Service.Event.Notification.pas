unit Service.Event.Notification;

interface

uses
  Model.Entity.Notification;

type
  TEventsNotification = class
  public
    class procedure OnSending(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass);
    class procedure OnSendingError(const AError: String; const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass);
    class procedure OnSendingSuccess(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass);
  end;

implementation

uses
  System.SysUtils,

  Service.Logger;

{ TEventsNotification }

class procedure TEventsNotification.OnSending(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass);
var
  LMessageFull: String;
begin
  LMessageFull  :=  Format('Enviando notificação[%s] para usuário[%s] no canal[%s]', [ATitle, AUserName, AChannel]);
  TLogger.GetInstance.Add(LMessageFull);
end;

class procedure TEventsNotification.OnSendingError(const AError: String; const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass);
begin
  TLogger.GetInstance.Add(Format('Ocorreu o erro[%s] ao enviar a notificação[%s] para usuário[%s] no canal[%s]', [AError, ATitle, AUserName, AChannel]));
end;

class procedure TEventsNotification.OnSendingSuccess(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass);
begin
  TLogger.GetInstance.Add(Format('Notificação[%s] enviada para usuário[%s] no canal[%s] com sucesso', [ATitle, AUserName, AChannel]));
end;

end.
