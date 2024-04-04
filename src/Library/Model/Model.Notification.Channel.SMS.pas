unit Model.Notification.Channel.SMS;

interface

uses
  Model.Interfaces,
  Model.Notification.Channel.Abstract;

type
  TModelNotificationChannelSMS = class(TModelNotificationChannelAbstract)
  protected
    procedure InternalSend; override;
  public
    constructor Create(AParent: IModelNotificationConfig); override;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,

  Commons,
  Service.SendSMS.Factory,
  Model.Notification.Channel.Manager;

{ TModelNotificationChannelSMS }

constructor TModelNotificationChannelSMS.Create(
  AParent: IModelNotificationConfig);
begin
  inherited Create(AParent);

  FChannel  :=  'SMS';
end;

procedure TModelNotificationChannelSMS.InternalSend;
begin
  // Função/Serviço que envia o SMS
  TServiceSendSMSFactory.New
    .SendSMS
      .CellPhone(FParent.Entity.UserCellphone)
      .Message(FParent.Entity.Message)
    .Send;
end;

initialization
  _NotificationChannelManager.RegisterChannel('SMS', TModelNotificationChannelSMS);

finalization
  _NotificationChannelManager.UnRegisterChannel('SMS');

end.

