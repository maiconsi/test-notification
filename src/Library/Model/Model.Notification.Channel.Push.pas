unit Model.Notification.Channel.Push;

interface

uses
  Model.Interfaces,
  Model.Notification.Channel.Abstract;

type
  TModelNotificationChannelPush = class(TModelNotificationChannelAbstract)
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
  Service.SendPush.Factory,
  Model.Notification.Channel.Manager;

{ TModelNotificationChannelPush }

constructor TModelNotificationChannelPush.Create(
  AParent: IModelNotificationConfig);
begin
  inherited Create(AParent);

  FChannel  :=  'Push';
end;

procedure TModelNotificationChannelPush.InternalSend;
begin
  // Função/Serviço que envia o Push
  TServiceSendPushFactory.New
    .SendPush
      .CellPhone(FParent.Entity.UserCellphone)
      .Message(FParent.Entity.Message)
    .Send;
end;

initialization
  _NotificationChannelManager.RegisterChannel('Push', TModelNotificationChannelPush);

finalization
  _NotificationChannelManager.UnRegisterChannel('Push');

end.

