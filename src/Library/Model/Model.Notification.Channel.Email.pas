unit Model.Notification.Channel.Email;

interface

uses
  Model.Interfaces,
  Model.Notification.Channel.Abstract;

type
  TModelNotificationChannelEmail = class(TModelNotificationChannelAbstract)
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
  Service.SendEmail.Factory,
  Model.Notification.Channel.Manager;

{ TModelNotificationChannelEmail }

constructor TModelNotificationChannelEmail.Create(
  AParent: IModelNotificationConfig);
begin
  inherited Create(AParent);

  FChannel  :=  'Email';
end;

procedure TModelNotificationChannelEmail.InternalSend;
begin
  // Serviço que envia o Email
  TServiceSendEmailFactory.New
    .SendEmail
      .AddAddress(FParent.Entity.UserEmail, FParent.Entity.UserName)
      .Subject(FParent.Entity.Title)
      .Message(FParent.Entity.Message)
    .Send;
end;

initialization
  _NotificationChannelManager.RegisterChannel('Email', TModelNotificationChannelEmail);

finalization
  _NotificationChannelManager.UnRegisterChannel('Email');

end.
