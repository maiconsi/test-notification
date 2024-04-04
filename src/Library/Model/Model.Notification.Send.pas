unit Model.Notification.Send;

interface

uses
  Commons,
  Model.Interfaces,
  Model.Notification.Send.Invoker;

type
  TModelNotificationSend = class(TInterfacedObject, IModelNotificationSend)
  private
    FParent: IModelNotificationConfig;
    FInvoker: IModelNotificationSendInvoker;

    procedure BuildChannels;
  public
    constructor Create(AParent: IModelNotificationConfig);
    destructor Destroy; override;
    class function New(AParent: IModelNotificationConfig): IModelNotificationSend;

    function Execute: Boolean;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  Model.Notification.Channel.Manager;

{ TModelNotificationSend }

procedure TModelNotificationSend.BuildChannels;
var
  FChannel: String;
begin
  for FChannel in FParent.Entity.Channels do
    FInvoker.Add(_NotificationChannelManager.ResolveChannel(FChannel, FParent));
end;

constructor TModelNotificationSend.Create(AParent: IModelNotificationConfig);
begin
  FParent   :=  AParent;
  FInvoker  :=  TModelNotificationSendInvoker.New;
end;

destructor TModelNotificationSend.Destroy;
begin

  inherited;
end;

function TModelNotificationSend.Execute: Boolean;
begin
  FParent.Entity.Validate;

  BuildChannels;
  FInvoker.Execute;

  Result  :=  True;
end;

class function TModelNotificationSend.New(AParent: IModelNotificationConfig): IModelNotificationSend;
begin
  Result  :=  Self.Create(AParent);
end;

end.
