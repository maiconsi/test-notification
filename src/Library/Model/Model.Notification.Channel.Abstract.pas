unit Model.Notification.Channel.Abstract;

interface

uses
  System.Classes,

  Commons,
  Model.Interfaces,
  Model.Entity.Notification;

type
  TModelNotificationChannelAbstract = class(TInterfacedObject, IModelNotificationChannel)
  private
  protected
    FParent: IModelNotificationConfig;
    FChannel: String;

    procedure InternalSend; virtual;
    procedure NotifyEventOnSending;
    procedure NotifyEventOnSendingError(const AError: String);
    procedure NotifyEventOnSendingSuccess;
  public
    constructor Create(AParent: IModelNotificationConfig); virtual;

    class function New(AParent: IModelNotificationConfig): IModelNotificationChannel;

    function Channel: String;
    function Send: IModelNotificationChannel;
  end;

implementation

uses
  System.SysUtils;

{ TModelNotificationChannelAbstract }

constructor TModelNotificationChannelAbstract.Create(AParent: IModelNotificationConfig);
begin
  FParent :=  AParent;
  FChannel:=  '';
end;

procedure TModelNotificationChannelAbstract.InternalSend;
begin
  raise Exception.Create('The internal send method must be implemented in the child class');
end;

function TModelNotificationChannelAbstract.Channel: String;
begin
  if FChannel = '' then
    raise Exception.Create(Format('Description channel not informed for classe[%s]', [Self.ToString]));

  Result  :=  FChannel;
end;

class function TModelNotificationChannelAbstract.New(
  AParent: IModelNotificationConfig): IModelNotificationChannel;
begin
  Result  :=  Self.Create(AParent);
end;

procedure TModelNotificationChannelAbstract.NotifyEventOnSending;
begin
  FParent.EventOnSending(FParent.Entity.Title,
                         FParent.Entity.Message,
                         FParent.Entity.UserName,
                         FChannel,
                         Self.ClassType);
end;

procedure TModelNotificationChannelAbstract.NotifyEventOnSendingError(
  const AError: String);
begin
  FParent.EventOnSendingError(AError,
                              FParent.Entity.Title,
                              FParent.Entity.Message,
                              FParent.Entity.UserName,
                              FChannel,
                              Self.ClassType);
end;

procedure TModelNotificationChannelAbstract.NotifyEventOnSendingSuccess;
begin
  FParent.EventOnSendingSuccess(FParent.Entity.Title,
                                FParent.Entity.Message,
                                FParent.Entity.UserName,
                                FChannel,
                                Self.ClassType);
end;

function TModelNotificationChannelAbstract.Send: IModelNotificationChannel;
begin
  Result  :=  Self;

  try
    NotifyEventOnSending;

    InternalSend;

    NotifyEventOnSendingSuccess;
  except
    on Exc: Exception do
      NotifyEventOnSendingError(Exc.Message);
  end;
end;

end.

