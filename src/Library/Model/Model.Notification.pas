unit Model.Notification;

interface

uses
  System.Classes,
  System.Generics.Collections,

  Commons,
  Model.Interfaces,
  Model.Entity.Notification;

type
  TModelNotification = class(TInterfacedObject, IModelNotification, IModelNotificationConfig)
  private
    FEntity: Tnotification;
    FEventOnSending: TEventOnSending;
    FEventOnSendingError: TEventOnSendingError;
    FEventOnSendingSuccess: TEventOnSendingSuccess;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IModelNotification;

    //IModelNotification
    function Entity(AValue: Tnotification): IModelNotification; overload;
    function Entity: Tnotification; overload;
    function Build: IModelNotificationBuild;
    function Send: IModelNotificationSend;
    function ListOfChannels: TArray<String>;
    function ListOfFrequency: TArray<String>;
    function EventOnSending(AValue: TEventOnSending): IModelNotification; overload;
    function EventOnSendingError(AValue: TEventOnSendingError): IModelNotification; overload;
    function EventOnSendingSuccess(AValue: TEventOnSendingSuccess): IModelNotification; overload;

    //IModelNotificationConfig
    function This: IModelNotification;
    function EventOnSending(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass): IModelNotification; overload;
    function EventOnSendingError(const AError: String; const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass): IModelNotification; overload;
    function EventOnSendingSuccess(const ATitle, AMessage, AUserName: String; const AChannel: String; const AClass: TClass): IModelNotification; overload;
  end;

implementation

uses
  Model.Notification.Send,
  Model.Notification.Build,
  Model.Notification.Channel.Manager;

{ TModelNotification }

function TModelNotification.Build: IModelNotificationBuild;
begin
  Result  :=  TModelNotificationBuild.New(Self);
end;

constructor TModelNotification.Create();
begin
  FEntity   :=  Tnotification.Create;
end;

destructor TModelNotification.Destroy;
begin
  Entity(nil);

  inherited;
end;

function TModelNotification.Entity: Tnotification;
begin
  Result  :=  FEntity;
end;

function TModelNotification.Entity(AValue: Tnotification): IModelNotification;
begin
  Result  :=  Self;

  if Assigned(FEntity) then
    FEntity.Free;

  FEntity :=  AValue;
end;

class function TModelNotification.New: IModelNotification;
begin
  Result  :=  Self.Create;
end;

function TModelNotification.EventOnSending(
  AValue: TEventOnSending): IModelNotification;
begin
  Result  :=  Self;

  FEventOnSending :=  AValue;
end;

function TModelNotification.EventOnSendingError(
  AValue: TEventOnSendingError): IModelNotification;
begin
  Result  :=  Self;

  FEventOnSendingError :=  AValue;
end;

function TModelNotification.EventOnSendingSuccess(
  AValue: TEventOnSendingSuccess): IModelNotification;
begin
  Result  :=  Self;

  FEventOnSendingSuccess :=  AValue;
end;

function TModelNotification.Send: IModelNotificationSend;
begin
  Result  :=  TModelNotificationSend.New(Self);
end;

function TModelNotification.This: IModelNotification;
begin
  Result  :=  Self;
end;

function TModelNotification.EventOnSending(const ATitle, AMessage,
  AUserName: String; const AChannel: String;
  const AClass: TClass): IModelNotification;
begin
  if Assigned(FEventOnSending) then
    FEventOnSending(ATitle, AMessage, AUserName, AChannel, AClass);
end;

function TModelNotification.EventOnSendingError(const AError, ATitle, AMessage,
  AUserName: String; const AChannel: String;
  const AClass: TClass): IModelNotification;
begin
  if Assigned(FEventOnSendingError) then
    FEventOnSendingError(AError, ATitle, AMessage, AUserName, AChannel, AClass);
end;

function TModelNotification.EventOnSendingSuccess(const ATitle, AMessage,
  AUserName: String; const AChannel: String;
  const AClass: TClass): IModelNotification;
begin
  if Assigned(FEventOnSendingSuccess) then
    FEventOnSendingSuccess(ATitle, AMessage, AUserName, AChannel, AClass);
end;

function TModelNotification.ListOfChannels: TArray<String>;
begin
  Result  :=  _NotificationChannelManager.GetListOfChannels;
end;

function TModelNotification.ListOfFrequency: TArray<String>;
begin
  Result  :=  _NotificationChannelManager.GetListOfFrequency;
end;

end.
