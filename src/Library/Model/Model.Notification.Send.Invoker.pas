unit Model.Notification.Send.Invoker;

interface

uses
  System.Generics.Collections,

  Model.Interfaces;

type
  TModelNotificationSendInvoker = class(TInterfacedObject, IModelNotificationSendInvoker)
  private
    FLista : TList<IModelNotificationChannel>;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelNotificationSendInvoker;

    function Add(Value : IModelNotificationChannel) : IModelNotificationSendInvoker;
    function Execute : IModelNotificationSendInvoker;
  end;

implementation

uses
  System.SysUtils;

{ TModelNotificationSendInvoker }

function TModelNotificationSendInvoker.Add(Value : IModelNotificationChannel) : IModelNotificationSendInvoker;
begin
  Result := Self;
  FLista.Add(Value);
end;

constructor TModelNotificationSendInvoker.Create();
begin
  FLista := TList<IModelNotificationChannel>.Create;
end;

destructor TModelNotificationSendInvoker.Destroy;
begin
  FreeAndNil(FLista);

  inherited;
end;

function TModelNotificationSendInvoker.Execute: IModelNotificationSendInvoker;
var
  LChannel: IModelNotificationChannel;
begin
  Result := Self;

  for LChannel in FLista do
    LChannel.Send;
end;

class function TModelNotificationSendInvoker.New(): IModelNotificationSendInvoker;
begin
  Result := Self.Create();
end;

end.

