unit Service.SendPush.Factory;

interface

uses
  Service.SendPush.Factory.Interfaces,
  Service.SendPush.Interfaces;

type
  TServiceSendPushFactory = class(TInterfacedObject, IServiceSendPushFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IServiceSendPushFactory;

    function SendPush: IServiceSendPush;
  end;

implementation

uses
  Service.SendPush;

{ TServiceSendPushFactory }

constructor TServiceSendPushFactory.Create;
begin

end;

destructor TServiceSendPushFactory.Destroy;
begin

  inherited;
end;

class function TServiceSendPushFactory.New: IServiceSendPushFactory;
begin
  Result := Self.Create();
end;

function TServiceSendPushFactory.SendPush: IServiceSendPush;
begin
  Result  :=  TServiceSendPush.New;
end;

end.

