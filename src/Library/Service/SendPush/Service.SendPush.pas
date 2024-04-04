unit Service.SendPush;

interface

uses
  Service.SendPush.Interfaces;

type
  TServiceSendPush = class(TInterfacedObject, IServiceSendPush)
  private
    FCellPhone: String;
    FMessage: String;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IServiceSendPush;

    function CellPhone(AValue: String): IServiceSendPush;
    function &Message(AValue: String): IServiceSendPush;
    function Send: IServiceSendPush;
  end;

implementation

{ TServiceSendPush }

function TServiceSendPush.CellPhone(AValue: String): IServiceSendPush;
begin
  Result  :=  Self;

  FCellPhone  := AValue;
end;

constructor TServiceSendPush.Create();
begin

end;

destructor TServiceSendPush.Destroy;
begin

  inherited;
end;

function TServiceSendPush.Message(AValue: String): IServiceSendPush;
begin
  Result  :=  Self;

  FMessage	:= AValue;
end;

class function TServiceSendPush.New: IServiceSendPush;
begin
  Result  :=  Self.Create;
end;

function TServiceSendPush.Send: IServiceSendPush;
begin
  Result  :=  Self;

  // Criar código para enviar Push
end;

end.

