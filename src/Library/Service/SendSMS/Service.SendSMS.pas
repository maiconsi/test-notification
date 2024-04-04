unit Service.SendSMS;

interface

uses
  Service.SendSMS.Interfaces;

type
  TServiceSendSMS = class(TInterfacedObject, IServiceSendSMS)
  private
    FCellPhone: String;
    FMessage: String;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IServiceSendSMS;

    function CellPhone(AValue: String): IServiceSendSMS;
    function &Message(AValue: String): IServiceSendSMS;
    function Send: IServiceSendSMS;
  end;

implementation

{ TServiceSendSMS }

function TServiceSendSMS.CellPhone(AValue: String): IServiceSendSMS;
begin
  Result  :=  Self;

  FCellPhone  := AValue;
end;

constructor TServiceSendSMS.Create();
begin

end;

destructor TServiceSendSMS.Destroy;
begin

  inherited;
end;

function TServiceSendSMS.Message(AValue: String): IServiceSendSMS;
begin
  Result  :=  Self;

  FMessage	:= AValue;
end;

class function TServiceSendSMS.New: IServiceSendSMS;
begin
  Result  :=  Self.Create;
end;

function TServiceSendSMS.Send: IServiceSendSMS;
begin
  Result  :=  Self;

  // Criar código para enviar SMS
end;

end.
