unit Service.SendSMS.Factory;

interface

uses
  Service.SendSMS.Factory.Interfaces,
  Service.SendSMS.Interfaces;

type
  TServiceSendSMSFactory = class(TInterfacedObject, IServiceSendSMSFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IServiceSendSMSFactory;

    function SendSMS: IServiceSendSMS;
  end;

implementation

uses
  Service.SendSMS;

{ TServiceSendSMSFactory }

constructor TServiceSendSMSFactory.Create;
begin

end;

destructor TServiceSendSMSFactory.Destroy;
begin

  inherited;
end;

class function TServiceSendSMSFactory.New: IServiceSendSMSFactory;
begin
  Result := Self.Create();
end;

function TServiceSendSMSFactory.SendSMS: IServiceSendSMS;
begin
  Result  :=  TServiceSendSMS.New;
end;

end.

