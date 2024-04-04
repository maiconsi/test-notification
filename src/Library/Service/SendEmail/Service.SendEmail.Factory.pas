unit Service.SendEmail.Factory;

interface

uses
  Service.SendEmail.Factory.Interfaces,
  Service.SendEmail.Interfaces;

type
  TServiceSendEmailFactory = class(TInterfacedObject, IServiceSendEmailFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IServiceSendEmailFactory;

    function SendEmail: IServiceSendEmail;
    function Engine(AParent: IServiceSendEmailConfig): IServiceSendEmailEngine;
  end;

implementation

uses
  Service.SendEmail.IndyEmail,
  Service.SendEmail;

{ TServiceSendEmailFactory }

constructor TServiceSendEmailFactory.Create;
begin

end;

destructor TServiceSendEmailFactory.Destroy;
begin

  inherited;
end;

function TServiceSendEmailFactory.Engine(
  AParent: IServiceSendEmailConfig): IServiceSendEmailEngine;
begin
  Result  :=  TServiceSendEmailIndyMail.New(AParent);
end;

class function TServiceSendEmailFactory.New: IServiceSendEmailFactory;
begin
  Result := Self.Create();
end;

function TServiceSendEmailFactory.SendEmail: IServiceSendEmail;
begin
  Result  :=  TServiceSendEmail.New;
end;

end.

