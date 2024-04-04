unit Service.SendEmail;

interface

uses
  System.Classes,

  Service.SendEmail.Interfaces;

type
  TServiceSendEmail = class(TInterfacedObject, IServiceSendEmail, IServiceSendEmailConfig)
  private
    FEngine: IServiceSendEmailEngine;

    procedure ValidateData;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IServiceSendEmail;

    // IServiceSendEmail
    function AddAddress(AEmail: String; AName: String): IServiceSendEmail;
    function AddCC(AEmail: String; AName: String): IServiceSendEmail;
    function AddAttachment(const AFileName: string): IServiceSendEmail; overload;
    function AddAttachment(const AFileName: string; ADescription: string): IServiceSendEmail; overload;
    function AddAttachment(AStream: TStream): IServiceSendEmail; overload;
    function AddAttachment(AStream: TStream; ADescription: string): IServiceSendEmail; overload;
    function Subject(AValue: String): IServiceSendEmail;
    function &Message(AValue: String): IServiceSendEmail;
    function Send: IServiceSendEmail;

    // IServiceSendEmailConfig
    function This: IServiceSendEmail;
  end;

implementation

uses
  System.SysUtils,

  Commons,
  Service.SendEmail.Factory;

{ TServiceSendEmail }

function TServiceSendEmail.AddAddress(AEmail, AName: String): IServiceSendEmail;
begin
  Result  :=  Self;

  FEngine.AddAddress(AEmail, AName);
end;

function TServiceSendEmail.AddAttachment(
  const AFileName: string): IServiceSendEmail;
begin
  Result  :=  Self;

  FEngine.AddAttachment(AFileName);
end;

function TServiceSendEmail.AddAttachment(const AFileName: string;
  ADescription: string): IServiceSendEmail;
begin
  Result  :=  Self;

  FEngine.AddAttachment(AFileName, ADescription);
end;

function TServiceSendEmail.AddAttachment(AStream: TStream): IServiceSendEmail;
begin
  Result  :=  Self;

  FEngine.AddAttachment(AStream);
end;

function TServiceSendEmail.AddAttachment(AStream: TStream;
  ADescription: string): IServiceSendEmail;
begin
  Result  :=  Self;

  FEngine.AddAttachment(AStream, ADescription);
end;

function TServiceSendEmail.AddCC(AEmail, AName: String): IServiceSendEmail;
begin
  Result  :=  Self;

  FEngine.AddCC(AEmail, AName);
end;

constructor TServiceSendEmail.Create;
begin
  FEngine :=  TServiceSendEmailFactory.New.Engine(Self);
end;

destructor TServiceSendEmail.Destroy;
begin

  inherited;
end;

function TServiceSendEmail.Message(AValue: String): IServiceSendEmail;
begin
  Result  :=  Self;

  FEngine.Message(AValue);
end;

class function TServiceSendEmail.New: IServiceSendEmail;
begin
  Result := Self.Create;
end;

function TServiceSendEmail.Send: IServiceSendEmail;
begin
  Result  :=  Self;

  try
    ValidateData;

    FEngine.Send;
  except
     on Exc: Exception do
      raise EServiceException.Create('Error trying to send the email',
                                    Format('[%s.Send] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TServiceSendEmail.Subject(AValue: String): IServiceSendEmail;
begin
  Result  :=  Self;

  FEngine.Subject(AValue);
end;

function TServiceSendEmail.This: IServiceSendEmail;
begin
  Result  :=  Self;
end;

procedure TServiceSendEmail.ValidateData;
begin
  // Validations

end;

end.

