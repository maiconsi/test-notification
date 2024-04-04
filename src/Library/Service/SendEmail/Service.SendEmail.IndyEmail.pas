unit Service.SendEmail.IndyEmail;

interface

uses
  System.Classes,

  Service.SendEmail.Interfaces;

type
  TServiceSendEmailIndyMail = class(TInterfacedObject, IServiceSendEmailEngine)
  private
    FParent: IServiceSendEmailConfig;

    Procedure LoadConfig;
  public
    constructor Create(AParent: IServiceSendEmailConfig);
    destructor Destroy; override;
    class function New(AParent: IServiceSendEmailConfig): IServiceSendEmailEngine;

    function AddAddress(AEmail: String; AName: String): IServiceSendEmailEngine;
    function AddCC(AEmail: String; AName: String): IServiceSendEmailEngine;
    function AddAttachment(const AFileName: string): IServiceSendEmailEngine; overload;
    function AddAttachment(const AFileName: string; ADescription: string): IServiceSendEmailEngine; overload;
    function AddAttachment(AStream: TStream): IServiceSendEmailEngine; overload;
    function AddAttachment(AStream: TStream; ADescription: string): IServiceSendEmailEngine; overload;
    function Subject(AValue: String): IServiceSendEmailEngine;
    function &Message(AValue: String): IServiceSendEmailEngine;
    function Send: IServiceSendEmailEngine;
  end;

implementation

uses
  System.SysUtils,

  Utils;

{ TServiceSendEmailIndyMail }

function TServiceSendEmailIndyMail.AddAddress(AEmail,
  AName: String): IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  FACBrMail.AddAddress(AEmail, AName);
end;

function TServiceSendEmailIndyMail.AddAttachment(const AFileName: string;
  ADescription: string): IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  FACBrMail.AddAttachment(AFileName, ADescription);
end;

function TServiceSendEmailIndyMail.AddAttachment(
  const AFileName: string): IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  FACBrMail.AddAttachment(AFileName);
end;

function TServiceSendEmailIndyMail.AddAttachment(
  AStream: TStream): IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  FACBrMail.AddAttachment(AStream);
end;

function TServiceSendEmailIndyMail.AddAttachment(AStream: TStream;
  ADescription: string): IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  FACBrMail.AddAttachment(AStream, ADescription);
end;

function TServiceSendEmailIndyMail.AddCC(AEmail,
  AName: String): IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  FACBrMail.AddCC(AEmail, AName);
end;

constructor TServiceSendEmailIndyMail.Create(AParent: IServiceSendEmailConfig);
begin
  FParent :=  AParent;

//  FACBrMail :=  TACBrMail.Create(Nil);

  LoadConfig;
end;

destructor TServiceSendEmailIndyMail.Destroy;
begin
//  if Assigned(FACBrMail) then
//    FACBrMail.Free;

  inherited;
end;

procedure TServiceSendEmailIndyMail.LoadConfig;
begin
  // Email account
//  FACBrMail.FromName:= FParent.ContaEmail.from_name;
//  FACBrMail.From    := FParent.ContaEmail.email;
//  FACBrMail.Username:= FParent.ContaEmail.UserName;
//  FACBrMail.Password:= FParent.ContaEmail.Password;

  // Provider
//  FACBrMail.Host    := FParent.ContaEmail.provedor_email.host;
//  FACBrMail.Port    := FParent.ContaEmail.provedor_email.port.ToString;
//  FACBrMail.SetTLS  := FParent.ContaEmail.provedor_email.use_ssl;
//  FACBrMail.SetSSL  := FParent.ContaEmail.provedor_email.use_tls;

//  if FParent.ContaEmail.provedor_email.default_charset <> '' then
//    FACBrMail.DefaultCharset := TUtilEnumerator<TMailCharset>.ToEnum(FParent.ContaEmail.provedor_email.default_charset);

//  if FParent.ContaEmail.provedor_email.ide_charset <> '' then
//    FACBrMail.IDECharset     := TUtilEnumerator<TMailCharset>.ToEnum(FParent.ContaEmail.provedor_email.ide_charset);

  // Others
//  FACBrMail.IsHTML  :=  True;
end;

function TServiceSendEmailIndyMail.Message(
  AValue: String): IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  if FACBrMail.IsHTML then
//    FACBrMail.Body.Text :=  AValue
//  else
//    FACBrMail.AltBody.Text :=  AValue;
end;

class function TServiceSendEmailIndyMail.New(AParent: IServiceSendEmailConfig): IServiceSendEmailEngine;
begin
  Result := Self.Create(AParent);
end;

function TServiceSendEmailIndyMail.Send: IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  FACBrMail.Send();
end;

function TServiceSendEmailIndyMail.Subject(
  AValue: String): IServiceSendEmailEngine;
begin
  Result  :=  Self;

//  FACBrMail.Subject :=  AValue;
end;

end.

