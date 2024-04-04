unit Model.Entity.Notification;

interface

uses
  DB,
  Classes,
  SysUtils,
  Generics.Collections,

  Commons,
  Entity.Abstract;

type
  Tnotification = class(TEntityAbstract)
  private
    { Private declarations }
    FUserEmail: String;
    FMessage: String;
    FUserCellphone: String;
    FTitle: String;
    FUserName: String;
    FChannels: TArray<String>;
  protected
    { Protected declarations }
    procedure BasicValidation; override;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    class function New(const ATitle, AMessage, AUserName, AUserEmail,
      AUserCellpone: string; const AChannels: TArray<String>): Tnotification;

    function AddChannel(AChannel: String): Tnotification;

    procedure Validate; override;

    property Title: String read FTitle write FTitle;
    property &Message: String read FMessage write FMessage;
    property UserName: String read FUserName write FUserName;
    property UserEmail: String read FUserEmail write FUserEmail;
    property UserCellphone: String read FUserCellphone write FUserCellphone;
    property Channels: TArray<String> read FChannels write FChannels;
  end;

implementation

uses
  System.Generics.Collections,

  Utils;


{ Tnotification }

function Tnotification.AddChannel(AChannel: String): Tnotification;
begin
  Result  :=  Self;

  SetLength(FChannels, Length(FChannels) + 1);
  FChannels[High(FChannels)] := AChannel;
end;

procedure Tnotification.BasicValidation;
begin
  inherited;
  if FTitle.IsEmpty then
    raise EEntityValidation.Create('Titulo da notificação não pode ser vazio',
                                    Format('[%s.BasicValidation] Informe um Titulo válido para o campo "Title"', [Self.ToString]));

  if FMessage.IsEmpty then
    raise EEntityValidation.Create('Mensagem da notificação não pode ser vazia',
                                    Format('[%s.BasicValidation] Informe uma mensagem válida para o campo "Message"', [Self.ToString]));

  if FUserName.IsEmpty then
    raise EEntityValidation.Create('Usuário da notificação não pode ser vazio',
                                    Format('[%s.BasicValidation] Informe um usuário para o campo "UserName"', [Self.ToString]));

end;

constructor Tnotification.Create;
begin

end;

destructor Tnotification.Destroy;
begin

  inherited;
end;

class function Tnotification.New(const ATitle, AMessage, AUserName, AUserEmail,
  AUserCellpone: string; const AChannels: TArray<String>): Tnotification;
begin
  Result  :=  Self.Create;

  Result.Title        :=  ATitle;
  Result.Message      :=  AMessage;
  Result.UserName     :=  AUserName;
  Result.UserEmail    :=  AUserEmail;
  Result.UserCellphone:=  AUserCellpone;
  Result.Channels     :=  AChannels;
end;

procedure Tnotification.Validate;
begin
  if Length(FChannels) = 0 then
    raise EEntityValidation.Create('Nenhum canal selecionado',
                                    Format('[%s.Validate] É necessário informar ao menos um canal de notificação para que a operação possa ser executada', [Self.ToString]));
  inherited;
end;

end.


