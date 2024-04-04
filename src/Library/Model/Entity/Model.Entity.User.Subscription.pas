unit Model.Entity.User.Subscription;

interface

uses
  Entity.Abstract,
  Model.Entity.Notification.Settings;

type
  TUserSubscription = class(TEntityAbstract)
  private
    FId: Integer;
    FIdUser: Integer;
    FBackup: TNoticationSettings;
    FTaskDue: TNoticationSettings;
  protected
    procedure BasicValidation; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Validate; override;

    property Id: Integer read FId write FId;
    property IdUser: Integer read FIdUser write FIdUser;
    property Backup: TNoticationSettings read FBackup write FBackup;
    property TaskDue: TNoticationSettings read FTaskDue write FTaskDue;
  end;

implementation

uses
  System.SysUtils,

  Commons;

{ TUserSubscriptionNotication }

procedure TUserSubscription.BasicValidation;
begin
  inherited;

end;

constructor TUserSubscription.Create;
begin
  FBackup :=  TNoticationSettings.Create;
  FTaskDue:=  TNoticationSettings.Create;;
end;

destructor TUserSubscription.Destroy;
begin
  FBackup.Free;
  FTaskDue.Free;

  inherited;
end;

procedure TUserSubscription.Validate;
begin
  inherited;
  if FIdUser <= 0 then
    raise EEntityValidation.Create('ID do usuário da assinatura não pode ser vazio',
                                    Format('[%s.BasicValidation] Informe um ID válido para o campo "IdUser"', [Self.ToString]));
  FBackup.Validate;
  FTaskDue.Validate;
end;

end.

