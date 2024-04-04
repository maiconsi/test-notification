unit Model.User.Subscription;

interface

uses
  Model.Interfaces,
  Model.Entity.User.Subscription,
  Repository.Interfaces;

type
  TModelUserSubscription = class(TInterfacedObject, IModelUserSubscription)
  private
    FEntity : TUserSubscription;
    FRepository: IRepositoryUserSubscription;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelUserSubscription;

    function Entity: TUserSubscription; overload;
    function Entity(AValue: TUserSubscription): IModelUserSubscription; overload;
    function FindById(const AID: Integer): TUserSubscription;
    function FindByIDUser(const AIDUser: Integer): TUserSubscription;
    function Update(const AID: Integer): IModelUserSubscription;
  end;

implementation

uses
  System.SysUtils,

  Commons,
  Repository.Factory;

{ TModelUserSubscription }

constructor TModelUserSubscription.Create();
begin
  FEntity :=  TUserSubscription.Create;
  FRepository:= TRepositoryFactory.New.UserSubscription;
end;

destructor TModelUserSubscription.Destroy;
begin
  Entity(Nil);
  inherited;
end;

function TModelUserSubscription.Entity: TUserSubscription;
begin
  Result  :=  FEntity;
end;

function TModelUserSubscription.Entity(AValue: TUserSubscription): IModelUserSubscription;
begin
  Result  :=  Self;

  if Assigned(FEntity) then
    FEntity.Free;

  FEntity :=  AValue;
end;

function TModelUserSubscription.FindById(const AID: Integer): TUserSubscription;
begin
  if AID <= 0 then
    raise EModelException.Create('ID da assinatura não informado',
                                  Format('[%s.GetByID] Informe um valor válido para o parâmetro "AID"',
                                         [Self.ToString]));

  Entity(FRepository.Find(AID));

  Result  :=  Entity;
end;

function TModelUserSubscription.FindByIDUser(
  const AIDUser: Integer): TUserSubscription;
begin
  if AIDUser <= 0 then
    raise EModelException.Create('ID do usuario da assinatura não informado',
                                  Format('[%s.GetByID] Informe um valor válido para o parâmetro "AIDUser"',
                                         [Self.ToString]));

  Entity(FRepository.FindByIDUser(AIDUser));

  Result  :=  Entity;
end;

class function TModelUserSubscription.New: IModelUserSubscription;
begin
  Result  :=  Self.Create;
end;

function TModelUserSubscription.Update(const AID: Integer): IModelUserSubscription;
begin
  Result:=  Self;

  if Entity.id <> AID then
    raise EModelValidation.Create('ID do parâmetro diverge do ID da entidade!',
                                  Format('[%s.Update] Valor passado como parâmetro da chamada é diferente do ' +
                                          'objeto do body.%s->ID parâmetro: %d%s->ID entidade: %d',
                                         [Self.ToString, #13, AID, #13, Entity.id]));
  Entity.Validate;

  FRepository.Update(AID, Entity);
end;

end.
