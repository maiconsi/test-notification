unit Model.User;

interface

uses
  Model.Interfaces,
  Model.Entity.User,
  Repository.Interfaces;

type
  TModelUser = class(TInterfacedObject, IModelUser)
  private
    FEntity : TUser;
    FRepository: IRepositoryUser;
    FModelUserSubscription: IModelUserSubscription;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelUser;

    function Entity: TUser; overload;
    function Entity(AValue: TUser): IModelUser; overload;
    function FindById(const AID: Integer): TUser;
    function Update(const AID: Integer): IModelUser;

    function Subscription: IModelUserSubscription;
  end;

implementation

uses
  System.SysUtils,

  Commons,
  Repository.Factory, Model.Factory;

{ TModelUser }

constructor TModelUser.Create();
begin
  FEntity :=  TUser.Create;
  FRepository:= TRepositoryFactory.New.User;
  FModelUserSubscription  :=  TModelFactory.New.UserSubscription;
end;

destructor TModelUser.Destroy;
begin
  Entity(Nil);
  inherited;
end;

function TModelUser.Entity: TUser;
begin
  Result  :=  FEntity;
end;

function TModelUser.Entity(AValue: TUser): IModelUser;
begin
  Result  :=  Self;

  if Assigned(FEntity) then
    FEntity.Free;

  FEntity :=  AValue;
end;

function TModelUser.FindById(const AID: Integer): TUser;
begin
  if AID <= 0 then
    raise EModelException.Create('ID do usuário não informado',
                                  Format('[%s.GetByID] Informe um valor válido para o parâmetro "AID"',
                                         [Self.ToString]));

  Entity(FRepository.Find(AID));

  FModelUserSubscription.FindByIDUser(Entity.Id);

  Result  :=  Entity;
end;

class function TModelUser.New: IModelUser;
begin
  Result  :=  Self.Create;
end;

function TModelUser.Subscription: IModelUserSubscription;
begin
  Result  :=  FModelUserSubscription;
end;

function TModelUser.Update(const AID: Integer): IModelUser;
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
