unit Model.User;

interface

uses
  Model.Interfaces,
  Model.Entity.User,
  Repository.Interfaces;

type
  TModelUser = class(TInterfacedObject, IModelUser, IModelUserConfig)
  private
    FEntity : Tuser;
    FRepository: IRepositoryUser;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelUser;

    //IModelUser
    function Entity: Tuser; overload;
    function Entity(AValue: Tuser): IModelUser; overload;
    function GetById(const AID: Integer): Tuser;
    function Update(const AID: Integer): IModelUser;
    function Photo: IModelUserPhoto;

    //IModelUserConfig
    function This: IModelUser;
  end;

implementation

uses
  System.SysUtils,

  Commons,
  Repository.Factory,
  Model.User.Photo;

{ TModelUser }

constructor TModelUser.Create();
begin
  FEntity :=  Tuser.Create;
  FRepository:= TRepositoryFactory.New.User;
end;

destructor TModelUser.Destroy;
begin
  Entity(Nil);
  inherited;
end;

function TModelUser.Entity: Tuser;
begin
  Result  :=  FEntity;
end;

function TModelUser.Entity(AValue: Tuser): IModelUser;
begin
  Result  :=  Self;

  if Assigned(FEntity) then
    FEntity.Free;

  FEntity :=  AValue;
end;

function TModelUser.GetById(const AID: Integer): Tuser;
begin
  if AID <= 0 then
    raise EModelException.Create('ID do usuário não informado',
                                  Format('[%s.GetByID] Informe um valor válido para o parâmetro "AID"',
                                         [Self.ToString]));

  Entity(FRepository.Find(AID));

  Result  :=  Entity;
end;

class function TModelUser.New: IModelUser;
begin
  Result  :=  Self.Create;
end;

function TModelUser.Photo: IModelUserPhoto;
begin
  Result  :=  TModelUserPhoto.New(Self);
end;

function TModelUser.This: IModelUser;
begin
  Result  :=  Self;
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
