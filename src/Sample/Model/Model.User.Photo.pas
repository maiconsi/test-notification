unit Model.User.Photo;

interface

uses
  System.Classes,

  Model.Interfaces,
  Model.Resource.Interfaces;

type
  TModelUserPhoto = class(TInterfacedObject, IModelUserPhoto)
  private
    FParent: IModelUserConfig;
    FResource: IModelResource;

    procedure GetUsuario(const AID: Integer);
    procedure ChecaDadosUsuario;
  public
    constructor Create(AParent: IModelUserConfig);
    destructor Destroy; override;
    class function New(AParent: IModelUserConfig): IModelUserPhoto;

    function Get: TStream; overload;
    function Get(const AID: Integer): TStream; overload;
    function Save(APhoto: TStream): IModelUserPhoto; overload;
    function Save(const AID: Integer; APhoto: TStream): IModelUserPhoto; overload;
    function Delete: IModelUserPhoto; overload;
    function Delete(const AID: Integer): IModelUserPhoto; overload;
    function &End: IModelUser;
  end;

implementation

uses
  System.SysUtils,

  Commons,
  Model.Resource.Factory;

{ TModelUserPhoto }

function TModelUserPhoto.&End: IModelUser;
begin
  Result  :=  FParent.This;
end;

procedure TModelUserPhoto.ChecaDadosUsuario;
begin
  if Not Assigned(FParent.This.Entity) then
    raise EModelException.Create('Usuário não carregado',
                                  Format('[%s.ChecaDadosUsuario] Para poder acessar a foto do usuário é necessário a entidade estar carregada', [Self.ToString]));

  if FParent.This.Entity.id = 0 then
    raise EModelException.Create('Usuário não carregado',
                                  Format('[%s.ChecaDadosUsuario] Para poder acessar a foto do usuário é necessário a entidade estar carregada', [Self.ToString]));
end;

constructor TModelUserPhoto.Create(AParent: IModelUserConfig);
begin
  FParent :=  AParent;

  FResource :=  TModelResourceFactory.New
                  .Local
                    .Path(ExtractFilePath(ParamStr(0)))
                    .Folder('images')
                    .DefaultResource('user_without_photo.png')
                    .MaskResourceName('%7.7d_%2.2d.png');
end;

function TModelUserPhoto.Delete: IModelUserPhoto;
begin
  Result  :=  Self;

  ChecaDadosUsuario;

  try
    FResource
      .GenerateResourceName(FParent.This.Entity.id)
        .Delete;
  except
    on Exc: ENotFoundException do
      raise ENotFoundException.Create(Format('Foto do usuário[%d] não encontrada', [FParent.This.Entity.id]),
                                      Format('[%s.Delete] %s(%s)', [Self.ToString, Exc.Message, Exc.DetailedMessage]));
    on Exc: EModelException do
      raise EModelException.Create(Format('Erro ao tentar deletar a foto do usuário[%d]', [FParent.This.Entity.id]),
                                   Format('[%s.Delete] %s(%s)', [Self.ToString, Exc.Message, Exc.DetailedMessage]));

    on Exc: Exception do
      raise EModelException.Create(Format('Erro ao tentar deletar a foto do usuário[%d]', [FParent.This.Entity.id]),
                                   Format('[%s.Delete] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TModelUserPhoto.Delete(
  const AID: Integer): IModelUserPhoto;
begin
  Result  :=  Self;

  GetUsuario(AID);

  Delete;
end;

destructor TModelUserPhoto.Destroy;
begin

  inherited;
end;

procedure TModelUserPhoto.GetUsuario(const AID: Integer);
begin
  FParent.This.GetByID(AID);

  if Not Assigned(FParent.This.Entity) then
    raise ENotFoundException.Create(Format('Usuário[%d] não encontrado', [AID]));
end;

function TModelUserPhoto.Get(const AID: Integer): TStream;
begin
  GetUsuario(AID);

  Result  :=  Get;
end;

function TModelUserPhoto.Get: TStream;
begin
  ChecaDadosUsuario;

  try
    Result  :=  FResource
                  .GenerateResourceName(FParent.This.Entity.id)
                    .Get;
  except
    on Exc: ENotFoundException do
      raise ENotFoundException.Create(Format('Foto do usuário[%d] não encontrada', [FParent.This.Entity.id]),
                                      Format('[%s.Get] %s(%s)', [Self.ToString, Exc.Message, Exc.DetailedMessage]));
    on Exc: EModelException do
      raise EModelException.Create(Format('Erro ao tentar buscar a foto do usuário[%d]', [FParent.This.Entity.id]),
                                   Format('[%s.Get] %s(%s)', [Self.ToString, Exc.Message, Exc.DetailedMessage]));

    on Exc: Exception do
      raise EModelException.Create(Format('Erro ao tentar buscar a foto do usuário[%d]', [FParent.This.Entity.id]),
                                   Format('[%s.Get] %s', [Self.ToString, Exc.Message]));
  end;
end;

class function TModelUserPhoto.New(AParent: IModelUserConfig): IModelUserPhoto;
begin
  Result := Self.Create(AParent);
end;

function TModelUserPhoto.Save(const AID: Integer;
  APhoto: TStream): IModelUserPhoto;
begin
  Result  :=  Self;

  GetUsuario(AID);

  Save(APhoto);
end;

function TModelUserPhoto.Save(APhoto: TStream): IModelUserPhoto;
begin
  Result  :=  Self;

  ChecaDadosUsuario;

  try
    FResource
      .GenerateResourceName(FParent.This.Entity.id)
        .Save(APhoto);
  except
    on Exc: EModelException do
      raise EModelException.Create(Format('Erro ao tentar salvar a foto do usuário[%d]', [FParent.This.Entity.id]),
                                   Format('[%s.Save] %s(%s)', [Self.ToString, Exc.Message, Exc.DetailedMessage]));

    on Exc: Exception do
      raise EModelException.Create(Format('Erro ao tentar salvar a foto do usuário[%d]', [FParent.This.Entity.id]),
                                   Format('[%s.Save] %s', [Self.ToString, Exc.Message]));
  end;
end;

end.

