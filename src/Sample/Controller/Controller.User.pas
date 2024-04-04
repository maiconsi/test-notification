unit Controller.User;

interface

uses
  Controller.Interfaces,
  Model.Entity.User,
  Model.Interfaces;

type
  TControllerUser = class(TInterfacedObject, IControllerUser)
  private
    FModel: IModelUser;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IControllerUser;

    function GetCurrentUser: IControllerUser;
    function Entity: TUser;
    function Photo: IModelUserPhoto;
    function Save: IControllerUser;
  end;

implementation

uses
  Model.Factory;

{ TControllerUser }

constructor TControllerUser.Create();
begin
  FModel  :=  TModelFactory.New.User;
end;

destructor TControllerUser.Destroy;
begin

  inherited;
end;

function TControllerUser.Entity: TUser;
begin
  Result  :=  FModel.Entity;
end;

function TControllerUser.GetCurrentUser: IControllerUser;
begin
  Result  :=  Self;

  FModel.GetById(1);
end;

class function TControllerUser.New: IControllerUser;
begin
  Result  :=  Self.Create;
end;

function TControllerUser.Photo: IModelUserPhoto;
begin
  Result  :=  FModel.Photo;
end;

function TControllerUser.Save: IControllerUser;
begin
  Result  :=  Self;

  FModel.Update(1);
end;

end.
