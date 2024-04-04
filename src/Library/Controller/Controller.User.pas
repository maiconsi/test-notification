unit Controller.User;

interface

uses
  Controller.Interfaces,
  Model.Interfaces,
  Model.Entity.User;

type
  TControllerUser = class(TInterfacedObject, IControllerUser, IControllerUserConfig)
  private
    FUser: IModelUser;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IControllerUser;

    //IControllerUser
    function Entity: TUser; overload;
    function Entity(AValue: TUser): IControllerUser; overload;
    function FindById(const AID: Integer): TUser;
    function Update(const AID: Integer): IControllerUser;
    function Subscription: IControllerUserSubscription;

    //IControllerUserConfig
    function This: IControllerUser;
    function User: IModelUser;
  end;

implementation

{ TControllerUser }

uses Model.Factory, Controller.Factory;

constructor TControllerUser.Create();
begin
  FUser :=  TModelFactory.New.User;
end;

destructor TControllerUser.Destroy;
begin

  inherited;
end;

function TControllerUser.Entity: TUser;
begin
  Result  :=  FUser.Entity;
end;

function TControllerUser.Entity(AValue: TUser): IControllerUser;
begin
  Result  :=  Self;

  FUser.Entity(AValue);
end;

function TControllerUser.FindById(const AID: Integer): TUser;
begin
  Result  :=  FUser.FindById(AID);
end;

class function TControllerUser.New: IControllerUser;
begin
  Result  :=  Self.Create;
end;

function TControllerUser.Subscription: IControllerUserSubscription;
begin
  Result  :=  TControllerFactory.New.UserSubscription(Self);
end;

function TControllerUser.This: IControllerUser;
begin
  Result  :=  Self;
end;

function TControllerUser.Update(const AID: Integer): IControllerUser;
begin
  Result  :=  Self;

  FUser.Update(AID);
end;

function TControllerUser.User: IModelUser;
begin
  Result  :=  FUser;
end;

end.
