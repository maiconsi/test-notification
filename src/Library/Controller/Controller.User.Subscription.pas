unit Controller.User.Subscription;

interface

uses
  Controller.Interfaces,
  Model.Interfaces,
  Model.Entity.User.Subscription;

type
  TControllerUserSubscription = class(TInterfacedObject, IControllerUserSubscription)
  private
    FParent: IControllerUserConfig;
  public
    constructor Create(AParent: IControllerUserConfig);
    destructor Destroy; override;
    class function New(AParent: IControllerUserConfig): IControllerUserSubscription;

    function Entity: TUserSubscription; overload;
    function Entity(AValue: TUserSubscription): IControllerUserSubscription; overload;
    function FindById(const AID: Integer): TUserSubscription;
    function FindByIDUser(const AIDUser: Integer): TUserSubscription;
    function Update(const AID: Integer): IControllerUserSubscription;
  end;

implementation

{ TControllerUserSubscription }

uses Model.Factory;

constructor TControllerUserSubscription.Create(AParent: IControllerUserConfig);
begin
  FParent :=  AParent;
end;

destructor TControllerUserSubscription.Destroy;
begin

  inherited;
end;

function TControllerUserSubscription.Entity: TUserSubscription;
begin
  Result  :=  FParent.User.Subscription.Entity;
end;

function TControllerUserSubscription.Entity(AValue: TUserSubscription): IControllerUserSubscription;
begin
  Result  :=  Self;

  FParent.User.Subscription.Entity(AValue);
end;

function TControllerUserSubscription.FindById(const AID: Integer): TUserSubscription;
begin
  Result  :=  FParent.User.Subscription.FindById(AID);
end;

function TControllerUserSubscription.FindByIDUser(
  const AIDUser: Integer): TUserSubscription;
begin
  Result  :=  FParent.User.Subscription.FindByIDUser(AIDUser);
end;

class function TControllerUserSubscription.New(AParent: IControllerUserConfig): IControllerUserSubscription;
begin
  Result  :=  Self.Create(AParent);
end;

function TControllerUserSubscription.Update(const AID: Integer): IControllerUserSubscription;
begin
  Result  :=  Self;

  FParent.User.Subscription.Update(AID);
end;

end.

