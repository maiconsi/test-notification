unit Controller.Factory;

interface

uses
  Controller.Factory.Interfaces,
  Controller.Interfaces;

type
  TControllerFactory = class(TInterfacedObject, IControllerFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IControllerFactory;

    function User: IControllerUser;
    function Notification: IControllerNotification;
  end;

implementation

uses
  Controller.User,
  Controller.Notification;

{ TControllerFactory }

constructor TControllerFactory.Create();
begin

end;

destructor TControllerFactory.Destroy;
begin

  inherited;
end;

class function TControllerFactory.New: IControllerFactory;
begin
  Result  :=  Self.Create;
end;

function TControllerFactory.Notification: IControllerNotification;
begin
  Result  :=  TControllerNotification.New;
end;

function TControllerFactory.User: IControllerUser;
begin
  Result  :=  TControllerUser.New;
end;

end.
