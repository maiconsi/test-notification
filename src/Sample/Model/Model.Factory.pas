unit Model.Factory;

interface

uses
  Model.Factory.Interfaces,
  Model.Interfaces;

type
  TModelFactory = class(TInterfacedObject, IModelFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelFactory;

    function User: IModelUser;
    function Notification: IModelNotification;
    function NotificationDLLMapping: IModelNotificationDLLMapping;
  end;

implementation

uses
  Model.User,
  Model.Notification,
  Model.Notification.DLLMapping;

{ TModelFactory }

constructor TModelFactory.Create();
begin

end;

destructor TModelFactory.Destroy;
begin

  inherited;
end;

class function TModelFactory.New: IModelFactory;
begin
  Result  :=  Self.Create;
end;

function TModelFactory.Notification: IModelNotification;
begin
  Result  :=  TModelNotification.New;
end;

function TModelFactory.NotificationDLLMapping: IModelNotificationDLLMapping;
begin
  Result  :=  TModelNotificationDLLMapping.New;
end;

function TModelFactory.User: IModelUser;
begin
  Result  :=  TModelUser.New;
end;

end.
