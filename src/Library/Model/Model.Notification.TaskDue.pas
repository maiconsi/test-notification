unit Model.Notification.TaskDue;

interface

uses
  Model.Interfaces,
  Model.Entity.Notification.TaskDue,
  Repository.Interfaces;

type
  TModelNotificationTaskDue = class(TInterfacedObject, IModelNotificationTaskDue)
  private
    FEntity : TNotificationTaskDue;
    FRepository: IRepositoryNotificationTaskDue;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelNotificationTaskDue;

    function Entity: TNotificationTaskDue; overload;
    function Entity(AValue: TNotificationTaskDue): IModelNotificationTaskDue; overload;
    function Get: TNotificationTaskDue;
    function Update: IModelNotificationTaskDue;
  end;

implementation

uses
  System.SysUtils,

  Commons,
  Repository.Factory;

{ TModelNotificationTaskDue }

constructor TModelNotificationTaskDue.Create();
begin
  FEntity :=  TNotificationTaskDue.Create;
  FRepository:= TRepositoryFactory.New.NotificationTaskDue;
end;

destructor TModelNotificationTaskDue.Destroy;
begin
  Entity(Nil);
  inherited;
end;

function TModelNotificationTaskDue.Entity: TNotificationTaskDue;
begin
  Result  :=  FEntity;
end;

function TModelNotificationTaskDue.Entity(AValue: TNotificationTaskDue): IModelNotificationTaskDue;
begin
  Result  :=  Self;

  if Assigned(FEntity) then
    FEntity.Free;

  FEntity :=  AValue;
end;

function TModelNotificationTaskDue.Get: TNotificationTaskDue;
begin
  Entity(FRepository.Get);

  Result  :=  Entity;
end;

class function TModelNotificationTaskDue.New: IModelNotificationTaskDue;
begin
  Result  :=  Self.Create;
end;

function TModelNotificationTaskDue.Update: IModelNotificationTaskDue;
begin
  Result:=  Self;

  Entity.Validate;

  FRepository.Update(Entity);
end;

end.
