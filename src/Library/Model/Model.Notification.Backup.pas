unit Model.Notification.Backup;

interface

uses
  Model.Interfaces,
  Model.Entity.Notification.Backup,
  Repository.Interfaces;

type
  TModelNotificationBackup = class(TInterfacedObject, IModelNotificationBackup)
  private
    FEntity : TNotificationBackup;
    FRepository: IRepositoryNotificationBackup;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelNotificationBackup;

    function Entity: TNotificationBackup; overload;
    function Entity(AValue: TNotificationBackup): IModelNotificationBackup; overload;
    function Get: TNotificationBackup;
    function Update: IModelNotificationBackup;
  end;

implementation

uses
  System.SysUtils,

  Commons,
  Repository.Factory;

{ TModelNotificationBackup }

constructor TModelNotificationBackup.Create();
begin
  FEntity :=  TNotificationBackup.Create;
  FRepository:= TRepositoryFactory.New.NotificationBackup;
end;

destructor TModelNotificationBackup.Destroy;
begin
  Entity(Nil);
  inherited;
end;

function TModelNotificationBackup.Entity: TNotificationBackup;
begin
  Result  :=  FEntity;
end;

function TModelNotificationBackup.Entity(AValue: TNotificationBackup): IModelNotificationBackup;
begin
  Result  :=  Self;

  if Assigned(FEntity) then
    FEntity.Free;

  FEntity :=  AValue;
end;

function TModelNotificationBackup.Get: TNotificationBackup;
begin
  Entity(FRepository.Get);

  Result  :=  Entity;
end;

class function TModelNotificationBackup.New: IModelNotificationBackup;
begin
  Result  :=  Self.Create;
end;

function TModelNotificationBackup.Update: IModelNotificationBackup;
begin
  Result:=  Self;

  Entity.Validate;

  FRepository.Update(Entity);
end;

end.
