unit Controller.Notification.TaskDue;

interface

uses
  Controller.Interfaces,
  Model.Entity.Notification.TaskDue,
  Model.Interfaces;

type
  TControllerNotificationTaskDue = class(TInterfacedObject, IControllerNotificationTaskDue)
  private
    FModel: IModelNotificationTaskDue;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IControllerNotificationTaskDue;

    function Get: TNotificationTaskDue;
    function Entity: TNotificationTaskDue;
    function SetTaskDueAt(ATaskDueAt: TDateTime): IControllerNotificationTaskDue;
    function SetNotifiedAt(ANotifiedAt: TDateTime): IControllerNotificationTaskDue;
    function SendNotificationNow: IControllerNotificationTaskDue;
  end;

implementation

uses
  System.SysUtils,

  Model.Factory;

{ TControllerNotificationTaskDue }

constructor TControllerNotificationTaskDue.Create();
begin
  FModel  :=  TModelFactory.New.NotificationTaskDue;
end;

destructor TControllerNotificationTaskDue.Destroy;
begin

  inherited;
end;

function TControllerNotificationTaskDue.Entity: TNotificationTaskDue;
begin
  Result  :=  FModel.Entity;
end;

function TControllerNotificationTaskDue.Get: TNotificationTaskDue;
begin
  Result  :=  FModel.Get;
end;

class function TControllerNotificationTaskDue.New: IControllerNotificationTaskDue;
begin
  Result  :=  Self.Create;
end;

function TControllerNotificationTaskDue.SendNotificationNow: IControllerNotificationTaskDue;
begin
  Result  :=  Self;

  FModel.Entity.TaskDueAt   :=  0;
  FModel.Entity.NotifiedAt  :=  0;
  FModel.Update;
end;

function TControllerNotificationTaskDue.SetTaskDueAt(ATaskDueAt: TDateTime): IControllerNotificationTaskDue;
begin
  Result  :=  Self;

  FModel.Entity.TaskDueAt  :=  Now;
  FModel.Update;
end;

function TControllerNotificationTaskDue.SetNotifiedAt(ANotifiedAt: TDateTime): IControllerNotificationTaskDue;
begin
  Result  :=  Self;

  FModel.Entity.NotifiedAt  :=  Now;
  FModel.Update;
end;

end.

