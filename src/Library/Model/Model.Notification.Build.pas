unit Model.Notification.Build;

interface

uses
  Commons,
  Model.Interfaces;

type
  TModelNotificationBuild = class(TInterfacedObject, IModelNotificationBuild)
  private
    FParent: IModelNotificationConfig;
  public
    constructor Create(AParent: IModelNotificationConfig);
    destructor Destroy; override;
    class function New(AParent: IModelNotificationConfig): IModelNotificationBuild;

    function Title(AValue: String): IModelNotificationBuild;
    function &Message(AValue: String): IModelNotificationBuild;
    function UserName(AValue: String): IModelNotificationBuild;
    function UserEmail(AValue: String): IModelNotificationBuild;
    function UserCellphone(AValue: String): IModelNotificationBuild;
    function Channels(AValue: TArray<String>): IModelNotificationBuild;
    function &End: IModelNotification;
  end;

implementation

{ TModelNotificationBuild }

function TModelNotificationBuild.Channels(AValue: TArray<String>): IModelNotificationBuild;
begin
  Result  :=  Self;

  FParent.Entity.Channels  :=  AValue;
end;

function TModelNotificationBuild.UserCellphone(
  AValue: String): IModelNotificationBuild;
begin
  Result  :=  Self;

  FParent.Entity.UserCellphone  :=  AValue;
end;

function TModelNotificationBuild.&End: IModelNotification;
begin
  Result  :=  FParent.This;
end;

constructor TModelNotificationBuild.Create(AParent: IModelNotificationConfig);
begin
  FParent :=  AParent;
end;

destructor TModelNotificationBuild.Destroy;
begin

  inherited;
end;

function TModelNotificationBuild.Message(
  AValue: String): IModelNotificationBuild;
begin
  Result  :=  Self;

  FParent.Entity.Message  :=  AValue;
end;

class function TModelNotificationBuild.New(AParent: IModelNotificationConfig): IModelNotificationBuild;
begin
  Result  :=  Self.Create(AParent);
end;

function TModelNotificationBuild.Title(AValue: String): IModelNotificationBuild;
begin
  Result  :=  Self;

  FParent.Entity.Title  :=  AValue;
end;

function TModelNotificationBuild.UserEmail(
  AValue: String): IModelNotificationBuild;
begin
  Result  :=  Self;

  FParent.Entity.UserEmail  :=  AValue;
end;

function TModelNotificationBuild.UserName(
  AValue: String): IModelNotificationBuild;
begin
  Result  :=  Self;

  FParent.Entity.UserName  :=  AValue;
end;

end.
