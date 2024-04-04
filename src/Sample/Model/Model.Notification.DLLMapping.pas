unit Model.Notification.DLLMapping;

interface

uses
  System.Generics.Collections,

  Model.Interfaces;

type
  TInitialize = function(): Boolean;
  TTerminate = function(): Boolean;
  TGetUser = function(AIDUser: Integer; out ALogin: String; out AName: String; out AEmail: String; out ACellphone: String): Boolean;
  TSaveUser = function(AIDUser: Integer; ALogin: String; AName: String; AEmail: String; ACellphone: String): Boolean;
  TGetListOfChannels = function(): TArray<String>;
  TGetListOfFrequency = function(): TArray<String>;
  TGetUserNotificationBackup = function(AIDUser: Integer; out AActive: Boolean; out AFrequency: String; out AChannels: TArray<String>): Boolean;
  TSaveUserNotificationBackup = function(AIDUser: Integer; AActive: Boolean; AFrequency: String; AChannels: TArray<String>): Boolean;
  TGetUserNotificationTaskDue = function(AIDUser: Integer; out AActive: Boolean; out AFrequency: String; out AChannels: TArray<String>): Boolean;
  TSaveUserNotificationTaskDue = function(AIDUser: Integer; AActive: Boolean; AFrequency: String; AChannels: TArray<String>): Boolean;
  TSendNotificationBackupNow = function(): Boolean;
  TSendNotificationTaskDueNow = function(): Boolean;
  TSetNotificationBackupExecutedAt = function(AExecutedAt: TDateTime): Boolean;
  TSetNotificationTaskDueAt = function(ATaskDueAt: TDateTime): Boolean;

  TModelNotificationDLLMapping = class(TInterfacedObject, IModelNotificationDLLMapping)
  private
    FHandle: THandle;
    FInitialize: TInitialize;
    FTerminate: TTerminate;
    FGetUser: TGetUser;
    FSaveUser: TSaveUser;
    FGetListOfChannels: TGetListOfChannels;
    FGetListOfFrequency: TGetListOfFrequency;
    FGetUserNotificationBackup: TGetUserNotificationBackup;
    FSaveUserNotificationBackup: TSaveUserNotificationBackup;
    FGetUserNotificationTaskDue: TGetUserNotificationTaskDue;
    FSaveUserNotificationTaskDue: TSaveUserNotificationTaskDue;
    FSendNotificationBackupNow: TSendNotificationBackupNow;
    FSendNotificationTaskDueNow: TSendNotificationTaskDueNow;
    FSetNotificationBackupExecutedAt: TSetNotificationBackupExecutedAt;
    FSetNotificationTaskDueAt: TSetNotificationTaskDueAt;
    procedure RegisterMethods;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelNotificationDLLMapping;

    function Initialize: Boolean;
    function Terminate: Boolean;
    function GetUser(AIDUser: Integer; out ALogin: String; out AName: String; out AEmail: String; out ACellphone: String): Boolean;
    function SaveUser(AIDUser: Integer; ALogin: String; AName: String; AEmail: String; ACellphone: String): Boolean;
    function GetListOfChannels: TArray<String>;
    function GetListOfFrequency: TArray<String>;
    function GetUserNotificationBackup(AIDUser: Integer; out AActive: Boolean; out AFrequency: String; out AChannels: TArray<String>): Boolean;
    function SaveUserNotificationBackup(AIDUser: Integer; AActive: Boolean; AFrequency: String; AChannels: TArray<String>): Boolean;
    function GetUserNotificationTaskDue(AIDUser: Integer; out AActive: Boolean; out AFrequency: String; out AChannels: TArray<String>): Boolean;
    function SaveUserNotificationTaskDue(AIDUser: Integer; AActive: Boolean; AFrequency: String; AChannels: TArray<String>): Boolean;
    function SendNotificationBackupNow(): Boolean;
    function SendNotificationTaskDueNow(): Boolean;
    function SetNotificationBackupExecutedAt(AExecutedAt: TDateTime): Boolean;
    function SetNotificationTaskDueAt(ATaskDueAt: TDateTime): Boolean;
  end;

implementation

uses
  Winapi.Windows,

  Commons;

{ TModelNotificationDLLMapping }

constructor TModelNotificationDLLMapping.Create();
begin
  FHandle := LoadLibrary('notifications.dll');

  if FHandle <> 0 then
    RegisterMethods
  else
    raise EModelException.Create('Unable to load DLL[notification.dll]');
end;

destructor TModelNotificationDLLMapping.Destroy;
begin
  @FInitialize:=  nil;
  @FTerminate :=  nil;
  @FGetUser   :=  nil;
  @FSaveUser  :=  nil;
  @FGetListOfChannels :=  nil;
  @FGetListOfFrequency:=  nil;
  @FGetUserNotificationBackup   :=  nil;
  @FSaveUserNotificationBackup  :=  nil;
  @FGetUserNotificationTaskDue  :=  nil;
  @FSaveUserNotificationTaskDue :=  nil;
  @FSendNotificationBackupNow   :=  nil;
  @FSendNotificationTaskDueNow  :=  nil;
  @FSetNotificationBackupExecutedAt :=  nil;
  @FSetNotificationTaskDueAt        :=  nil;

  FreeLibrary(FHandle);

  inherited;
end;

function TModelNotificationDLLMapping.GetListOfChannels: TArray<String>;
begin
  Result  :=  FGetListOfChannels;
end;

function TModelNotificationDLLMapping.GetListOfFrequency: TArray<String>;
begin
  Result  :=  FGetListOfFrequency;
end;

function TModelNotificationDLLMapping.GetUser(AIDUser: Integer; out ALogin,
  AName, AEmail, ACellphone: String): Boolean;
begin
  Result  :=  FGetUser(AIDUser, ALogin, AName, AEmail, ACellphone);
end;

function TModelNotificationDLLMapping.GetUserNotificationBackup(
  AIDUser: Integer; out AActive: Boolean; out AFrequency: String;
  out AChannels: TArray<String>): Boolean;
begin
  Result  :=  FGetUserNotificationBackup(AIDUser, AActive, AFrequency, AChannels);
end;

function TModelNotificationDLLMapping.GetUserNotificationTaskDue(
  AIDUser: Integer; out AActive: Boolean; out AFrequency: String;
  out AChannels: TArray<String>): Boolean;
begin
  Result  :=  FGetUserNotificationTaskDue(AIDUser, AActive, AFrequency, AChannels);
end;

function TModelNotificationDLLMapping.Initialize: Boolean;
begin
  Result  :=  FInitialize;
end;

class function TModelNotificationDLLMapping.New: IModelNotificationDLLMapping;
begin
  Result  :=  Self.Create;
end;

procedure TModelNotificationDLLMapping.RegisterMethods;
begin
  @FInitialize  :=  GetProcAddress(FHandle, 'Initialize');
  if @FInitialize = nil then
    raise EModelException.Create('Error on get proc address(Initialize) of DLL[notification.dll]');

  @FTerminate :=  GetProcAddress(FHandle, 'Terminate');
  if @FTerminate = nil then
    raise EModelException.Create('Error on get proc address(Terminate) of DLL[notification.dll]');

  @FGetListOfChannels :=  GetProcAddress(FHandle, 'GetListOfChannels');
  if @FGetListOfChannels = nil then
    raise EModelException.Create('Error on get proc address(GetListOfChannels) of DLL[notification.dll]');

  @FGetListOfFrequency :=  GetProcAddress(FHandle, 'GetListOfFrequency');
  if @FGetListOfFrequency = nil then
    raise EModelException.Create('Error on get proc address(GetListOfChannels) of DLL[notification.dll]');

  @FGetUser :=  GetProcAddress(FHandle, 'GetUser');
  if @FGetUser = nil then
    raise EModelException.Create('Error on get proc address(GetUser) of DLL[notification.dll]');

  @FSaveUser :=  GetProcAddress(FHandle, 'SaveUser');
  if @FSaveUser = nil then
    raise EModelException.Create('Error on get proc address(SaveUser) of DLL[notification.dll]');

  @FGetUserNotificationBackup :=  GetProcAddress(FHandle, 'GetUserNotificationBackup');
  if @FGetUserNotificationBackup = nil then
    raise EModelException.Create('Error on get proc address(GetUserNotificationBackup) of DLL[notification.dll]');

  @FSaveUserNotificationBackup :=  GetProcAddress(FHandle, 'SaveUserNotificationBackup');
  if @FSaveUserNotificationBackup = nil then
    raise EModelException.Create('Error on get proc address(SaveUserNotificationBackup) of DLL[notification.dll]');

  @FGetUserNotificationTaskDue :=  GetProcAddress(FHandle, 'GetUserNotificationTaskDue');
  if @FGetUserNotificationTaskDue = nil then
    raise EModelException.Create('Error on get proc address(GetUserNotificationTaskDue) of DLL[notification.dll]');

  @FSaveUser :=  GetProcAddress(FHandle, 'SaveUser');
  if @FSaveUser = nil then
    raise EModelException.Create('Error on get proc address(SaveUser) of DLL[notification.dll]');

  @FSaveUserNotificationTaskDue :=  GetProcAddress(FHandle, 'SaveUserNotificationTaskDue');
  if @FSaveUserNotificationTaskDue = nil then
    raise EModelException.Create('Error on get proc address(SaveUserNotificationTaskDue) of DLL[notification.dll]');

  @FSendNotificationBackupNow :=  GetProcAddress(FHandle, 'SendNotificationBackupNow');
  if @FSendNotificationBackupNow = nil then
    raise EModelException.Create('Error on get proc address(SendNotificationBackupNow) of DLL[notification.dll]');

  @FSendNotificationTaskDueNow :=  GetProcAddress(FHandle, 'SendNotificationTaskDueNow');
  if @FSendNotificationTaskDueNow = nil then
    raise EModelException.Create('Error on get proc address(SendNotificationTaskDueNow) of DLL[notification.dll]');

  @FSetNotificationBackupExecutedAt :=  GetProcAddress(FHandle, 'SetNotificationBackupExecutedAt');
  if @FSetNotificationBackupExecutedAt = nil then
    raise EModelException.Create('Error on get proc address(SetNotificationBackupExecutedAt) of DLL[notification.dll]');

  @FSetNotificationTaskDueAt :=  GetProcAddress(FHandle, 'SetNotificationTaskDueAt');
  if @FSetNotificationTaskDueAt = nil then
    raise EModelException.Create('Error on get proc address(SetNotificationTaskDueAt) of DLL[notification.dll]');
end;

function TModelNotificationDLLMapping.SaveUser(AIDUser: Integer; ALogin, AName,
  AEmail, ACellphone: String): Boolean;
begin
  Result  :=  FSaveUser(AIDUser, ALogin, AName, AEmail, ACellphone);
end;

function TModelNotificationDLLMapping.SaveUserNotificationBackup(
  AIDUser: Integer; AActive: Boolean; AFrequency: String;
  AChannels: TArray<String>): Boolean;
begin
  Result  :=  FSaveUserNotificationBackup(AIDUser, AActive, AFrequency, AChannels);
end;

function TModelNotificationDLLMapping.SaveUserNotificationTaskDue(
  AIDUser: Integer; AActive: Boolean; AFrequency: String;
  AChannels: TArray<String>): Boolean;
begin
  Result  :=  FSaveUserNotificationTaskDue(AIDUser, AActive, AFrequency, AChannels);
end;

function TModelNotificationDLLMapping.SendNotificationBackupNow(): Boolean;
begin
  Result  :=  FSendNotificationBackupNow;
end;

function TModelNotificationDLLMapping.SendNotificationTaskDueNow(): Boolean;
begin
  Result  :=  FSendNotificationTaskDueNow;
end;

function TModelNotificationDLLMapping.SetNotificationBackupExecutedAt(AExecutedAt: TDateTime): Boolean;
begin
  Result  :=  FSetNotificationBackupExecutedAt(AExecutedAt);
end;

function TModelNotificationDLLMapping.SetNotificationTaskDueAt(ATaskDueAt: TDateTime): Boolean;
begin
  Result  :=  FSetNotificationTaskDueAt(ATaskDueAt);
end;

function TModelNotificationDLLMapping.Terminate: Boolean;
begin
  Result  :=  FTerminate;
end;

end.
