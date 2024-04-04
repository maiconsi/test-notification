unit Model.Notification.Channel.Manager;

interface

uses
  System.TypInfo,
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.RTLConsts,
  System.Generics.Collections,

  Model.Interfaces;

type
  TNotificationChannelManager = class
  private
    class var FRegisterChannel: TDictionary<string, TClass>;
    class var FRepositoryChannel: TDictionary<string, IModelNotificationChannel>;

    class constructor Create;
    class destructor Destroy;
  public
    procedure RegisterChannel(AChannel: String; AClass: TClass); overload;
    procedure UnRegisterChannel(AChannel: String);

    function ResolveChannel(const AChannel: string; AConfig: IModelNotificationConfig): IModelNotificationChannel;
    procedure ResolveRemoveChannel(const AChannel: string);

    function MethodInvoker(AObject: TObject; const AMethodName: string): Boolean; overload;
    function MethodInvoker(AObject: TObject; const AMethodName: string;
      const AParams: array of TValue): Boolean; overload;

    function GetListOfChannels: TArray<String>;
    function GetListOfFrequency: TArray<String>;
  end;

var
  _NotificationChannelManager: TNotificationChannelManager;

implementation

uses
  Commons;

{ TNotificationChannelManager }

class constructor TNotificationChannelManager.Create;
begin
  inherited;
  FRegisterChannel  :=  TDictionary<string, TClass>.Create;
  FRepositoryChannel:=  TDictionary<string, IModelNotificationChannel>.Create;
end;

class destructor TNotificationChannelManager.Destroy;
begin
  FRegisterChannel.Free;
  FRepositoryChannel.Free;

  inherited;
end;

function TNotificationChannelManager.GetListOfChannels: TArray<String>;
var
  LChannel: String;
begin
  for LChannel in FRegisterChannel.Keys do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)]  :=  LChannel;
  end;
end;

function TNotificationChannelManager.GetListOfFrequency: TArray<String>;
var
  LCount: Integer;
begin
  for LCount := Ord(Low(TFrequency)) to Ord(High(TFrequency)) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)]  :=  GetEnumName(TypeInfo(TFrequency), LCount);
  end;
end;

function TNotificationChannelManager.MethodInvoker(AObject: TObject;
  const AMethodName: string): Boolean;
begin
  Result := MethodInvoker(AObject, AMethodName, []);
end;

function TNotificationChannelManager.MethodInvoker(AObject: TObject;
  const AMethodName: string; const AParams: array of TValue): Boolean;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  Result := True;
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AObject.ClassType);
    LType.GetMethod(AMethodName).Invoke(AObject, AParams);
  finally
    LContext.Free;
  end;
end;

procedure TNotificationChannelManager.RegisterChannel(AChannel: String; AClass: TClass);
begin
  if not FRegisterChannel.ContainsKey(AChannel) then
    FRegisterChannel.Add(AChannel, AClass);
end;

function TNotificationChannelManager.ResolveChannel(const AChannel: string; AConfig: IModelNotificationConfig): IModelNotificationChannel;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LValue: TValue;
  LKey: string;
  LParamConfig: TValue;
begin
  Result := nil;
  LKey := AChannel;

  if FRepositoryChannel.ContainsKey(LKey) then
    Exit(IModelNotificationChannel(FRepositoryChannel.Items[LKey]));

  if FRegisterChannel.ContainsKey(LKey) then
  begin
    LContext := TRttiContext.Create;
    try
      LType := LContext.GetType(FRegisterChannel.Items[LKey]);

      LParamConfig := TObject(AConfig);
      if LType.GetMethod('New') <> nil then
        LValue := LType.GetMethod('New').Invoke(FRegisterChannel.Items[LKey], [LParamConfig])
      else
      if LType.GetMethod('Create') <> nil then
        LValue := LType.GetMethod('Create').Invoke(FRegisterChannel.Items[LKey], [LParamConfig])
      else
        Exit;

      Result := IModelNotificationChannel(LValue.AsInterface);

      FRepositoryChannel.Add(LKey, IModelNotificationChannel(LValue.AsInterface));
    finally
      LContext.Free;
    end;
  end
  else
    raise EExceptionBase.Create(Format('Channel[%s] not found', [AChannel]));
end;

procedure TNotificationChannelManager.ResolveRemoveChannel(const AChannel: string);
begin
  if FRepositoryChannel.ContainsKey(AChannel) then
    FRepositoryChannel.Remove(AChannel);

  FRepositoryChannel.TrimExcess;
end;

procedure TNotificationChannelManager.UnRegisterChannel(AChannel: String);
begin
  if FRegisterChannel.ContainsKey(AChannel) then
    FRegisterChannel.Remove(AChannel);

  FRegisterChannel.TrimExcess;

  ResolveRemoveChannel(AChannel);
end;

end.
