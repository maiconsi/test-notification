unit Repository.User.Subscription;

interface

uses
  System.SyncObjs,

  Repository.Interfaces,
  Model.Entity.User.Subscription;

type
  TRepositoryUserSubscription = class(TInterfacedObject, IRepositoryUserSubscription)
  private
    FFileStorageUserSubscription: String;
    FCritical: TCriticalSection;

    function GetDataUserSubscription(AFileStorage: String): TUserSubscription;
    procedure SaveDataUserSubscription(AUserSubscription: TUserSubscription; AFileStorage: String);
    procedure CheckDataUserSubscription;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IRepositoryUserSubscription;

    function Find(const AID: Integer): TUserSubscription;
    function FindByIDUser(const AIDUser: Integer): TUserSubscription;
    function Insert(AUserSubscription: TUserSubscription): IRepositoryUserSubscription;
    function Update(const AID: Integer; AUserSubscriptionNew: TUserSubscription): IRepositoryUserSubscription;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Commons;

{ TRepositoryUserSubscription }

procedure TRepositoryUserSubscription.CheckDataUserSubscription;
var
  LUserSubscription: TUserSubscription;
begin
  if not FileExists(FFileStorageUserSubscription) then
  begin
    LUserSubscription :=  TUserSubscription.Create;
    try
      LUserSubscription.Id    :=  1;
      LUserSubscription.IdUser:=  1;
      SaveDataUserSubscription(LUserSubscription, FFileStorageUserSubscription);
    finally
      LUserSubscription.Free;
    end;
  end;
end;

constructor TRepositoryUserSubscription.Create();
begin
  FFileStorageUserSubscription  := TPath.Combine(ExtractFilePath(ParamStr(0)), 'StorageUserSubscription.json');
  FCritical :=  TCriticalSection.Create;
end;

destructor TRepositoryUserSubscription.Destroy;
begin
  FCritical.Free;

  inherited;
end;

function TRepositoryUserSubscription.Find(const AID: Integer): TUserSubscription;
begin
  try
    CheckDataUserSubscription;

    Result  :=  GetDataUserSubscription(FFileStorageUserSubscription);
  except
    on Exc: Exception do
      raise ERepositoryException.Create(Format('Error when trying to search for user subscription by id[%d]', [AID]),
                                        Format('[%s.Find] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TRepositoryUserSubscription.FindByIDUser(
  const AIDUser: Integer): TUserSubscription;
begin
  try
    CheckDataUserSubscription;

    Result  :=  GetDataUserSubscription(FFileStorageUserSubscription);
  except
    on Exc: Exception do
      raise ERepositoryException.Create(Format('Error when trying to search for user subscription by id user[%d]', [AIDUser]),
                                        Format('[%s.FindByIDUser] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TRepositoryUserSubscription.Insert(AUserSubscription: TUserSubscription): IRepositoryUserSubscription;
begin
  Result  :=  Self;

  try
    SaveDataUserSubscription(AUserSubscription, FFileStorageUserSubscription);
  except
    on Exc: Exception do
      raise ERepositoryException.Create('Error when trying to enter user subscription',
                                        Format('[%s.Insert] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TRepositoryUserSubscription.GetDataUserSubscription(AFileStorage: String): TUserSubscription;
var
  LFile: TStringList;
begin
  Result  :=  TUserSubscription.Create;

  LFile :=  TStringList.Create;
  try
    FCritical.Enter;
    try
      LFile.LoadFromFile(FFileStorageUserSubscription);
    finally
      FCritical.Release;
    end;

    Result.LoadJsonString(LFile.Text);
  finally
    LFile.Free;
  end;
end;

class function TRepositoryUserSubscription.New: IRepositoryUserSubscription;
begin
  Result  :=  Self.Create;
end;

procedure TRepositoryUserSubscription.SaveDataUserSubscription(AUserSubscription: TUserSubscription; AFileStorage: String);
var
  LFile: TStringList;
begin
  LFile :=  TStringList.Create;
  try
    LFile.Text  :=  AUserSubscription.ToJsonString(False);
    FCritical.Enter;
    try
      LFile.SaveToFile(AFileStorage);
    finally
      FCritical.Release;
    end;
  finally
    LFile.Free;
  end;
end;

function TRepositoryUserSubscription.Update(const AID: Integer;
  AUserSubscriptionNew: TUserSubscription): IRepositoryUserSubscription;
begin
  Result  :=  Self;

  try
    SaveDataUserSubscription(AUserSubscriptionNew, FFileStorageUserSubscription);
  except
    on Exc: Exception do
      raise ERepositoryException.Create(Format('Error when trying to update user subscription[%d]', [AID]),
                                        Format('[%s.Update] %s', [Self.ToString, Exc.Message]));
  end;
end;

end.
