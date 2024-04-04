unit Repository.User;

interface

uses
  System.SyncObjs,

  Repository.Interfaces,
  Model.Entity.User;

type
  TRepositoryUser = class(TInterfacedObject, IRepositoryUser)
  private
    FFileStorageUser: String;
    FCritical: TCriticalSection;

    function GetDataUser(AFileStorage: String): TUser;
    procedure SaveDataUser(AUser: TUser; AFileStorage: String);
    procedure CheckDataUser;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IRepositoryUser;

    function Find(const AID: Integer): TUser;
    function Insert(AUser: TUser): IRepositoryUser;
    function Update(const AID: Integer; AUserNew: TUser): IRepositoryUser;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Commons;

{ TRepositoryUser }

procedure TRepositoryUser.CheckDataUser;
var
  LUser: TUser;
begin
  if not FileExists(FFileStorageUser) then
  begin
    LUser :=  TUser.Create;
    try
      LUser.Id :=  1;
      SaveDataUser(LUser, FFileStorageUser);
    finally
      LUser.Free;
    end;
  end;
end;

constructor TRepositoryUser.Create();
begin
  FFileStorageUser  := TPath.Combine(ExtractFilePath(ParamStr(0)), 'StorageUser.json');
  FCritical :=  TCriticalSection.Create;
end;

destructor TRepositoryUser.Destroy;
begin
  FCritical.Free;

  inherited;
end;

function TRepositoryUser.Find(const AID: Integer): TUser;
begin
  try
    CheckDataUser;

    Result  :=  GetDataUser(FFileStorageUser);
  except
    on Exc: Exception do
      raise ERepositoryException.Create(Format('Error when trying to search for user by id[%d]', [AID]),
                                        Format('[%s.Find] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TRepositoryUser.Insert(AUser: TUser): IRepositoryUser;
begin
  Result  :=  Self;

  try
    SaveDataUser(AUser, FFileStorageUser);
  except
    on Exc: Exception do
      raise ERepositoryException.Create('Error when trying to enter user',
                                        Format('[%s.Insert] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TRepositoryUser.GetDataUser(AFileStorage: String): TUser;
var
  LFile: TStringList;
begin
  Result  :=  TUser.Create;

  LFile :=  TStringList.Create;
  try
    FCritical.Enter;
    try
      LFile.LoadFromFile(FFileStorageUser);
    finally
      FCritical.Release;
    end;

    Result.LoadJsonString(LFile.Text);
  finally
    LFile.Free;
  end;
end;

class function TRepositoryUser.New: IRepositoryUser;
begin
  Result  :=  Self.Create;
end;

procedure TRepositoryUser.SaveDataUser(AUser: TUser; AFileStorage: String);
var
  LFile: TStringList;
begin
  LFile :=  TStringList.Create;
  try
    LFile.Text  :=  AUser.ToJsonString(False);
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

function TRepositoryUser.Update(const AID: Integer;
  AUserNew: TUser): IRepositoryUser;
begin
  Result  :=  Self;

  try
    SaveDataUser(AUserNew, FFileStorageUser);
  except
    on Exc: Exception do
      raise ERepositoryException.Create(Format('Error when trying to update user[%d]', [AID]),
                                        Format('[%s.Update] %s', [Self.ToString, Exc.Message]));
  end;

end;

end.
