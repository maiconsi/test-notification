unit Model.Storage.BucketS3;

interface

uses
  System.Classes,
  Data.Cloud.CloudAPI,
  Data.Cloud.AmazonAPI,

  Model.Storage.Interfaces;

type
  TModelStorageBucketS3 = class(TInterfacedObject, IModelStorage)
  private
    FStorageType: TStorageType;

    FAmazon: TAmazonConnectionInfo;
    FS3: TAmazonStorageService;

    FBucketName: String;
    FAccountName: String;
    FAccountKey: String;
    function ResourceParameters(AResource: String): String;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelStorage;

    function Save(AStream: TStream; AResource: String): IModelStorage;
    function Get(const AResource: String): TStream;
    function Delete(const AResource: String): IModelStorage;
    function Exists(const AResource: String): Boolean;
    function AccessPath(const AResource: String): String;
    function StorageType: TStorageType;
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils,

  Utils,
  Commons,
  Model.Settings.Global;

{ TModelStorageBucketS3 }

function TModelStorageBucketS3.ResourceParameters(AResource: String): String;
begin
  Result  :=  ReplaceStr(AResource, '\', '/');
end;

function TModelStorageBucketS3.AccessPath(const AResource: String): String;
begin
  Result  :=  Format('https://s3.amazonaws.com/%s/%s', [FBucketName, AResource]);
end;

constructor TModelStorageBucketS3.Create();
begin
  FStorageType:=  stoBucketS3;

  FBucketName :=  _Settings.BucketS3.Bucket;
  FAccountName:=  _Settings.BucketS3.AccessKey;
  FAccountKey :=  _Settings.BucketS3.SecretKey;

  FAmazon :=  TAmazonConnectionInfo.Create(Nil);

  FAmazon.AccountName :=  FAccountName;
  FAmazon.AccountKey  :=  FAccountKey;

  FS3 :=  TAmazonStorageService.Create(FAmazon);
end;

function TModelStorageBucketS3.Delete(
  const AResource: String): IModelStorage;
begin
  Result  :=  Self;

  try
    if not FS3.DeleteObject(FBucketName, ResourceParameters(AResource)) then
      raise EModelException.Create('delete failed!');
  except
    On Exc: Exception do
      raise EModelException.Create('Error trying to delete object in bucket S3',
                                    Format('[%s.Delete] %s', [Self.ToString, Exc.Message]));
  end;
end;

destructor TModelStorageBucketS3.Destroy;
begin
  FS3.Free;
  FAmazon.Free;

  inherited;
end;

function TModelStorageBucketS3.Exists(const AResource: String): Boolean;
Var
  FResponseInfo: TCloudResponseInfo;
  FStrTemp: TStrings;
begin
  Result  :=  False;

  FResponseInfo  :=  TCloudResponseInfo.Create;
  try
    FStrTemp  := nil;
    try
      FStrTemp  :=  FS3.GetObjectMetadata(FBucketName, ResourceParameters(AResource), FResponseInfo);
    finally
      FStrTemp.Free;
    end;

    if FResponseInfo.StatusCode = 200 then
      Result  :=  True;

  finally
    FResponseInfo.Free;
  end;
end;

function TModelStorageBucketS3.Get(const AResource: String): TStream;
begin
  Result  :=  TMemoryStream.Create;

  try
    if not FS3.GetObject(FBucketName, ResourceParameters(AResource), Result) then
      raise EModelException.Create('get failed!');
  except
    On Exc: Exception do
      raise EModelException.Create('Error trying to get object in bucket S3',
                                    Format('[%s.Get] %s', [Self.ToString, Exc.Message]));
  end;
end;

class function TModelStorageBucketS3.New(): IModelStorage;
begin
  Result  :=  Self.Create();
end;

function TModelStorageBucketS3.Save(AStream: TStream;
  AResource: String): IModelStorage;
var
  LOutputFullPath: string;
  LMeta: TStringList;

  LResourceContent: TBytes;
  LReader: TBinaryReader;
begin
  Result  :=  Self;

  try
    LOutputFullPath := ResourceParameters(AResource);

    LReader     :=  TBinaryReader.Create(AStream);
    try
      LResourceContent :=  LReader.ReadBytes(LReader.BaseStream.Size);
    finally
      LReader.Free;
    end;

    LMeta :=  TStringList.Create;
    LMeta.Add(TUtil.GetContentType(AResource));
    try
      if Not FS3.UploadObject(FBucketName, LOutputFullPath, LResourceContent, False, LMeta) then
        raise EModelException.Create('upload failed!');
    finally
      LMeta.Free;
    end;
  except
    On Exc: Exception do
      raise EModelException.Create('Error when trying to save the object in bucket S3',
                                    Format('[%s.Save] %s', [Self.ToString, Exc.Message]));
  end;
end;

function TModelStorageBucketS3.StorageType: TStorageType;
begin
  Result  :=  FStorageType;
end;

end.
