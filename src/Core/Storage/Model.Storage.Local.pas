unit Model.Storage.Local;

interface

uses
  System.Classes,

  Model.Storage.Interfaces;

type
  TModelStorageLocal = class(TInterfacedObject, IModelStorage)
  private
    FStorageType: TStorageType;

    function GetResourceName(AResource: String): String;
    function GetResourcePath(AResource: String): String;
    procedure CheckPathResources(AResource: String);
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
  System.IOUtils,
  System.SysUtils,

  Commons;

{ TModelStorageLocal }

function TModelStorageLocal.AccessPath(const AResource: String): String;
begin
  Result  :=  AResource;
end;

procedure TModelStorageLocal.CheckPathResources(AResource: String);
var
  LPath: String;
begin
  LPath :=  GetResourcePath(AResource);

  if not TDirectory.Exists(LPath) then
    TDirectory.CreateDirectory(LPath);
end;

constructor TModelStorageLocal.Create();
begin
  FStorageType  :=  stoLocal;
end;

function TModelStorageLocal.Delete(const AResource: String): IModelStorage;
begin
  Result  :=  Self;

  if Exists(AResource) then
    TFile.Delete(AResource);
end;

destructor TModelStorageLocal.Destroy;
begin

  inherited;
end;

function TModelStorageLocal.Exists(const AResource: String): Boolean;
begin
  Result  :=  TFile.Exists(AResource);
end;

function TModelStorageLocal.Get(const AResource: String): TStream;
var
  LFileStream: TFileStream;
  LMemoStream: TMemoryStream;
begin
  if not Exists(AResource) then
    raise ENotFoundException.Create(Format('File[%s] not found', [GetResourceName(AResource)]),
                                    Format('[%s.Get] File not found in current directory[%s]', [Self.ToString, GetResourcePath(AResource)]));

  LFileStream :=  TFileStream.Create(AResource, fmOpenRead or fmShareDenyNone);
  try
    LMemoStream :=  TMemoryStream.Create;
    LMemoStream.CopyFrom(LFileStream, LFileStream.Size);

    LMemoStream.Position := 0;
    Result  :=  LMemoStream;
  finally
    LFileStream.Free;
  end;
end;

function TModelStorageLocal.GetResourceName(AResource: String): String;
begin
  Result  :=  ExtractFileName(AResource);
end;

function TModelStorageLocal.GetResourcePath(AResource: String): String;
begin
  Result  :=  ExtractFilePath(AResource);
end;

class function TModelStorageLocal.New(): IModelStorage;
begin
  Result := Self.Create();
end;

function TModelStorageLocal.Save(AStream: TStream;
  AResource: String): IModelStorage;
var
  LOutFile: TFileStream;
begin
  Result  :=  Self;

  CheckPathResources(AResource);

  if Not Exists(AResource) then
    LOutFile := TFileStream.Create(AResource, fmCreate)
  else
    LOutFile := TFileStream.Create(AResource, fmOpenWrite);
  try
    LOutFile.CopyFrom(AStream, 0);
  finally
    LOutFile.Free;
  end;
end;

function TModelStorageLocal.StorageType: TStorageType;
begin
  Result  :=  FStorageType;
end;

end.
