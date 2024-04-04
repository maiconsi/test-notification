unit Model.Resource;

interface

uses
  System.Classes,

  Model.Resource.Interfaces,
  Model.Storage.Interfaces;

type
  TModelResource = class(TInterfacedObject, IModelResource)
  private
    FPath: String;
    FFolder: String;
    FResourceName: String;
    FDefaultResource: String;
    FMaskResourceName: String;

    FStorage: IModelStorage;

    function FullResourcePath(AResourceName: string; AResourceDefault: Boolean = False): String;
  public
    constructor Create(AStorage: IModelStorage);
    destructor Destroy; override;
    class function New(AStorage: IModelStorage): IModelResource;

    function Path: String; overload;
    function Path(AValue: String): IModelResource; overload;
    function Folder: String; overload;
    function Folder(AValue: String): IModelResource; overload;
    function ResourceName: String; overload;
    function ResourceName(AValue: String): IModelResource; overload;
    function DefaultResource: String; overload;
    function DefaultResource(AValue: String): IModelResource; overload;
    function MaskResourceName: String; overload;
    function MaskResourceName(AValue: String): IModelResource; overload;
    function GenerateResourceName(const AID: Integer; AIndex: Integer = 0; AExtension: String = ''): IModelResource; overload;
    function GenerateResourceName(const AID: String; AIndex: Integer = 0; AExtension: String = ''): IModelResource; overload;
    function StorageType: TStorageType;

    function Save(AStream: TStream): IModelResource; overload;
    function Save(AStream: TStream; AResourceName: String): IModelResource; overload;
    function Get: TStream; overload;
    function Get(const AResourceName: String): TStream; overload;
    function Delete: IModelResource; overload;
    function Delete(const AResourceName: String): IModelResource; overload;
    function Exists: Boolean; overload;
    function Exists(const AResourceName: String): Boolean; overload;
    function AccessPath: String;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils, Commons;

{ TModelResource }

function TModelResource.AccessPath: String;
var
  LFullResourceName: String;
begin
  Result  :=  '';

  LFullResourceName :=  FullResourcePath(FResourceName);

  if FStorage.Exists(LFullResourceName) then
    Result  :=  FStorage.AccessPath(LFullResourceName)
  else
  begin
    if Not FDefaultResource.IsEmpty then
      Result  :=  FStorage.AccessPath(FullResourcePath(FDefaultResource, True))
    else
      raise ENotFoundException.Create(Format('Resource[%s] not found', [FResourceName]),
                                    Format('[%s.AccessPath] Resource[%s] not found in storage', [Self.ToString, LFullResourceName]));
  end;
end;

constructor TModelResource.Create(AStorage: IModelStorage);
begin
  FStorage  :=  AStorage;

  FPath   :=  '';
  FFolder :=  '';
  FResourceName     :=  '';
  FDefaultResource  :=  '';
  FMaskResourceName :=  '';
end;

function TModelResource.DefaultResource(AValue: String): IModelResource;
begin
  Result  :=  Self;

  FDefaultResource  :=  AValue;
end;

function TModelResource.Delete: IModelResource;
var
  LFullResourceName: String;
begin
  Result  :=  Self;

  LFullResourceName :=  FullResourcePath(FResourceName);

  if FStorage.Exists(LFullResourceName) then
    FStorage.Delete(LFullResourceName)
  else
    raise ENotFoundException.Create(Format('Resource[%s] not found', [FResourceName]),
                                    Format('[%s.Delete] Resource[%s] not found in storage', [Self.ToString, LFullResourceName]));
end;

function TModelResource.DefaultResource: String;
begin
  Result  :=  FDefaultResource;
end;

function TModelResource.Delete(const AResourceName: String): IModelResource;
begin
  FResourceName :=  AResourceName;

  Result  :=  Delete;
end;

destructor TModelResource.Destroy;
begin

  inherited;
end;

function TModelResource.Exists: Boolean;
begin
  Result  :=  FStorage.Exists(FullResourcePath(FResourceName));
end;

function TModelResource.Exists(const AResourceName: String): Boolean;
begin
  FResourceName :=  AResourceName;

  Result  :=  Exists;
end;

function TModelResource.Folder: String;
begin
  Result  :=  FFolder;
end;

function TModelResource.Folder(AValue: String): IModelResource;
begin
  Result  :=  Self;

  FFolder :=  AValue;
end;

function TModelResource.FullResourcePath(AResourceName: string; AResourceDefault: Boolean = False): String;
var
  LResourceFolder: string;
  LFullResourcePath: string;
begin
  if Not AResourceDefault then
  begin
    LResourceFolder := TPath.Combine(FPath, FFolder);
    LFullResourcePath := TPath.Combine(LResourceFolder, AResourceName);
  end
  else
    LFullResourcePath := TPath.Combine(FPath, AResourceName);

  Result  :=  LFullResourcePath;
end;

function TModelResource.GenerateResourceName(const AID: Integer;
  AIndex: Integer; AExtension: String): IModelResource;
begin
  Result  :=  Self;

  FResourceName :=  Format(FMaskResourceName, [AID, AIndex]);
end;

function TModelResource.Get: TStream;
var
  LFullResourceName: String;
begin
  LFullResourceName :=  FullResourcePath(FResourceName);

  if FStorage.Exists(LFullResourceName) then
    Result  :=  FStorage.Get(LFullResourceName)
  else
  begin
    if Not FDefaultResource.IsEmpty then
      Result  :=  FStorage.Get(FullResourcePath(FDefaultResource, True))
    else
      raise ENotFoundException.Create(Format('Resource[%s] not found', [FResourceName]),
                                    Format('[%s.Get] Resource[%s] not found in storage', [Self.ToString, LFullResourceName]));
  end;
end;

function TModelResource.GenerateResourceName(const AID: String; AIndex: Integer;
  AExtension: String): IModelResource;
begin
  Result  :=  Self;

  FResourceName :=  Format(FMaskResourceName, [AID, AIndex]);
end;

function TModelResource.Get(const AResourceName: String): TStream;
begin
  FResourceName :=  AResourceName;

  Result  :=  Get;
end;

function TModelResource.MaskResourceName(AValue: String): IModelResource;
begin
  Result  :=  Self;

  FMaskResourceName :=  AValue;
end;

function TModelResource.MaskResourceName: String;
begin
  Result  :=  FMaskResourceName;
end;

class function TModelResource.New(AStorage: IModelStorage): IModelResource;
begin
  Result := Self.Create(AStorage);
end;

function TModelResource.Path(AValue: String): IModelResource;
begin
  Result  :=  Self;

  FPath  :=  AValue;
end;

function TModelResource.Path: String;
begin
  Result  :=  FPath;
end;

function TModelResource.ResourceName(AValue: String): IModelResource;
begin
  Result  :=  Self;

  FResourceName :=  AValue;
end;

function TModelResource.ResourceName: String;
begin
  Result  :=  FResourceName;
end;

function TModelResource.Save(AStream: TStream;
  AResourceName: String): IModelResource;
begin
  FResourceName :=  AResourceName;

  Result  :=  Save(AStream);
end;

function TModelResource.Save(AStream: TStream): IModelResource;
begin
  Result  :=  Self;

  FStorage.Save(AStream, FullResourcePath(FResourceName));
end;

function TModelResource.StorageType: TStorageType;
begin
  Result  :=  FStorage.StorageType;
end;

end.
