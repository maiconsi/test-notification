unit Model.Resource.Interfaces;

interface

uses
  System.Classes,

  Model.Storage.Interfaces;

type
  IModelResource = interface
    ['{FBC112B9-BBE7-4A07-91E9-F41710E70D2A}']
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

end.
