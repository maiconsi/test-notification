unit Model.Storage.Interfaces;

interface

uses
  System.Classes;

type
  TStorageType = (stoLocal, stoBucketS3);

  IModelStorage = interface
    ['{687A3828-4323-45A0-A65A-CA7B683DD175}']
    function Save(AStream: TStream; AResource: String): IModelStorage;
    function Get(const AResource: String): TStream;
    function Delete(const AResource: String): IModelStorage;
    function Exists(const AResource: String): Boolean;
    function AccessPath(const AResource: String): String;
    function StorageType: TStorageType;
  end;

implementation

end.
