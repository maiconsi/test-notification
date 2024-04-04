unit Model.Storage.Factory.Interfaces;

interface

uses
  Model.Storage.Interfaces;

type
  IModelStorageFactory = interface
    ['{0E98DE2C-88E5-4948-99FC-9293D3148DAF}']
    function Local: IModelStorage;
  end;

implementation

end.
