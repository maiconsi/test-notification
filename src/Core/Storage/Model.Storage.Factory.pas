unit Model.Storage.Factory;

interface

uses
  Model.Storage.Factory.Interfaces,
  Model.Storage.Interfaces;

type
  TModelStorageFactory = class(TInterfacedObject, IModelStorageFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelStorageFactory;

    function Local: IModelStorage;
  end;

implementation

uses
  Model.Storage.Local;

{ TModelStorageFactory }

constructor TModelStorageFactory.Create;
begin

end;

destructor TModelStorageFactory.Destroy;
begin

  inherited;
end;

function TModelStorageFactory.Local: IModelStorage;
begin
  Result  :=  TModelStorageLocal.New();
end;

class function TModelStorageFactory.New: IModelStorageFactory;
begin
  Result := Self.Create();
end;

end.
