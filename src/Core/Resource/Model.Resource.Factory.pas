unit Model.Resource.Factory;

interface

uses
  Model.Resource.Factory.Interfaces,
  Model.Resource.Interfaces;

type
  TModelResourceFactory = class(TInterfacedObject, IModelResourceFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IModelResourceFactory;

    function Local: IModelResource;
  end;

implementation

uses
  Model.Resource,
  Model.Storage.Factory;

{ TModelResourceFactory }

constructor TModelResourceFactory.Create;
begin

end;

destructor TModelResourceFactory.Destroy;
begin

  inherited;
end;

function TModelResourceFactory.Local: IModelResource;
begin
  Result  :=  TModelResource.New(TModelStorageFactory.New.Local);
end;

class function TModelResourceFactory.New: IModelResourceFactory;
begin
  Result := Self.Create();
end;

end.
