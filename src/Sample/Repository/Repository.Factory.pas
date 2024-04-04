unit Repository.Factory;

interface

uses
  Repository.Factory.Interfaces,
  Repository.Interfaces;

type
  TRepositoryFactory = class(TInterfacedObject, IRepositoryFactory)
  private
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IRepositoryFactory;

    function User: IRepositoryUser;
  end;

implementation

uses
  Repository.User;

{ TRepositoryFactory }

constructor TRepositoryFactory.Create();
begin

end;

destructor TRepositoryFactory.Destroy;
begin

  inherited;
end;

class function TRepositoryFactory.New: IRepositoryFactory;
begin
  Result  :=  Self.Create;
end;

function TRepositoryFactory.User: IRepositoryUser;
begin
  Result  :=  TRepositoryUser.New;
end;

end.
