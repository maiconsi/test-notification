unit Repository.Factory.Interfaces;

interface

uses
  Repository.Interfaces;

type
  IRepositoryFactory = interface
    ['{E8511955-89E9-4678-AD25-73D3E3FCAFF9}']
    function User: IRepositoryUser;
  end;

implementation

end.

