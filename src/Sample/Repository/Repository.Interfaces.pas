unit Repository.Interfaces;

interface

uses
  Model.Entity.User;

type
  IRepositoryUser = interface
    ['{F6BF1245-7EF6-404F-9F67-186A15E1F228}']
    function Find(const AID: Integer): Tuser;
    function Insert(AUser: Tuser): IRepositoryUser;
    function Update(const AID: Integer; AUserNew: Tuser): IRepositoryUser;
  end;

implementation

end.
