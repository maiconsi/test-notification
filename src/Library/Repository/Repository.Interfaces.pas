unit Repository.Interfaces;

interface

uses
  Model.Entity.User,
  Model.Entity.Notification.Backup,
  Model.Entity.Notification.TaskDue,
  Model.Entity.User.Subscription;

type
  IRepositoryUser = interface
    ['{5B5C5DC0-144B-4C10-9CC6-6E36FEBB4689}']
    function Find(const AID: Integer): Tuser;
    function Insert(AUser: Tuser): IRepositoryUser;
    function Update(const AID: Integer; AUserNew: Tuser): IRepositoryUser;
  end;

  IRepositoryUserSubscription = interface
    ['{34DAC936-E42D-49C1-8970-27E733B5D12F}']
    function Find(const AID: Integer): TUserSubscription;
    function FindByIDUser(const AIDUser: Integer): TUserSubscription;
    function Insert(AUserSubscription: TUserSubscription): IRepositoryUserSubscription;
    function Update(const AID: Integer; AUserSubscriptionNew: TUserSubscription): IRepositoryUserSubscription;
  end;

  IRepositoryNotificationBackup = interface
    ['{BABAC369-F90C-4BB3-8E0B-C9C67318D779}']
    function Get: TNotificationBackup;
    function Update(ANotificationBackupNew: TNotificationBackup): IRepositoryNotificationBackup;
  end;

  IRepositoryNotificationTaskDue = interface
    ['{8898AF0F-BC84-4E01-AB3E-201C3C9BF964}']
    function Get: TNotificationTaskDue;
    function Update(ANotificationTaskDueNew: TNotificationTaskDue): IRepositoryNotificationTaskDue;
  end;

implementation

end.
