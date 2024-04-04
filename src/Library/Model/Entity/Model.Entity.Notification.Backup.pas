unit Model.Entity.Notification.Backup;

interface

uses
  Entity.Abstract;

type
  TNotificationBackup = class(TEntityAbstract)
  private
    FTitle: String;
    FBackupExecutedAt: TDateTime;
    FNotifiedAt: TDateTime;
  public
    constructor Create;

    function Title: String;
    function &Message: String;
    function PendingNotification: Boolean;

    property BackupExecutedAt: TDateTime read FBackupExecutedAt write FBackupExecutedAt;
    property NotifiedAt: TDateTime read FNotifiedAt write FNotifiedAt;
  end;

implementation

uses
  System.SysUtils;

{ TNotificationBackup }

constructor TNotificationBackup.Create;
begin
  FTitle  :=  'Pending backup notification'
end;

function TNotificationBackup.Message: String;
begin
  if FBackupExecutedAt = 0 then
    Result  :=  ('Backup never generated')
  else
    Result  :=  Format('The backup is not pending since %s', [DateToStr(FBackupExecutedAt)]);
end;

function TNotificationBackup.PendingNotification: Boolean;
begin
  Result  :=  FNotifiedAt <= FBackupExecutedAt;
end;

function TNotificationBackup.Title: String;
begin
  Result  :=  FTitle;
end;

end.
