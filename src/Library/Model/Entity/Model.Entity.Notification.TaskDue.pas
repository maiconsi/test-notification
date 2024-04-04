unit Model.Entity.Notification.TaskDue;

interface

uses
  Entity.Abstract;

type
  TNotificationTaskDue = class(TEntityAbstract)
  private
    FTitle: String;
    FTaskDueAt: TDateTime;
    FNotifiedAt: TDateTime;
  public
    constructor Create;

    function Title: String;
    function &Message: String;
    function PendingNotification: Boolean;

    property TaskDueAt: TDateTime read FTaskDueAt write FTaskDueAt;
    property NotifiedAt: TDateTime read FNotifiedAt write FNotifiedAt;
  end;

implementation

uses
  System.SysUtils;

{ TNotificationTaskDue }

function TNotificationTaskDue.Title: String;
begin
  Result  :=  FTitle;
end;

{ TNotificationTaskDue }

constructor TNotificationTaskDue.Create;
begin
  FTitle  :=  'Task due notification';
end;

function TNotificationTaskDue.Message: String;
begin
  if FTaskDueAt = 0 then
    Result  :=  ('Task due')
  else
    Result  :=  Format('Task due since %s', [DateToStr(FTaskDueAt)]);
end;

function TNotificationTaskDue.PendingNotification: Boolean;
begin
  Result  :=  FNotifiedAt <= FTaskDueAt;
end;

end.
