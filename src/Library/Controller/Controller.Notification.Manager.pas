unit Controller.Notification.Manager;

interface

uses
  System.Classes,

  Commons,
  Controller.Interfaces,
  Model.Interfaces;

type
  TControllerNotificationManager = class(TInterfacedObject, IControllerNotificationManager)
  private
    FUser: IControllerUser;
    FNotificationBackup: IControllerNotificationBackup;
    FNotificationTaskDue: IControllerNotificationTaskDue;
    FNotificationBackupThread: TThread;
    FNotificationTaskDueThread: TThread;
    procedure JobNotificationBackup;
    procedure JobNotificationTaskDue;
    function GetDaysOfFrequency(AFrequency: TFrequency): Integer;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): IControllerNotificationManager;

    function Initialize: IControllerNotificationManager;
    function Terminate: IControllerNotificationManager;
    function ListOfChannels: TArray<String>;
    function ListOfFrequency: TArray<String>;
  end;

implementation

uses
  System.SysUtils,
  Service.Logger,
  Model.Notification.Channel.Manager,
  Model.Factory,
  Controller.Factory,
  Service.Event.Notification, System.SyncObjs;

{ TControllerNotificationManager }

constructor TControllerNotificationManager.Create();
begin
  FUser :=  TControllerFactory.New.User;

  FNotificationBackup :=  TControllerFactory.New.NotificationBackup;
  FNotificationTaskDue:=  TControllerFactory.New.NotificationTaskDue;

  FNotificationBackupThread :=  TThread.CreateAnonymousThread(JobNotificationBackup);
  FNotificationBackupThread.FreeOnTerminate :=  False;

  FNotificationTaskDueThread:=  TThread.CreateAnonymousThread(JobNotificationTaskDue);
  FNotificationTaskDueThread.FreeOnTerminate :=  False;
end;

destructor TControllerNotificationManager.Destroy;
begin
  Terminate;

  inherited;
end;

function TControllerNotificationManager.GetDaysOfFrequency(
  AFrequency: TFrequency): Integer;
begin
  case AFrequency of
    Daily   : Result :=  1;
    Weekly  : Result :=  7;
    Monthly : Result :=  30;
  else
    Result :=  0;
  end;
end;

class function TControllerNotificationManager.New: IControllerNotificationManager;
begin
  Result  :=  Self.Create;
end;

procedure TControllerNotificationManager.JobNotificationBackup;
var
  LUser: IControllerUser;
  LNotificationBackup: IControllerNotificationBackup;
  LDateLastNotification: TDateTime;
  LNotification: IModelNotification;
begin
  while not FNotificationBackupThread.Finished do
  begin
    LUser :=  TControllerFactory.New.User;
    LUser.FindById(1); // Substituir por uma lista com os usuarios que assinaram essa notificação

    if LUser.Subscription.Entity.Backup.Active then
    begin
      LNotificationBackup :=  TControllerFactory.New.NotificationBackup;

      LNotificationBackup.Get;

      if LNotificationBackup.Entity.PendingNotification then
      begin
        LDateLastNotification :=  LNotificationBackup.Entity.BackupExecutedAt +
                                    GetDaysOfFrequency(LUser.Subscription.Entity.Backup.Frequency);

        if LDateLastNotification <= Now then
        begin
          try
            LNotification :=  TModelFactory.New.Notification;

            LNotification
              .Build
                .Title(LNotificationBackup.Entity.Title)
                .Message(LNotificationBackup.Entity.Message)
                .UserName(LUser.Entity.Name)
                .UserEmail(LUser.Entity.Email)
                .UserCellphone(LUser.Entity.Cellphone)
                .Channels(LUser.Subscription.Entity.Backup.Channels)
              .&End
              .EventOnSending(TEventsNotification.OnSending)
              .EventOnSendingError(TEventsNotification.OnSendingError)
              .EventOnSendingSuccess(TEventsNotification.OnSendingSuccess)
              .Send
                .Execute;

            LNotificationBackup.SetNotifiedAt(Now);

            LUser.Subscription.Entity.Backup.LastNotification  :=  Now;
            LUser.Subscription.Update(1);
          except
            on Exc: Exception do
              TLogger.GetInstance.Add('Error on notification backup: ' + Exc.Message);
          end;
        end;
      end;
    end;

    TThread.Sleep(5000);
  end;
end;

procedure TControllerNotificationManager.JobNotificationTaskDue;
var
  LUser: IControllerUser;
  LNotificationTaskDue: IControllerNotificationtaskDue;
  LDateLastNotification: TDateTime;
  LNotification: IModelNotification;
begin
  while not FNotificationTaskDueThread.Finished do
  begin
    LUser :=  TControllerFactory.New.User;
    LUser.FindById(1); // Substituir por uma lista com os usuarios que assinaram essa notificação

    if LUser.Subscription.Entity.TaskDue.Active then
    begin
      LNotificationTaskDue :=  TControllerFactory.New.NotificationTaskDue;
      LNotificationTaskDue.Get;

      if LNotificationTaskDue.Entity.PendingNotification then
      begin
        LDateLastNotification :=  LNotificationTaskDue.Entity.TaskDueAt +
                                    GetDaysOfFrequency(LUser.Subscription.Entity.Backup.Frequency);

        if LDateLastNotification <= Now then
        begin
          LNotification :=  TModelFactory.New.Notification;

          LNotification
            .Build
              .Title(LNotificationTaskDue.Entity.Title)
              .Message(LNotificationTaskDue.Entity.Message)
              .UserName(LUser.Entity.Name)
              .UserEmail(LUser.Entity.Email)
              .UserCellphone(LUser.Entity.Cellphone)
              .Channels(LUser.Subscription.Entity.TaskDue.Channels)
            .&End
            .EventOnSending(TEventsNotification.OnSending)
            .EventOnSendingError(TEventsNotification.OnSendingError)
            .EventOnSendingSuccess(TEventsNotification.OnSendingSuccess)
            .Send
              .Execute;

          LNotificationTaskDue.SetNotifiedAt(Now);

          LUser.Subscription.Entity.TaskDue.LastNotification  :=  Now;
          LUser.Subscription.Update(1);
        end;
      end;
    end;
    TThread.Sleep(5000);
  end;
end;

function TControllerNotificationManager.Initialize: IControllerNotificationManager;
begin
  Result  :=  Self;

  FNotificationBackupThread.Start;
  FNotificationTaskDueThread.Start;
end;

function TControllerNotificationManager.ListOfChannels: TArray<String>;
begin
  Result  :=  _NotificationChannelManager.GetListOfChannels;
end;

function TControllerNotificationManager.ListOfFrequency: TArray<String>;
begin
  Result  :=  _NotificationChannelManager.GetListOfFrequency;
end;

function TControllerNotificationManager.Terminate: IControllerNotificationManager;
begin
  Result  :=  Self;

  FNotificationBackupThread.Terminate;
  FNotificationTaskDueThread.Terminate;
end;

end.
