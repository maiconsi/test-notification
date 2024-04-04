library notifications;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters.

  Important note about VCL usage: when this DLL will be implicitly
  loaded and this DLL uses TWicImage / TImageCollection created in
  any unit initialization section, then Vcl.WicImageInit must be
  included into your library's USES clause. }

uses
  System.ShareMem,
  System.SysUtils,
  System.Classes,
  Entity.Abstract in '..\Core\Entity\Entity.Abstract.pas',
  Commons in '..\Core\Commons.pas',
  Utils in '..\Core\Utils\Utils.pas',
  Model.Interfaces in 'Model\Model.Interfaces.pas',
  Model.Entity.Notification in 'Model\Entity\Model.Entity.Notification.pas',
  Model.Notification.Channel.Abstract in 'Model\Model.Notification.Channel.Abstract.pas',
  Model.Notification in 'Model\Model.Notification.pas',
  Model.Notification.Send in 'Model\Model.Notification.Send.pas',
  Model.Notification.Channel.Email in 'Model\Model.Notification.Channel.Email.pas',
  Model.Notification.Channel.SMS in 'Model\Model.Notification.Channel.SMS.pas',
  Model.Notification.Channel.Push in 'Model\Model.Notification.Channel.Push.pas',
  Model.Notification.Send.Invoker in 'Model\Model.Notification.Send.Invoker.pas',
  Model.Factory.Interfaces in 'Model\Model.Factory.Interfaces.pas',
  Model.Factory in 'Model\Model.Factory.pas',
  Model.Notification.Build in 'Model\Model.Notification.Build.pas',
  Service.Event.Notification in 'Service\Event\Service.Event.Notification.pas',
  Service.Logger in 'Service\Logger\Service.Logger.pas',
  Service.SendEmail.Interfaces in 'Service\SendEmail\Service.SendEmail.Interfaces.pas',
  Service.SendEmail in 'Service\SendEmail\Service.SendEmail.pas',
  Service.SendEmail.Factory.Interfaces in 'Service\SendEmail\Service.SendEmail.Factory.Interfaces.pas',
  Service.SendEmail.Factory in 'Service\SendEmail\Service.SendEmail.Factory.pas',
  Service.SendEmail.IndyEmail in 'Service\SendEmail\Service.SendEmail.IndyEmail.pas',
  Service.SendSMS.Interfaces in 'Service\SendSMS\Service.SendSMS.Interfaces.pas',
  Service.SendSMS in 'Service\SendSMS\Service.SendSMS.pas',
  Service.SendSMS.Factory.Interfaces in 'Service\SendSMS\Service.SendSMS.Factory.Interfaces.pas',
  Service.SendSMS.Factory in 'Service\SendSMS\Service.SendSMS.Factory.pas',
  Service.SendPush.Interfaces in 'Service\SendPush\Service.SendPush.Interfaces.pas',
  Service.SendPush in 'Service\SendPush\Service.SendPush.pas',
  Service.SendPush.Factory.Interfaces in 'Service\SendPush\Service.SendPush.Factory.Interfaces.pas',
  Service.SendPush.Factory in 'Service\SendPush\Service.SendPush.Factory.pas',
  Model.Notification.Channel.Manager in 'Model\Model.Notification.Channel.Manager.pas',
  Controller.Interfaces in 'Controller\Controller.Interfaces.pas',
  Controller.Notification.Manager in 'Controller\Controller.Notification.Manager.pas',
  Controller.Factory.Interfaces in 'Controller\Controller.Factory.Interfaces.pas',
  Controller.Factory in 'Controller\Controller.Factory.pas',
  Model.Entity.Notification.Settings in 'Model\Entity\Model.Entity.Notification.Settings.pas',
  Model.Entity.User in 'Model\Entity\Model.Entity.User.pas',
  Model.Entity.User.Subscription in 'Model\Entity\Model.Entity.User.Subscription.pas',
  Model.User in 'Model\Model.User.pas',
  Repository.Interfaces in 'Repository\Repository.Interfaces.pas',
  Repository.User in 'Repository\Repository.User.pas',
  Repository.Factory.Interfaces in 'Repository\Repository.Factory.Interfaces.pas',
  Repository.Factory in 'Repository\Repository.Factory.pas',
  Model.Entity.Notification.Backup in 'Model\Entity\Model.Entity.Notification.Backup.pas',
  Model.Entity.Notification.TaskDue in 'Model\Entity\Model.Entity.Notification.TaskDue.pas',
  Repository.Notification.Backup in 'Repository\Repository.Notification.Backup.pas',
  Repository.Notification.TaskDue in 'Repository\Repository.Notification.TaskDue.pas',
  Model.Notification.Backup in 'Model\Model.Notification.Backup.pas',
  Model.Notification.TaskDue in 'Model\Model.Notification.TaskDue.pas',
  Controller.Notification.Backup in 'Controller\Controller.Notification.Backup.pas',
  Controller.Notification.TaskDue in 'Controller\Controller.Notification.TaskDue.pas',
  Repository.User.Subscription in 'Repository\Repository.User.Subscription.pas',
  Model.User.Subscription in 'Model\Model.User.Subscription.pas',
  Controller.User in 'Controller\Controller.User.pas',
  Controller.User.Subscription in 'Controller\Controller.User.Subscription.pas';

{$R *.res}

var
  _NotificationManager: IControllerNotificationManager;

procedure ErroHandlingExceptionBase(AMessage: String; AException: EExceptionBase);
var
  LMenssageError: String;
begin
  LMenssageError  :=  Format('%s [%s]%s%s', [AMessage, AException.Message, #13, AException.DetailedMessage]);
  TLogger.GetInstance.Add(LMenssageError);
  raise Exception.Create(LMenssageError);
end;

procedure ErroHandlingException(AMessage: String; AException: Exception);
var
  LMenssageError: String;
begin
  LMenssageError  :=  Format('%s [%s]', [AMessage, AException.Message]);
  TLogger.GetInstance.Add(LMenssageError);
  raise Exception.Create(LMenssageError);
end;

function Initialize: Boolean;
begin
  Result  :=  False;
  try
    _NotificationManager.Initialize;
    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on initialize thread notification', E);
    on E: Exception do ErroHandlingException('Error on initialize thread notification', E);
  end;
end;

function Terminate: Boolean;
begin
  Result  :=  False;
  try
    _NotificationManager.Terminate;
    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on terminate thread notification', E);
    on E: Exception do ErroHandlingException('Error on terminate thread notification', E);
  end;
end;

function SendNotificationBackupNow: Boolean;
begin
  Result  :=  False;
  try
    TControllerFactory.New
      .NotificationBackup
        .SendNotificationNow;
    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error sending notification backup', E);
    on E: Exception do ErroHandlingException('Error on sendig notification backup', E);
  end;
end;

function SendNotificationTaskDueNow: Boolean;
begin
  Result  :=  False;
  try
    TControllerFactory.New
      .NotificationTaskDue
        .SendNotificationNow;
    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error sending notification task due', E);
    on E: Exception do ErroHandlingException('Error on sendig notification task due', E);
  end;
end;

function GetListOfChannels: TArray<String>;
begin
  try
    Result  :=  _NotificationManager.ListOfChannels;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on get channels', E);
    on E: Exception do ErroHandlingException('Error on get channels', E);
  end;
end;

function GetListOfFrequency: TArray<String>;
begin
  try
    Result  :=  _NotificationManager.ListOfFrequency;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on get frequencies', E);
    on E: Exception do ErroHandlingException('Error on get frequencies', E);
  end;
end;

function GetUser(AIDUser: Integer; out ALogin: String; out AName: String; out AEmail: String; out ACellphone: String): Boolean;
var
  LUser: IControllerUser;
begin
  Result  :=  False;
  try
    LUser :=  TControllerFactory.New.User;

    LUser.FindById(AIDUser);

    ALogin    :=  LUser.Entity.Login;
    AName     :=  LUser.Entity.Name;
    AEmail    :=  LUser.Entity.Email;
    ACellphone:=  LUser.Entity.Cellphone;

    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on save user', E);
    on E: Exception do ErroHandlingException('Error on save user', E);
  end;
end;

function SaveUser(AIDUser: Integer; ALogin: String; AName: String; AEmail: String; ACellphone: String): Boolean;
var
  LUser: IControllerUser;
begin
  Result  :=  False;
  try
    LUser :=  TControllerFactory.New.User;

    LUser.FindById(AIDUser);

    LUser.Entity.Login    :=  ALogin;
    LUser.Entity.Name     :=  AName;
    LUser.Entity.Email    :=  AEmail;
    LUser.Entity.Cellphone:=  ACellphone;
    LUser.Update(AIDUser);

    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on save user', E);
    on E: Exception do ErroHandlingException('Error on save user', E);
  end;
end;

function GetUserNotificationBackup(AIDUser: Integer; out AActive: Boolean; out AFrequency: String; out AChannels: TArray<String>): Boolean;
var
  LUser: IControllerUser;
begin
  Result  :=  False;
  try
    LUser :=  TControllerFactory.New.User;

    LUser.FindById(AIDUser);

    AActive   :=  LUser.Subscription.Entity.Backup.Active;
    AFrequency:=  TUtilEnumerator<TFrequency>.ToString(LUser.Subscription.Entity.Backup.Frequency);
    AChannels :=  LUser.Subscription.Entity.Backup.Channels;

    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on get config notification backup', E);
    on E: Exception do ErroHandlingException('Error on get config notification backup', E);
  end;
end;

function SaveUserNotificationBackup(AIDUser: Integer; AActive: Boolean; AFrequency: String; AChannels: TArray<String>): Boolean;
var
  LUser: IControllerUser;
begin
  Result  :=  False;
  try
    LUser :=  TControllerFactory.New.User;

    LUser.FindById(AIDUser);

    LUser.Subscription.Entity.Backup.Active   :=  AActive;
    LUser.Subscription.Entity.Backup.Frequency:=  TUtilEnumerator<TFrequency>.ToEnum(AFrequency);
    LUser.Subscription.Entity.Backup.Channels :=  AChannels;
    LUser.Subscription.Update(LUser.Subscription.Entity.Id);

    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on save config notification backup', E);
    on E: Exception do ErroHandlingException('Error on save config notification backup', E);
  end;
end;

function GetUserNotificationTaskDue(AIDUser: Integer; out AActive: Boolean; out AFrequency: String; out AChannels: TArray<String>): Boolean;
var
  LUser: IControllerUser;
begin
  Result  :=  False;
  try
    LUser :=  TControllerFactory.New.User;

    LUser.FindById(AIDUser);

    AActive   :=  LUser.Subscription.Entity.TaskDue.Active;
    AFrequency:=  TUtilEnumerator<TFrequency>.ToString(LUser.Subscription.Entity.TaskDue.Frequency);
    AChannels :=  LUser.Subscription.Entity.TaskDue.Channels;

    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on get config notification task due', E);
    on E: Exception do ErroHandlingException('Error on get config notification task due', E);
  end;
end;

function SaveUserNotificationTaskDue(AIDUser: Integer; AActive: Boolean; AFrequency: String; AChannels: TArray<String>): Boolean;
var
  LUser: IControllerUser;
begin
  Result  :=  False;
  try
    LUser :=  TControllerFactory.New.User;

    LUser.FindById(AIDUser);

    LUser.Subscription.Entity.TaskDue.Active   :=  AActive;
    LUser.Subscription.Entity.TaskDue.Frequency:=  TUtilEnumerator<TFrequency>.ToEnum(AFrequency);
    LUser.Subscription.Entity.TaskDue.Channels :=  AChannels;
    LUser.Subscription.Update(LUser.Subscription.Entity.Id);

    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error on save config notification task due', E);
    on E: Exception do ErroHandlingException('Error on save config notification task due', E);
  end;
end;

function SetNotificationBackupExecutedAt(AExecutedAt: TDateTime): Boolean;
begin
  Result  :=  False;
  try
    TControllerFactory.New
      .NotificationBackup
        .SetBackupExecutedAt(AExecutedAt);

    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error setting the backup execution date', E);
    on E: Exception do ErroHandlingException('Error setting the backup execution date', E);
  end;
end;

function SetNotificationTaskDueAt(ATaskDueAt: TDateTime): Boolean;
begin
  Result  :=  False;
  try

    TControllerFactory.New
      .NotificationTaskDue
        .SetTaskDueAt(ATaskDueAt);

    Result  :=  True;
  except
    on E: EExceptionBase do ErroHandlingExceptionBase('Error setting the task due date', E);
    on E: Exception do ErroHandlingException('Error setting the task due date', E);
  end;
end;

exports
  Initialize,
  Terminate,
  GetUser,
  SaveUser,
  GetListOfChannels,
  GetListOfFrequency,
  GetUserNotificationBackup,
  SaveUserNotificationBackup,
  GetUserNotificationTaskDue,
  SaveUserNotificationTaskDue,
  SendNotificationBackupNow,
  SendNotificationTaskDueNow,
  SetNotificationBackupExecutedAt,
  SetNotificationTaskDueAt;

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  _NotificationManager :=  TControllerFactory.New.NotificationManager;
end.
