program notification_dll_test;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  Entity.Abstract in '..\src\Core\Entity\Entity.Abstract.pas',
  Commons in '..\src\Core\Commons.pas',
  Utils in '..\src\Core\Utils\Utils.pas',
  Model.Notification.Test in 'Model.Notification.Test.pas',
  Model.Interfaces in '..\src\Library\Model\Model.Interfaces.pas',
  Model.Factory in '..\src\Library\Model\Model.Factory.pas',
  Model.Factory.Interfaces in '..\src\Library\Model\Model.Factory.Interfaces.pas',
  Model.Notification.Build in '..\src\Library\Model\Model.Notification.Build.pas',
  Model.Notification in '..\src\Library\Model\Model.Notification.pas',
  Model.Notification.Channel.Email in '..\src\Library\Model\Model.Notification.Channel.Email.pas',
  Model.Notification.Channel.Push in '..\src\Library\Model\Model.Notification.Channel.Push.pas',
  Model.Notification.Channel.SMS in '..\src\Library\Model\Model.Notification.Channel.SMS.pas',
  Model.Notification.Channel.Abstract in '..\src\Library\Model\Model.Notification.Channel.Abstract.pas',
  Model.Notification.Send.Invoker in '..\src\Library\Model\Model.Notification.Send.Invoker.pas',
  Model.Notification.Send in '..\src\Library\Model\Model.Notification.Send.pas',
  Service.SendEmail.Factory.Interfaces in '..\src\Library\Service\SendEmail\Service.SendEmail.Factory.Interfaces.pas',
  Service.SendEmail.Factory in '..\src\Library\Service\SendEmail\Service.SendEmail.Factory.pas',
  Service.SendEmail.IndyEmail in '..\src\Library\Service\SendEmail\Service.SendEmail.IndyEmail.pas',
  Service.SendEmail.Interfaces in '..\src\Library\Service\SendEmail\Service.SendEmail.Interfaces.pas',
  Service.SendEmail in '..\src\Library\Service\SendEmail\Service.SendEmail.pas',
  Service.SendSMS.Factory.Interfaces in '..\src\Library\Service\SendSMS\Service.SendSMS.Factory.Interfaces.pas',
  Service.SendSMS.Factory in '..\src\Library\Service\SendSMS\Service.SendSMS.Factory.pas',
  Service.SendSMS.Interfaces in '..\src\Library\Service\SendSMS\Service.SendSMS.Interfaces.pas',
  Service.SendSMS in '..\src\Library\Service\SendSMS\Service.SendSMS.pas',
  Service.SendPush.Factory.Interfaces in '..\src\Library\Service\SendPush\Service.SendPush.Factory.Interfaces.pas',
  Service.SendPush.Factory in '..\src\Library\Service\SendPush\Service.SendPush.Factory.pas',
  Service.SendPush.Interfaces in '..\src\Library\Service\SendPush\Service.SendPush.Interfaces.pas',
  Service.SendPush in '..\src\Library\Service\SendPush\Service.SendPush.pas',
  Model.Entity.Notification.Test in 'Model.Entity.Notification.Test.pas',
  Model.Entity.User in '..\src\Library\Model\Entity\Model.Entity.User.pas',
  Model.Entity.User.Subscription in '..\src\Library\Model\Entity\Model.Entity.User.Subscription.pas',
  Model.Entity.Notification in '..\src\Library\Model\Entity\Model.Entity.Notification.pas',
  Model.Entity.Notification.Settings in '..\src\Library\Model\Entity\Model.Entity.Notification.Settings.pas',
  Model.Entity.Notification.Backup in '..\src\Library\Model\Entity\Model.Entity.Notification.Backup.pas',
  Model.Entity.Notification.TaskDue in '..\src\Library\Model\Entity\Model.Entity.Notification.TaskDue.pas',
  Model.Notification.Channel.Manager in '..\src\Library\Model\Model.Notification.Channel.Manager.pas',
  Model.User in '..\src\Library\Model\Model.User.pas',
  Model.User.Subscription in '..\src\Library\Model\Model.User.Subscription.pas',
  Repository.Factory.Interfaces in '..\src\Library\Repository\Repository.Factory.Interfaces.pas',
  Repository.Factory in '..\src\Library\Repository\Repository.Factory.pas',
  Repository.Interfaces in '..\src\Library\Repository\Repository.Interfaces.pas',
  Repository.Notification.Backup in '..\src\Library\Repository\Repository.Notification.Backup.pas',
  Repository.Notification.TaskDue in '..\src\Library\Repository\Repository.Notification.TaskDue.pas',
  Repository.User in '..\src\Library\Repository\Repository.User.pas',
  Repository.User.Subscription in '..\src\Library\Repository\Repository.User.Subscription.pas',
  Model.Notification.Backup in '..\src\Library\Model\Model.Notification.Backup.pas',
  Model.Notification.TaskDue in '..\src\Library\Model\Model.Notification.TaskDue.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
