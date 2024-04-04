unit Model.Notification.Test;

interface

uses
  DUnitX.TestFramework,

  Commons,
  Model.Entity.Notification,
  Model.Interfaces;

type

  [TestFixture]
  TNotificationTest = class(TObject)
  private
    FNotification: IModelNotification;
  public
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;
  [Test]
  [TestCase('TestCaseNotificationBuild', 'Titulo teste,Mensagem teste,Usuario teste,email.teste@email.com.br,+55(18)99716-1234,Email')]
  procedure TestNotificationBuild(const ATitle, AMessage, AUserName, AUserEmail,
  AUserCellpone: string; const AChannels: TArray<String>);
  end;

implementation

uses
  Model.Factory;

{ TNotificationTest }

procedure TNotificationTest.Setup;
begin
  FNotification :=  TModelFactory.New.Notification;
end;

procedure TNotificationTest.TearDown;
begin

end;

procedure TNotificationTest.TestNotificationBuild(const ATitle, AMessage,
  AUserName, AUserEmail, AUserCellpone: string;
  const AChannels: TArray<String>);
begin
  FNotification
    .Build
      .Title(ATitle)
      .Message(AMessage)
      .UserName(AUserName)
      .UserEmail(AUserEmail)
      .UserCellphone(AUserCellpone)
      .Channels(AChannels)
    .&End;

  Assert.AreEqual(FNotification.Entity.Title, ATitle, 'Titulo da notificação inválido');
  Assert.AreEqual(FNotification.Entity.Message, AMessage, 'Mensagem da notificação inválida');
  Assert.AreEqual(FNotification.Entity.UserName, AUserName, 'Usuário da notificação inválido');
  Assert.AreEqual(FNotification.Entity.UserEmail, AUserEmail, 'Email da notificação inválido');
  Assert.AreEqual(FNotification.Entity.UserCellphone, AUserCellpone, 'Celular da notificação inválido');
  Assert.AreEqual(FNotification.Entity.Channels, AChannels, 'Canais da notificação inválidos');
end;

initialization
  TDUnitX.RegisterTestFixture(TNotificationTest);
end.

