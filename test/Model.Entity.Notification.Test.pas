unit Model.Entity.Notification.Test;

interface

uses
  DUnitX.TestFramework,

  Model.Entity.Notification, Commons;

type

  [TestFixture]
  TEntityNotificationTest = class(TObject)
  private
    FNotification: Tnotification;
  public
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;
  [Test]
  [TestCase('TestCaseTitleIsEmpty', ',Mensagem teste,Usuario teste,Email')]
  [TestCase('TestCaseMessageIsEmpty', 'Titulo teste,,Usuario teste,Email')]
  [TestCase('TestCaseUserNameIsEmpty', 'Titulo teste,Mensagem teste,,Email')]
  [TestCase('TestCaseChannelIsEmpty', 'Titulo teste,Mensagem teste,Usuario teste,')]
  procedure TestNotificationValidate(ATitle, AMessage, AUserName: String; AChannels: TArray<String>);
  [Test]
  [TestCase('TestCaseNotificationNew', 'Titulo teste,Mensagem teste,Usuario teste,email.teste@email.com.br,+55(18)99716-1234,Email')]
  procedure TestNotificationNew(const ATitle, AMessage, AUserName, AUserEmail,
  AUserCellpone: string; const AChannels: TArray<String>);
  [Test]
  [TestCase('TestCaseAddChannel', 'Email')]
  procedure TestAddChannel(const AChannel: String);
  end;

implementation

{ TEntityNotificationTest }

procedure TEntityNotificationTest.Setup;
begin
  FNotification :=  Tnotification.Create;
end;

procedure TEntityNotificationTest.TearDown;
begin
  FNotification.Free;
end;

procedure TEntityNotificationTest.TestAddChannel(const AChannel: String);
begin
  FNotification.AddChannel(AChannel);

  Assert.AreEqual(FNotification.Channels[0], AChannel, 'Canais da notificação inválidos');
end;

procedure TEntityNotificationTest.TestNotificationNew(const ATitle, AMessage, AUserName, AUserEmail,
  AUserCellpone: string; const AChannels: TArray<String>);
begin
  FNotification.Free;

  FNotification :=  Tnotification.New(ATitle,
                                      AMessage,
                                      AUserName,
                                      AUserEmail,
                                      AUserCellpone,
                                      AChannels);

  Assert.AreEqual(FNotification.Title, ATitle, 'Titulo da notificação inválido');
  Assert.AreEqual(FNotification.Message, AMessage, 'Mensagem da notificação inválida');
  Assert.AreEqual(FNotification.UserName, AUserName, 'Usuário da notificação inválido');
  Assert.AreEqual(FNotification.UserEmail, AUserEmail, 'Email da notificação inválido');
  Assert.AreEqual(FNotification.UserCellphone, AUserCellpone, 'Celular da notificação inválido');
  Assert.AreEqual(FNotification.Channels, AChannels, 'Canais da notificação inválidos');
end;

procedure TEntityNotificationTest.TestNotificationValidate(ATitle, AMessage, AUserName: String; AChannels: TArray<String>);
begin
  FNotification.Title   :=  ATitle;
  FNotification.Message :=  AMessage;
  FNotification.UserName:=  AUserName;
  FNotification.Channels:=  AChannels;

  Assert.WillRaise(procedure
                    begin
                      FNotification.Validate;
                    end, nil, 'Valição da notificação não está ocorrendo corretamente');
end;

initialization
  TDUnitX.RegisterTestFixture(TEntityNotificationTest);
end.

