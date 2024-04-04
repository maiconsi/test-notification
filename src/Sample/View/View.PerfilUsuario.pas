unit View.PerfilUsuario;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Base, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Imaging.pngimage,

  Controller.Interfaces, Commons;

type
  TFormPerfilUsuario = class(TFormBase)
    PanPerfilUsuario: TPanel;
    PanFundo: TPanel;
    ShapeFundo: TShape;
    sbtEditarPerfil: TSpeedButton;
    lblNome: TLabel;
    lblUsuario: TLabel;
    Label3: TLabel;
    lblEmail: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    lblCelular: TLabel;
    Label6: TLabel;
    Shape3: TShape;
    lblNotificacaoBackup: TLabel;
    Label8: TLabel;
    Shape4: TShape;
    Label9: TLabel;
    lblNotificacaoTarefaVencida: TLabel;
    PanFotoPerfil: TPanel;
    ImgFotoPerfil: TImage;
    PanBadgesNotificacaoBackup: TPanel;
    PanBadgesNotificacaoTarefaVencida: TPanel;
    sbtNotificacaoBackupTeste: TSpeedButton;
    sbtNotificacaoTarefaVencidaTeste: TSpeedButton;
    lbxNotificacaoBackupCanais: TListBox;
    lbxNotificacaoTarefaVencidaCanais: TListBox;
    procedure sbtEditarPerfilClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbtNotificacaoBackupTesteClick(Sender: TObject);
    procedure sbtNotificacaoTarefaVencidaTesteClick(Sender: TObject);
  private
    { Private declarations }
    FController: IControllerUser;
    procedure LoadDataCurrentUser;
    procedure BindClasseToControl;
    procedure RefreshControlsNotificacaoBackup;
    procedure RefreshControlsNotificacaoTaskDue;
    procedure LoadPhoto;
  public
    { Public declarations }
  end;

var
  FormPerfilUsuario: TFormPerfilUsuario;

implementation

uses
  System.StrUtils,
  System.UITypes,
  System.Generics.Collections,

  View.EditarPerfilUsuario,
  Controller.Factory,
  Controller.Notification.Global, Model.Entity.Notification.Settings;

{$R *.dfm}

procedure TFormPerfilUsuario.BindClasseToControl;
begin
  lblNome.Caption   :=  FController.Entity.Name;
  lblUsuario.Caption:=  FController.Entity.Login;
  lblEmail.Caption  :=  FController.Entity.Email;
  lblCelular.Caption:=  FController.Entity.Cellphone;

  RefreshControlsNotificacaoBackup;
  RefreshControlsNotificacaoTaskDue;

  LoadPhoto;
end;

procedure TFormPerfilUsuario.FormCreate(Sender: TObject);
begin
  inherited;
  LoadDataCurrentUser;
end;

procedure TFormPerfilUsuario.FormShow(Sender: TObject);
begin
  inherited;
  RoundImage(ImgFotoPerfil, PanFotoPerfil);
end;

procedure TFormPerfilUsuario.LoadDataCurrentUser;
begin
  FController :=  TControllerFactory.New
                    .User
                      .GetCurrentUser;

  BindClasseToControl;
end;

procedure TFormPerfilUsuario.LoadPhoto;
var
  LPhotoStream: TStream;
begin
  LPhotoStream  :=  FController.Photo.Get;
  try
    ImgFotoPerfil.Picture.LoadFromStream(LPhotoStream);
  finally
    LPhotoStream.Free;
  end;
end;

procedure TFormPerfilUsuario.RefreshControlsNotificacaoBackup;
var
  LSettings: TNoticationSettings;
  LChannel: String;
begin
  LSettings :=  _ControllerNotification.GetUserNotificationBackup(FController.Entity.Id);
  try
    lblNotificacaoBackup.Caption          :=  IfThen(LSettings.Active, 'Ativa', 'Inativa');

    lbxNotificacaoBackupCanais.Items.Clear;
    if LSettings.Active then
    begin
      lbxNotificacaoBackupCanais.Columns  :=  Length(LSettings.Channels);
      for LChannel in LSettings.Channels do
        lbxNotificacaoBackupCanais.Items.Add(LChannel);
    end;

    sbtNotificacaoBackupTeste.Visible     :=  LSettings.Active;
  finally
    LSettings.Free;
  end;
end;

procedure TFormPerfilUsuario.RefreshControlsNotificacaoTaskDue;
var
  LSettings: TNoticationSettings;
  LChannel: String;
begin
  LSettings :=  _ControllerNotification.GetUserNotificationTaskDue(FController.Entity.Id);
  try
    lblNotificacaoTarefaVencida.Caption :=  IfThen(LSettings.Active, 'Ativa', 'Inativa');

    lbxNotificacaoTarefaVencidaCanais.Items.Clear;
    if LSettings.Active then
    begin
      lbxNotificacaoTarefaVencidaCanais.Columns  :=  Length(LSettings.Channels);
      for LChannel in LSettings.Channels do
        lbxNotificacaoTarefaVencidaCanais.Items.Add(LChannel);
    end;

    sbtNotificacaoTarefaVencidaTeste.Visible     :=  LSettings.Active;
  finally
    LSettings.Free;
  end;
end;

procedure TFormPerfilUsuario.sbtEditarPerfilClick(Sender: TObject);
begin
  inherited;
  Application.CreateForm(TFormEditarPerfilUsuario, FormEditarPerfilUsuario);
  try
    if FormEditarPerfilUsuario.ShowModal = mrOk then
      LoadDataCurrentUser;
  finally
    FormEditarPerfilUsuario.Free;
  end;
end;

procedure TFormPerfilUsuario.sbtNotificacaoBackupTesteClick(Sender: TObject);
begin
  inherited;
  _ControllerNotification.SendNotificationBackupNow;

  MessageDlg(Format('Notificação de teste enviada com sucesso!%s%s', [#13, 'Para detalhes do envio consultar o arquivo de log do sistema na pasta ".\logs"']), mtInformation, [mbOK], 0);
end;

procedure TFormPerfilUsuario.sbtNotificacaoTarefaVencidaTesteClick(
  Sender: TObject);
begin
  inherited;
  _ControllerNotification.SendNotificationTaskDueNow;

  MessageDlg(Format('Notificação de teste enviada com sucesso!%s%s', [#13, 'Para detalhes do envio consultar o arquivo de log do sistema na pasta ".\logs"']), mtInformation, [mbOK], 0);
end;

end.
