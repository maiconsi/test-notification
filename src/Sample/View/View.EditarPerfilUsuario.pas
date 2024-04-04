unit View.EditarPerfilUsuario;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  View.Base, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.StdCtrls, Vcl.WinXCtrls,
  Vcl.Buttons, Controller.Interfaces, Commons, Vcl.ExtDlgs, Vcl.CheckLst;

type
  TFormEditarPerfilUsuario = class(TFormBase)
    PanFotoPerfil: TPanel;
    ImgFotoPerfil: TImage;
    PanDadosUsuario: TPanel;
    Label1: TLabel;
    edtUsuario: TEdit;
    Label2: TLabel;
    edtNome: TEdit;
    Label3: TLabel;
    edtEmail: TEdit;
    Label4: TLabel;
    edtCelular: TEdit;
    Label5: TLabel;
    PanNotificacoes: TPanel;
    tswNotificacaoBackup: TToggleSwitch;
    tswNotificacaoTarefaVencida: TToggleSwitch;
    Label6: TLabel;
    lblNotificacaoBackup: TLabel;
    lblNotificacaoTarefaVencida: TLabel;
    Label9: TLabel;
    cbxFrequenciaNotificacaoBackup: TComboBox;
    cbxFrequenciaNotificacaoTarefaVencida: TComboBox;
    lblNotificacaoBackupInfo: TLabel;
    lblNotificacaoTarefaVencidaInfo: TLabel;
    btnSalvar: TBitBtn;
    btnCancelar: TBitBtn;
    OpenPictureDialog: TOpenPictureDialog;
    Label7: TLabel;
    lbxCanaisNotificacaoBackup: TCheckListBox;
    lbxCanaisNotificacaoTarefaVencida: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure tswNotificacaoBackupClick(Sender: TObject);
    procedure tswNotificacaoTarefaVencidaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImgFotoPerfilClick(Sender: TObject);
  private
    { Private declarations }
    FController: IControllerUser;
    procedure LoadListOfFrequencies(AFrequencies: TArray<String>);
    procedure LoadListOfChannels(AChannels: TArray<String>);
    procedure LoadSettingsNotificationBackup;
    procedure LoadSettingsNotificationtaskDue;
    procedure BindClasseToControl;
    procedure BindControlToClasse;
    procedure SaveSettingsNotificationBackup;
    procedure SaveSettingsNotificationtaskDue;
    procedure RefreshControlsNotificacaoBackup;
    procedure RefreshControlsNotificacaoTaskDue;
    procedure LoadPhoto;
  public
    { Public declarations }
  end;

var
  FormEditarPerfilUsuario: TFormEditarPerfilUsuario;

implementation

uses
  System.UITypes,

  Controller.Factory,
  Controller.Notification.Global,
  Model.Entity.Notification.Settings;

{$R *.dfm}

procedure TFormEditarPerfilUsuario.BindClasseToControl;
begin
  edtUsuario.Text:=  FController.Entity.Login;
  edtNome.Text   :=  FController.Entity.Name;
  edtEmail.Text  :=  FController.Entity.Email;
  edtCelular.Text:=  FController.Entity.Cellphone;

  LoadSettingsNotificationBackup;
  RefreshControlsNotificacaoBackup;

  LoadSettingsNotificationtaskDue;
  RefreshControlsNotificacaoTaskDue;

  LoadPhoto;
end;

procedure TFormEditarPerfilUsuario.BindControlToClasse;
begin
  FController.Entity.Login  :=  edtUsuario.Text;
  FController.Entity.Name   :=  edtNome.Text;
  FController.Entity.Email  :=  edtEmail.Text;
  FController.Entity.Cellphone  :=  edtCelular.Text;
end;

procedure TFormEditarPerfilUsuario.btnSalvarClick(Sender: TObject);
var
  LPhotoStream: TStream;
begin
  inherited;
  BindControlToClasse;

  FController.Save;

  SaveSettingsNotificationBackup;
  SaveSettingsNotificationtaskDue;

  LPhotoStream  :=  TMemoryStream.Create;
  try
    ImgFotoPerfil.Picture.Graphic.SaveToStream(LPhotoStream);
    LPhotoStream.Position :=  0;
    FController.Photo.Save(LPhotoStream);
  finally
    LPhotoStream.Free;
  end;
end;

procedure TFormEditarPerfilUsuario.FormCreate(Sender: TObject);
begin
  inherited;
  FController :=  TControllerFactory.New
                    .User
                      .GetCurrentUser;

  LoadListOfFrequencies(_ControllerNotification.GetListOfFrequency);
  LoadListOfChannels(_ControllerNotification.GetListOfChannels);
  BindClasseToControl;
end;

procedure TFormEditarPerfilUsuario.FormShow(Sender: TObject);
begin
  inherited;
  RoundImage(ImgFotoPerfil, PanFotoPerfil);
end;

procedure TFormEditarPerfilUsuario.ImgFotoPerfilClick(Sender: TObject);
begin
  inherited;
  if OpenPictureDialog.Execute then
  begin
    if FileExists(OpenPictureDialog.FileName) then
    begin
      ImgFotoPerfil.Picture.Graphic.LoadFromFile(OpenPictureDialog.FileName);
      ImgFotoPerfil.Repaint;
    end
    else
      raise Exception.Create('File does not exist.');
  end;
end;

procedure TFormEditarPerfilUsuario.LoadListOfChannels(AChannels: TArray<String>);
var
  LChannel: String;
begin
  lbxCanaisNotificacaoBackup.Items.Clear;
  lbxCanaisNotificacaoTarefaVencida.Items.Clear;
  for LChannel in AChannels do
  begin
    lbxCanaisNotificacaoBackup.Items.Add(LChannel);
    lbxCanaisNotificacaoTarefaVencida.Items.Add(LChannel);
  end;
end;

procedure TFormEditarPerfilUsuario.LoadListOfFrequencies(
  AFrequencies: TArray<String>);
var
  LFrequency: String;
begin
  cbxFrequenciaNotificacaoBackup.Items.Clear;
  cbxFrequenciaNotificacaoTarefaVencida.Items.Clear;
  for LFrequency in AFrequencies do
  begin
    cbxFrequenciaNotificacaoBackup.Items.Add(LFrequency);
    cbxFrequenciaNotificacaoTarefaVencida.Items.Add(LFrequency);
  end;
end;

procedure TFormEditarPerfilUsuario.LoadPhoto;
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

procedure TFormEditarPerfilUsuario.LoadSettingsNotificationBackup;
var
  LSettings: TNoticationSettings;
  LChannel: String;
begin
  LSettings :=  _ControllerNotification.GetUserNotificationBackup(FController.Entity.Id);
  try
    if LSettings.Active then
      tswNotificacaoBackup.State  :=  tssOn
    else
      tswNotificacaoBackup.State  :=  tssOff;

    cbxFrequenciaNotificacaoBackup.ItemIndex  :=  cbxFrequenciaNotificacaoBackup.Items.IndexOf(LSettings.Frequency);

    lbxCanaisNotificacaoBackup.CheckAll(cbUnchecked);
    for LChannel in LSettings.Channels do
      lbxCanaisNotificacaoBackup.Checked[lbxCanaisNotificacaoBackup.Items.IndexOf(LChannel)]  :=  True;

  finally
    LSettings.Free;
  end;
end;

procedure TFormEditarPerfilUsuario.LoadSettingsNotificationtaskDue;
var
  LSettings: TNoticationSettings;
  LChannel: String;
begin
  LSettings :=  _ControllerNotification.GetUserNotificationTaskDue(FController.Entity.Id);
  try
    if LSettings.Active then
      tswNotificacaoTarefaVencida.State  :=  tssOn
    else
      tswNotificacaoTarefaVencida.State  :=  tssOff;

    cbxFrequenciaNotificacaoTarefaVencida.ItemIndex  :=  cbxFrequenciaNotificacaoTarefaVencida.Items.IndexOf(LSettings.Frequency);

    lbxCanaisNotificacaoTarefaVencida.CheckAll(cbUnchecked);
    for LChannel in LSettings.Channels do
      lbxCanaisNotificacaoTarefaVencida.Checked[lbxCanaisNotificacaoTarefaVencida.Items.IndexOf(LChannel)]  :=  True;
  finally
    LSettings.Free;
  end;
end;

procedure TFormEditarPerfilUsuario.RefreshControlsNotificacaoBackup;
var
  LActive: Boolean;
begin
  LActive :=  (tswNotificacaoBackup.State = tssOn);

  cbxFrequenciaNotificacaoBackup.Enabled  :=  LActive;
  lbxCanaisNotificacaoBackup.Enabled      :=  LActive;
end;

procedure TFormEditarPerfilUsuario.RefreshControlsNotificacaoTaskDue;
var
  LActive: Boolean;
begin
  LActive :=  (tswNotificacaoTarefaVencida.State = tssOn);

  cbxFrequenciaNotificacaoTarefaVencida.Enabled :=  LActive;
  lbxCanaisNotificacaoTarefaVencida.Enabled     :=  LActive;
end;

procedure TFormEditarPerfilUsuario.SaveSettingsNotificationBackup;
var
  FSettings: TNoticationSettings;
  LCount: Integer;
begin
  FSettings :=  TNoticationSettings.Create;
  try
    FSettings.Active    :=  (tswNotificacaoBackup.State = tssOn);
    FSettings.Frequency :=  cbxFrequenciaNotificacaoBackup.Items[cbxFrequenciaNotificacaoBackup.ItemIndex];
    for LCount := 0 to Pred(lbxCanaisNotificacaoBackup.Items.Count) do
    begin
      if lbxCanaisNotificacaoBackup.Checked[LCount] then
        FSettings.AddChannel(lbxCanaisNotificacaoBackup.Items[LCount]);
    end;
    _ControllerNotification.SaveUserNotificationBackup(FController.Entity.Id, FSettings);
  finally
    FSettings.Free;
  end;
end;

procedure TFormEditarPerfilUsuario.SaveSettingsNotificationtaskDue;
var
  FSettings: TNoticationSettings;
  LCount: Integer;
begin
  FSettings :=  TNoticationSettings.Create;
  try
    FSettings.Active    :=  (tswNotificacaoTarefaVencida.State = tssOn);
    FSettings.Frequency :=  cbxFrequenciaNotificacaoTarefaVencida.Items[cbxFrequenciaNotificacaoTarefaVencida.ItemIndex];
    for LCount := 0 to Pred(lbxCanaisNotificacaoTarefaVencida.Items.Count) do
    begin
      if lbxCanaisNotificacaoTarefaVencida.Checked[LCount] then
        FSettings.AddChannel(lbxCanaisNotificacaoTarefaVencida.Items[LCount]);
    end;
    _ControllerNotification.SaveUserNotificationTaskDue(FController.Entity.Id, FSettings);
  finally
    FSettings.Free;
  end;
end;

procedure TFormEditarPerfilUsuario.tswNotificacaoBackupClick(Sender: TObject);
begin
  inherited;
  RefreshControlsNotificacaoBackup;
end;

procedure TFormEditarPerfilUsuario.tswNotificacaoTarefaVencidaClick(
  Sender: TObject);
begin
  inherited;
  RefreshControlsNotificacaoTaskDue;
end;

end.
