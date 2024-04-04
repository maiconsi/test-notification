unit View.NovaFormaNotificacao;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Base, Vcl.OleCtrls, SHDocVw;

type
  TFormNovaFormaNotificacao = class(TFormBase)
    WebBrowser1: TWebBrowser;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNovaFormaNotificacao: TFormNovaFormaNotificacao;

implementation

{$R *.dfm}

procedure TFormNovaFormaNotificacao.FormShow(Sender: TObject);
begin
  inherited;
  LoadHTML('adicionar-nova-notificacao.html', WebBrowser1);
end;

end.
