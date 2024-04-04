unit View.Integracoes;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Base, Vcl.OleCtrls, SHDocVw;

type
  TFormIntegracoes = class(TFormBase)
    WebBrowser1: TWebBrowser;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormIntegracoes: TFormIntegracoes;

implementation

{$R *.dfm}

procedure TFormIntegracoes.FormShow(Sender: TObject);
begin
  inherited;
  LoadHTML('integrar-outros-sistemas.html', WebBrowser1);
end;

end.
