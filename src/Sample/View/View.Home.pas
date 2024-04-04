unit View.Home;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Base, Vcl.OleCtrls, SHDocVw;

type
  TFormHome = class(TFormBase)
    WebBrowser1: TWebBrowser;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHome: TFormHome;

implementation

Uses
  System.IOUtils;

{$R *.dfm}

{ TFormHome }

procedure TFormHome.FormShow(Sender: TObject);
begin
  inherited;
  LoadHTML('readme.html', WebBrowser1);
end;

end.
