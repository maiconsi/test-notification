unit View.Base;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, SHDocVw;

type
  TFormBase = class(TForm)
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  protected
    procedure RoundImage(AImage: TImage; APanel: TPanel);
    procedure LoadHTML(AFileName: String; AWebBrowser: TWebBrowser);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormBase: TFormBase;

implementation

uses
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  System.IOUtils;

{$R *.dfm}

procedure TFormBase.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    // Verifica se foi precionada a tecla Enter e manda o foco para o proximo componente
    #13: begin
      if (ActiveControl is TLabeledEdit) or
        (ActiveControl is TEdit) or
        (ActiveControl is TSpinEdit) then
      begin
        Perform(Wm_NextDlgCtl, 0, 0);
        Key:= #0;
      end;
    end;
  end;
end;

procedure TFormBase.LoadHTML(AFileName: String; AWebBrowser: TWebBrowser);
var
  LPath: String;
  LNameFileHTML: String;
begin
  LPath :=  TPath.Combine(ExtractFilePath(Application.ExeName), 'www');
  LNameFileHTML :=  TPath.Combine(LPath, AFileName);
  if FileExists(LNameFileHTML) then
    AWebBrowser.Navigate(Format('file://%s', [LNameFileHTML]));
end;

procedure TFormBase.RoundImage(AImage: TImage; APanel: TPanel);
var
  LRoundRect: HRGN;
  LDC: HDC;
begin
  LRoundRect := CreateRoundRectRgn(0, 0, AImage.Width, AImage.Height, AImage.Width, AImage.Height);
  LDC := GetDC(APanel.Handle);
  SetWindowRgn(APanel.Handle, LRoundRect, true);
  ReleaseDC(APanel.Handle, LDC);
  DeleteObject(LRoundRect);
end;

end.
