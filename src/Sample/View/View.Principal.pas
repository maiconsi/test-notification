unit View.Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.CategoryButtons, Vcl.WinXCtrls,
  Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.StdStyleActnCtrls,
  Vcl.ActnMan, System.ImageList, Vcl.ImgList, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,

  View.Base,
  Commons;

type
  TFormPrincipal = class(TFormBase)
    pnlBarraFerramentas: TPanel;
    imgMenu: TImage;
    lblTitulo: TLabel;
    Image32: TImageList;
    ActionManager: TActionManager;
    AcHome: TAction;
    AcPerfilUsuario: TAction;
    AcFecharPaginaAtual: TAction;
    AcFecharTodasPaginas: TAction;
    AcAbrirPaginaJanela: TAction;
    PopupMenu: TPopupMenu;
    menuFechar: TMenuItem;
    menuFecharTodasExcetoEssa: TMenuItem;
    N2: TMenuItem;
    menuSepararAba: TMenuItem;
    spvMenus: TSplitView;
    catMenuItems: TCategoryButtons;
    pgcListaPaginas: TPageControl;
    imgNotificacao: TImage;
    AcIntegracoes: TAction;
    AcNovaFormaNotificacao: TAction;
    procedure AcHomeExecute(Sender: TObject);
    procedure AcPerfilUsuarioExecute(Sender: TObject);
    procedure AcFecharPaginaAtualExecute(Sender: TObject);
    procedure AcFecharTodasPaginasExecute(Sender: TObject);
    procedure AcAbrirPaginaJanelaExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure imgMenuClick(Sender: TObject);
    procedure pgcListaPaginasChange(Sender: TObject);
    procedure AcIntegracoesExecute(Sender: TObject);
    procedure AcNovaFormaNotificacaoExecute(Sender: TObject);
  private
    { Private declarations }
    FNomeFormAtivo: String;

    function PodeAbrirFormulario(AClasseForm: TFormClass; var APagina: TTabSheet): Boolean;
    function TotalFormsAbertos(AClasseForm: TFormClass): Integer;
    procedure AjustarCaptionAbas(AClasseForm: TFormClass);
  public
    { Public declarations }
    procedure NovaPagina(AClasseForm: TFormClass);
    procedure SepararAba(APagina: TTabSheet);
    function FecharPagina(APagina: TTabSheet; AFreePagina : Boolean): Boolean; overload;
    function FecharPagina(APagina: TTabSheet; ATodasExcetoEssa: Boolean; AFreePagina : Boolean): Boolean; overload;
    function FecharTodasPaginas: Boolean;

    property NomeFormAtivo: String read FNomeFormAtivo write FNomeFormAtivo;
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses
  View.Home,
  View.PerfilUsuario,
  View.Integracoes,
  View.NovaFormaNotificacao;

{$R *.dfm}

procedure TFormPrincipal.AcAbrirPaginaJanelaExecute(Sender: TObject);
begin
  SepararAba(pgcListaPaginas.ActivePage);
end;

procedure TFormPrincipal.AcFecharPaginaAtualExecute(Sender: TObject);
begin
  FecharPagina(pgcListaPaginas.ActivePage, True);
end;

procedure TFormPrincipal.AcFecharTodasPaginasExecute(Sender: TObject);
begin
  FecharTodasPaginas;
end;

procedure TFormPrincipal.AcPerfilUsuarioExecute(Sender: TObject);
begin
  NovaPagina(TFormPerfilUsuario);
end;

procedure TFormPrincipal.AcNovaFormaNotificacaoExecute(Sender: TObject);
begin
  inherited;
  NovaPagina(TFormNovaFormaNotificacao);
end;

procedure TFormPrincipal.AcHomeExecute(Sender: TObject);
begin
  NovaPagina(TFormHome);
end;

procedure TFormPrincipal.AcIntegracoesExecute(Sender: TObject);
begin
  inherited;
  NovaPagina(TFormIntegracoes);
end;

procedure TFormPrincipal.AjustarCaptionAbas(AClasseForm: TFormClass);
var
  I: Integer;
  LIndice: Integer;
  LTotalForms: Integer;
  LNovoCaption: string;
begin
  LTotalForms := TotalFormsAbertos(AClasseForm);

  if LTotalForms > 1 then
  begin
    LIndice := 1;
    for I := 0 to pgcListaPaginas.PageCount - 1 do
    begin
      with pgcListaPaginas do
      begin
        if Pages[I].Components[0].ClassType = AClasseForm then
        begin
          LNovoCaption := (Pages[I].Components[0] as TForm).Caption + ' (' + IntToStr(LIndice) + ')';
          (Pages[I] as TTabSheet).Caption := LNovoCaption;
          Inc(LIndice);
        end;
      end;
    end;
  end;

end;

function TFormPrincipal.FecharPagina(APagina: TTabSheet; AFreePagina: Boolean): Boolean;
var
  LForm: TForm;
  LClasseForm: TFormClass;
  LNovaPaginaAtiva: TTabSheet;
begin
  LForm :=  nil;
  LClasseForm :=  nil;
  LNovaPaginaAtiva := nil;
  Try
    LForm := (APagina.Components[0] as TForm);
    LClasseForm :=  TFormClass(LForm.ClassType);

    Result := LForm.CloseQuery;
  Except
    //--Evitar erros de ListOfBond -1
    Result  :=  False;
  End;

  if Result then
  begin
    if APagina.PageIndex > 0 then
      LNovaPaginaAtiva := pgcListaPaginas.Pages[APagina.PageIndex - 1]
    else
    begin
      if pgcListaPaginas.PageCount > 1 then
        LNovaPaginaAtiva :=  pgcListaPaginas.Pages[0];
    end;

    //--Elimino o LForm da memoria "garanto que não vai ficar nada em memoria"
    LForm.Close;
    FreeAndNil(LForm);

    //--Para resolver o problema quando o usuario clica no botão X da aba
    if AFreePagina then
      APagina.Free;

    //Ajusta o título (caption) das abas
    AjustarCaptionAbas(LClasseForm);

    //--Seto a nova APagina a tiva
    pgcListaPaginas.ActivePage  := LNovaPaginaAtiva;
    pgcListaPaginas.OnChange(Self);

    //--Ajusto o Caption do FormPrincipal caso não existe nenuma aba ativa
    if LNovaPaginaAtiva = nil then
    begin
      Caption := 'App Delphi';
      Application.Title := Caption
    end;
  end;
end;

function TFormPrincipal.FecharPagina(APagina: TTabSheet; ATodasExcetoEssa,
  AFreePagina: Boolean): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := pgcListaPaginas.PageCount - 1 downto 0 do
  begin
    if pgcListaPaginas.Pages[I] <> APagina then
    begin
      Result := FecharPagina(pgcListaPaginas.Pages[I], AFreePagina);
      if not Result then
        Exit;
    end;
  end;
end;

function TFormPrincipal.FecharTodasPaginas: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := pgcListaPaginas.PageCount - 1 downto 0 do
  begin
    Result := FecharPagina(pgcListaPaginas.Pages[I], True);
    if not Result then
      Exit;
  end;
end;

procedure TFormPrincipal.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FecharTodasPaginas;
end;

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  FNomeFormAtivo := '';

  AcHome.Execute;
end;

procedure TFormPrincipal.imgMenuClick(Sender: TObject);
begin
  if spvMenus.Opened then
    spvMenus.Close
  else
    spvMenus.Open;
end;

procedure TFormPrincipal.NovaPagina(AClasseForm: TFormClass);
var
  LPagina: TTabSheet;
  LForm: TForm;
begin
  //Verifica se pode abrir o LForm
  if not PodeAbrirFormulario(AClasseForm, LPagina) then
  begin
    pgcListaPaginas.ActivePage := LPagina;
    Exit;
  end;

  //Instancia uma página padrão
  LPagina := TTabSheet.Create(pgcListaPaginas);

  //Seta a PageList da nova página para aquela que já está no LForm principal
  LPagina.PageControl := pgcListaPaginas;

  //Cria um LForm passando a página para o seu construtor, que recebe um TComponent
  LForm := AClasseForm.Create(LPagina);

  //Propriedades do LForm
  LForm.Align       :=  alClient;
  LForm.BorderStyle :=  bsNone;
  LForm.BorderIcons :=  [biSystemMenu];
  LForm.Parent      :=  LPagina;

  //Propriedades da Aba
  LPagina.Caption   := LForm.Caption;
  LPagina.PopupMenu := Self.PopupMenu;

  //Ajusta o título (caption) das abas
  AjustarCaptionAbas(AClasseForm);

  //Ativa a página
  pgcListaPaginas.ActivePage := LPagina;

  FNomeFormAtivo :=  LForm.Name;

  //Ajusto o Caption do FormPrincipal
  Caption := LPagina.Caption;
  Application.Title := Caption;

  //Exibe o formulário
  LForm.Show;
end;

procedure TFormPrincipal.pgcListaPaginasChange(Sender: TObject);
begin
  if pgcListaPaginas.ActivePage.Caption <> '' then
  begin
    Caption := pgcListaPaginas.ActivePage.Caption;
    Application.Title := Caption;

    with (pgcListaPaginas.ActivePage.Components[0] as TForm) do
    begin
      NomeFormAtivo :=  Name;
      if not Assigned(Parent) then
        Show;
    end;
  end;
end;

function TFormPrincipal.PodeAbrirFormulario(AClasseForm: TFormClass;
  var APagina: TTabSheet): Boolean;
var
  I: Integer;
begin
  Result := True;
  //varre a pgcListaPaginas para saber se já existe um Form aberto
  for I := 0 to pgcListaPaginas.PageCount - 1 do
  begin
    //se achou um form
    if pgcListaPaginas.Pages[I].Components[0].ClassType = AClasseForm then
    begin
      APagina := pgcListaPaginas.Pages[I];
      //permite abrir o form novamente caso a Tag tenha o valor zero
      Result := (APagina.Components[0] as TForm).Tag = 0;
      Break;
    end;
  end;
end;

procedure TFormPrincipal.SepararAba(APagina: TTabSheet);
Var
  LPaginaEsquerda: TTabSheet;
begin
  LPaginaEsquerda := nil;

  with APagina.Components[0] as TForm do
  begin
    Align       := alNone;
    BorderStyle := bsSizeable;
    Parent      := Self;
  end;

  //Tratamento para esconter a aba do pagecontrol
  if APagina.PageIndex > 0 then
    LPaginaEsquerda := pgcListaPaginas.Pages[APagina.PageIndex - 1];

  APagina.TabVisible :=  False;

  pgcListaPaginas.ActivePage := LPaginaEsquerda;
  pgcListaPaginas.OnChange(Self);
end;

function TFormPrincipal.TotalFormsAbertos(AClasseForm: TFormClass): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to pgcListaPaginas.PageCount - 1 do
  begin
    if pgcListaPaginas.Pages[I].Components[0].ClassType = AClasseForm then
      Inc(Result);
  end;

end;

end.
