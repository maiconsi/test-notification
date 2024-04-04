program Sample;

uses
  Vcl.Forms,
  View.Principal in 'View\View.Principal.pas' {FormPrincipal},
  Commons in '..\Core\Commons.pas',
  Utils in '..\Core\Utils\Utils.pas',
  View.Base in 'View\View.Base.pas' {FormBase},
  View.PerfilUsuario in 'View\View.PerfilUsuario.pas' {FormPerfilUsuario},
  View.EditarPerfilUsuario in 'View\View.EditarPerfilUsuario.pas' {FormEditarPerfilUsuario},
  View.Home in 'View\View.Home.pas' {FormHome},
  Model.Interfaces in 'Model\Model.Interfaces.pas',
  Model.User in 'Model\Model.User.pas',
  Repository.Interfaces in 'Repository\Repository.Interfaces.pas',
  Repository.User in 'Repository\Repository.User.pas',
  Repository.Factory.Interfaces in 'Repository\Repository.Factory.Interfaces.pas',
  Repository.Factory in 'Repository\Repository.Factory.pas',
  Model.Resource.Factory.Interfaces in '..\Core\Resource\Model.Resource.Factory.Interfaces.pas',
  Model.Resource.Factory in '..\Core\Resource\Model.Resource.Factory.pas',
  Model.Resource.Interfaces in '..\Core\Resource\Model.Resource.Interfaces.pas',
  Model.Resource in '..\Core\Resource\Model.Resource.pas',
  Model.Storage.Factory.Interfaces in '..\Core\Storage\Model.Storage.Factory.Interfaces.pas',
  Model.Storage.Factory in '..\Core\Storage\Model.Storage.Factory.pas',
  Model.Storage.Interfaces in '..\Core\Storage\Model.Storage.Interfaces.pas',
  Model.Storage.Local in '..\Core\Storage\Model.Storage.Local.pas',
  Model.User.Photo in 'Model\Model.User.Photo.pas',
  Controller.Interfaces in 'Controller\Controller.Interfaces.pas',
  Controller.User in 'Controller\Controller.User.pas',
  Model.Factory.Interfaces in 'Model\Model.Factory.Interfaces.pas',
  Model.Factory in 'Model\Model.Factory.pas',
  Controller.Factory.Interfaces in 'Controller\Controller.Factory.Interfaces.pas',
  Controller.Factory in 'Controller\Controller.Factory.pas',
  Controller.Notification in 'Controller\Controller.Notification.pas',
  View.Integracoes in 'View\View.Integracoes.pas' {FormIntegracoes},
  View.NovaFormaNotificacao in 'View\View.NovaFormaNotificacao.pas' {FormNovaFormaNotificacao},
  Entity.Abstract in '..\Core\Entity\Entity.Abstract.pas',
  Model.Notification.DLLMapping in 'Model\Model.Notification.DLLMapping.pas',
  Controller.Notification.Global in 'Controller\Controller.Notification.Global.pas',
  Model.Entity.User in 'Model\Entity\Model.Entity.User.pas',
  Model.Entity.Notification.Settings in 'Model\Entity\Model.Entity.Notification.Settings.pas',
  Model.Notification in 'Model\Model.Notification.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.CreateForm(TFormIntegracoes, FormIntegracoes);
  Application.CreateForm(TFormNovaFormaNotificacao, FormNovaFormaNotificacao);
  Application.Run;
end.
