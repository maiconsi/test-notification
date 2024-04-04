# Adicionar novas formas de envio de notificação na DLL

### Passos para adicionar uma nova forma de notificação na DLL

> Como exemplo vou utilizar uma notificação que irá utilizar o **'whatsapp'**

* Criar um novo arquivo para o envio das notificação pelo novo canal **WhatsApp** com o seguinte nome: **Model.Notification.Channel.WhatsApp.pas**
  > Utilizar como referência qualquer um dos outros métodos de envio Ex.: **TModelNotificationChannelEmail**
  * A classe deve herdar de **TModelNotificationChannelAbstract**
    ``` Pascal
    TModelNotificationChannelWhastApp = class(TModelNotificationChannelAbstract)
    protected
        procedure InternalSend; override;
    public
        constructor Create(AParent: IModelNotificationConfig); override;
    end;
    ```
  * Implementar o Create para inserir o nome do canal
    ``` Pascal  
    constructor TModelNotificationChannelWhastApp.Create(
    AParent: IModelNotificationConfig);
    begin
        inherited Create(AParent);

        FChannel  :=  'Whastsapp';
    end;
    ```
  * Sobrescrever o método **InternalSend** na classe concreta para realizar a implentação do envio da nova notificação
    > É recomendado criar um serviço para realizar esse processo e apenas chamados dentro deste método 'InternalSend'
    ``` Pascal
    procedure TModelNotificationChannelWhastApp.InternalSend;
    begin
        // Função/Serviço que envia para o WhastApp
        TServiceSendWhastAppFactory.New
        .SendWhastApp
            .CellPhone(FParent.Entity.UserCellphone)
            .Message(FParent.Entity.Message)
        .Send;
    end;
    ```

    * Ajustar o inicialization e finalization da classe para registrar o novo canal

    ``` Pascal
    initialization
        _NotificationChannelManager.RegisterChannel('Whastsapp', TModelNotificationSendChannelWhatsApp);

    finalization
        _NotificationChannelManager.UnRegisterChannel('Whastsapp');
    ```