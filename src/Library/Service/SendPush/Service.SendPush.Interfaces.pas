unit Service.SendPush.Interfaces;

interface

type
  IServiceSendPush = interface
    ['{A378C421-4044-4739-9AF2-56F488ED3E84}']
    function CellPhone(AValue: String): IServiceSendPush;
    function &Message(AValue: String): IServiceSendPush;
    function Send: IServiceSendPush;
  end;

implementation

end.

