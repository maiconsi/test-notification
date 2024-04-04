unit Service.SendSMS.Interfaces;

interface

type
  IServiceSendSMS = interface
    ['{93041F4D-AB79-4CB9-AFF0-F52AD6599FAD}']
    function CellPhone(AValue: String): IServiceSendSMS;
    function &Message(AValue: String): IServiceSendSMS;
    function Send: IServiceSendSMS;
  end;

implementation

end.
