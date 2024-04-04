unit Service.SendSMS.Factory.Interfaces;

interface

uses
  Service.SendSMS.Interfaces;

type
  IServiceSendSMSFactory = interface
    ['{8F4F6FFB-C090-4C0A-83CF-4135E56CF03B}']
    function SendSMS: IServiceSendSMS;
  end;

implementation

end.

