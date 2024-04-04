unit Service.SendPush.Factory.Interfaces;

interface

uses
  Service.SendPush.Interfaces;

type
  IServiceSendPushFactory = interface
    ['{C1F2315F-D744-4DC1-AA8A-A333A4C9FDE5}']
    function SendPush: IServiceSendPush;
  end;

implementation

end.

