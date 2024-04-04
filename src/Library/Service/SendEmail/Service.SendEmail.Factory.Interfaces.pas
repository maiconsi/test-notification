unit Service.SendEmail.Factory.Interfaces;

interface

uses
  Service.SendEmail.Interfaces;

type
  IServiceSendEmailFactory = interface
    ['{FCE21E4C-FB25-425E-9B88-9DBC5915DF4B}']
    function SendEmail: IServiceSendEmail;
    function Engine(AParent: IServiceSendEmailConfig): IServiceSendEmailEngine;
  end;

implementation

end.

