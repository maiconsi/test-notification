unit Service.SendEmail.Interfaces;

interface

uses
  System.Classes;

type
  IServiceSendEmail = interface
    ['{507FBBB2-DE22-430C-AEC7-3DAAA573E223}']
    function AddAddress(AEmail: String; AName: String): IServiceSendEmail;
    function AddCC(AEmail: String; AName: String): IServiceSendEmail;
    function AddAttachment(const AFileName: string): IServiceSendEmail; overload;
    function AddAttachment(const AFileName: string; ADescription: string): IServiceSendEmail; overload;
    function AddAttachment(AStream: TStream): IServiceSendEmail; overload;
    function AddAttachment(AStream: TStream; ADescription: string): IServiceSendEmail; overload;
    function Subject(AValue: String): IServiceSendEmail;
    function &Message(AValue: String): IServiceSendEmail;
    function Send: IServiceSendEmail;
  end;

  IServiceSendEmailConfig = interface
    ['{C477F326-8005-4ACE-887C-9DCC809C6D1D}']
    function This: IServiceSendEmail;
  end;

  IServiceSendEmailEngine = interface
    ['{0B5E27C3-C3D7-40E8-AB76-0AB0376AAD94}']
    function AddAddress(AEmail: String; AName: String): IServiceSendEmailEngine;
    function AddCC(AEmail: String; AName: String): IServiceSendEmailEngine;
    function AddAttachment(const AFileName: string): IServiceSendEmailEngine; overload;
    function AddAttachment(const AFileName: string; ADescription: string): IServiceSendEmailEngine; overload;
    function AddAttachment(AStream: TStream): IServiceSendEmailEngine; overload;
    function AddAttachment(AStream: TStream; ADescription: string): IServiceSendEmailEngine; overload;
    function Subject(AValue: String): IServiceSendEmailEngine;
    function &Message(AValue: String): IServiceSendEmailEngine;
    function Send: IServiceSendEmailEngine;
  end;

implementation

end.

