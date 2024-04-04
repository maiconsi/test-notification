unit Commons;

interface

uses
  System.SysUtils;

type
  TFrequency = (Daily, Weekly, Monthly);

  // Custom Exceptions
  EExceptionBase = class(Exception)
  protected
    FDetailedMessage: string;
  public
    constructor Create(const AMsg: string); overload; virtual;
    constructor Create(const AMsg: string; const ADetailedMessage: string); overload; virtual;

    property DetailedMessage: String read FDetailedMessage write FDetailedMessage;
  end;

  EEntityValidation = class(EExceptionBase);
  EEntityException = class(EExceptionBase);
  EModelValidation = class(EExceptionBase);
  EModelException = class(EExceptionBase);
  EServiceException = class(EExceptionBase);
  ERepositoryvalidation = class(EExceptionBase);
  ERepositoryException = class(EExceptionBase);
  ENotFoundException = class(EExceptionBase);

implementation

{ EExceptionBase }

constructor EExceptionBase.Create(const AMsg, ADetailedMessage: string);
begin
  Create(AMsg);
  FDetailedMessage := ADetailedMessage;
end;

constructor EExceptionBase.Create(const AMsg: string);
begin
  inherited Create(AMsg);
end;

end.
