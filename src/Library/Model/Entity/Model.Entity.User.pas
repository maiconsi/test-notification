unit Model.Entity.User;

interface

uses
  Entity.Abstract;

type
  TUser = class(TEntityAbstract)
  private
    FId: Integer;
    FLogin: String;
    FName: String;
    FEmail: String;
    FCellphone: String;

  protected
    procedure BasicValidation; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Validate; override;

    property Id: Integer read FId write FId;
    property Login: String read FLogin write FLogin;
    property Name: String read FName write FName;
    property Email: String read FEmail write FEmail;
    property Cellphone: String read FCellphone write FCellphone;
  end;

implementation

{ TUser }

procedure TUser.BasicValidation;
begin
  inherited;

end;

constructor TUser.Create;
begin

end;

destructor TUser.Destroy;
begin


  inherited;
end;

procedure TUser.Validate;
begin
  inherited;

end;

end.
