unit Entity.Abstract;

interface

uses
  System.JSON;

type
  TEntityAbstract = class
  private
    { private declarations }
  protected
    { protected declarations }
    procedure BasicValidation; virtual;
  public
    { public declarations }
    procedure LoadJsonString(AJsonString: String);

    //Prototype
    function Clone<T: class, constructor>: T;

    function ToJsonObject: TJSONObject;
    function ToJsonString(ACompact: Boolean = False): String;

    procedure CheckInsert; virtual;
    procedure CheckUpdate; virtual;
    procedure CheckDelete; virtual;
    procedure Validate; virtual;
  end;

implementation

uses
  REST.Json, System.SysUtils;

{ TEntityAbstract }

procedure TEntityAbstract.BasicValidation;
begin
  //Implementar nas classes filhas
end;

procedure TEntityAbstract.CheckDelete;
begin

end;

procedure TEntityAbstract.CheckInsert;
begin
  BasicValidation;
end;

procedure TEntityAbstract.CheckUpdate;
begin
  BasicValidation;
end;

function TEntityAbstract.Clone<T>: T;
begin
  Result  :=  T.Create;
end;

procedure TEntityAbstract.LoadJsonString(AJsonString: String);
var
  LJsonObject: TJSONObject;
begin
  LJsonObject :=  TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(AJsonString), 0) as TJSONObject;
  try
    TJson.JsonToObject(Self, LJsonObject);
  finally
    LJsonObject.Free;
  end;
end;

function TEntityAbstract.ToJsonObject: TJSONObject;
begin
  Result  :=  TJson.ObjectToJsonObject(Self);
end;

function TEntityAbstract.ToJsonString(ACompact: Boolean): String;
begin
  Result  :=  TJson.ObjectToJsonString(Self);
end;

procedure TEntityAbstract.Validate;
begin
  BasicValidation;
end;

end.
