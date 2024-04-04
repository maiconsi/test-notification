unit Model.Entity.Notification.Settings;

interface

uses
  Commons,
  Entity.Abstract;

type
  TNoticationSettings = class(TEntityAbstract)
  private
    FActive: Boolean;
    FFrequency: String;
    FChannels: TArray<String>;
  protected
    procedure BasicValidation; override;
  public
    constructor Create;

    procedure Validate; override;

    function AddChannel(AChannel: String): TNoticationSettings;
    function ClearChannels: TNoticationSettings;
    function ExistsChannelInList(const AChannelFind: array of String): Boolean;

    property Active: Boolean read FActive write FActive;
    property Frequency: String read FFrequency write FFrequency;
    property Channels: TArray<String> read FChannels write FChannels;
  end;

implementation

uses
  System.Generics.Collections;

{ TNoticationSettings }

function TNoticationSettings.AddChannel(AChannel: String): TNoticationSettings;
begin
  Result  :=  Self;

  SetLength(FChannels, Length(FChannels) + 1);
  FChannels[High(FChannels)] := AChannel;
end;

procedure TNoticationSettings.BasicValidation;
begin
  inherited;

end;

function TNoticationSettings.ClearChannels: TNoticationSettings;
begin
  Result  :=  Self;

  Finalize(FChannels);
  SetLength(FChannels, 0);
end;

constructor TNoticationSettings.Create;
begin
  FActive   :=  False;
  FFrequency:=  '';
end;

function TNoticationSettings.ExistsChannelInList(const AChannelFind: array of String): Boolean;
var
  LChannel: String;
  LFoundIndex: Integer;
begin
  Result  :=  False;

  for LChannel in FChannels do
  begin
    if TArray.BinarySearch<String>(AChannelFind, LChannel, LFoundIndex) then
      Exit(True);
  end;
end;

procedure TNoticationSettings.Validate;
begin
  inherited;

end;

end.

