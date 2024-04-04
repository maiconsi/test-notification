unit Utils;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.TypInfo;

type

  TUtil = class
  private
  public
    class function GerarNovaGUID(ATiraTracos: Boolean = False): String;
    class function Arredondar(AValue: Double; AQuantidadeDecimais: Integer): Double;
    class function GetContentType(AFileName: String): String;
    class function BuildRedisKey(AResource, AKey: String): String;
    class function ArrayIntegerToIN(AList: TArray<System.Integer>): String;
  end;

  TUtilDate = class
    class function DataHoraUTC(ANow: TDateTime): TDateTime; overload;
    class function DataHoraUTCToLocalTime(ADateTimeUTC: TDateTime; AFusoUTC: Double): TDateTime; overload;
    class function DataHoraUTC: TDateTime; overload;
    class function DataUTC(ADate: TDateTime): TDate; overload;
    class function DataUTC: TDate; overload;
    class function DataPrimeiroDiaSemana(AData: TDate): TDate;
    class function DataUltimoDiaSemana(AData: TDate): TDate;
    class function DataPrimeiroDiaMes(AData: TDate): TDate;
    class function DataUltimoDiaMes(AData: TDate): TDate;
    class function SetHoraInicialDia(AData: TDate): TDateTime;
    class function SetHoraFinalDia(AData: TDate): TDateTime;
    class function IncDate(ADias: Integer): TDate; overload;
    class function IncDate(AData: TDate; ADias: Integer): TDate; overload;
    class function StrToDate(ADataStr: String): TDate; overload;
    class function StrToDate(ADataStr: String; ADateDef: TDate): TDate; overload;
    class function StrToDateTime(ADataStr: String): TDatetime; overload;
    class function StrToDateTime(ADataStr: String; ADateTimeDef: TDateTime): TDatetime; overload;
    class function DateToStrDB(AData: TDate): String;
    class function TimeToStrDB(AHora: TTime): String;
    class function DateTimeToStrDB(ADataHora: TDatetime): String;
  end;

  TUtilStr = class
  public
    class function ApenasNumerosStr(AValue:String): String;
    class function StrZero(AValue: String; AQuantidade: Integer):String;
    class function StrFull(AValue: String; ATamanho: Byte; AChar: Char; AFrente: Boolean = True):string;
    class function TiraPontos(AValue: string): string;
    class function TiraAcentos(AValue: string): string;
    class function TiraAcento( const AChar : AnsiChar ) : AnsiChar ;
    class procedure Split(const ADelimiter: Char; AInput: string; const AStrings: TStrings);
    class function Equals(CONST AValue1: String; CONST AValue2: String): Boolean; reintroduce; overload;
    class function StrToBoolean(AValue: String): Boolean;
  end;

  TUtilFiles = class
  public
    class function GetExtension(AFileName: String): String;
  end;

  TUtilEnumerator<T> = class
  strict private
    class function TypeInfo: PTypeInfo; inline; static;
    class function TypeData: PTypeData; inline; static;
  public
    class function IsEnum: Boolean; static;
    class function ToOrdinal(Enum: T): Integer; inline; static;
    class function ToString(Enum: T): String; reintroduce; inline; static;
    class function ToEnum(Value: Integer): T; overload;
    class function ToEnum(Value: String): T; overload;
    class function MinValue: Integer; inline; static;
    class function MaxValue: Integer; inline; static;
    class function InRange(Value: Integer): Boolean; inline; static;
    class function EnsureRange(Value: Integer): Integer; inline; static;
  end;

  TUtilVarRec = class
  public
    class function CopyVarRec(const AItem: TVarRec): TVarRec;
    class procedure FinalizeVarRec(var AItem: TVarRec);
  end;

implementation

uses
  System.IOUtils,
  System.DateUtils,
  System.AnsiStrings,

  Commons;

{ TUtil }

class function TUtil.ArrayIntegerToIN(AList: TArray<System.Integer>): String;
Var
  LItem: Integer;
  LListaStr: String;
begin
  LListaStr :=  '';

  for LItem in AList do
  begin
    if LListaStr = '' then
      LListaStr  :=  Format('%d', [LItem])
    else
      LListaStr  :=  Format('%s, %d', [LListaStr, LItem]);
  end;

  Result  :=  LListaStr;
end;

class function TUtil.Arredondar(AValue: Double;
  AQuantidadeDecimais: Integer): Double;
  function RoundABNT(const AValue: Double; const Digits: SmallInt):Double;
  var
     Pow, PowValue, RestPart : Extended;
     IntPart, FracPart, LastNumber : Integer;
  Begin
     Pow      := intpower(10, abs(Digits) );
     PowValue := SimpleRoundTo( AValue * Pow, -9) ; // SimpleRoundTo elimina dizimas ;
     IntPart  := trunc( PowValue );
     FracPart := trunc( frac( PowValue ) * 100);

     if (FracPart > 50) then
        Inc( IntPart )

     else if (FracPart = 50) then
      begin
        LastNumber := round( frac( IntPart / 10) * 10);

        if odd(LastNumber) then
           Inc( IntPart )
        else
         begin
           RestPart := frac( PowValue * 10 ) ;

           if RestPart > 0 then
              Inc( IntPart );
         end ;
      end ;

     Result := (IntPart / Pow);
  end;
begin
  Result := RoundABNT(AValue, AQuantidadeDecimais);
end;

class function TUtil.BuildRedisKey(AResource, AKey: String): String;
var
  LPrefixo: String;
begin
//  LPrefixo  :=  dotEnv.Env('environment', 'dev');

  Result  :=  Format('%s::%s:%s', [LPrefixo, AResource, AKey]);
end;

class function TUtil.GerarNovaGUID(ATiraTracos: Boolean = False): String;
var
  LID: TGUID;
  LGUID: String;
begin
  Result:=  '';
  LGUID :=  '';

  if CreateGUID(LID) = S_OK then
  begin
    LGUID := GUIDToString(LID);

    if ATiraTracos then
    begin
      LGUID :=  TUtilStr.TiraPontos(LGUID);
      LGUID :=  Copy(LGUID,2, 32);
    end;
  end;
  Result  :=  LGUID;
end;


class function TUtil.GetContentType(AFileName: String): String;
Var
  LExtensao: String;
begin
  Result  :=  '';

  LExtensao :=  ExtractFileExt(AFileName);

  if LExtensao = 'hqx' then
    Result  :=  'application/mac-binhex40'
  else if LExtensao = 'cpt' then
    Result  :=  'application/mac-compactpro'
  else if LExtensao = 'csv' then
    Result  :=  'text/x-comma-separated-values'
  else if LExtensao = 'bin' then
    Result  :=  'application/macbinary'
  else if LExtensao = 'dms' then
    Result  :=  'application/octet-stream'
  else if LExtensao = 'lha' then
    Result  :=  'application/octet-stream'
  else if LExtensao = 'lzh' then
    Result  :=  'application/octet-stream'
  else if LExtensao = 'exe' then
    Result  :=  'application/octet-stream'
  else if LExtensao = 'class' then
    Result  :=  'application/octet-stream'
  else if LExtensao = 'psd' then
    Result  :=  'application/x-photoshop'
  else if LExtensao = 'so' then
    Result  :=  'application/octet-stream'
  else if LExtensao = 'sea' then
    Result  :=  'application/octet-stream'
  else if LExtensao = 'dll' then
    Result  :=  'application/octet-stream'
  else if LExtensao = 'oda' then
    Result  :=  'application/oda'
  else if LExtensao = 'pdf' then
    Result  :=  'application/pdf'
  else if LExtensao = 'ai' then
    Result  :=  'application/postscript'
  else if LExtensao = 'eps' then
    Result  :=  'application/postscript'
  else if LExtensao = 'ps' then
    Result  :=  'application/postscript'
  else if LExtensao = 'smi' then
    Result  :=  'application/smil'
  else if LExtensao = 'smil' then
    Result  :=  'application/smil'
  else if LExtensao = 'mif' then
    Result  :=  'application/vnd.mif'
  else if LExtensao = 'xls' then
    Result  :=  'application/excel'
  else if LExtensao = 'ppt'  then
    Result  :=  'application/powerpoint'
  else if LExtensao = 'wbxml' then
    Result  :=  'application/wbxml'
  else if LExtensao = 'wmlc' then
    Result  :=  'application/wmlc'
  else if LExtensao = 'dcr' then
    Result  :=  'application/x-director'
  else if LExtensao = 'dir' then
    Result  :=  'application/x-director'
  else if LExtensao = 'dxr' then
    Result  :=  'application/x-director'
  else if LExtensao = 'dvi' then
    Result  :=  'application/x-dvi'
  else if LExtensao = 'gtar' then
    Result  :=  'application/x-gtar'
  else if LExtensao = 'gz' then
    Result  :=  'application/x-gzip'
  else if LExtensao = 'php' then
    Result  :=  'application/x-httpd-php'
  else if LExtensao = 'php4' then
    Result  :=  'application/x-httpd-php'
  else if LExtensao = 'php3' then
    Result  :=  'application/x-httpd-php'
  else if LExtensao = 'phtml' then
    Result  :=  'application/x-httpd-php'
  else if LExtensao = 'phps' then
    Result  :=  'application/x-httpd-php-source'
  else if LExtensao = 'js' then
    Result  :=  'application/x-javascript'
  else if LExtensao = 'swf' then
    Result  :=  'application/x-shockwave-flash'
  else if LExtensao = 'sit' then
    Result  :=  'application/x-stuffit'
  else if LExtensao = 'tar' then
    Result  :=  'application/x-tar'
  else if LExtensao = 'tgz' then
    Result  :=  'application/x-tar'
  else if LExtensao = 'xhtml' then
    Result  :=  'application/xhtml+xml'
  else if LExtensao = 'xht' then
    Result  :=  'application/xhtml+xml'
  else if LExtensao = 'zip' then
    Result  :=  'application/zip'
  else if LExtensao = 'mid' then
    Result  :=  'audio/midi'
  else if LExtensao = 'midi' then
    Result  :=  'audio/midi'
  else if LExtensao = 'mpga' then
    Result  :=  'audio/mpeg'
  else if LExtensao = 'mp2' then
    Result  :=  'audio/mpeg'
  else if LExtensao = 'mp3' then
    Result  :=  'audio/mp3'
  else if LExtensao = 'aif' then
    Result  :=  'audio/x-aiff'
  else if LExtensao = 'aiff' then
    Result  :=  'audio/x-aiff'
  else if LExtensao = 'aifc' then
    Result  :=  'audio/x-aiff'
  else if LExtensao = 'ram' then
    Result  :=  'audio/x-pn-realaudio'
  else if LExtensao = 'rm' then
    Result  :=  'audio/x-pn-realaudio'
  else if LExtensao = 'rpm' then
    Result  :=  'audio/x-pn-realaudio-plugin'
  else if LExtensao = 'ra' then
    Result  :=  'audio/x-realaudio'
  else if LExtensao = 'rv' then
    Result  :=  'video/vnd.rn-realvideo'
  else if LExtensao = 'wav' then
    Result  :=  'audio/x-wav'
  else if LExtensao = 'bmp' then
    Result  :=  'image/bmp'
  else if LExtensao = 'gif' then
    Result  :=  'image/gif'
  else if LExtensao = 'jpeg' then
    Result  :=  'image/jpeg'
  else if LExtensao = 'jpg' then
    Result  :=  'image/jpeg'
  else if LExtensao = 'jpe' then
    Result  :=  'image/jpeg'
  else if LExtensao = 'png' then
    Result  :=  'image/png'
  else if LExtensao = 'tiff' then
    Result  :=  'image/tiff'
  else if LExtensao = 'tif' then
    Result  :=  'image/tiff'
  else if LExtensao = 'css' then
    Result  :=  'text/css'
  else if LExtensao = 'html' then
    Result  :=  'text/html'
  else if LExtensao = 'htm' then
    Result  :=  'text/html'
  else if LExtensao = 'shtml' then
    Result  :=  'text/html'
  else if LExtensao = 'txt' then
    Result  :=  'text/plain'
  else if LExtensao = 'text' then
    Result  :=  'text/plain'
  else if LExtensao = 'log' then
    Result  :=  'text/plain'
  else if LExtensao = 'rtx' then
    Result  :=  'text/richtext'
  else if LExtensao = 'rtf' then
    Result  :=  'text/rtf'
  else if LExtensao = 'xml' then
    Result  :=  'text/xml'
  else if LExtensao = 'xsl' then
    Result  :=  'text/xml'
  else if LExtensao = 'mpeg' then
    Result  :=  'video/mpeg'
  else if LExtensao = 'mpg' then
    Result  :=  'video/mpeg'
  else if LExtensao = 'mpe' then
    Result  :=  'video/mpeg'
  else if LExtensao = 'qt'  then
    Result  :=  'video/quicktime'
  else if LExtensao = 'mov' then
    Result  :=  'video/quicktime'
  else if LExtensao = 'avi' then
    Result  :=  'video/x-msvideo'
  else if LExtensao = 'mp4' then
    Result  :=  'video/mp4'
  else if LExtensao = 'wmv' then
    Result  :=  'video/x-ms-asf'
  else if LExtensao = 'movie' then
    Result  :=  'video/x-sgi-movie'
  else if LExtensao = 'doc' then
    Result  :=  'application/msword'
  else if LExtensao = 'docx' then
    Result  :=  'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  else if LExtensao = 'xlsx' then
    Result  :=  'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  else if LExtensao = 'word' then
    Result  :=  'application/msword'
  else if LExtensao = 'xl' then
    Result  :=  'application/excel'
  else if LExtensao = 'eml' then
    Result  :=  'message/rfc822'
  else if LExtensao = 'json' then
    Result  :=  'application/json'
  else
    Result  :=  'application/octet-stream';
end;

{ TUtilStr }

class function TUtilStr.ApenasNumerosStr(AValue: String): String;
Var
  I: Integer;
begin
  Result := '';
  For I := 1 To Length(AValue) do
   If CharInSet(AValue[I], ['1','2','3','4','5','6','7','8','9','0']) Then
     Result := Result + AValue[I];
end;

class function TUtilStr.Equals(const AValue1, AValue2: String): Boolean;
begin
  Result  :=  AnsiCompareText(AValue1, AValue2) = 0;
end;

class procedure TUtilStr.Split(const ADelimiter: Char; AInput: string;
  const AStrings: TStrings);
begin
  Assert(Assigned(AStrings));
  AStrings.Clear;
  AStrings.Delimiter := ADelimiter;
  AStrings.DelimitedText := AInput;
end;

class function TUtilStr.StrFull(AValue: String; ATamanho: Byte; AChar: Char;
  AFrente: Boolean): string;
var
  LNewValue  :  String;
  I  :  Byte;
begin
  LNewValue  := Copy(AValue, 1, ATamanho);
  if Length(LNewValue) >= ATamanho then
  begin
    Result := LNewValue;
    Exit;
  end
  else
  begin
     Dec(ATamanho);
  end;

  for i := Length(LNewValue) to ATamanho do
  begin
    If AFrente then
      LNewValue  := AChar + LNewValue
    else
      LNewValue  := LNewValue + AChar;
  end;
  Result := LNewValue;
end;

class function TUtilStr.StrZero(AValue: String; AQuantidade: Integer): String;
begin
  Result  :=  StrFull(AValue, AQuantidade, '0', True);
end;

class function TUtilStr.TiraAcento(const AChar: AnsiChar): AnsiChar;
begin
  Result  :=  AChar;//ACBrUtil.TiraAcento(AChar);
end;

class function TUtilStr.TiraAcentos(AValue: string): string;
begin
  Result  :=  AValue;//ACBrUtil.TiraAcentos(AValue);
end;

class function TUtilStr.TiraPontos(AValue: string): string;
begin
  Result  :=  AValue;//ACBrUtil.TiraPontos(AValue);
end;

class function TUtilStr.StrToBoolean(AValue: String): Boolean;
begin
  Result  :=  False;
  if AValue <> '' then
    Result  :=  StrToBool(AValue);
end;

{ TUtilFiles }

class function TUtilFiles.GetExtension(AFileName: String): String;
begin
  Result  :=  TPath.GetExtension(AFileName);
end;

{ TUtilDate }

class function TUtilDate.DataHoraUTC(ANow: TDateTime): TDateTime;
begin
  Result  :=  TTimeZone.Local.ToUniversalTime(ANow);
end;

class function TUtilDate.DataHoraUTC: TDateTime;
begin
  Result  :=  TUtilDate.DataHoraUTC(Now);
end;

class function TUtilDate.DataHoraUTCToLocalTime(ADateTimeUTC: TDateTime;
  AFusoUTC: Double): TDateTime;
begin
  Result  :=  IncMinute(ADateTimeUTC, Trunc((AFusoUTC * 60)));
end;

class function TUtilDate.DataPrimeiroDiaMes(AData: TDate): TDate;
begin
  Result  :=  StartOfTheMonth(AData);
end;

class function TUtilDate.DataPrimeiroDiaSemana(AData: TDate): TDate;
Var
  LDataTratada: TDate;
begin
  //--Tratamento para resolver o problema com o dia da semana que no delphi começa na segunda...
  if DayOfTheWeek(AData) = DaySunday then
    LDataTratada :=  IncDay(AData)
  else
    LDataTratada :=  AData;

  Result  :=  StartOfAWeek(YearOf(LDataTratada), WeekOf(LDataTratada));
end;

class function TUtilDate.DataUltimoDiaMes(AData: TDate): TDate;
begin
  Result  :=  EndOfTheMonth(AData);
end;

class function TUtilDate.DataUltimoDiaSemana(AData: TDate): TDate;
Var
  LDataTratada: TDate;
begin
  //--Tratamento para resolver o problema com o dia da semana que no delphi começa na segunda...
  if DayOfTheWeek(AData) = DaySunday then
    LDataTratada :=  IncDay(AData)
  else
    LDataTratada :=  AData;

  Result  :=  EndOfAWeek(YearOf(LDataTratada), WeekOf(LDataTratada));
end;

class function TUtilDate.DataUTC: TDate;
begin
  Result  :=  TUtilDate.DataUTC(Now);
end;

class function TUtilDate.IncDate(ADias: Integer): TDate;
begin
  Result  :=  TUtilDate.IncDate(TUtilDate.DataUTC, ADias);
end;

class function TUtilDate.IncDate(AData: TDate; ADias: Integer): TDate;
begin
  Result  :=  IncDay(AData, ADias);
end;

class function TUtilDate.SetHoraFinalDia(AData: TDate): TDateTime;
begin
  Result  :=  EncodeDate(YearOf(AData), MonthOf(AData), DayOf(AData)) + StrTotime('23:59:59');
end;

class function TUtilDate.SetHoraInicialDia(AData: TDate): TDateTime;
begin
  Result  :=  EncodeDate(YearOf(AData), MonthOf(AData), DayOf(AData)) + StrTotime('00:00:00');
end;

class function TUtilDate.DataUTC(ADate: TDateTime): TDate;
begin
  Result  :=  DateOf(TUtilDate.DataHoraUTC(ADate))
end;

class function TUtilDate.DateTimeToStrDB(ADataHora: TDatetime): String;
begin
  Result  :=  Format('%s %s',
                    [DateToStrDB(ADataHora),
                    TimeToStrDB(ADataHora)]);
end;

class function TUtilDate.DateToStrDB(AData: TDate): String;
Var
  LDia,
  LMes,
  LAno: Word;
begin
  DecodeDate(AData, LAno, LMes, LDia);

  Result  :=  TUtilStr.StrZero(IntToStr(LAno), 4) + '-'  +
              TUtilStr.StrZero(IntToStr(LMes), 2) + '-' +
              TUtilStr.StrZero(IntToStr(LDia), 2);
end;

class function TUtilDate.StrToDate(ADataStr: String): TDate;
begin
  Result  :=  StrToDate(ADataStr, Now);
end;

class function TUtilDate.StrToDateTime(ADataStr: String): TDatetime;
begin
  Result  :=  StrToDateTime(ADataStr, Now);
end;

class function TUtilDate.TimeToStrDB(AHora: TTime): String;
Var
  LHora,
  LMinuto,
  LSegundo,
  LMili: Word;
begin
  DecodeTime(AHora, LHora, LMinuto, LSegundo, LMili);

  Result  :=  TUtilStr.StrZero(IntToStr(LHora), 2) + ':'  +
              TUtilStr.StrZero(IntToStr(LMinuto), 2) + ':' +
              TUtilStr.StrZero(IntToStr(LSegundo), 2);
end;

class function TUtilDate.StrToDate(ADataStr: String; ADateDef: TDate): TDate;
Var
  LDia,
  LMes,
  LAno: Word;
  LDataAuxiliar: String;
begin
  try
    LDataAuxiliar :=  TUtilStr.ApenasNumerosStr(ADataStr);

    if LDataAuxiliar <> '' then
    begin
      LAno :=  StrToInt(Copy(LDataAuxiliar, 1, 4));
      LMes :=  StrToInt(Copy(LDataAuxiliar, 5, 2));
      LDia :=  StrToInt(Copy(LDataAuxiliar, 7, 2));

      Result :=  EncodeDate(LAno, LMes, LDia);
    end
    else
      Result  :=  ADateDef;
  except
    Result :=  ADateDef;
  end;
end;

class function TUtilDate.StrToDateTime(ADataStr: String;
  ADateTimeDef: TDateTime): TDatetime;
Var
  LDia,
  LMes,
  LAno : Word;

  LHora,
  LMinuto,
  LSegundo: Word;

  LDataAuxiliar: String;
begin

  try
    LDataAuxiliar :=  TUtilStr.ApenasNumerosStr(ADataStr);

    if LDataAuxiliar <> '' then
    begin
      LAno :=  StrToInt(Copy(LDataAuxiliar, 1, 4));
      LMes :=  StrToInt(Copy(LDataAuxiliar, 5, 2));
      LDia :=  StrToInt(Copy(LDataAuxiliar, 7, 2));

      LHora    :=  StrToInt(Copy(LDataAuxiliar, 9, 2));
      LMinuto  :=  StrToInt(Copy(LDataAuxiliar, 11, 2));
      LSegundo :=  StrToInt(Copy(LDataAuxiliar, 13, 2));

      Result :=  EncodeDate(LAno, LMes, LDia) + EncodeTime(LHora, LMinuto, LSegundo, 0);
    end
    else
      Result  :=  ADateTimeDef;
  except
    Result :=  ADateTimeDef;
  end;
end;

{ TUtilEnumerator<T> }

class function TUtilEnumerator<T>.EnsureRange(Value: Integer): Integer;
var
  Lptd: PTypeData;
begin
  Assert(IsEnum);
  Lptd := TypeData;
  Result := System.Math.EnsureRange(Value, Lptd.MinValue, Lptd.MaxValue);
end;

class function TUtilEnumerator<T>.InRange(Value: Integer): Boolean;
var
  Lptd: PTypeData;
begin
  Assert(IsEnum);
  Lptd := TypeData;
  Result := System.Math.InRange(Value, Lptd.MinValue, Lptd.MaxValue);
end;

class function TUtilEnumerator<T>.IsEnum: Boolean;
begin
  Result := TypeInfo.Kind = tkEnumeration;
end;

class function TUtilEnumerator<T>.MaxValue: Integer;
begin
  Assert(IsEnum);
  Result := TypeData.MaxValue;
end;

class function TUtilEnumerator<T>.MinValue: Integer;
begin
  Assert(IsEnum);
  Result := TypeData.MinValue;
end;

class function TUtilEnumerator<T>.ToEnum(Value: Integer): T;
begin
  Assert(IsEnum);
  Assert(InRange(Value));
  Assert(SizeOf(Result) <= SizeOf(Value));
  Move(Value, Result, SizeOf(Result));
end;

class function TUtilEnumerator<T>.ToEnum(Value: String): T;
begin
  Assert(IsEnum);
  try
    Result  :=  ToEnum(GetEnumValue(TypeInfo, Value));
  except
    Result  :=  ToEnum(0);
  end;
end;

class function TUtilEnumerator<T>.ToOrdinal(Enum: T): Integer;
begin
  Assert(IsEnum);
  Assert(SizeOf(Enum) <= SizeOf(Result));
  Result := 0;
  Move(Enum, Result, SizeOf(Enum));
  Assert(InRange(Result));
end;

class function TUtilEnumerator<T>.ToString(Enum: T): String;
begin
  Assert(IsEnum);

  Result  := GetEnumName(TypeInfo, ToOrdinal(Enum));
end;

class function TUtilEnumerator<T>.TypeData: PTypeData;
begin
  Result := System.TypInfo.GetTypeData(TypeInfo);
end;

class function TUtilEnumerator<T>.TypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(T);
end;

{ TUtilVarRec }

class function TUtilVarRec.CopyVarRec(const AItem: TVarRec): TVarRec;
var
  LWChar: WideString;
begin
  // Copy entire TVarRec first
  Result := AItem;

  // Now handle special cases
  case AItem.VType of
    vtExtended:
      begin
        New(Result.VExtended);
        Result.VExtended^ := AItem.VExtended^;
      end;
    vtString:
      begin
        // Improvement suggestion by Hallvard Vassbotn: only copy real length.
        GetMem(Result.VString, Length(AItem.VString^) + 1);
        Result.VString^ := AItem.VString^;
      end;
    vtPChar:
      Result.VPChar := System.AnsiStrings.StrNew(AItem.VPChar);
    // There is no StrNew for PWideChar
    vtPWideChar:
      begin
        LWChar := AItem.VPWideChar;
        GetMem(Result.VPWideChar,
               (Length(LWChar) + 1) * SizeOf(WideChar));
        Move(PWideChar(LWChar)^, Result.VPWideChar^,
             (Length(LWChar) + 1) * SizeOf(WideChar));
      end;
    // A little trickier: casting to AnsiString will ensure
    // reference counting is done properly.
    vtAnsiString:
      begin
        // nil out first, so no attempt to decrement reference count.
        Result.VAnsiString := nil;
        AnsiString(Result.VAnsiString) := AnsiString(AItem.VAnsiString);
      end;
    vtCurrency:
      begin
        New(Result.VCurrency);
        Result.VCurrency^ := AItem.VCurrency^;
      end;
    vtVariant:
      begin
        New(Result.VVariant);
        Result.VVariant^ := AItem.VVariant^;
      end;
    // Casting ensures proper reference counting.
    vtInterface:
      begin
        Result.VInterface := nil;
        IInterface(Result.VInterface) := IInterface(AItem.VInterface);
      end;
    // Casting ensures a proper copy is created.
    vtWideString:
      begin
        Result.VWideString := nil;
        WideString(Result.VWideString) := WideString(AItem.VWideString);
      end;
    vtInt64:
      begin
        New(Result.VInt64);
        Result.VInt64^ := AItem.VInt64^;
      end;
    vtUnicodeString:
      begin
        // Similar to AnsiString.
        Result.VUnicodeString := nil;
        UnicodeString(Result.VUnicodeString) := UnicodeString(AItem.VUnicodeString);
      end;
    // VPointer and VObject don't have proper copy semantics so it
    // is impossible to write generic code that copies the contents
  end;
end;

class procedure TUtilVarRec.FinalizeVarRec(var AItem: TVarRec);
begin
  case AItem.VType of
    vtExtended: Dispose(AItem.VExtended);
    vtString: Dispose(AItem.VString);
    vtPChar: System.AnsiStrings.StrDispose(AItem.VPChar);
    vtPWideChar: FreeMem(AItem.VPWideChar);
    vtAnsiString: AnsiString(AItem.VAnsiString) := '';
    vtCurrency: Dispose(AItem.VCurrency);
    vtVariant: Dispose(AItem.VVariant);
    vtInterface: IInterface(AItem.VInterface) := nil;
    vtWideString: WideString(AItem.VWideString) := '';
    vtInt64: Dispose(AItem.VInt64);
    vtUnicodeString: UnicodeString(AItem.VUnicodeString) := '';
  end;
  AItem.VInteger := 0;
end;

end.

