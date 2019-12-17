unit ResponseUnit;
//------------------------------------------------------------------------------
// Модуль подготовки ответа сервиса на запросы
//------------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypesUnit;

type

  { TResponse }

  TResponse = class (TObject)
  public
    class function createErrorResponseString(const err: string; const err_code: integer = 0): string;
    class function responseTypeToString(const resp: ResponseType): string;
    class function jsonReal(const r:real): string;
    class function jsonBoolean(const b: boolean): string;
    class function getDeviceCashRegisterInfResponseString(const inf: CashRegisterInfType): string;
    class function getDeviceCashRegisterInfResponseStringDefault: string;
  end;

implementation

{ TResponse }

class function TResponse.createErrorResponseString(const err: string;
  const err_code: integer): string;
begin
  result := '{';

  result := result + '"response": "",';
  result := result + '"error": "'+err+'",';
  result := result + '"error_code": '+IntToStr(err_code);

  result := result + '}';
end;

class function TResponse.responseTypeToString(const resp: ResponseType): string;
begin
  result := '{';

  result := result + '"response": '+resp.response+',';
  result := result + '"error": "'+resp.error+'",';
  result := result + '"error_code": '+IntToStr(resp.error_code);

  result := result + '}';
end;

class function TResponse.jsonReal(const r: real): string;
begin
  result := StringReplace(FormatFloat('0.00',r), DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll,rfIgnoreCase]);
end;

class function TResponse.jsonBoolean(const b: boolean): string;
begin
  if b then
    result := 'true'
  else result := 'false';
end;

class function TResponse.getDeviceCashRegisterInfResponseString(
  const inf: CashRegisterInfType): string;
begin
  result := '{';

  result := result + '"KKMDateTime": "'+inf.KKMDateTime+'",';
  result := result + '"KKMEarnings": '+jsonReal(inf.KKMEarnings)+',';
  result := result + '"KKMEarningSession": '+jsonReal(inf.KKMEarningSession)+',';
  result := result + '"KKMSum": '+jsonReal(inf.KKMSum)+',';
  result := result + '"Mode": '+IntToStr(inf.Mode)+',';
  result := result + '"SerialNumber": "'+inf.SerialNumber+'",';
  result := result + '"CheckFontSizeString": "'+inf.CheckFontSizeString+'",';
  result := result + '"CheckLineSpacing": '+IntToStr(inf.CheckLineSpacing)+',';
  result := result + '"SessionNumber": '+IntToStr(inf.SessionNumber)+',';
  result := result + '"isFiskal": '+jsonBoolean(inf.isFiskal)+',';
  result := result + '"isSessionExceedLimit24": '+jsonBoolean(inf.isSessionExceedLimit24)+',';
  result := result + '"isSessionOpened": '+jsonBoolean(inf.isSessionOpened)+',';
  result := result + '"CashRegisterNum": '+IntToStr(inf.CashRegisterNum);

  result := result + '}';
end;

class function TResponse.getDeviceCashRegisterInfResponseStringDefault: string;
begin
  result := '{';

  result := result + '"KKMDateTime": "'+FormatDateTime('yyyy-mm-dd hh:nn:ss',now)+'",';
  result := result + '"KKMEarnings": '+jsonReal(0)+',';
  result := result + '"KKMEarningSession": '+jsonReal(0)+',';
  result := result + '"KKMSum": '+jsonReal(0)+',';
  result := result + '"Mode": '+IntToStr(1)+',';
  result := result + '"SerialNumber": "1234567890",';
  result := result + '"CheckFontSizeString": "1x1",';
  result := result + '"CheckLineSpacing": '+IntToStr(3)+',';
  result := result + '"SessionNumber": '+IntToStr(1)+',';
  result := result + '"isFiskal": '+jsonBoolean(false)+',';
  result := result + '"isSessionExceedLimit24": '+jsonBoolean(false)+',';
  result := result + '"isSessionOpened": '+jsonBoolean(true)+',';
  result := result + '"CashRegisterNum": '+IntToStr(1);

  result := result + '}';
end;

end.

