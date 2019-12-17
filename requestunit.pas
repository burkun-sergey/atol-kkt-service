unit RequestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JsonTools, TypesUnit, AtolKKTUnit;

type

  { TRequest }

  TRequest = class(TObject)
  public
    class function getRequestTypeByRequest(const fRequest: string): RequestType;
    class function getStringByJSONNode(jn: TJsonNode): string;
    class function requestDeviceDataToAtolDeviceDataType(js: TJsonNode): AtolDeviceDataType;
    class function GenerateReadableText(js: TJsonNode): string;
    class function requestComandToRequestComandType(const st: string): RequestComandType;
    class procedure getPrintTStringListRequestData(var lst: TStringList; var setmode_one: boolean; var use_print_string: boolean; const data: string);
    class procedure setTStringListFromRequestDataJSONobject(js: TJsonNode; const field_name: string; var lst: TStringList);
    class function getAtolFontByJSON(js: TJsonNode): TAtolFont;
    class procedure getPrintStringListRequestData(var text: string; var setmode_one: boolean; var use_print_string: boolean; const data: string);
    class procedure getIncomeRequestData(var header_lst, footer_lst: TStringList; var cheque_data: ChequeDataType; const data: string);
    class function getChequeGoodsTypeByJSON(js: TJsonNode): ChequeGoodsType;
    class function getTypeCloseTypeByStr(const tc: string): TypeCloseType;
    class function getNdsTypeByStr(const nds: string): NDSType;
    class function getCashIncomeOutcomeRequestData(const data: string): real;
    class function getSegmentRequestData(const text: string): RequestSegmentType;
  end;

implementation

{ TRequest }

class function TRequest.getRequestTypeByRequest(const fRequest: string): RequestType;
var js: TJsonNode;
    cmd: RequestComandType;
    data: string;
    req: RequestType;
begin
  js := TJsonNode.Create();
  js.Parse(AnsiToUtf8(fRequest));
  cmd := requestComandToRequestComandType(js.Find('comand').AsString);
  data := getStringByJSONNode(js.Find('data'));

  req.comand := cmd;
  req.data := data;
  req.atol_device_data := requestDeviceDataToAtolDeviceDataType(js.Find('atol_device_data'));

  js.Free;

  result := req;
end;

class function TRequest.getStringByJSONNode(jn: TJsonNode): string;
begin
  result := Utf8ToAnsi(GenerateReadableText(jn));
end;

class function TRequest.requestDeviceDataToAtolDeviceDataType(js: TJsonNode
  ): AtolDeviceDataType;
begin
  result.atol_device_number := round(js.Find('atol_device_number').AsNumber);
  result.atol_operator_password := js.Find('atol_operator_password').AsString;
end;

class function TRequest.GenerateReadableText(js: TJsonNode): string;
begin
  result := js.AsJson;
end;

class function TRequest.requestComandToRequestComandType(const st: string
  ): RequestComandType;
begin
  result := rctUnknown;

  if AnsiCompareText(st,'PRINT_TEXT_CHEQUE')=0 then
    result := rctPrintTestCheque;

  if AnsiCompareText(st,'CASH_REGISTER_INF')=0 then
    result := rctCashRegisterInf;

  if AnsiCompareText(st,'PRINT_TSTRING_LIST')=0 then
    result := rctPrintTStringList;

  if AnsiCompareText(st,'PRINT_STRING_LIST')=0 then
    result := rctPrintStringList;

  if AnsiCompareText(st,'CHEQUE_PARTIAL_CUT')=0 then
    result := rctChequePartialCut;

  if AnsiCompareText(st,'CHEQUE_FULL_CUT')=0 then
    result := rctChequeFullCut;

  if AnsiCompareText(st,'RESET_SUMMARY')=0 then
    result := rctResetSummary;

  if AnsiCompareText(st,'TECH_RESET')=0 then
    result := rctTechReset;

  if AnsiCompareText(st,'INCOME')=0 then
    result := rctIncome;

  if AnsiCompareText(st,'RETURN')=0 then
    result := rctReturn;

  if AnsiCompareText(st,'XREPORT')=0 then
    result := rctXReport;

  if AnsiCompareText(st,'ZREPORT')=0 then
    result := rctZReport;

  if AnsiCompareText(st,'CASH_INCOME')=0 then
    result := rctCashIncome;

  if AnsiCompareText(st,'CASH_OUTCOME')=0 then
    result := rctCashOutcome;

  if AnsiCompareText(st,'OPEN_SESSION')=0 then
    result := rctOpenSession;

  if AnsiCompareText(st,'PRINT_LAST_CHEQUE_COPY')=0 then
    result := rctPrintLastCheckCopy;

  if AnsiCompareText(st,'SEGMENT')=0 then
    result := rctSegment;
end;

class procedure TRequest.getPrintTStringListRequestData(var lst: TStringList;
  var setmode_one: boolean; var use_print_string: boolean; const data: string);
var js: TJsonNode;
begin
  js := TJsonNode.Create;
  js.Parse(AnsiToUtf8(data));

  setmode_one := js.Find('setmode_one').AsBoolean;
  use_print_string := js.Find('use_print_string').AsBoolean;

  setTStringListFromRequestDataJSONobject(js, 'text', lst);

  js.free;
end;

class procedure TRequest.setTStringListFromRequestDataJSONobject(
  js: TJsonNode; const field_name: string; var lst: TStringList);
var chd: TJsonNode;
    st: string;
    af: TAtolFont;
begin
  for chd in js.Find(field_name) do
    begin
      af := getAtolFontByJSON(chd.Find('font'));
      st := chd.Find('text').AsString;
      lst.AddObject(st, af);
    end;
end;

class function TRequest.getAtolFontByJSON(js: TJsonNode): TAtolFont;
begin
  result := TAtolFont.Create;
  result.FontBold := js.Find('b').AsBoolean;
  result.FontItalic := js.Find('i').AsBoolean;
  result.FontNegative := js.Find('n').AsBoolean;
  result.FontUnderline := js.Find('u').AsBoolean;
  result.FontDblHeight := js.Find('dh').AsBoolean;
  result.FontDblWidth := js.Find('dw').AsBoolean;
  result.TextWrap := round(js.Find('w').AsNumber);
  result.Alignment := round(js.Find('a').AsNumber);
end;

class procedure TRequest.getPrintStringListRequestData(var text: string;
  var setmode_one: boolean; var use_print_string: boolean; const data: string);
var js: TJsonNode;
begin
  js := TJsonNode.Create;
  js.Parse(AnsiToUtf8(data));
  setmode_one := js.Find('setmode_one').AsBoolean;
  use_print_string := js.Find('use_print_string').AsBoolean;
  text := js.Find('text').AsString;
  js.free;
end;

class procedure TRequest.getIncomeRequestData(var header_lst,
  footer_lst: TStringList; var cheque_data: ChequeDataType; const data: string);
var js, ijs: TJsonNode;
    i: integer;
    g: ChequeGoodsType;
begin
  js := TJsonNode.Create;
  js.Parse(AnsiToUtf8(data));

  setTStringListFromRequestDataJSONobject(js, 'header_text', header_lst);
  setTStringListFromRequestDataJSONobject(js, 'footer_text', footer_lst);

  SetLength(cheque_data.goods, 0);

  for ijs in js.Find('goods') do
    begin
      g := getChequeGoodsTypeByJSON(ijs);
      SetLength(cheque_data.goods, Length(cheque_data.goods)+1);
      cheque_data.goods[Length(cheque_data.goods)-1] := g;
    end;
  cheque_data.check_sm := js.Find('check_sm').AsNumber;
  cheque_data.check_discount_sm := js.Find('check_discount_sm').AsNumber;
  cheque_data.type_close := getTypeCloseTypeByStr(js.Find('type_close').AsString);
  cheque_data.cash_sm := js.Find('cash_sm').AsNumber;
  cheque_data.tr_id := round(js.Find('tr_id').AsNumber);

  js.free;
end;

class function TRequest.getChequeGoodsTypeByJSON(js: TJsonNode): ChequeGoodsType;
begin
  result.name := js.Find('name').AsString;
  result.price := js.Find('price').AsNumber;
  result.kol := round(js.Find('kol').AsNumber);
  result.nds := getNdsTypeByStr(js.Find('nds').AsString);
  result.barcode := js.Find('barcode').AsString;
  result.dep := round(js.Find('dep').AsNumber);
  result.discount := js.Find('discount').AsNumber;
end;

class function TRequest.getTypeCloseTypeByStr(const tc: string): TypeCloseType;
begin
  result := tcCash;

  if AnsiCompareText(tc, 'tcCash')=0 then
    result := tcCash;

  if AnsiCompareText(tc, 'tc1')=0 then
    result := tc1;

  if AnsiCompareText(tc, 'tc2')=0 then
    result := tc2;

  if AnsiCompareText(tc, 'tc3')=0 then
    result := tc3;

  if AnsiCompareText(tc, 'tc4')=0 then
    result := tc4;

  if AnsiCompareText(tc, 'tc5')=0 then
    result := tc5;

  if AnsiCompareText(tc, 'tc6')=0 then
    result := tc6;
end;

class function TRequest.getNdsTypeByStr(const nds: string): NDSType;
begin
  result := ndsNo;

  if AnsiCompareText(nds, 'ndsSection')=0 then
    result := ndsSection;

  if AnsiCompareText(nds, 'nds0')=0 then
    result := nds0;

  if AnsiCompareText(nds, 'nds10')=0 then
    result := nds10;

  if AnsiCompareText(nds, 'nds18')=0 then
    result := nds18;

  if AnsiCompareText(nds, 'nds18118')=0 then
    result := nds18118;

  if AnsiCompareText(nds, 'nds10110')=0 then
    result := nds10110;
end;

class function TRequest.getCashIncomeOutcomeRequestData(const data: string): real;
var js: TJsonNode;
begin
  js := TJsonNode.Create;
  js.Parse(AnsiToUtf8(data));
  result := js.Find('sm').AsNumber;
  js.Free;
end;

class function TRequest.getSegmentRequestData(const text: string): RequestSegmentType;
var js: TJsonNode;
begin
  js := TJsonNode.Create;
  js.Parse(AnsiToUtf8(text));
  result.guid := js.Find('segment_guid').AsString;
  result.num := round(js.Find('segment_num').AsNumber);
  result.count := round(js.Find('segment_count').AsNumber);
  result.text := js.Find('text').AsString;
  js.Free;
end;

end.

