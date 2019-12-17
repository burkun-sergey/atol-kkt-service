unit DeviceListUnit;
//------------------------------------------------------------------------------
// Модуль, отвечающий за выполнение команд на подключенных ККТ
//------------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypesUnit, AtolKKTUnit, LoggerUnit, ResponseUnit,
  RequestUnit, SegmentArrayUnit, base64;

type

  { TDeviceList }

  TDeviceList = class(TObject)
  private
    class var devices: AtolDeviceArrayType;
    class var atol_do_comand: boolean;
  public
    class procedure addDevice(device: AtolDeviceType);
    class procedure clearDeviceList;
    class procedure DisconnectFromDevices;
    class function getDeviceByDeviceNumber( const num: integer): TAtolKKT;
    class function runComand(const cmd: RequestType): ResponseType;
    class function runDeviceComand(const cmd: RequestType): ResponseType;

    class procedure setAtolDoCommand(fDo: boolean);

    class function doDeviceComandPrintLastCheckCopy(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandCashRegisterInf(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandPrintTStringList(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandPrintStringList(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandChequePartialCut(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandChequeFullCut(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandResetSummary(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandTechReset(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandIncome(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandReturn(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandXReport(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandZReport(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandCashIncome(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandCashOutcome(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandOpenSession(var device: TAtolKKT; const cmd: RequestType): ResponseType;

    class function doDeviceComand(var device: TAtolKKT; const cmd: RequestType): ResponseType;
    class function doDeviceComandSegment(var device: TAtolKKT; const cmd: RequestType): ResponseType;

    class function getDeviceCashRegisterInf(var device: TAtolKKT): CashRegisterInfType;
    class function getSuccessResult(): ResponseType;
  end;

implementation

{ TDeviceList }

class procedure TDeviceList.addDevice(device: AtolDeviceType);
begin
  SetLength(devices, Length(devices)+1);
  devices[Length(devices)-1] := device;
end;

class procedure TDeviceList.clearDeviceList;
begin
  SetLength(devices, 0);
end;

class procedure TDeviceList.DisconnectFromDevices;
var i: integer;
begin
  for i := 0 to Length(devices)-1  do
    if devices[i].atol_cash_register then
      if Assigned(devices[i].atol) then
        begin
          devices[i].atol.free;
          devices[i].atol := nil;
        end;
end;

class function TDeviceList.getDeviceByDeviceNumber(const num: integer): TAtolKKT;
var i: integer;
begin
  result := nil;
  MyLogger.LogText('Searching device #'+IntToStr(num)+'...');

  for i:=0 to Length(devices)-1 do
    if devices[i].atol_device_number=num then
      begin
        MyLogger.LogText('Device #'+IntToStr(num)+' found: index='+IntToStr(i));
        if Assigned(devices[i].atol) then
          MyLogger.LogText('Assigned')
        else  MyLogger.LogText('NOT assigned!!!');

        result := devices[i].atol;
        break;
      end;
end;

class function TDeviceList.runComand(const cmd: RequestType): ResponseType;
begin
  if cmd.comand = rctUnknown then
    begin
      result.response := '""';
      result.error_code := -1;
      result.error := 'Неизвестная команда';
    end
  else
    begin
      result := runDeviceComand(cmd);
    end;
end;

class function TDeviceList.runDeviceComand(const cmd: RequestType): ResponseType;
var device: TAtolKKT;
    err_code: integer;
begin
  device := getDeviceByDeviceNumber(cmd.atol_device_data.atol_device_number);
  if Assigned(device) or (cmd.comand = rctSegment) then begin
    try
      //raise EATOLException.create('Тестовое сообщение об окончании бумаги!', -3807);

      //result := device.printStringList(TEST_CHEQUE, true, true);
      if Assigned(device) then
        device.OperatorPassword := cmd.atol_device_data.atol_operator_password;

      result := doDeviceComand(device, cmd);
    except
      on e: exception do
        begin
          if (e is EATOLException) then
            err_code := (e as EATOLException).ErrorCode
          else err_code := -1;

          MyLogger.LogError(err_code, e.Message);
          result.error := e.Message;
          result.response := '""';
          result.error_code := err_code;
          exit;
        end;
    end;
  end else begin
    Result.error := 'Устройство с номером '+IntToStr(cmd.atol_device_data.atol_device_number)+' не найдено';
    Result.response := '""';
    Result.error_code := -1;
  end;
end;

class procedure TDeviceList.setAtolDoCommand(fDo: boolean);
begin
  atol_do_comand := fDo;
end;

class function TDeviceList.doDeviceComandPrintLastCheckCopy(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.PrintLastCheckCopy;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandCashRegisterInf(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
var inf: CashRegisterInfType;
begin
  if atol_do_comand then
    begin
      inf := getDeviceCashRegisterInf(device);
      result.response := TResponse.getDeviceCashRegisterInfResponseString(inf);
    end
  else result.response := TResponse.getDeviceCashRegisterInfResponseStringDefault();
  result.error_code := 0;
  result.error := '';
end;

class function TDeviceList.doDeviceComandPrintTStringList(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
var lst: TStringList;
    setmode_one: boolean;
    use_print_string: boolean;
begin
  lst := TStringList.create;
  TRequest.getPrintTStringListRequestData(lst, setmode_one, use_print_string, cmd.data);
  try
    if atol_do_comand then
      device.printTStringList(lst, setmode_one, use_print_string);
  finally
    lst.Free;
  end;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandPrintStringList(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
var text: string;
    setmode_one: boolean;
    use_print_string: boolean;
begin
  text := '';
  TRequest.getPrintStringListRequestData(text, setmode_one, use_print_string, cmd.data);

  if atol_do_comand then
    device.printStringList(text, setmode_one, use_print_string);

  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandChequePartialCut(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.PartialCut;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandChequeFullCut(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.FullCut;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandResetSummary(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.resetSummary;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandTechReset(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.techReset;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandIncome(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
var h_lst, f_lst: TStringList;
    cheque_data: ChequeDataType;
    i: integer;
begin
  h_lst := TStringList.create;
  f_lst := TStringList.create;

  TRequest.getIncomeRequestData(h_lst, f_lst, cheque_data, cmd.data);

  try
    if atol_do_comand then
      begin
        // печать заголовка
        if h_lst.count>0 then
          device.printTStringList(h_lst, true, false);

        device.openIncomeCheck(cheque_data.tr_id);

        for i:=0 to Length(cheque_data.goods)-1 do
          begin
            device.addProductToIncomeCheck(cheque_data.goods[i].name,
                                           cheque_data.goods[i].price,
                                           cheque_data.goods[i].kol,
                                           cheque_data.goods[i].nds,
                                           cheque_data.goods[i].barcode,
                                           cheque_data.goods[i].dep,
                                           cheque_data.goods[i].discount);
          end;

        // печать подвала
        if f_lst.count>0 then
          device.printTStringList(f_lst, false, false);

        if cheque_data.check_sm>0 then
          device.closeIncomeCheck(cheque_data.check_sm,
                                  cheque_data.check_discount_sm,
                                  cheque_data.type_close,
                                  cheque_data.cash_sm);
      end;
  finally
    h_lst.Free;
    f_lst.Free;
  end;

  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandReturn(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
var h_lst, f_lst: TStringList;
    cheque_data: ChequeDataType;
    i: integer;
begin
  h_lst := TStringList.create;
  f_lst := TStringList.create;

  TRequest.getIncomeRequestData(h_lst, f_lst, cheque_data, cmd.data);

  try
    if atol_do_comand then
      begin
        // печать заголовка
        if h_lst.count>0 then
          device.printTStringList(h_lst, true, false);

        device.openReturnCheck(cheque_data.tr_id);

        for i:=0 to Length(cheque_data.goods)-1 do
          begin
            device.addProductToReturnCheck(cheque_data.goods[i].name,
                                           cheque_data.goods[i].price,
                                           cheque_data.goods[i].kol,
                                           cheque_data.goods[i].nds,
                                           cheque_data.goods[i].barcode,
                                           cheque_data.goods[i].dep,
                                           cheque_data.goods[i].discount);
          end;

        // печать подвала
        if f_lst.count>0 then
          device.printTStringList(f_lst, false, false);

        device.closeReturnCheck(cheque_data.type_close,
                                cheque_data.check_discount_sm);
      end;
  finally
    h_lst.Free;
    f_lst.Free;
  end;

  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandXReport(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.getXReport;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandZReport(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.getZReport;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandCashIncome(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.CashIncome(TRequest.getCashIncomeOutcomeRequestData(cmd.data));
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandCashOutcome(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.cashOutcome(TRequest.getCashIncomeOutcomeRequestData(cmd.data));
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComandOpenSession(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  if atol_do_comand then
    device.openSession;
  result := getSuccessResult();
end;

class function TDeviceList.doDeviceComand(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
begin
  case cmd.comand of
    rctPrintTestCheque: begin
      if atol_do_comand then
        device.printStringList(TEST_CHEQUE, true, true);

      result.response := '"готово"';
      result.error_code := 0;
      result.error := '';
    end;

    rctPrintLastCheckCopy: begin
      result := doDeviceComandPrintLastCheckCopy(device, cmd);
    end;

    rctCashRegisterInf: begin
      result := doDeviceComandCashRegisterInf(device, cmd);
    end;

    rctPrintTStringList: begin
      result := doDeviceComandPrintTStringList(device, cmd);
    end;

    rctPrintStringList: begin
      result := doDeviceComandPrintStringList(device, cmd);
    end;

    rctChequePartialCut: begin
      result := doDeviceComandChequePartialCut(device, cmd);
    end;

    rctChequeFullCut: begin
      result := doDeviceComandChequeFullCut(device, cmd);
    end;

    rctResetSummary: begin
      result := doDeviceComandResetSummary(device, cmd);
    end;

    rctTechReset: begin
      result := doDeviceComandTechReset(device, cmd);
    end;

    rctIncome: begin
      result := doDeviceComandIncome(device, cmd);
    end;

    rctReturn: begin
      result := doDeviceComandReturn(device, cmd);
    end;

    rctXReport: begin
      result := doDeviceComandXReport(device, cmd);
    end;

    rctZReport: begin
      result := doDeviceComandZReport(device, cmd);
    end;

    rctCashIncome: begin
      result := doDeviceComandCashIncome(device, cmd);
    end;

    rctCashOutcome: begin
      result := doDeviceComandCashOutcome(device, cmd);
    end;

    rctOpenSession: begin
      result := doDeviceComandOpenSession(device, cmd);
    end;

    rctSegment: begin
      result := doDeviceComandSegment(device, cmd);
    end;
  end;
end;

class function TDeviceList.doDeviceComandSegment(var device: TAtolKKT;
  const cmd: RequestType): ResponseType;
var data: RequestSegmentType;
    full_data: string;
    req: RequestType;
    request_string: string;
begin
  MyLogger.LogText('doDeviceComandSegment');

  data := TRequest.getSegmentRequestData(cmd.data);

  MyLogger.LogText('doDeviceComandSegment: before addSegment()');
  // накапливаем сегменты
  segments_arr.addSegment(data.guid, data.text, data.count);
  full_data := segments_arr.getGuidFullData(data.guid, true);
  if Trim(full_data)<>'' then
    begin // все сегменты пришли
      // декодируем данные-реальную-команду
      request_string := DecodeStringBase64(full_data);
      MyLogger.LogText('decoded request: '+request_string);

      // разберем команду
      req := TRequest.getRequestTypeByRequest(request_string);

      // и выполним, наконец
      result := runComand(req);
    end
  else // ждем следующий сегмент
    begin
      result := getSuccessResult();
    end;
end;

class function TDeviceList.getDeviceCashRegisterInf(var device: TAtolKKT
  ): CashRegisterInfType;
begin
  device.getStatus;

  result.KKMDateTime := formatdatetime('dd.mm.yyyy hh:nn:ss', device.getKKMDateTime);
  result.KKMEarnings := device.getKKMEarnings;
  result.KKMEarningSession := device.getKKMEarningSession;
  result.KKMSum := device.getKKMSum;
  result.Mode := device.getMode;
  result.SerialNumber := device.getSerialNumber;
  result.CheckFontSizeString := device.getCheckFontSizeString;
  result.CheckLineSpacing := device.getCheckLineSpacing;
  result.SessionNumber := device.getSessionNumber;
  result.isFiskal := device.isFiskal();
  result.isSessionExceedLimit24 := device.isSessionExceedLimit24();
  result.isSessionOpened := device.isSessionOpened;
  result.CashRegisterNum := device.CashRegisterNum;
end;

class function TDeviceList.getSuccessResult(): ResponseType;
begin
  result.response := '"готово"';
  result.error_code := 0;
  result.error := '';
end;

end.

