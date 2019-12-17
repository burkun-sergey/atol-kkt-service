unit AtolKKTUnit;
//------------------------------------------------------------------------------
// Фасад для взаимодействия с драйвером АТОЛ через COM\OLE
//------------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Windows, Variants, DateUtils;

type

  FFDVersionType=(ffd10, ffd105, ffd11);

  NDSType=(ndsSection, ndsNo, nds0, nds10, nds18, nds18118, nds10110);

  TypeCloseType=(tcCash, tc1, tc2, tc3, tc4, tc5, tc6);

  DiscountType=(dtDisabled, dtWholeCheck, dtCheckPosition, dtWholeAndCheckPosition);

  EATOLException = class(Exception)
  private
    FErrorCode: integer;
  public
    constructor Create(const Msg: string; const ErrCode: integer);
    property ErrorCode: integer read FErrorCode write FErrorCode;
  end;

  EATOLLogicException = class(Exception);

  TAtolFont = class(TObject)
  public
    FontBold: boolean;
    FontItalic: boolean;
    FontNegative: boolean;
    FontUnderline: boolean;
    FontDblHeight: boolean;
    FontDblWidth: boolean;
    TextWrap: byte;
    Alignment: byte;
  end;

  TAtolKKT = class(TObject)
  private
    ECR: OleVariant;
    fResultCode: integer;
    fResultStr: string;
    fCurrentDeviceNumber: integer;
    fFFDVersion: FFDVersionType;
    fAdminPassword: string;
    fOperatorPassword: string;
    fDTOVersion: integer; // версия ДТО в виде числа (получается после создания объекта)
    fNameAndPriceOnOneLine: boolean;
    fTextWrap: integer;
    fEnabledDiscount: DiscountType;
    fCashRegisterNum: byte;
    function GetConnected: boolean;
    procedure SetCurrentDeviceNumber(const Value: integer);
    function getNDSInt(const nds: NDSType): integer;
    function getVersionInt: integer; // для определения версии ДТО, чтобы понять как кодировать размер НДС
    function getDocNumber: integer;
    function TypeCloseToInt(const tc: TypeCloseType): integer;
    procedure setResultCodeAndStrAndRaiseException;
    procedure setEnabledDiscount(const Value: DiscountType);
    function getEnabledDiscount: DiscountType;
    procedure setCashRegisterNum(const Value: byte);
    function getCashRegisterNum: byte;
    procedure getResultCodeAndCheckOutOfPaper;
    procedure setFFDVersion(const Value: FFDVersionType);
    procedure UpdateDeviceProperties;
    procedure LogText(const st: string);
  public
    property ResultCode: integer read fResultCode;
    property ResultStr: string read fResultStr;
    property Connected: boolean read GetConnected;
    property CurrentDeviceNumber: integer read fCurrentDeviceNumber write SetCurrentDeviceNumber;
    property FFDVersion: FFDVersionType read fFFDVersion write setFFDVersion;
    property AdminPassword: string read fAdminPassword write fAdminPassword;
    property OperatorPassword: string read fOperatorPassword write fOperatorPassword;
    property NameAndPriceOnOneLine: boolean read fNameAndPriceOnOneLine write fNameAndPriceOnOneLine;
    property TextWrap: integer read fTextWrap write fTextWrap;
    property EnabledDiscount: DiscountType read fEnabledDiscount write SetEnabledDiscount;
    property CashRegisterNum: byte read fCashRegisterNum write setCashRegisterNum;

    constructor Create(handle: HWnd);
    destructor Destroy; override;

    function getKKMSum: real;
    function getNonTransferedToOFDCount: integer;
    function isSessionOpened: boolean;
    function isSessionExceedLimit24: boolean;
    procedure showProperties;
    function getStatus: string;
    function getMode(const get_register: boolean = false): integer;
    function isFiskal: boolean;
    procedure connectToDevice;
    function cashIncome(const sm:real): boolean;
    function cashOutcome(const sm:real): boolean;
    function openIncomeCheck(const int_check_num: integer = 0; const open: boolean = true): boolean;
    function addProductToIncomeCheck(const name: string; const price: real; const kol: integer; const nds: NDSType; const barcode: string = ''; const dep: integer = 0; const discount: real = 0): boolean;
    function addProductToReturnCheck(const name: string; const price: real; const kol: integer; const nds: NDSType; const barcode: string = ''; const dep: integer = 0; const discount: real = 0): boolean;
    function closeIncomeCheck(const check_sm: real; const check_discount_sm: real = 0; const type_close: TypeCloseType = tcCash; const cash_sm: real = 0): integer;
    function closeReturnCheck(const type_close: TypeCloseType = tcCash; const check_discount_sm: real = 0): boolean;
    function openReturnCheck(const int_check_num: integer = 0; const open: boolean = false): boolean;
    function getROMVersion: string;
    function openSession: boolean;
    procedure getXReport;
    procedure getZReport; // closeSession
    function setTime(const hour,minute,sec: byte): boolean;
    function getKKMDateTime: TDateTime;
    function getKKMEarnings: real;
    function getKKMEarningSession: real;
    function getNDSByInt(const nds: integer): NDSType;
    function getSerialNumber: string;
    procedure printStringList(const text: string; const setmode_one: boolean = false; const use_print_string: boolean = false);
    procedure printTStringList(var lst: TStringList; const setmode_one: boolean = false; const use_print_string: boolean = false);
    procedure printHeader();
    procedure printFooter();
    procedure resetSummary;
    procedure techReset;
    function getCheckFontSizeString: string;
    procedure setCheckFontSize(const sz: byte);
    function getCheckFontSizeVertical: integer;
    function getCharSpacing: integer;
    function getFontSizeHorizontal: integer;
    function getCheckLineSpacing: integer;
    function setMode(const mode: byte; const force: boolean = false): boolean;
    function getSessionNumber: integer;
    function progGetValue(const p: integer): integer;
    function progGetCaption(const p: integer): string;
    procedure progSetValue(const p,val:integer);
    procedure progSetCaption(const p:integer; const val: string);
    procedure FullCut;
    procedure PartialCut;
    function getDevicesSettingsString: string;
    procedure PrintLastCheckCopy;
  end;

implementation

procedure TAtolKKT.LogText(const st: string);
var f:textfile;
    tmp,fnm:string;
begin

  fnm := ExtractFilePath(ParamStr(0))+'log.txt';
  AssignFile(f,fnm);
  if FileExists(fnm)
    then Append(f)
    else Rewrite(f);
  try
    tmp:=FormatDateTime('yyyy.mm.dd hh":"nn":"ss',now)+#9+st;
    Writeln(f,tmp);

  finally
    CloseFile(f);
  end;

end;

function TAtolKKT.addProductToIncomeCheck(const name: string; const price: real; const kol: integer; const nds: NDSType;
                                    const barcode: string = ''; const dep: integer = 0; const discount: real = 0): boolean;
begin
  if not Connected then begin
    result := false;
    exit;
  end;

  if (price=0) or (kol=0) then
    begin
      fResultStr := 'Ошибка: нулевое количество и\или цена!';
      raise EATOLLogicException.create(fResultStr);
    end;

  result := true;

  ECR.Name := name;
  ECR.Quantity := kol;
  ECR.Price := price;
  ECR.Department := dep;
  if trim(barcode)<>'' then
    ECR.Barcode := barcode;
  ECR.TaxTypeNumber := getNDSInt(nds);
  ECR.AdvancedRegistration := fNameAndPriceOnOneLine;
  ECR.TextWrap := fTextWrap;

  ECR.Registration;
  getResultCodeAndCheckOutOfPaper;

  if discount>0 then
    begin
      if (EnabledDiscount<>dtCheckPosition) or (EnabledDiscount<>dtWholeAndCheckPosition) then
        begin
          fResultStr := 'Ошибка: Скидка по позициям отключена!';
          raise EATOLLogicException.create(fResultStr);
        end;

      ECR.Summ := discount;
      ECR.Destination := 1; // на последнюю регистрацию\товар
      ECR.SummDiscount; // добавляем скидку в размере ECR.Summ
    end;
end;

function TAtolKKT.addProductToReturnCheck(const name: string;
  const price: real; const kol: integer; const nds: NDSType;
  const barcode: string; const dep: integer;
  const discount: real): boolean;
begin
  if not Connected then begin
    result := false;
    exit;
  end;

  if (price=0) or (kol=0) then
    begin
      fResultStr := 'Ошибка: нулевое количество и\или цена!';
      raise EATOLLogicException.create(fResultStr);
    end;

  result := true;

  ECR.Name := name;
  ECR.Quantity := kol;
  ECR.Price := price;
  ECR.Department := dep;
  if trim(barcode)<>'' then
    ECR.Barcode := barcode;
  ECR.TaxTypeNumber := getNDSInt(nds);
  ECR.AdvancedRegistration := fNameAndPriceOnOneLine;
  ECR.TextWrap := fTextWrap;

  ECR.Return;
  getResultCodeAndCheckOutOfPaper;

  if discount>0 then
    begin
      if (EnabledDiscount<>dtCheckPosition) or (EnabledDiscount<>dtWholeAndCheckPosition) then
        begin
          fResultStr := 'Ошибка: Скидка по позициям отключена!';
          raise EATOLLogicException.create(fResultStr);
        end;

      ECR.Summ := discount;
      ECR.Destination := 1; // на последнюю регистрацию\товар
      ECR.SummDiscount; // добавляем скидку в размере ECR.Summ
    end;
end;

function TAtolKKT.CashIncome(const sm: real): boolean;
begin
  result := false;
  if not Connected then exit;

  // устанавливаем пароль кассира
  ECR.Password := fOperatorPassword;

  // входим в режим регистрации
  ECR.Mode := 1;
  if ECR.SetMode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  ECR.Summ := sm;
  if ECR.CashIncome <> 0 then begin
      setResultCodeAndStrAndRaiseException;
    end
  else result := true;
end;

function TAtolKKT.CashOutcome(const sm: real): boolean;
begin
  result := false;
  if not Connected then exit;

  // устанавливаем пароль кассира
  ECR.Password := fOperatorPassword;

  // входим в режим регистрации
  ECR.Mode := 1;
  if ECR.SetMode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  ECR.Summ := sm;
  if ECR.CashOutcome <> 0 then
    begin
      setResultCodeAndStrAndRaiseException;
    end
  else result := true;
end;

function TAtolKKT.closeIncomeCheck(const check_sm: real; const check_discount_sm: real = 0; const type_close: TypeCloseType = tcCash;  const cash_sm: real = 0): integer;
begin
  result := 0;
  if not Connected then exit;

  if check_discount_sm>0 then // есть скидка суммой на весь чек
    begin
      if (EnabledDiscount<>dtWholeCheck) or (EnabledDiscount<>dtWholeAndCheckPosition) then
        begin
          fResultStr := 'Ошибка: Скидка на весь чек отключена!';
          raise EATOLLogicException.create(fResultStr);
        end;

      ECR.Summ := check_discount_sm;
      ECR.Destination := 0; // скидка на весь чек
      if ECR.SummDiscount <> 0 then Exit;
    end;

  if type_close=tcCash then
    begin

      if cash_sm < check_sm then
        begin // не хватает денег у покупателя
          fResultStr := 'Ошибка: не хватает денег у покупателя!';
          raise EATOLLogicException.create(fResultStr);
        end
      else
          if cash_sm = check_sm then
            begin // без сдачи
              ECR.Summ := cash_sm;
              ECR.TypeClose := 0; // нал
              ECR.CloseCheck;
              getResultCodeAndCheckOutOfPaper;
            end
          else // нужна сдача
            begin
              ECR.Summ := cash_sm;
              ECR.TypeClose := 0; // нал
              ECR.Delivery;
              getResultCodeAndCheckOutOfPaper;
            end;

    end
  else
    begin
      ECR.TypeClose := TypeCloseToInt(type_close); // безнал - Тип оплаты 1
      ECR.CloseCheck;
      getResultCodeAndCheckOutOfPaper;
    end;

  result := GetDocNumber; // номер чека в ККТ (сквозной, а не по сменам)
end;

function TAtolKKT.closeReturnCheck( const type_close: TypeCloseType = tcCash; const check_discount_sm: real = 0): boolean;
begin
  result := false;
  if not Connected then exit;

  if check_discount_sm>0 then // есть скидка суммой на весь чек
    begin
      ECR.Summ := check_discount_sm;
      ECR.Destination := 0; // скидка на весь чек
      if ECR.SummDiscount <> 0 then Exit;
    end;

  ECR.TypeClose := TypeCloseToInt(type_close);
  if ECR.CloseCheck <> 0 then begin
    getResultCodeAndCheckOutOfPaper;
    Exit;
  end;

  result := true;
end;

procedure TAtolKKT.connectToDevice;
begin
  if not Connected then exit;

  if ECR.DeviceEnabled then
    ECR.DeviceEnabled := false;

  ECR.DeviceEnabled := true;
  if ECR.ResultCode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  // если есть открытый чек, то отменяем его
  if ECR.CheckState <> 0 then
    if ECR.CancelCheck <> 0 then setResultCodeAndStrAndRaiseException;

  UpdateDeviceProperties;

end;

constructor TAtolKKT.Create(handle: HWnd);
begin
  inherited Create;

  fFFDVersion := ffd10;
  fAdminPassword := '30';
  fOperatorPassword := '1';

  fResultStr := '';
  try
    LogText('before creating ole object');
    ECR := CreateOleObject('AddIn.FprnM45');
    LogText('ole object created');
    if handle<>0 then
      ECR.ApplicationHandle := Handle; // необходимо для корректного отображения окон драйвера в контексте приложения
    fResultCode := 0;
  except
    fResultCode := ECR.ResultCode;
    fResultStr := 'Не удалось создать объект общего драйвера ККМ!';
    ECR := null;
    raise Exception.create(fResultStr);
  end;

  fDTOVersion := getVersionInt;
  fNameAndPriceOnOneLine := false;
  fTextWrap := 2; // перенос по символам (строке)
end;

destructor TAtolKKT.Destroy;
begin
  ECR := 0;
  inherited;
end;

procedure TAtolKKT.FullCut;
var m: byte;
begin
  if not Connected then exit;

  m := getMode(true);
  if m<>1 then
    begin
      // устанавливаем пароль кассира
      ECR.Password := fOperatorPassword;

      // входим в режим регистрации
      ECR.Mode := 1;
      if ECR.SetMode <> 0 then begin
        setResultCodeAndStrAndRaiseException;
      end;
    end;

  ECR.FullCut;
end;

function TAtolKKT.getCashRegisterNum: byte;
begin
  result := 0;
  if not Connected then exit;

  ECR.ValuePurpose := 0;

  if ECR.GetValue<>0 then
    setResultCodeAndStrAndRaiseException
  else result := ECR.Value;
end;

function TAtolKKT.getCharSpacing: integer;
begin
  if not Connected then exit;

  {if setMode(4) then
    begin
      ECR.ValuePurpose := 212;
      ECR.GetValue;

      Result := ECR.Value;
    end
  else Result := -1;}
end;

function TAtolKKT.getCheckFontSizeString: string;
begin
  if not Connected then exit;

  if setMode(4) then
    begin
      ECR.ValuePurpose := 62;
      ECR.GetValue;

      case ECR.Value of
        1: Result := '8x5';
        2: Result := '7x5';
        3: Result := '6x5';
        4: Result := '5x5';
        else Result := '';
      end;
    end
  else Result := '';
end;

function TAtolKKT.getCheckFontSizeVertical: integer;
begin
  if not Connected then exit;

  if setMode(4) then
    begin
      ECR.ValuePurpose := 64;
      ECR.GetValue;

      Result := ECR.Value;
    end
  else Result := -1;
end;

function TAtolKKT.getCheckLineSpacing: integer;
begin
  if not Connected then exit;

  if setMode(4) then
    begin
      ECR.ValuePurpose := 60;
      ECR.GetValue;
      Result := ECR.Value;
    end
   else Result := -1;
end;

function TAtolKKT.GetConnected: boolean;
begin
  Result := not (VarIsEmpty(ECR) or VarIsNull(ECR));
end;

function TAtolKKT.getDocNumber: integer;
begin
  result := 0;
  if not Connected then exit;

  ECR.RegisterNumber := 51;
  ECR.GetRegister;
  result := ECR.DocNumber;
end;

function TAtolKKT.getEnabledDiscount: DiscountType;
begin
  result := dtDisabled;
  if not Connected then exit;

  ECR.ValuePurpose := 11;

  if ECR.GetValue<>0 then
    setResultCodeAndStrAndRaiseException
  else begin
    case ECR.Value of
      0: result := dtDisabled;
      1: result := dtWholeCheck;
      2: result := dtCheckPosition;
      3: result := dtWholeAndCheckPosition;
      else result := dtDisabled;
    end;
  end;
end;

function TAtolKKT.getFontSizeHorizontal: integer;
begin
  if not Connected then exit;

  {if setMode(4) then
    begin
      ECR.ValuePurpose := 201;
      ECR.GetValue;

      Result := ECR.Value;
    end
  else Result := -1;}
end;

function TAtolKKT.getKKMDateTime: TDateTime;
begin
  result := 0;
  if not Connected then exit;

  ECR.RegisterNumber := 17;
  ECR.GetRegister;

  result := EncodeDateTime(ECR.Year, ECR.Month, ECR.Day, ECR.Hour, ECR.Minute, ECR.Second, 0);
end;

function TAtolKKT.getKKMEarnings: real;
begin
  result := 0;
  if not Connected then exit;

  ECR.RegisterNumber := 11;
  ECR.GetRegister;

  result := ECR.Summ;
end;

function TAtolKKT.getKKMEarningSession: real;
begin
  result := 0;
  if not Connected then exit;

  ECR.RegisterNumber := 12;
  ECR.GetRegister;

  result := ECR.Summ;
end;

function TAtolKKT.getKKMSum: real;
begin
  if Connected then
    begin
      //Result := ECR.GetSumm

      ECR.RegisterNumber := 10; // 2018-05-19
      ECR.GetRegister;

      result := ECR.Summ;
    end
  else Result := 0;
end;

function TAtolKKT.getMode(const get_register: boolean = false): integer;
begin
  if Connected then
    begin
      if get_register then
        begin
          ECR.RegisterNumber := 19;
          ECR.GetRegister;
        end;

      Result := ECR.Mode;
    end
  else Result := -1;
end;

function TAtolKKT.getNDSByInt(const nds: integer): NDSType;
begin
  case nds of
    0: result := ndsNo;
    10: result := nds10;
    18: result := nds18;
    else Result := ndsNo;
  end;
end;

function TAtolKKT.getNDSInt(const nds: NDSType): integer;
begin
  case fFFDVersion of

    ffd10: begin
      case nds of
        ndsSection: result := 0;
        ndsNo: result := 4;
        nds0: result := 1;
        nds10: result := 2;
        nds18: result := 3;
        nds10110: result := 5;
        nds18118: result := 6;
        else result := 0;
      end;
    end;

    ffd105: begin

      // ДТО 8.16 кодирует все НДС единообразно для всех версий ФФД (1.0 и 1.05)
      if fDTOVersion>=816 then
        case nds of
          ndsSection: result := 0;
          ndsNo: result := 4;
          nds0: result := 1;
          nds10: result := 2;
          nds18: result := 3;
          nds10110: result := 5;
          nds18118: result := 6;
          else result := 0;
        end
      else
        // ДТО 8.15 делал так
        case nds of
          ndsSection: result := 0;
          ndsNo: result := 6;
          nds0: result := 5;
          nds10: result := 2;
          nds18: result := 1;
          nds10110: result := 3;
          nds18118: result := 4;
          else result := 6;
        end;


    end;

    ffd11: begin
      // непонятно пока
      result := 0;
    end;

    else begin
      case nds of // ДТО 8.16
        ndsSection: result := 0;
        ndsNo: result := 4;
        nds0: result := 1;
        nds10: result := 2;
        nds18: result := 3;
        nds10110: result := 5;
        nds18118: result := 6;
        else result := 0;
      end;
    end;

  end;
end;

function TAtolKKT.getNonTransferedToOFDCount: integer;
begin
  result := 0;
  if not Connected then exit;

  ECR.REgisterNumber := 44;
  if ECR.GetRegister<>0 then exit;
  result := ECR.Count;
end;

procedure TAtolKKT.getResultCodeAndCheckOutOfPaper;
begin
  fResultCode := ECR.ResultCode;
  if fResultCode=-3807 then // нет бумаги
    begin
      fResultStr := 'Ошибка ККМ: ' + string(ECR.ResultDescription) + '! Нет бумаги!';
      raise EATOLException.create(fResultStr, fResultCode);
    end;
end;

function TAtolKKT.getROMVersion: string;
begin
  result := '';
  if not Connected then exit;

  ECR.RegisterNumber := 54;
  if ECR.GetRegister<>0 then exit;

  result := ECR.ROMVersion;
end;

function TAtolKKT.getSerialNumber: string;
begin
  result := '';
  if not Connected then exit;

  ECR.RegisterNumber := 22;
  if ECR.GetRegister<>0 then exit;

  result := ECR.SerialNumber;
end;

function TAtolKKT.getSessionNumber: integer;
begin
  if not Connected then exit;

  ECR.RegisterNumber := 21;
  if ECR.GetRegister<>0 then exit;

  result := ECR.Session;
end;

function TAtolKKT.getStatus: string;
begin
  result := '';
  if not Connected then exit;

  if ECR.GetStatus <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  result := result + 'PointPosition = '+IntToStr(ECR.PointPosition)+#13#10;
  result := result + 'Operator = '+IntToStr(ECR.&Operator)+#13#10; // Экранируем & зарезервированное слово operator
  result := result + 'LogicalNumber = '+IntToStr(ECR.LogicalNumber)+#13#10;
  result := result + 'Mode = '+IntToStr(ECR.Mode)+#13#10;
  if ECR.SessionOpened then
    result := result + 'SessionOpened = true'#13#10
  else result := result + 'SessionOpened = false'#13#10;
  result := result + 'Session = '+IntToStr(ECR.Session)+#13#10;
  result := result + 'CheckState = '+IntToStr(ECR.CheckState)+#13#10;
  result := result + 'Summ = '+FloatToStr(ECR.Summ)+#13#10;
end;

function TAtolKKT.getVersionInt: integer;
var st, odt: string;
    n: integer;
begin
  result := 0;
  if not Connected then exit;

  odt := ECR.Version;
  n := AnsiPos('.', odt);
  if n>1 then
    begin
      st := copy(odt, 1, n-1);
      n := length(st);
    end
  else
    begin
      n := length(odt);
    end;

  st := copy(odt, 1, 4 + n - 1);
  st := StringReplace(st, '.', '', [rfReplaceAll,rfIgnoreCase]);
  st := StringReplace(st, '-', '', [rfReplaceAll,rfIgnoreCase]);
  st := StringReplace(st, '_', '', [rfReplaceAll,rfIgnoreCase]);
  result := StrToIntDef(st, 0);
end;

procedure TAtolKKT.getXReport;
begin
  if not Connected then exit;

  // вроде, как не надо проверять
  {if not isSessionOpened then
    begin
      fResultCode := 1;
      fResultStr := 'Смена не открыта!';
      raise EATOLLogicException.Create(fResultStr);
    end;}

  // x-report
  ECR.Password := fAdminPassword;
  // входим в режим отчетов без гашения
  ECR.Mode := 2;
  if ECR.SetMode <> 0 then setResultCodeAndStrAndRaiseException;
  // снимаем отчет
  ECR.ReportType := 2;
  if ECR.Report <> 0 then setResultCodeAndStrAndRaiseException;
end;

procedure TAtolKKT.getZReport;
begin
  if not Connected then exit;

  // z-report
  if isSessionOpened then
    begin

      // устанавливаем пароль системного администратора ККМ
      ECR.Password := fAdminPassword;
      // входим в режим отчетов с гашением
      ECR.Mode := 3;
      if ECR.SetMode <> 0 then begin
        setResultCodeAndStrAndRaiseException;
      end;
      // снимаем отчет
      ECR.ReportType := 1;
      if ECR.Report <> 0 then begin
        setResultCodeAndStrAndRaiseException;
      end;

    end
  else
    begin
      fResultCode := 1;
      fResultStr := 'Смена не открыта!';
      raise EATOLLogicException.Create(fResultStr);
    end;

end;

function TAtolKKT.isFiskal: boolean;
begin
  result := false;
  if not Connected then exit;

  result := ECR.Fiscal;
end;

function TAtolKKT.isSessionExceedLimit24: boolean;
begin
  result := false;
  if not Connected then exit;

  ECR.RegisterNumber := 18;
  ECR.GetRegister;

  result := ECR.SessionExceedLimit;
end;

function TAtolKKT.isSessionOpened: boolean;
begin
  result := false;
  if not Connected then exit;

  result := ECR.SessionOpened;
end;

function TAtolKKT.openIncomeCheck(const int_check_num: integer = 0; const open: boolean = true): boolean;
begin
  result := false;
  if not Connected then exit;

  // если есть открытый чек, то отменяем его
  if ECR.CheckState <> 0 then
    if ECR.CancelCheck <> 0 then Exit;

  result := true;

  // устанавливаем пароль кассира
  ECR.Password := fOperatorPassword;

  // входим в режим регистрации
  ECR.Mode := 1;
  if ECR.SetMode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  ECR.CheckMode := 1; // печатаем на бумаге
  ECR.CheckType := 1; // приход - продажа товара


  if open then
    ECR.OpenCheck;

  if int_check_num>0 then
    begin
      ECR.TextWrap := 0; // не переносим
      ECR.Caption := 'Чек № '+IntToStr(int_check_num);
      ECR.PrintString;
      getResultCodeAndCheckOutOfPaper;
    end;

end;

function TAtolKKT.openReturnCheck(const int_check_num: integer = 0; const open: boolean = false): boolean;
begin
  result := false;
  if not Connected then exit;

  result := true;
  // устанавливаем пароль кассира
  ECR.Password := fOperatorPassword;

  // входим в режим регистрации
  ECR.Mode := 1;
  if ECR.SetMode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  // если есть открытый чек, то отменяем его
  if ECR.CheckState <> 0 then
    if ECR.CancelCheck <> 0 then Exit;

  ECR.CheckMode := 1; // печатаем на бумаге
  // при возврате, вроде, так не надо делать, иначе первый вызов Return вызовет ошибку
  //ECR.CheckType := 2; // возврат товара

  if open then begin// при возврате, вроде, так не надо делать, чек автоматом открывается, при первом вызове Return (регистрации первого возвращаемого товара)
    ECR.OpenCheck;
    getResultCodeAndCheckOutOfPaper;
  end;

  // это не получится, пока не откроем чек вызовом Return()
  {if int_check_num>0 then
    begin
      ECR.TextWrap := 0; // не переносим
      ECR.Caption := 'Чек № '+IntToStr(int_check_num);
      ECR.PrintString;
    end;}
end;

function TAtolKKT.openSession: boolean;
begin
  result := false;
  if not Connected then exit;

  // устанавливаем пароль кассира
  ECR.Password := fOperatorPassword;

  // входим в режим регистрации
  ECR.Mode := 1;
  if ECR.SetMode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  ECR.Caption := 'Заголовок смены 1';
  if ECR.OpenSession<>0 then
    begin
      setResultCodeAndStrAndRaiseException;
    end;

  result := true;
end;

procedure TAtolKKT.PartialCut;
var m: byte;
begin
  if not Connected then exit;

  m := getMode(true);
  if m<>1 then
    begin
      // устанавливаем пароль кассира
      ECR.Password := fOperatorPassword;

      // входим в режим регистрации
      ECR.Mode := 1;
      if ECR.SetMode <> 0 then begin
        setResultCodeAndStrAndRaiseException;
      end;
    end;

  ECR.FullCut;
end;

procedure TAtolKKT.printFooter;
var m: integer;
begin
  if not Connected then exit;

  m := getMode(true);
  if m<>2 then
    begin
      // устанавливаем пароль кассира
      ECR.Password := fAdminPassword;

      // входим в режим регистрации
      ECR.Mode := 2;
      if ECR.SetMode <> 0 then begin
        setResultCodeAndStrAndRaiseException;
      end;
    end;

  ECR.PrintFooter;
end;

procedure TAtolKKT.printHeader;
begin
  if not Connected then exit;
  ECR.PrintHeader;
end;

procedure TAtolKKT.printStringList(const text: string; const setmode_one: boolean = false; const use_print_string: boolean = false);
var lst: TStringList;
    i,m: integer;
    st: string;
begin
  if not Connected then exit;

  if setmode_one then
    begin
      m := getMode(true);
      if m<>1 then
        begin
          // устанавливаем пароль кассира
          ECR.Password := fOperatorPassword;

          // входим в режим регистрации
          ECR.Mode := 1;
          if ECR.SetMode <> 0 then begin
            setResultCodeAndStrAndRaiseException;
          end;
        end;
    end;

  lst := TStringList.create;
  lst.Text := text;

  for i:=0 to lst.Count-1 do
    begin
      st := lst.Strings[i];

      if use_print_string then
        begin
          ECR.Alignment := 0;
          ECR.TextWrap := 0; // не переносим
          ECR.Caption := st;
          ECR.PrintString;
        end
      else
        begin
          ECR.TextWrap := 0;
          ECR.FontBold := false;
          ECR.FontUnderline := false;
          ECR.Caption := st;
          ECR.Alignment := 0;
          ECR.AddField();
          ECR.PrintField();
        end;
    end;

  lst.free;
end;

procedure TAtolKKT.printTStringList(var lst: TStringList; const setmode_one: boolean = false; const use_print_string: boolean = false);
var m, i: integer;
    st: string;
    fObj: TAtolFont;
begin
  if not Connected then exit;

  if setmode_one then
    begin
      m := getMode(true);
      if m<>1 then
        begin
          // устанавливаем пароль кассира
          ECR.Password := fOperatorPassword;

          // входим в режим регистрации
          ECR.Mode := 1;
          if ECR.SetMode <> 0 then begin
            setResultCodeAndStrAndRaiseException;
          end;
        end;
    end;

  for i:=0 to lst.Count-1 do
    begin
      st := lst.Strings[i];

      if use_print_string then
        begin
          ECR.Alignment := integer(lst.Objects[i]);
          ECR.TextWrap := 0; // не переносим
          ECR.Caption := st;
          ECR.PrintString;
        end
      else
        begin
          ECR.TextWrap := 0;
          //ECR.FontBold := false;
          //ECR.FontUnderline := false;
          //ECR.AddField;
          if (lst.Objects[i] is TAtolFont) then
            begin
              fObj := TAtolFont(lst.Objects[i]);
              ECR.FontBold := fObj.FontBold;
              ECR.FontItalic := fObj.FontItalic;
              ECR.FontUnderline := fObj.FontUnderline;
              ECR.Alignment := fobj.Alignment;
              ECR.FontNegative := fobj.FontNegative;
            end;
          ECR.Caption := st;
          ECR.AddField;
          ECR.PrintField;
        end;
    end;
end;

function TAtolKKT.progGetCaption(const p: integer): string;
begin
  if not connected then
    exit;

  if setMode(4) then
    begin
      ECR.CaptionPurpose := p;
      try
        ECR.GetCaption;
        if ECR.ResultCode<>0 then
          begin
            fResultStr := ECR.ResultDescription;
            Result := '<null>';
          end;
      except
        on e:exception do
          begin
            Result := '<null>';
            raise Exception.Create(ECR.ResultDescription);
          end;
      end;

      Result := ECR.Caption;
    end
  else Result := '';
end;

function TAtolKKT.progGetValue(const p: integer): integer;
begin
  if not connected then
    exit;

  if setMode(4) then
    begin
      ECR.ValuePurpose := p;
      try
        ECR.GetValue;
        if ECR.ResultCode<>0 then
          begin
            fResultStr := ECR.ResultDescription;
            Result := ECR.ResultCode;
          end;
      except
        on e:exception do
          begin
            raise Exception.Create(ECR.ResultDescription);
          end;
      end;

      Result := ECR.Value;
    end
  else Result := -1;
end;

procedure TAtolKKT.progSetCaption(const p: integer; const val: string);
begin
  if not connected then
    exit;

  if setMode(4) then
    begin
      ECR.CaptionPurpose := p;
      ECR.Caption := val;
      try
        ECR.SetCaption;
        if ECR.ResultCode<>0 then
          begin
            fResultStr := ECR.ResultDescription;
          end;
      except
        on e:exception do
          begin
            raise Exception.Create(ECR.ResultDescription);
          end;
      end;
    end;
end;

procedure TAtolKKT.progSetValue(const p, val: integer);
begin
  if not connected then
    exit;

  if setMode(4) then
    begin
      ECR.ValuePurpose := p;
      ECR.Value := val;
      try
        ECR.SetValue;
        if ECR.ResultCode<>0 then
          begin
            fResultStr := ECR.ResultDescription;
          end;
      except
        on e:exception do
          begin
            raise Exception.Create(ECR.ResultDescription);
          end;
      end;
    end;
end;

procedure TAtolKKT.resetSummary;
begin
  if not Connected then exit;

  ECR.Password := fAdminPassword;

  ECR.Mode := 3;
  if ECR.SetMode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  ECR.ResetSummary;
end;

procedure TAtolKKT.setCashRegisterNum(const Value: byte);
begin
  if not Connected then exit;

  ECR.Password := fAdminPassword;

  // входим в режим регистрации
  ECR.Mode := 4;
  if ECR.SetMode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  ECR.ValuePurpose := 0;
  ECR.Value := Value;
  if ECR.SetValue<>0 then
    setResultCodeAndStrAndRaiseException
  else fCashRegisterNum := Value;
end;

procedure TAtolKKT.setCheckFontSize(const sz: byte);
begin
  if not Connected then exit;

  if setMode(4) then
    begin
      ECR.ValuePurpose := 62;
      ECR.Value := sz;
      ECR.SetValue;
    end;
end;

procedure TAtolKKT.SetCurrentDeviceNumber(const Value: integer);
begin
  if not Connected then exit;

  if fCurrentDeviceNumber=Value then
    exit;

  fCurrentDeviceNumber := Value;
  ECR.CurrentDeviceNumber := fCurrentDeviceNumber;
end;

procedure TAtolKKT.SetEnabledDiscount(const Value: DiscountType);
begin
  if not Connected then exit;

  ECR.ValuePurpose := 11;
  case Value of
    dtDisabled: ECR.Value := 0;
    dtWholeCheck: ECR.Value := 1;
    dtCheckPosition: ECR.Value := 2;
    dtWholeAndCheckPosition: ECR.Value := 3;
    else ECR.Value := 0;
  end;

  if ECR.SetValue<>0 then
    setResultCodeAndStrAndRaiseException
  else fEnabledDiscount := Value;
end;

procedure TAtolKKT.setFFDVersion(const Value: FFDVersionType);
begin
  fFFDVersion := Value;
end;

function TAtolKKT.setMode(const mode: byte; const force: boolean = false): boolean;
begin
  if not Connected then exit;

  Result := true;
  if (getMode(true)=mode) and (not force) then
    exit;

  ECR.Password := fAdminPassword;

  ECR.Mode := mode;
  if ECR.SetMode <> 0 then begin
    result := false;
    setResultCodeAndStrAndRaiseException;
  end;
end;

procedure TAtolKKT.setResultCodeAndStrAndRaiseException;
begin
  fResultCode := ECR.ResultCode;
  fResultStr := 'Ошибка ККМ: ' + string(ECR.ResultDescription) + '!';
  raise EATOLException.create(fResultStr, fResultCode);
end;

function TAtolKKT.setTime(const hour, minute, sec: byte): boolean;
begin
  result := false;
  if not Connected then exit;

  if isSessionOpened then
    exit;

  ECR.Hour := hour;
  ECR.Minute := minute;
  ECR.Second := sec;
  if ECR.SetTime<>0 then
    begin
      setResultCodeAndStrAndRaiseException;
    end;
  result := true;
end;

procedure TAtolKKT.ShowProperties;
begin
  if not Connected then exit;
  ECR.ShowProperties;

  fCurrentDeviceNumber := ECR.CurrentDeviceNumber;
  UpdateDeviceProperties;
end;

procedure TAtolKKT.techReset;
begin
  if not Connected then exit;

  ECR.Password := fAdminPassword;

  ECR.Mode := 0;
  if ECR.SetMode <> 0 then begin
    setResultCodeAndStrAndRaiseException;
  end;

  ECR.ResetSettings;
end;

function TAtolKKT.TypeCloseToInt(const tc: TypeCloseType): integer;
begin
  case tc of
    tcCash: result := 0;
    tc1: result := 1;
    tc2: result := 2;
    tc3: result := 3;
    tc4: result := 4;
    tc5: result := 5;
    else result := 0;
  end;
end;

procedure TAtolKKT.UpdateDeviceProperties;
begin
  fEnabledDiscount := getEnabledDiscount;
  fCashRegisterNum := getCashRegisterNum;
end;

function TAtolKKT.getDevicesSettingsString: string;
begin
  if not Connected then exit;

  result := ECR.DevicesSettings;
end;

procedure TAtolKKT.PrintLastCheckCopy;
begin
  if not Connected then exit;

  ECR.PrintLastCheckCopy;
end;

{ EATOLException }

constructor EATOLException.Create(const Msg: string; const ErrCode: integer);
begin
  inherited Create(Msg);
  FErrorCode := ErrCode;
end;

end.

