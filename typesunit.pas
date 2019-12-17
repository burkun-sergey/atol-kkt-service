unit TypesUnit;
//------------------------------------------------------------------------------
// Типы данных
//------------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AtolKKTUnit;

const
  // размер входного и выходного буфера именованного канала 65536 байт
  // команды в формате json, превышающие размер в байтах в 65536 должны быть разделены на более короткие и отосланы специальной командой
  PIPE_BUFFER = $FFFF;

  PIPE_TIMEOUT = 5000;

  PIPE_SHUTDOWN = 'SHUTDOWN';

  TEST_CHEQUE  = '         Магазин "Детский Мир"          '#13#10
                +'        Москва,ул.Вавилова,д.19,        '#13#10
                +'             тел. 123-4567              '#13#10
                +'                                        '#13#10
                +'24.12.14                           10:41'#13#10
                +'                  ЧЕК                   '#13#10
                +'                 Оплата                 '#13#10
                +'Номер операции:                     0007'#13#10
                +'Терминал:                       00859916'#13#10
                +'Пункт обслуживания:         854444445555'#13#10
                +'                   Visa                 '#13#10
                +'Карта:(D)               ************0002'#13#10
                +'Клиент:                                 '#13#10
                +'                                        '#13#10
                +'Сумма (Руб):                            '#13#10
                +'                4.28                    '#13#10
                +'Комиссия за операцию - 0 руб.           '#13#10
                +'ОДОБРЕНО   Код авторизации:       12Y561'#13#10
                +'                                        '#13#10
                +'                                        '#13#10
                +'                                        '#13#10
                +'   ________________________________     '#13#10
                +'            подпись клиента             '#13#10
                +'                                        '#13#10
                +'                                        '#13#10
                +'   _________________________________    '#13#10
                +'      подпись кассира(контролера)       '#13#10
                +'2FEBABF549641069E2C9DCAC831DD04373B24A83'#13#10
                +'========================================';

  type

  RequestComandType=(rctUnknown, rctPrintTestCheque, rctCashRegisterInf,
                     rctPrintTStringList, rctPrintStringList, rctChequePartialCut,
                     rctChequeFullCut, rctResetSummary, rctTechReset, rctIncome,
                     rctXReport, rctZReport, rctCashIncome, rctCashOutcome,
                     rctReturn, rctOpenSession, rctPrintLastCheckCopy,
                     rctSegment);

  AtolDeviceDataType=record
    atol_device_number: integer;
    atol_operator_password: string;
  end;

  RequestType=record
    comand: RequestComandType;
    data: string;
    atol_device_data: AtolDeviceDataType;
  end;

  ResponseType=record
    response: string;
    error_code: integer;
    error: string;
  end;

  RequestSegmentType=record
    guid: string;
    num: integer;
    count: integer;
    text: string;
  end;

  CashRegisterInfType=record
    KKMDateTime: string;
    KKMEarnings: real;
    KKMEarningSession: real;
    KKMSum: real;
    Mode: integer;
    SerialNumber: string;
    CheckFontSizeString: string;
    CheckLineSpacing: integer;
    SessionNumber: integer;
    isFiskal: boolean;
    isSessionExceedLimit24: boolean;
    isSessionOpened: boolean;
    CashRegisterNum: integer;
  end;

  TFileVersionInfo = record
    FileType,
    CompanyName,
    FileDescription,
    FileVersion,
    InternalName,
    LegalCopyRight,
    LegalTradeMarks,
    OriginalFileName,
    ProductName,
    ProductVersion,
    Comments,
    SpecialBuildStr,
    PrivateBuildStr,
    FileFunction : string;
    DebugBuild,
    PreRelease,
    SpecialBuild,
    PrivateBuild,
    Patched,
    InfoInferred : Boolean;
  end;

  ExceptionEventLogType=(eeltSQL, eeltInput, eeltOther);

  EventLogType=(eltInf,eltWarn,eltErr);

  CashRegAtolConnectModeType=(crAtolCM_NO_DEVICE, crAtolCM_OLE);

  AtolDeviceType=record
    atol_cash_register: boolean; // подключаться или нет
    atol_device_number: integer;
    atol_ffd_version: FFDVersionType;
    atol: TAtolKKT;
    atol_set_time_on_connect: boolean;
    atol_reg_product_discount,
    atol_reg_product_barcode: boolean;
    atol_admin_password: string;
    atol_operator_password: string;
    atol_cash_register_num: integer; // номер кассы для печати на чеке АТОЛ
    atol_view_options_button: boolean;
    atol_connect_mode: CashRegAtolConnectModeType; // режим взаимодействия с АТОЛ
    atol_reg_product_nds_zero: boolean;
    atol_print_reports_on_real_printer: boolean;
    atol_cut_acquiring_cheque: integer;
  end;

  AtolDeviceArrayType=array of AtolDeviceType;

  ChequeGoodsType = record
    name: string;
    price: real;
    kol: integer;
    nds: NDSType;
    barcode: string;
    dep: integer;
    discount: real;
  end;

  ChequeGoodsArrayType = array of ChequeGoodsType;

  ChequeDataType = record
    goods: ChequeGoodsArrayType;
    check_sm: real;
    check_discount_sm: real;
    type_close: TypeCloseType;
    cash_sm: real;
    tr_id: integer;
  end;


implementation

end.

