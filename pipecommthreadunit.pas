unit PipeCommThreadUnit;
//------------------------------------------------------------------------------
// Основной поток сервиса
//------------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypesUnit, Windows, IniFiles, AtolKKTUnit,
  SegmentArrayUnit, comobj, LoggerUnit, RequestUnit, ResponseUnit,
  DeviceListUnit;

const
  COINIT_APARTMENTTHREADED = 2;

type

  { TPipeComm }

  TPipeComm = class(TThread)
  private
    { Private declarations }
    hPipe : THandle;
    pipe_name: string;

    response: string;

    atol_header_lst,
    atol_footer_lst: TStringList;
    atol_full_log: boolean;
    atol_print_test_check_on_device1_after_init: boolean;

    // EncodeStringBase64(const s:string):String;   base64 unit
    // DecodeStringBase64(const s:string;strict:boolean=false):String;  base64 unit

    var_lst:TStringList;
    var_phone,
    var_phone2,
    var_fax,
    var_email,
    var_www,
    var_worktime,
    var_adr,
    var_inn,
    var_kpp,
    var_name,
    var_bankname,
    var_ks,
    var_rs,
    var_bik:string;

    procedure onServiceStart;
    procedure onServiceDestroy;
    procedure logPipeData(const text: string);
    procedure InitAtol(var device: AtolDeviceType);

    procedure setResponse(const request: string);

    function decodeVariablesInString(const st: string): string;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspennded: Boolean; const pipename: string);
  end;

implementation

{ TPipeComm }

procedure TPipeComm.onServiceStart;
var ini: TIniFile;
    n, i, align: integer;
    section, st: string;
    af: TAtolFont;
    device: TAtolKKT;
    KKT_device: AtolDeviceType;
begin
  var_lst := TStringList.create;
  atol_header_lst := TStringList.create;
  atol_footer_lst := TStringList.create;
  segments_arr := TSegmentArray.create;

  //CoInitializeEx(nil, COINIT_MULTITHREADED); // не получилось заставить так работать драйвер ККТ Атол (потому, в том числе, невозможно одновременное выполнение команд на разных ККТ)
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  ini := TIniFile.Create(ExtractFilePath(ParamStr(0))+'ini.ini');

  TDeviceList.setAtolDoCommand(ini.ReadBool('Options','AtolRunComands',true));
  atol_print_test_check_on_device1_after_init := ini.ReadBool('Options','PrintTestChequeOnDevice1AfterInit',true);
  atol_full_log := ini.ReadBool('Options','FullLog',true);

  logPipeData('disconnect devices');
  TDeviceList.DisconnectFromDevices;
  logPipeData('after disconnect devices');

  //TDeviceList.addDevice(device);

  TDeviceList.clearDeviceList();
  n := ini.ReadInteger('Devices','Count',0);
  for i:=1 to n do
    begin
      logPipeData('Read ini for device #'+IntToStr(i));

      section := IntToStr(i)+'Device';

      KKT_device.atol_cash_register := ini.ReadBool(section,'AtolCashRegister',false);
      KKT_device.atol_device_number := ini.ReadInteger(section,'AtolDeviceNumber',1);
      case ini.ReadInteger(section,'AtolFFDVersion',0) of
        0: KKT_device.atol_ffd_version := ffd10;
        1: KKT_device.atol_ffd_version := ffd105;
        2: KKT_device.atol_ffd_version := ffd11;
        else KKT_device.atol_ffd_version := ffd10;
      end;
      KKT_device.atol_set_time_on_connect := ini.ReadBool(section,'AtolSetTimeOnConnect',false);
      KKT_device.atol_reg_product_discount := ini.ReadBool(section,'AtolRegProductDiscount',false);
      KKT_device.atol_reg_product_barcode := ini.ReadBool(section,'AtolRegProductBarcode',false);
      KKT_device.atol_admin_password := ini.ReadString(section,'AtolAdminPassword','30');
      KKT_device.atol_operator_password := ini.ReadString(section,'AtolOperatorPassword','30');
      KKT_device.atol_cash_register_num := ini.ReadInteger(section,'AtolCashRegisterNumber',1);
      KKT_device.atol_reg_product_nds_zero := ini.ReadBool(section,'AtolRegProductNdsZero',false);
      KKT_device.atol_print_reports_on_real_printer := ini.ReadBool(section,'AtolPrintReportsOnRealPrinter',false);
      KKT_device.atol_cut_acquiring_cheque := ini.readinteger(section, 'AtolCutAcquiringCheque', 0);

      // определим режим работы программы с АТОЛ
      KKT_device.atol_connect_mode := crAtolCM_NO_DEVICE;
      if KKT_device.atol_cash_register  then
        KKT_device.atol_connect_mode := crAtolCM_OLE;

      logPipeData('Reading ini for device #'+IntToStr(i)+' is done');

      TDeviceList.addDevice(KKT_device);

      if (KKT_device.atol_cash_register)
        and (KKT_device.atol_connect_mode = crAtolCM_OLE) then
        begin
          logPipeData('before init atol device #'+IntToStr(KKT_device.atol_device_number));
          InitAtol(KKT_device);

          if Assigned(KKT_device.atol) then
            logPipeData('device is still assigned')
          else logPipeData('device is not assigned anymore!!!');
        end
      else logPipeData('skip init for device #'+IntToStr(i));
    end;

  var_lst.Clear;
  for i:=1 to 10 do
    begin
      st:=ini.ReadString('Variables','P'+IntToStr(i),'');
      var_lst.Add(st);
    end;
  var_phone:=ini.ReadString('Variables','Phone','');
  var_phone2:=ini.ReadString('Variables','Phone2','');
  var_fax:=ini.ReadString('Variables','Fax','');
  var_email:=ini.ReadString('Variables','Email','');
  var_www:=ini.ReadString('Variables','WWW','');
  var_worktime:=ini.ReadString('Variables','WorkTime','');
  var_adr:=ini.ReadString('Variables','Adr','');
  var_inn:=ini.ReadString('Variables','INN','');
  var_kpp:=ini.ReadString('Variables','KPP','');
  var_name:=ini.ReadString('Variables','Name','');
  var_bankname:=ini.ReadString('Variables','BankName','');
  var_ks:=ini.ReadString('Variables','KS','');
  var_rs:=ini.ReadString('Variables','RS','');
  var_bik:=ini.ReadString('Variables','BIK','');

  logPipeData('Vars is set');

  atol_header_lst.clear;
  n := ini.ReadInteger('AtolHeader','Count',0);
  for i:=1 to n do
    begin
      align := ini.ReadInteger('AtolHeader',IntToStr(i)+'Align',0);
      st := ini.ReadString('AtolHeader',IntToStr(i)+'Str','');
      st := decodeVariablesInString(st);
      af := TAtolFont.create;
      af.Alignment := align;
      atol_header_lst.AddObject(st, af);
    end;

  atol_footer_lst.clear;
  n := ini.ReadInteger('AtolFooter','Count',0);
  for i:=1 to n do
    begin
      st := ini.ReadString('AtolFooter',IntToStr(i)+'Str','');
      st := decodeVariablesInString(st);
      align := ini.ReadInteger('AtolFooter',IntToStr(i)+'Align',0);
      af := TAtolFont.create;
      af.Alignment := align;
      atol_footer_lst.AddObject(st, af);
    end;

  logPipeData('Headers\Footers is set');

  ini.Free;

  // однократная печать тестового чека на первом подключенном ККТ при запуске службы (только для отладки)
  device := TDeviceList.getDeviceByDeviceNumber(1);
  if Assigned(device) then
    begin
      if atol_print_test_check_on_device1_after_init then
        begin
          logPipeData('Print on device #1');
          device.printStringList(TEST_CHEQUE, true, true);
        end;
    end
  else logPipeData('Device #1 not assigned');
end;

procedure TPipeComm.onServiceDestroy;
begin
  //CoUninitialize; Вроде как comobj сам это делает в finalization
  var_lst.Free;
  atol_header_lst.Free;
  atol_footer_lst.Free;
  TDeviceList.clearDeviceList();

  if Assigned(segments_arr) then
    segments_arr.Free;
end;

procedure TPipeComm.logPipeData(const text: string);
begin
  if atol_full_log then
    MyLogger.LogText(text);
end;

procedure TPipeComm.InitAtol(var device: AtolDeviceType);
var hour, min, sec, msec: word;
begin
  logPipeData('init atol device #'+IntToStr(device.atol_device_number));

  //device.atol := TAtolKKT.Create(GetDesktopWindow); // Application.Handle - драйверу нужен handle главного окна приложения, чтобы корректно выводить свои всплывающие окна
  device.atol := TAtolKKT.Create(0); // у сервиса нет окна, потому передаем 0
  logPipeData('atol device created');

  if Assigned(device.atol) then
    logPipeData('atol device assigned')
  else logPipeData('atol device is NOT assigned!!!');

  logPipeData('запрашиваем список логических устройств драйвера с параметрами их настройки...');
  logPipeData('логические устройства с настройками:'#13#10+device.atol.getDevicesSettingsString);

  device.atol.FFDVersion := device.atol_ffd_version;
  device.atol.CurrentDeviceNumber := device.atol_device_number;
  logPipeData('готовимся соединиться с устройством...');
  device.atol.connectToDevice;
  logPipeData('связь драйвера с устройством установлена');
  device.atol.getStatus;
  logPipeData('device getting status is done');
  device.atol.AdminPassword := device.atol_admin_password;
  device.atol.OperatorPassword := device.atol_operator_password;
  logPipeData('device setting CR Num...');
  device.atol.CashRegisterNum := device.atol_cash_register_num;
  logPipeData('device setting CR Num is done');

  //UpdateAtolCashRegisterInf;
  if device.atol_set_time_on_connect then
    begin
      logPipeData('init atol device #'+IntToStr(device.atol_device_number)+' setting date\time');
      DecodeTime(now, hour, min, sec, msec);
      device.atol.setTime(hour, min, sec);
    end;
  logPipeData('init atol device #'+IntToStr(device.atol_device_number)+' is done!');
end;

procedure TPipeComm.setResponse(const request: string);
var req: RequestType;
begin
  if trim(request)<>'' then
    begin
      logPipeData('request: '+request);
      // разбираем команду
      try
        req := TRequest.getRequestTypeByRequest(request);
      except
        on e:exception do
          begin
            response := TResponse.createErrorResponseString(e.message, -1);
            MyLogger.LogError(0, e.Message);
            exit;
          end;
      end;
      // готовим ответ
      response := TResponse.responseTypeToString(TDeviceList.runComand(req));
      logPipeData('response: '+response);
    end;
end;

function TPipeComm.decodeVariablesInString(const st: string): string;
var i: integer;
begin
  Result := StringReplace(st, '{INN}', var_inn, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{Phone}', var_phone, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{Phone2}', var_phone2, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{Fax}', var_fax, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{Email}', var_email, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{WWW}', var_www, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{WorkTime}', var_worktime, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{Adr}', var_adr, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{KPP}', var_kpp, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{Name}', var_name, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{BankName}', var_bankname, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{KS}', var_ks, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{RS}', var_rs, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{BIK}', var_bik, [rfReplaceAll, rfIgnoreCase]);
  for i:=0 to var_lst.Count-1 do
    Result := StringReplace(Result, '{P'+IntToStr(i+1)+'}', var_lst.Strings[i], [rfReplaceAll, rfIgnoreCase]);
end;

procedure TPipeComm.Execute;
var
 Data : array[0..PIPE_BUFFER] of char;
 dwBytesToRead,dwBytesToWrite : DWORD;
 dwWritten,dwRead : DWORD;
 Stream : TStringStream;
 pSD : PSECURITY_DESCRIPTOR;
 sa  : SECURITY_ATTRIBUTES;
 request: string;
begin
  onServiceStart;

  try
    GetMem(pSD,SECURITY_DESCRIPTOR_MIN_LENGTH);
    try
     Win32Check(InitializeSecurityDescriptor(pSD, SECURITY_DESCRIPTOR_REVISION));
     Win32Check(SetSecurityDescriptorDacl(pSD,True,nil,False));
     Sa.nLength := SizeOf(Sa);
     Sa.lpSecurityDescriptor := pSD;
     Sa.bInheritHandle := True;
     hPipe := CreateNamedPipe(PAnsiChar(PIPE_NAME),PIPE_ACCESS_DUPLEX,PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT, 255, PIPE_BUFFER,PIPE_BUFFER,PIPE_TIMEOUT,@Sa);
    finally
     FreeMem(pSD);
    end;

  if hPipe <> INVALID_HANDLE_VALUE then
   begin
    Stream := TStringStream.Create('');
    try
     while (ConnectNamedPipe(hPipe,nil) or (GetLastError = ERROR_PIPE_CONNECTED)) and (not Terminated) do
      try
       ZeroMemory(@Data,SizeOf(Data));
       try
        dwBytesToRead := PIPE_BUFFER;
        if ReadFile(hPipe,Data,dwBytesToRead,dwRead,nil) then
         if (dwRead > 0) then
          begin
           {Check if we should exit}
           if (Data = PIPE_SHUTDOWN) then
            begin
             WriteFile(hPipe,Data,dwRead,dwWritten,nil);
             Break;
            end;

           //some processing here
           request := Data;

           {Processing}
           setResponse(request);
           //Data := response;

           {Prepare response}
           Stream.Size := 0;
           Stream.WriteString(response);
           Stream.Seek(0,soFromBeginning);
           ZeroMemory(@Data,SizeOf(Data));
           Stream.ReadBuffer(Data,Stream.Size);
           //Stream.WriteBuffer(Data,dwRead);
           //Stream.Seek(0,soFromBeginning);

           {Send response}
           dwBytesToWrite := Stream.Size;
           if WriteFile(hPipe,Data,dwBytesToWrite,dwWritten,nil) then
            if dwWritten = dwBytesToWrite then
             begin
              {OK}
             end
            else
             raise Exception.Create('Cannot write to pipe #1')
           else
            raise Exception.Create('Cannot write to pipe #2');
          end
         else
          raise Exception.Create('Cannot read from pipe #1')
        else
         raise Exception.Create('Cannot read from pipe #2')
       except
        on e : Exception do
          //SysErrorlog(Format('Pipe error (%s)',[e.Message]),SEV_EVENTLOG_ERR);
       end;
      finally
       FlushFileBuffers(hPipe);
       DisconnectNamedPipe(hPipe);
      end;
    finally
     Stream.Free;
     CloseHandle(hPipe);
    end;
   end
  else
   ;//HandleError('Unable to create a named pipe');
 except
  on e : Exception do
  //HandleError(E.Message);
 end;

 onServiceDestroy;
end;

constructor TPipeComm.Create(CreateSuspennded: Boolean; const pipename: string);
begin
  inherited Create(CreateSuspennded);
  pipe_name := pipename;
end;


end.

