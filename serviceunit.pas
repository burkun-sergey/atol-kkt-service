unit serviceunit;
//------------------------------------------------------------------------------
// Сервис
//------------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, DaemonApp, PipeCommThreadUnit, syncobjs,
  IniFiles, AtolKKTUnit, LoggerUnit;

const
  APP_NAME = 'AtolKKTService';

type

  { TAtolKKTService }

  TAtolKKTService = class(TDaemon)
    ApplicationProperties1: TApplicationProperties;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure DataModuleContinue(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleExecute(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    local_log: boolean;
    //event_log: boolean;

    cs, cs_evt: TCriticalSection;

    atol_cash_register_pipe: string;

    PipeComm: TPipeComm;

    procedure InitPipe;
    procedure ClosePipe;
  public

  end;

var
  AtolKKTService: TAtolKKTService;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TAtolKKTService)
end;

{$R *.lfm}

{ TAtolKKTService }

procedure TAtolKKTService.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
var ini: TIniFile;
begin
  MyLogger.LogText('Служба запускается');

  ini := TIniFile.Create(ExtractFilePath(ParamStr(0))+'ini.ini');

  local_log := ini.ReadBool('Logs','LocalLog',True);
  //event_log := ini.ReadBool('Logs','EventLog',True);

  atol_cash_register_pipe := ini.ReadString('Options','AtolCashRegisterPipe','');

  ini.Free;

  MyLogger.LogText('init pipe');

  // запускаем поток чтения PIPE
  InitPipe;
  MyLogger.LogText('pipe init is done');

  MyLogger.LogText('Служба запущена');
end;

procedure TAtolKKTService.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
begin
  ClosePipe;
  MyLogger.LogText('pipe is closed');
end;

procedure TAtolKKTService.ApplicationProperties1Exception(Sender: TObject; E: Exception);
var err_code: integer;
begin
  if (E is EATOLException) then
    begin
      err_code := (E as EATOLException).ErrorCode;
    end
  else err_code := 0;

  MyLogger.LogError(err_code, e.Message);
end;

procedure TAtolKKTService.DataModuleContinue(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  MyLogger.LogText('Служба возобновлена!');
end;

procedure TAtolKKTService.DataModuleCreate(Sender: TObject);
begin
  cs := TCriticalSection.Create;
  cs_evt := TCriticalSection.Create;
end;

procedure TAtolKKTService.DataModuleDestroy(Sender: TObject);
begin
  cs.Free;
  cs_evt.Free;
end;

procedure TAtolKKTService.DataModuleExecute(Sender: TCustomDaemon);
begin
  MyLogger.LogText('Служба запущена!');

  ReportStatus;
end;

procedure TAtolKKTService.InitPipe;
begin
  PipeComm := TPipeComm.Create(False, atol_cash_register_pipe);
end;

procedure TAtolKKTService.ClosePipe;
begin
  PipeComm.Terminate;
end;


initialization
  RegisterDaemon;
end.

