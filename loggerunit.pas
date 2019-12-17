unit LoggerUnit;
//------------------------------------------------------------------------------
// Модуль журналирования
//------------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type

  { TLogger }

  TLogger = class(TObject)
  private
    cs: TCriticalSection;
    local_log: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LogText(const st:string; const force_log:boolean = false);
    procedure LogError(const err_code:integer; const log:string);
    procedure startLog;
    procedure pauseLog;
  end;

var MyLogger: TLogger;

implementation

{ TLogger }

constructor TLogger.Create;
begin
  inherited Create;
  local_log := false;
  cs := TCriticalSection.Create;
end;

destructor TLogger.Destroy;
begin
  cs.Free;
  inherited Destroy;
end;

procedure TLogger.LogText(const st: string; const force_log: boolean);
var f: textfile;
    tmp,fnm: string;
begin
  if local_log or force_log
    then
      begin
        cs.Enter;
        try
          fnm := ExtractFilePath(ParamStr(0))+'log.txt';
          AssignFile(f,fnm);
          if FileExists(fnm)
            then Append(f)
            else Rewrite(f);
          try
            tmp := FormatDateTime('yyyy.mm.dd hh":"nn":"ss',now)+#9+st;
            Writeln(f,tmp);

          finally
            CloseFile(f);
          end;
        finally
          cs.Leave;
        end;
      end;
end;

procedure TLogger.LogError(const err_code: integer; const log: string);
begin
  LogText('Код ошибки: '+IntToStr(err_code), false);
  LogText(log, false);
end;

procedure TLogger.startLog;
begin
  local_log := true;
end;

procedure TLogger.pauseLog;
begin
  local_log := false;
end;

initialization

finalization

end.

