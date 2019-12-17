Program AtolKktService;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, daemonmapperunit, serviceunit, Interfaces,
  SegmentArrayUnit, LoggerUnit
  { add your units here };

begin
  Application.Title:='Daemon application';
  MyLogger := TLogger.Create;
  segments_arr := TSegmentArray.Create;

  Application.Initialize;
  Application.Run;

  MyLogger.Free;
  segments_arr.Free;
end.
