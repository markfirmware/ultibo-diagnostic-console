program DiagnosticConsoleProgram;
{$mode delphi}{$h+}

uses
 QEMUVersatilePB,PlatformQemuVpb,VersatilePb,
 Classes,Console,GlobalConfig,GlobalConst,GlobalTypes,
 HeapManager,Http,Logging,Platform,Serial,
 Services,StrUtils,SysUtils,Threads,Ultibo,WebStatus,Winsock2;

var
 ScopeWindow,MessagesWindow:TWindowHandle;

procedure Log(S:String);
begin
 LoggingOutput(S);
 ConsoleWindowWriteLn(MessagesWindow,S);
end;

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 LOGGING_INCLUDE_TICKCOUNT:=True;
 SERIAL_REGISTER_LOGGING:=True;
 SerialLoggingDeviceAdd(SerialDeviceGetDefault);
 SERIAL_REGISTER_LOGGING:=False;
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_SERIAL));
end;

const
 HEAP_FLAG_SYSTEM_DIAGNOSTIC=$08000000;

type
 PSystemDiagnosticStorage = ^TSystemDiagnosticStorage;
 TSystemDiagnosticStorage = packed record
  ReservedForHeapManager:array[0..31] of Byte;
  Signature:LongWord;
  MostRecentClockCount:LongWord;
  TotalFree:LongWord;
  TerminatedThreadCount:LongWord;
 end;

 TSystemDiagnostic = record
  Valid:Boolean;
  Storage:PSystemDiagnosticStorage;
  constructor Create(Where:Pointer);
  procedure Update;
 end;

var
 SystemDiagnostic:TSystemDiagnostic;

constructor TSystemDiagnostic.Create(Where:Pointer);
const
 InitializationSignature=$73AF72EC;
begin
 Storage:=PSystemDiagnosticStorage(RequestHeapBlock(Where,1*1024*1024,HEAP_FLAG_SYSTEM_DIAGNOSTIC,CPU_AFFINITY_NONE));
 Valid:=Storage = Where;
 if Valid then
  begin
   if Storage.Signature <> InitializationSignature then
    begin
     Storage.Signature:=InitializationSignature;
     Storage.MostRecentClockCount:=0;
     Storage.TotalFree:=0;
     Storage.TerminatedThreadCount:=0;
    end;
  end;
end;

const
 ThreadIndices=10;
 ThreadStateIndices=10;

function ThreadIndex(Name:String):LongWord;
begin
 if Name = 'TxThread' then
   Result:=1
 else if Name = 'TCP Listener' then
   Result:=2
 else if Name = 'TCP Server' then
   Result:=3
 else
   Result:=0;
end;

procedure TSystemDiagnostic.Update;
var
 HeapStatus:THeapStatus;
 ThreadSnapShot,Current:PThreadSnapShot;
 ThreadStates:array[0..ThreadIndices,0..ThreadStateIndices] of LongWord;
 I,J:Integer;
 procedure EndLine;
 begin
  ConsoleClrEol;
  ConsoleGotoXY(1,ConsoleWhereY + 1);
 end;
 procedure Line(S:String);
 begin
  Write(S);
  EndLine;
 end;
 procedure WriteStates(ThreadIndex:Integer;Name:String);
 var
  J:Integer;
 begin
  Write(Format('   %12s ',[Name]));
  for J:=0 to ThreadStateIndices do
   Write(Format(' %3d',[ThreadStates[ThreadIndex,J]]));
  EndLine;
 end;
begin
 if Valid then
  begin
   ConsoleGotoXY(1,1);
   HeapStatus:=GetHeapStatus;
   Storage.MostRecentClockCount:=ClockGetCount;
   Storage.TotalFree:=HeapStatus.TotalFree;
   Storage.TerminatedThreadCount:=0;
   for I:=0 to ThreadIndices do
    for J:=0 to ThreadStateIndices do
     ThreadStates[I,J]:=0;
   ThreadSnapShot:=ThreadSnapShotCreate;
   Current:=ThreadSnapShot;
   while Assigned(Current) do
    begin
     if Current.State = THREAD_STATE_TERMINATED then
      Inc(Storage.TerminatedThreadCount);
     Inc(ThreadStates[ThreadIndex(Current.Name),Current.State]);
     Current:=Current.Next;
    end;
   ThreadSnapShotDestroy(ThreadSnapShot);
   Line(Format('   Up Time %s',[FileTimeToSysLogDateTime(UpTime)]));
   Line(Format('   THeapstatus.TotalFree %8d Terminated Threads %3d',[Storage.TotalFree,Storage.TerminatedThreadCount]));
   WriteStates(1,'TxThread');
   WriteStates(2,'TCP Listener');
   WriteStates(3,'TCP Server');
  end;
end;

procedure ParseCommandLine;
var
 I:Cardinal;
 Param:String;
 procedure ParseString(Option:String; var Value:String);
 var
  Start:Cardinal;
 begin
  if AnsiStartsStr(Option + '=',Param) then
   begin
    Start:=PosEx('=',Param);
    Value:=MidStr(Param,Start + 1,Length(Param) - Start);
   end;
 end;
begin
 Log(Format('Command Line = <%s>',[GetCommandLine]));
 for I:=0 to ParamCount do
  begin
   Param:=ParamStr(I);
   Log(Format('Param %d = %s',[I,Param]));
  end;
end;

var
 HTTPListener:THTTPListener;

procedure StartHttpServer;
begin
 HTTPListener:=THTTPListener.Create;
 WebStatusRegister(HTTPListener,'','',True);
 HTTPListener.Active:=True;
end;

function GetIpAddress:String;
var
 Winsock2TCPClient:TWinsock2TCPClient;
begin
 Log('Obtaining IP address ...');
 Winsock2TCPClient:=TWinsock2TCPClient.Create;
 Result:=Winsock2TCPClient.LocalAddress;
 while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do
  begin
   Sleep(100);
   Result:=Winsock2TCPClient.LocalAddress;
  end;
 Winsock2TCPClient.Free;
end;

const
 DiagnosticPointer=(64 - 1) * 1024*1024;

procedure Main;
begin
 SystemDiagnostic:=TSystemDiagnostic.Create(Pointer(DiagnosticPointer));
 ScopeWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOP,True);
 ConsoleWindowSetBackColor(ScopeWindow,COLOR_ORANGE);
 MessagesWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOM,False);
 ConsoleWindowSetBackColor(MessagesWindow,COLOR_WHITE);
 ConsoleClrScr;
 StartLogging;
 Sleep(500);
 Log('');
 Log('program start');
 GetIpAddress;
 StartHttpServer;
 Log(Format('TSystemDiagnostic data starts at 0x%x',[DiagnosticPointer + 32]));
 ParseCommandLine;
 Log(Format('Ultibo Release %s %s %s',[ULTIBO_RELEASE_DATE,ULTIBO_RELEASE_NAME,ULTIBO_RELEASE_VERSION]));
 Log('');
 while True do
  begin
   SystemDiagnostic.Update;
   PLongWord(VERSATILEPB_UART2_REGS_BASE)^:=Ord('a');
  end;
end;

begin
 try
  Main;
 except on E:Exception do
  begin
   Log(Format('Exception.Message %s',[E.Message]));
   Sleep(5*1000);
  end;
 end;
 Log('program stop');
end.
