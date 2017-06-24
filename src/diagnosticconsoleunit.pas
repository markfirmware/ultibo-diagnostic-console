unit DiagnosticConsoleUnit;
{$mode delphi}{$h+}

interface

procedure DiagnosticConsoleLog(const S: String);

implementation

uses
 QEMUVersatilePB,VersatilePb,
 Classes,Console,GlobalConfig,GlobalConst,GlobalTypes,
 HeapManager,Logging,Platform,Serial,
 Services,SysUtils,Threads,Ultibo;

var
 ScopeWindow, MessagesWindow: TWindowHandle;

procedure DiagnosticConsoleLog(const S: String);
begin
  LoggingOutput(S);
  ConsoleWindowWriteLn(MessagesWindow,S);
end;

procedure StartLogging;
begin
  LOGGING_INCLUDE_COUNTER := False;
  LOGGING_INCLUDE_TICKCOUNT := True;
  SERIAL_REGISTER_LOGGING := True;
  SerialLoggingDeviceAdd(SerialDeviceGetDefault);
  SERIAL_REGISTER_LOGGING := False;
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
 ThreadIndices = 10;
 ThreadStateIndices = 10;

function ThreadIndex(Name:String):LongWord;
begin
  if Name = 'TCP Listener' then
    Result:=1
  else if Name = 'TCP Server' then
    Result:=2
  else
    Result:=0;
end;

type
  TThreadStates = array[0..ThreadIndices, 0..ThreadStateIndices] of LongWord;

procedure TSystemDiagnostic.Update;

  procedure EndLine;
  begin
    ConsoleClrEol;
    ConsoleGotoXY(1, ConsoleWhereY + 1);
  end;

  procedure Line(S: String);
  begin
    Write(S);
    EndLine;
  end;

  procedure WriteStates(var ThreadStates: TThreadStates; ThreadIndex:Integer; Name:String);
  var
    J: Integer;
  begin
    Write(Format('   %12s ', [Name]));
    for J := 0 to ThreadStateIndices do
      Write(Format(' %3d', [ThreadStates[ThreadIndex, J]]));
    EndLine;
  end;

var
  HeapStatus:THeapStatus;
  ThreadSnapShot, Current: PThreadSnapShot;
  ThreadStates: TThreadStates;
  AgeInSeconds:Integer;
begin
  if not Valid then
    Exit;
  ConsoleGotoXY(1,1);
  HeapStatus:=GetHeapStatus;
  Storage.MostRecentClockCount:=ClockGetCount;
  Storage.TotalFree:=HeapStatus.TotalFree;
  Storage.TerminatedThreadCount:=0;
  FillChar(ThreadStates, SizeOf(ThreadStates), 0);
  ThreadSnapShot := ThreadSnapShotCreate;
  try
    Current := ThreadSnapShot;
    while Current <> nil do
    begin
      if Current.State = THREAD_STATE_TERMINATED then
        Inc(Storage.TerminatedThreadCount);
      Inc(ThreadStates[ThreadIndex(Current.Name), Current.State]);
      Current := Current.Next;
    end;
    Line(Format('   Up Time %s Clock %6.3f', [FileTimeToSysLogDateTime(UpTime), ClockGetTotal / (1000 * 1000)]));
    Line(Format('   THeapstatus.TotalFree %8d Total Terminated Threads %3d', [Storage.TotalFree,Storage.TerminatedThreadCount]));
    WriteStates(ThreadStates, 1, 'TCP Listener');
    WriteStates(ThreadStates, 2, 'TCP Server');
    Current := ThreadSnapShot;
    while Current <> nil do
    begin
      AgeInSeconds:=Round((ClockGetTime - Current.CreateTime) / (10 * 1000 * 1000));
      if (Current.Name = 'TCP Server') and (AgeInSeconds >= 35) then
        WriteLn(Format('TCP Server Handle %8.8x Age %3d Seconds Current State %d',[Current.Handle,AgeinSeconds,Current.State]));
      Current := Current.Next;
    end;
  finally
    ThreadSnapShotDestroy(ThreadSnapShot);
  end;
end;

procedure DiagnosticConsoleUpdate;
begin
  SystemDiagnostic.Update;
end;

const
 DiagnosticPointer=(64 - 1) * 1024*1024;

var
 IdleThread:TThreadHandle;

function IdleThreadProcedure(Parameter: Pointer): PtrInt;
begin
 Result:=0;
 ThreadSetName(GetCurrentThreadId, 'IdleThread');
 while True do
  begin
   PLongWord(VERSATILEPB_UART1_REGS_BASE)^ := Ord('a');
   Sleep(1*1000);
  end;
end;

var
 ScopeThread: TThreadHandle;

function ScopeThreadProcedure(Parameter: Pointer): PtrInt;
begin
  Result:=0;
  ThreadSetName(GetCurrentThreadId, 'ScopeThread');
  while True do
  begin
    SystemDiagnostic.Update;
    Sleep(200);
  end;
end;

procedure InitDiagnosticConsole;
begin
  SystemDiagnostic := TSystemDiagnostic.Create(Pointer(DiagnosticPointer));
  IdleThread := BeginThread(@IdleThreadProcedure,nil,IdleThread,THREAD_STACK_DEFAULT_SIZE);
  ScopeWindow := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_TOP, True);
  ConsoleWindowSetBackColor(ScopeWindow, COLOR_ORANGE);
  MessagesWindow := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOM, False);
  ConsoleWindowSetBackColor(MessagesWindow, COLOR_WHITE);
  ConsoleClrScr;
  ScopeThread := BeginThread(@ScopeThreadProcedure,nil,ScopeThread,THREAD_STACK_DEFAULT_SIZE);
  StartLogging;
  DiagnosticConsoleLog(Format('Ultibo Release %s %s %s', [ULTIBO_RELEASE_DATE, ULTIBO_RELEASE_NAME, ULTIBO_RELEASE_VERSION]));
  DiagnosticConsoleLog(Format('TSystemDiagnostic data starts at 0x%x', [DiagnosticPointer + 32]));
  DiagnosticConsoleLog('');
end;

initialization
 InitDiagnosticConsole;

end.
