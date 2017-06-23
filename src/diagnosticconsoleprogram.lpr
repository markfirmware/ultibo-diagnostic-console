program DiagnosticConsoleProgram;
{$mode delphi}{$h+}

uses
 QEMUVersatilePB,PlatformQemuVpb,VersatilePb,
 DiagnosticConsoleUnit,
 Classes,Console,GlobalConfig,GlobalConst,GlobalTypes,
 Http,Platform,StrUtils,SysUtils,Threads,Ultibo,WebStatus,Winsock2;

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
 DiagnosticConsoleLog(Format('Command Line = <%s>',[GetCommandLine]));
 for I:=0 to ParamCount do
  begin
   Param:=ParamStr(I);
   DiagnosticConsoleLog(Format('Param %d = %s',[I,Param]));
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
 DiagnosticConsoleLog('Obtaining IP address ...');
 Winsock2TCPClient:=TWinsock2TCPClient.Create;
 Result:=Winsock2TCPClient.LocalAddress;
 while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do
  begin
   Sleep(100);
   Result:=Winsock2TCPClient.LocalAddress;
  end;
 Winsock2TCPClient.Free;
end;

procedure Main;
begin
 Sleep(500);
 DiagnosticConsoleLog('');
 DiagnosticConsoleLog('program start');
 GetIpAddress;
 StartHttpServer;
 ParseCommandLine;
 while True do
  Sleep(100);
end;

begin
 try
  Main;
 except on E:Exception do
  begin
   DiagnosticConsoleLog(Format('Exception.Message %s',[E.Message]));
   Sleep(5*1000);
  end;
 end;
 DiagnosticConsoleLog('program stop');
end.
