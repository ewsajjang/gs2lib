{
  author: see - https://stackoverflow.com/a/8820298/1174572
}
(* Makes AllocateHwnd safe to call from threads. For example this makes TTimer
   safe to use from threads.  Include this unit as early as possible in your
   .dpr file.  It must come after any memory manager, but it must be included
   immediately after that before any included unit has an opportunity to call
   Classes.AllocateHwnd. *)
unit MakeAllocateHwndThreadsafe;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages
  ;

var
  //DSiAllocateHwnd lock
  GDSiWndHandlerCritSect: TRTLCriticalSection;

function ThreadSafeAllocateHWnd(wndProcMethod: TWndMethod): HWND;
procedure ThreadSafeDeallocateHWnd(wnd: HWND);

implementation

const //DSiAllocateHwnd window extra data offsets
  GWL_METHODCODE = SizeOf(pointer) * 0;
  GWL_METHODDATA = SizeOf(pointer) * 1;

  //DSiAllocateHwnd hidden window (and window class) name
  CDSiHiddenWindowName = 'DSiUtilWindow';

var
  //DSiAllocateHwnd lock
  //GDSiWndHandlerCritSect: TRTLCriticalSection;
  //Count of registered windows in this instance
  GDSiWndHandlerCount: integer;

//Class message dispatcher for the DSiUtilWindow class. Fetches instance's WndProc from
//the window extra data and calls it.
function DSiClassWndProc(Window: HWND; Message, WParam, LParam: longint): longint; stdcall;
var
  instanceWndProc: TMethod;
  msg            : TMessage;
begin
  {$IFDEF CPUX64}
  instanceWndProc.Code := pointer(GetWindowLongPtr(Window, GWL_METHODCODE));
  instanceWndProc.Data := pointer(GetWindowLongPtr(Window, GWL_METHODDATA));
  {$ELSE}
  instanceWndProc.Code := pointer(GetWindowLong(Window, GWL_METHODCODE));
  instanceWndProc.Data := pointer(GetWindowLong(Window, GWL_METHODDATA));
  {$ENDIF ~CPUX64}
  if Assigned(TWndMethod(instanceWndProc)) then
  begin
    msg.msg := Message;
    msg.wParam := WParam;
    msg.lParam := LParam;
    msg.Result := 0;
    TWndMethod(instanceWndProc)(msg);
    Result := msg.Result
  end
  else
    Result := DefWindowProc(Window, Message, WParam,LParam);
end; { DSiClassWndProc }

//Thread-safe AllocateHwnd.
//  @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
//                 TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
//  @since   2007-05-30
function DSiAllocateHWnd(wndProcMethod: TWndMethod): HWND;
var
  alreadyRegistered: boolean;
  tempClass        : TWndClass;
  utilWindowClass  : TWndClass;
begin
  //Result := 0;
  FillChar(utilWindowClass, SizeOf(utilWindowClass), 0);
  EnterCriticalSection(GDSiWndHandlerCritSect);
  try
    alreadyRegistered := GetClassInfo(HInstance, CDSiHiddenWindowName, tempClass);
    if (not alreadyRegistered) or (tempClass.lpfnWndProc <> @DSiClassWndProc) then begin
      if alreadyRegistered then
        Winapi.Windows.UnregisterClass(CDSiHiddenWindowName, HInstance);
      utilWindowClass.lpszClassName := CDSiHiddenWindowName;
      utilWindowClass.hInstance := HInstance;
      utilWindowClass.lpfnWndProc := @DSiClassWndProc;
      utilWindowClass.cbWndExtra := SizeOf(TMethod);
      if Winapi.Windows.RegisterClass(utilWindowClass) = 0 then
        raise Exception.CreateFmt('Unable to register DSiWin32 hidden window class. %s',
          [SysErrorMessage(GetLastError)]);
    end;
    Result := CreateWindowEx(WS_EX_TOOLWINDOW, CDSiHiddenWindowName, '', WS_POPUP,
      0, 0, 0, 0, 0, 0, HInstance, nil);
    if Result = 0 then
      raise Exception.CreateFmt('Unable to create DSiWin32 hidden window. %s',
              [SysErrorMessage(GetLastError)]);
    {$IFDEF CPUX64}
    SetWindowLongPtr(Result, GWL_METHODDATA, NativeInt(TMethod(wndProcMethod).Data));
    SetWindowLongPtr(Result, GWL_METHODCODE, NativeInt(TMethod(wndProcMethod).Code));
    {$ELSE}
    SetWindowLong(Result, GWL_METHODDATA, cardinal(TMethod(wndProcMethod).Data));
    SetWindowLong(Result, GWL_METHODCODE, cardinal(TMethod(wndProcMethod).Code));
    {$ENDIF ~CPUX64}
    Inc(GDSiWndHandlerCount);
  finally LeaveCriticalSection(GDSiWndHandlerCritSect); end;
end; { DSiAllocateHWnd }

//Thread-safe DeallocateHwnd.
//  @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
//                 TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
//  @since   2007-05-30
procedure DSiDeallocateHWnd(wnd: HWND);
begin
  if wnd = 0 then
    Exit;
  DestroyWindow(wnd);
  EnterCriticalSection(GDSiWndHandlerCritSect);
  try
    Dec(GDSiWndHandlerCount);
    if GDSiWndHandlerCount <= 0 then
      Winapi.Windows.UnregisterClass(CDSiHiddenWindowName, HInstance);
  finally LeaveCriticalSection(GDSiWndHandlerCritSect); end;
end; { DSiDeallocateHWnd }

procedure PatchCode(Address: Pointer; const NewCode; Size: Integer);
var
  OldProtect: DWORD;
begin
  if VirtualProtect(Address, Size, PAGE_EXECUTE_READWRITE, OldProtect) then begin
    Move(NewCode, Address^, Size);
    FlushInstructionCache(GetCurrentProcess, Address, Size);
    VirtualProtect(Address, Size, OldProtect, @OldProtect);
  end;
end;

type
  PInstruction = ^TInstruction;
  TInstruction = packed record
    Opcode: Byte;
    Offset: Integer;
  end;

procedure RedirectProcedure(OldAddress, NewAddress: Pointer);
var
  NewCode: TInstruction;
begin
  NewCode.Opcode := $E9;//jump relative
  NewCode.Offset := NativeInt(NewAddress)-NativeInt(OldAddress)-SizeOf(NewCode);
  PatchCode(OldAddress, NewCode, SizeOf(NewCode));
end;

function ThreadSafeAllocateHWnd(wndProcMethod: TWndMethod): HWND;
begin
  Result := DSiAllocateHWnd(wndProcMethod);
end;

procedure ThreadSafeDeallocateHWnd(wnd: HWND);
begin
  DSiDeallocateHWnd(Wnd);
end;

initialization
  InitializeCriticalSection(GDSiWndHandlerCritSect);
//  RedirectProcedure(@AllocateHWnd, @DSiAllocateHWnd);
//  RedirectProcedure(@DeallocateHWnd, @DSiDeallocateHWnd);

finalization
  DeleteCriticalSection(GDSiWndHandlerCritSect);

end.
