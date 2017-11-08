{
License:
 This work is copyright Professional Software Development / Pierre le Riche. It
 is released under a dual license, and you may choose to use it under either the
 Mozilla Public License 1.1 (MPL 1.1, available from
 http://www.mozilla.org/MPL/MPL-1.1.html)
 or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
 http://www.opensource.org/licenses/lgpl-license.php).

 If you find FastMM useful or you would like to support further development,
 a donation would be much

 appreciated. My banking details are:
   Country: South Africa
   Bank: ABSA Bank Ltd
   Branch: Somerset West
   Branch Code: 334-712
   Account Name: PSD (Distribution)
   Account No.: 4041827693
   Swift Code: ABSAZAJJ

 My PayPal account is:
   bof@psd.co.za


Contact Details:
 My contact details are shown below if you would like to get in touch with me.
 If you use this memory manager I would like to hear from you: please e-mail me
 your comments - good and bad.

 Snailmail:
   PO Box 2514
   Somerset West
   7129
   South Africa

 E-mail:
   plr@psd.co.za

   
Original Implementation Homepage:
 https://github.com/pleriche/FastMM4


Completed Change Log:
 Please see FChange.log file
}

unit FUtil;

interface

{$include FOption.inc}

uses
  FTypeLib; // System

const
  // New line
  CNL: PAnsiChar = #13#10;

  // Hexadecimal characters
  CHexTable: array[0..15] of AnsiChar = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
  );

procedure LockAcquire(AAddress: PByte);
//procedure LockRelease(AAddress: PByte);

// Compare [AAddress] with CompareVal
// If Equal: [AAddress] := NewVal and Result = CompareVal
// If Unequal: Result := [AAddress]
function LockCmpxchg(CompareVal, NewVal: Byte; AAddress: PByte): Byte;
function LockCmpxchgPointer(CompareVal, NewVal: Pointer; AAddress: PPointer): Pointer;

procedure LockDec(AValue: PNativeUInt);
procedure LockInc(AValue: PNativeUInt);
function LockxchgInc(AValue: PNativeUInt): NativeUInt;
function TryLockAcquire(AAddress: PByte): Boolean;
procedure ThreadYield(var SpinCounter: NativeUInt);

function FindFirstSetBit(AValue: UInt32): UInt32;

{$ifdef F4mUseCustomFixedSizeMoveRoutines}
{$ifdef CPU386}
procedure Move12(const ASource; var Dest; ACount: NativeInt);
procedure Move20(const ASource; var Dest; ACount: NativeInt);
procedure Move28(const ASource; var Dest; ACount: NativeInt);
procedure Move36(const ASource; var Dest; ACount: NativeInt);
procedure Move44(const ASource; var Dest; ACount: NativeInt);
procedure Move52(const ASource; var Dest; ACount: NativeInt);
procedure Move60(const ASource; var Dest; ACount: NativeInt);
procedure Move68(const ASource; var Dest; ACount: NativeInt);

procedure MoveX8LP(const ASource; var Dest; ACount: NativeInt);
{$endif}

{$ifdef CPUX64}
procedure Move8(const ASource; var Dest; ACount: NativeInt);
procedure Move24(const ASource; var Dest; ACount: NativeInt);
procedure Move40(const ASource; var Dest; ACount: NativeInt);
procedure Move56(const ASource; var Dest; ACount: NativeInt);
{$endif}

procedure MoveX16LP(const ASource; var Dest; ACount: NativeInt);
{$endif}

// Appends the source text to the destination and returns the new destination position
function AppendToBuffer(const ASource: AnsiChar; const ADestination: PAnsiChar): PAnsiChar; overload;
function AppendToBuffer(const ASource: PAnsiChar; const ASourceCount: UInt32; const ADestination: PAnsiChar): PAnsiChar; overload;

// Converts number to string and appends it to the destination and returns the new destination position
function AppendToBuffer(ANumber: NativeUInt; ADestination: PAnsiChar): PAnsiChar; overload;

// Appends line-feed to the destination and returns the new destination position
function AppendToBufferLn(const ADestination: PAnsiChar): PAnsiChar;

// Writes the module filename to the specified buffer and returns the new destination position.
// If failed, return nil
function AppendToBufferModuleFileName(ADestination: PAnsiChar): PAnsiChar;

function FileClose(AHandle: THandle): Boolean;
function FileCreate(AFileName: PAnsiChar; out Handle: THandle): Boolean;
function FileDelete(AFileName: PAnsiChar): Boolean;
function FileMoveBegin(AHandle: THandle): UInt32;
function FileMoveEnd(AHandle: THandle): UInt32;
function FileOpen(AFileName: PAnsiChar; out Handle: THandle): Boolean;
function FileOpenOrCreate(AFileName: PAnsiChar; out Handle: THandle): Boolean;
function FileRead(AHandle: THandle; var Buffer; ABytesToRead: UInt32): UInt32;
function FileSize(AHandle: THandle): UInt32;
function FileWrite(AHandle: THandle; const ABuffer; ABytesToWrite: UInt32): UInt32;
function FileWriteText(AFileName: PAnsiChar; const ABuffer; ABytesToWrite: UInt32): Boolean;

implementation

uses
  Windows;

procedure LockAcquire(AAddress: PByte);
var
  SpinCounter: NativeUInt;
begin
  SpinCounter := CDefaultSpinCounter;
  while LockCmpxchg(0, 1, AAddress) <> 0 do
    ThreadYield(SpinCounter);
end;

function TryLockAcquire(AAddress: PByte): Boolean;
begin
  Result := LockCmpxchg(0, 1, AAddress) = 0;
end;

(*
procedure LockRelease(AAddress: PByte);
asm
{$ifdef CPU386}
  {On entry:
    eax = AAddress}
  xor ecx, ecx
  lock xchg [eax], cl
{$else}
  {On entry:
    rcx = AAddress}
  .noframe
  xor rax, rax
  lock xchg [rcx], al
{$endif}
end;
*)

// Compare [AAddress] with CompareVal
// If Equal: [AAddress] := NewVal and Result = CompareVal
// If Unequal: Result := [AAddress]
function LockCmpxchg(CompareVal, NewVal: Byte; AAddress: PByte): Byte;
asm
{$ifdef CPU386}
  {On entry:
    al = CompareVal,
    dl = NewVal,
    ecx = AAddress}
  lock cmpxchg [ecx], dl
{$else}
  {On entry:
    cl = CompareVal
    dl = NewVal
    r8 = AAddress}
  .noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

function LockCmpxchgPointer(CompareVal, NewVal: Pointer; AAddress: PPointer): Pointer;
asm
{$ifdef CPU386}
  {On entry:
    eax = CompareVal,
    edx = NewVal,
    ecx = AAddress}
  lock cmpxchg [ecx], edx
{$else}
  {On entry:
    rcx = CompareVal
    rdx = NewVal
    r8 = AAddress}
  .noframe
  mov rax, rcx
  lock cmpxchg [r8], rdx
{$endif}
end;

procedure LockDec(AValue: PNativeUInt);
asm
{$ifdef CPU386}
  {On entry:
    eax = AValue}
  lock sub [eax], 1
{$else}
  {On entry:
    rcx = AValue}
  .noframe
  lock sub [rcx], 1
{$endif}
end;

procedure LockInc(AValue: PNativeUInt);
asm
{$ifdef CPU386}
  {On entry:
    eax = AValue}
  lock add [eax], 1
{$else}
  {On entry:
    rcx = AValue}
  .noframe
  lock add [rcx], 1
{$endif}
end;

function LockxchgInc(AValue: PNativeUInt): NativeUInt;
asm
{$ifdef CPU386}
  {On entry:
    eax = AValue}
  xchg eax, ecx
  mov eax, 1
  lock xadd [ecx], eax
{$else}
  {On entry:
    rcx = AValue}
  .noframe
  mov rax, 1
  lock xadd [rcx], rax
{$endif}
end;

procedure ThreadSpin(ACounter: NativeUInt);
asm
{$ifdef CPU386}
  {On entry:
    eax = ACounter}
  mov ecx, eax
  rep nop
{$else}
  {On entry:
    rcx = ACounter}
  .noframe
  rep nop
{$endif}
end;

procedure ThreadYield(var SpinCounter: NativeUInt);
begin
  if SpinCounter <> 0 then
  begin
    ThreadSpin(SpinCounter);
    SpinCounter := 0;
  end
  else if not SwitchToThread then
    Sleep(1);

{$ifdef F4mStatisticInfo}
  LockInc(@StatisticCalls.ThreadYieldCount);
{$endif}
end;

// Gets the first set bit, returning the bit index
function FindFirstSetBit(AValue: UInt32): UInt32;
asm
{$ifdef CPU386}
  {On entry:
    eax = AValue}
  bsf eax, eax
{$else}
  {On entry:
    ecx = AValue}
  .noframe
  mov rax, rcx
  bsf eax, eax
{$endif}
end;

{$ifdef F4mUseCustomFixedSizeMoveRoutines}
// Fixed size move operations ignore the size parameter. All moves are assumed to be non-overlapping.
{$ifdef CPU386}
procedure Move12(const ASource; var Dest; ACount: NativeInt);
asm
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov eax, [eax + 8]
  mov [edx + 4], ecx
  mov [edx + 8], eax
end;

procedure Move20(const ASource; var Dest; ACount: NativeInt);
asm
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov [edx + 4], ecx
  mov ecx, [eax + 8]
  mov [edx + 8], ecx
  mov ecx, [eax + 12]
  mov eax, [eax + 16]
  mov [edx + 12], ecx
  mov [edx + 16], eax
end;

procedure Move28(const ASource; var Dest; ACount: NativeInt);
asm
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov [edx + 4], ecx
  mov ecx, [eax + 8]
  mov [edx + 8], ecx
  mov ecx, [eax + 12]
  mov [edx + 12], ecx
  mov ecx, [eax + 16]
  mov [edx + 16], ecx
  mov ecx, [eax + 20]
  mov eax, [eax + 24]
  mov [edx + 20], ecx
  mov [edx + 24], eax
end;

procedure Move36(const ASource; var Dest; ACount: NativeInt);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  mov ecx, [eax + 32]
  mov [edx + 32], ecx
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

procedure Move44(const ASource; var Dest; ACount: NativeInt);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  mov ecx, [eax + 40]
  mov [edx + 40], ecx
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

procedure Move52(const ASource; var Dest; ACount: NativeInt);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  mov ecx, [eax + 48]
  mov [edx + 48], ecx
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

procedure Move60(const ASource; var Dest; ACount: NativeInt);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  mov ecx, [eax + 56]
  mov [edx + 56], ecx
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

procedure Move68(const ASource; var Dest; ACount: NativeInt);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  fild qword ptr [eax + 56]
  mov ecx, [eax + 64]
  mov [edx + 64], ecx
  fistp qword ptr [edx + 56]
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

// Variable size move procedure: Rounds ACount up to the next multiple of 8 less
// SizeOf(Pointer). Important note: Always moves at least 8 - SizeOf(Pointer)
// bytes (the minimum small block size with 8 byte alignment), irrespective of ACount.
procedure MoveX8LP(const ASource; var Dest; ACount: NativeInt);
asm
  // Make the counter negative based: The last 4 bytes are moved separately
  sub ecx, 4
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  // Move an 8 byte block
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  // Are there another 8 bytes to move?
  add ecx, 8
  js @MoveLoop
  // Do the last 4 bytes
  mov eax, [eax + ecx]
  mov [edx + ecx], eax
end;
{$endif}

{$ifdef CPUX64}
procedure Move8(const ASource; var Dest; ACount: NativeInt);
asm
  mov rax, [rcx]
  mov [rdx], rax
end;

procedure Move24(const ASource; var Dest; ACount: NativeInt);
asm
  movdqa xmm0, [rcx]
  mov r8, [rcx + 16]
  movdqa [rdx], xmm0
  mov [rdx + 16], r8
end;

procedure Move40(const ASource; var Dest; ACount: NativeInt);
asm
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  mov r8, [rcx + 32]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  mov [rdx + 32], r8
end;

procedure Move56(const ASource; var Dest; ACount: NativeInt);
asm
  movdqa xmm0, [rcx]
  movdqa xmm1, [rcx + 16]
  movdqa xmm2, [rcx + 32]
  mov r8, [rcx + 48]
  movdqa [rdx], xmm0
  movdqa [rdx + 16], xmm1
  movdqa [rdx + 32], xmm2
  mov [rdx + 48], r8
end;
{$endif}

// Variable size move procedure: Rounds ACount up to the next multiple of 16 less
// SizeOf(Pointer). Important note: Always moves at least 16 - SizeOf(Pointer)
// bytes (the minimum small block size with 16 byte alignment), irrespective of ACount.
procedure MoveX16LP(const ASource; var Dest; ACount: NativeInt);
asm
{$ifdef CPU386}
  // Make the counter negative based: The last 12 bytes are moved separately
  sub ecx, 12
  add eax, ecx
  add edx, ecx
  neg ecx
  jns @MoveLast12
@MoveLoop:
  // Move a 16 byte block
  fild qword ptr [eax + ecx]
  fild qword ptr [eax + ecx + 8]
  fistp qword ptr [edx + ecx + 8]
  fistp qword ptr [edx + ecx]

  // Are there another 16 bytes to move?
  add ecx, 16
  js @MoveLoop
@MoveLast12:
  // Do the last 12 bytes
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  mov eax, [eax + ecx + 8]
  mov [edx + ecx + 8], eax
{$else}
  // Make the counter negative based: The last 8 bytes are moved separately
  sub r8, 8
  add rcx, r8
  add rdx, r8
  neg r8
  jns @MoveLast12
@MoveLoop:
  // Move a 16 byte block
  movdqa xmm0, [rcx + r8]
  movdqa [rdx + r8], xmm0
  // Are there another 16 bytes to move?
  add r8, 16
  js @MoveLoop
@MoveLast12:
  // Do the last 8 bytes
  mov r9, [rcx + r8]
  mov [rdx + r8], r9
{$endif}
end;
{$endif}


// Appends the source text to the destination and returns the new destination position
function AppendToBuffer(const ASource: AnsiChar; const ADestination: PAnsiChar): PAnsiChar; overload;
begin
  ADestination^ := ASource;
  Result := Pointer(NativeUInt(ADestination) + 1);
end;

function AppendToBuffer(const ASource: PAnsiChar; const ASourceCount: UInt32; const ADestination: PAnsiChar): PAnsiChar; overload;
begin
  System.Move(ASource^, ADestination^, ASourceCount);
  Result := Pointer(NativeUInt(ADestination) + ASourceCount);
end;

function AppendToBuffer(ANumber: NativeUInt; ADestination: PAnsiChar): PAnsiChar;
const
  CMaxDigits = 32;
var
  LBuffer: array[0..CMaxDigits - 1] of AnsiChar;
  LDigit: NativeUInt;
  LCount: Byte;
begin
  LCount := 0;
  repeat
    LDigit := ANumber;
    ANumber := ANumber div 10;
    LDigit := LDigit - (ANumber * 10);
    Inc(LCount);
    LBuffer[CMaxDigits - LCount] := AnsiChar(Ord('0') + LDigit);
  until ANumber = 0;

  System.Move(LBuffer[CMaxDigits - LCount], ADestination^, LCount);
  Result := ADestination + LCount;
end;

function AppendToBufferLn(const ADestination: PAnsiChar): PAnsiChar;
begin
  Result := AppendToBuffer(CNL, Length(CNL), ADestination);
end;

function AppendToBufferModuleFileName(ADestination: PAnsiChar): PAnsiChar;
var
  Len: UInt32;
begin
  // Get the module name
  Len := GetModuleFileNameA(0, ADestination, 1023);
  if Len > 0 then
  begin
    Result := ADestination + Len;
    while Result^ <> '.' do
      Dec(Result);
  end
  else
    Result := nil;
end;

function FileClose(AHandle: THandle): Boolean;
begin
  Result := CloseHandle(AHandle);
end;

function FileCreate(AFileName: PAnsiChar; out Handle: THandle): Boolean;
begin
  Handle := CreateFileA(AFileName, GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if not Result then
    Handle := 0;
end;

function FileDelete(AFileName: PAnsiChar): Boolean;
begin
  Result := DeleteFileA(AFileName);
end;

function FileMoveBegin(AHandle: THandle): UInt32;
begin
  Result := SetFilePointer(AHandle, 0, nil, FILE_BEGIN);
end;

function FileMoveEnd(AHandle: THandle): UInt32;
begin
  Result := SetFilePointer(AHandle, 0, nil, FILE_END);
end;

function FileOpen(AFileName: PAnsiChar; out Handle: THandle): Boolean;
begin
  Handle := CreateFileA(AFileName, GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if not Result then
    Handle := 0;
end;

function FileOpenOrCreate(AFileName: PAnsiChar; out Handle: THandle): Boolean;
begin
  Handle := CreateFileA(AFileName, GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if not Result then
    Handle := 0;
end;

function FileRead(AHandle: THandle; var Buffer; ABytesToRead: UInt32): UInt32;
begin
  if not ReadFile(AHandle, Buffer, ABytesToRead, DWORD(Result), nil) then
    Result := 0;
end;

function FileSize(AHandle: THandle): UInt32;
var
  Current: UInt32;
begin
  Current := SetFilePointer(AHandle, 0, nil, FILE_CURRENT);
  SetFilePointer(AHandle, 0, nil, FILE_BEGIN);
  Result := SetFilePointer(AHandle, 0, nil, FILE_END);
  SetFilePointer(AHandle, Current, nil, FILE_BEGIN);
end;

function FileWrite(AHandle: THandle; const ABuffer; ABytesToWrite: UInt32): UInt32;
begin
  Result := 0;
  WriteFile(AHandle, ABuffer, ABytesToWrite, Result, nil);
end;

function FileWriteText(AFileName: PAnsiChar; const ABuffer; ABytesToWrite: UInt32): Boolean;
var
  Handle: THandle;
begin
  if FileOpenOrCreate(AFileName, Handle) then
  begin
    Result := FileWrite(Handle, ABuffer, ABytesToWrite) = ABytesToWrite;
      
    FileClose(Handle);
  end
  else
    Result := False;
end;

end.
