{
License:
 This work is copyright Professional Software Development / An Pham. It
 is released under a dual license, and you may choose to use it under either the
 Mozilla Public License 1.1 (MPL 1.1, available from
 http://www.mozilla.org/MPL/MPL-1.1.html) or the GNU Lesser General Public
 License 2.1 (LGPL 2.1, available from
 http://www.opensource.org/licenses/lgpl-license.php).


Completed Change Log:
 Please see FChange.log file
}

unit FDebug;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil; // System

function IsMemoryUsed(APool: PThreadPool; const APointer: Pointer): Boolean;
function MarkMemoryUsed(APool: PThreadPool; const APointer: Pointer): Boolean;
function UnmarkMemoryUsed(APool: PThreadPool; const APointer: Pointer): Boolean;

procedure WriteError(const APointer: Pointer; const AProc: PAnsiChar);
procedure WriteTrace(const APointer: Pointer; const AProc: PAnsiChar);

procedure WriteGetMemMedium(APool: PThreadPool; const APointer: Pointer;
  const ALBinGroupMasked, ALBinNumber, ALBlockSize, ASize: NativeUInt);
procedure WriteGetMemErrorMedium(APool: PThreadPool; const APointer: Pointer;
  const ALBinGroupMasked, ALBinNumber, ALBlockSize, ASize: NativeUInt);
procedure WriteFreeMemErrorMedium(APool: PThreadPool; const APointer: Pointer;
  const ASizeAndFlags: NativeUInt);
procedure WriteReallocMemErrorMedium(APool: PThreadPool; const APointer: Pointer;
  const ASizeAndFlags, ASize: NativeUInt);

procedure WriteGetMemSmall(APool: PThreadPool; const APointer: Pointer;
  const ABlockPool: PSmallBlockPoolHeader; ABlockType: PSmallBlockType; const ASize: NativeUInt);
procedure WriteGetMemErrorSmall(APool: PThreadPool; const APointer: Pointer;
  const ABlockPool: PSmallBlockPoolHeader; ABlockType: PSmallBlockType;
  const ASize: NativeUInt);
procedure WriteFreeMemErrorSmall(APool: PThreadPool; const APointer: Pointer;
  const ABlockPool: PSmallBlockPoolHeader; ABlockType: PSmallBlockType);
procedure WriteReallocMemErrorSmall(APool: PThreadPool; const APointer: Pointer;
  const ABlockPool: PSmallBlockPoolHeader; const ABlockType: PSmallBlockType;
  const ALOldAvailableSize, ASize: NativeUInt);

implementation

uses
  FVirtual;
  
type
  TLogLine = array[0..1023] of AnsiChar;

const
  CMaximumMemoryUsedSlot = 100000;
  CMaximumMemoryUsedPrim = 99991;

  EFileName: PAnsiChar = 'DebugFastMM4me.log';
  GFileName: PAnsiChar = 'DebugFastMM4mg.log';

var
  LogSequence: NativeUInt;
  LogEFileHandle: THandle;
  LogGFileHandle: THandle;

  AllocedMemc: Int32;
  AllocedMems: PPointerArray;

procedure FinalizeDebug;
begin
  if AllocedMems <> nil then
  begin
    OSFree(AllocedMems);
    AllocedMems := nil;
    AllocedMemc := 0;
  end;

  if LogGFileHandle <> 0 then
  begin
    FileClose(LogGFileHandle);
    LogGFileHandle := 0;
  end;

  if LogEFileHandle <> 0 then
  begin
    FileClose(LogEFileHandle);
    LogEFileHandle := 0;
  end;
end;

procedure InitializeDebug;
var
  Ptr: PAnsiChar;
  DebugFastMM4mFileName: array[0..1023] of AnsiChar;
begin
  Ptr := AppendToBufferModuleFileName(@DebugFastMM4mFileName[0]);
  if Ptr <> nil then
  begin
    Ptr := AppendToBuffer(EFileName, Length(EFileName), Ptr);
    AppendToBuffer(#0, Ptr);

    FileDelete(@DebugFastMM4mFileName[0]);
    FileOpenOrCreate(@DebugFastMM4mFileName[0], LogEFileHandle);
  end;

  Ptr := AppendToBufferModuleFileName(@DebugFastMM4mFileName[0]);
  if Ptr <> nil then
  begin
    Ptr := AppendToBuffer(GFileName, Length(GFileName), Ptr);
    AppendToBuffer(#0, Ptr);

    FileDelete(@DebugFastMM4mFileName[0]);
    FileOpenOrCreate(@DebugFastMM4mFileName[0], LogGFileHandle);
  end;

  if AllocedMems = nil then
  begin
    AllocedMemc := 0;
    AllocedMems := OSAlloc(CMaximumMemoryUsedSlot * SizeOf(Pointer));
  end;
end;

function IsMemoryUsed(APool: PThreadPool; const APointer: Pointer): Boolean;
var
  I: UInt32;
  N: Int32;
begin
  N := AllocedMemc;
  I := NativeUInt(APointer) mod CMaximumMemoryUsedPrim;
  repeat
    if AllocedMems[I] = APointer then
    begin
      Result := True;
      Exit;
    end;

    Inc(I);
    if I >= CMaximumMemoryUsedSlot then
      I := 0;

    Dec(N);
  until N <= 0;

  Result := False;
end;

function MarkMemoryUsed(APool: PThreadPool; const APointer: Pointer): Boolean;
var
  I: UInt32;
  N, C, M: Int32;
begin
  C := 1;
  M := Maxint;
  N := AllocedMemc;
  I := NativeUInt(APointer) mod CMaximumMemoryUsedPrim;
  repeat
    // Keep track number of collisions
    if AllocedMems[I] <> nil then
      Inc(C);

    // Mark the first available slot
    if (M = Maxint) and (AllocedMems[I] = nil) then
      M := I;

    if (N >= 0) and (AllocedMems[I] = APointer) then
    begin
      Result := False;
      Exit;
    end;

    Inc(I);
    if I >= CMaximumMemoryUsedSlot then
      I := 0;

    Dec(N);
  until (N <= 0) and (M <> Maxint);

  AllocedMems[M] := APointer;
  if AllocedMemc < C then
    AllocedMemc := C;

  Result := True;
end;

function UnmarkMemoryUsed(APool: PThreadPool; const APointer: Pointer): Boolean;
var
  I: UInt32;
  N: Int32;
begin
  N := AllocedMemc;
  I := NativeUInt(APointer) mod CMaximumMemoryUsedPrim;
  repeat
    if AllocedMems[I] = APointer then
    begin
      AllocedMems[I] := nil;
      Result := True;
      Exit;
    end;

    Inc(I);
    if I >= CMaximumMemoryUsedSlot then
      I := 0;

    Dec(N);
  until N <= 0;

  Result := False;
end;

procedure WriteError(const APointer: Pointer; const AProc: PAnsiChar);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogEFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' ', Ptr);
  Ptr := AppendToBuffer(AProc, Length(AProc), Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogEFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteTrace(const APointer: Pointer; const AProc: PAnsiChar);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogGFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' ', Ptr);
  Ptr := AppendToBuffer(AProc, Length(AProc), Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogGFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteGetMemMedium(APool: PThreadPool; const APointer: Pointer;
  const ALBinGroupMasked, ALBinNumber, ALBlockSize, ASize: NativeUInt);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogGFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' GetMem M:', Length(' GetMem M:'), Ptr);
  Ptr := AppendToBuffer(' pool: ', Length(' pool: '), Ptr);
  Ptr := AppendToBuffer(APool.Index shr CMediumSlotIndexShift, Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(' mask: ', Length(' mask: '), Ptr);
  Ptr := AppendToBuffer(ALBinGroupMasked, Ptr);
  Ptr := AppendToBuffer(' bin: ', Length(' bin: '), Ptr);
  Ptr := AppendToBuffer(ALBinNumber, Ptr);
  Ptr := AppendToBuffer(' block: ', Length(' block: '), Ptr);
  Ptr := AppendToBuffer(ALBlockSize, Ptr);
  Ptr := AppendToBuffer(' size: ', Length(' size: '), Ptr);
  Ptr := AppendToBuffer(ASize, Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogGFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteGetMemErrorMedium(APool: PThreadPool; const APointer: Pointer;
  const ALBinGroupMasked, ALBinNumber, ALBlockSize, ASize: NativeUInt);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogEFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' EGetMem M:', Length(' EGetMem M:'), Ptr);
  Ptr := AppendToBuffer(' pool: ', Length(' pool: '), Ptr);
  Ptr := AppendToBuffer(APool.Index shr CMediumSlotIndexShift, Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(' mask: ', Length(' mask: '), Ptr);
  Ptr := AppendToBuffer(ALBinGroupMasked, Ptr);
  Ptr := AppendToBuffer(' bin: ', Length(' bin: '), Ptr);
  Ptr := AppendToBuffer(ALBinNumber, Ptr);
  Ptr := AppendToBuffer(' block: ', Length(' block: '), Ptr);
  Ptr := AppendToBuffer(ALBlockSize, Ptr);
  Ptr := AppendToBuffer(' size: ', Length(' size: '), Ptr);
  Ptr := AppendToBuffer(ASize, Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogEFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteFreeMemErrorMedium(APool: PThreadPool; const APointer: Pointer;
  const ASizeAndFlags: NativeUInt);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogEFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' EFreeMem M:', Length(' EFreeMem M:'), Ptr);
  Ptr := AppendToBuffer(' pool: ', Length(' pool: '), Ptr);
  Ptr := AppendToBuffer(APool.Index shr CMediumSlotIndexShift, Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(' flag: ', Length(' flag: '), Ptr);
  Ptr := AppendToBuffer(ASizeAndFlags, Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogEFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteReallocMemErrorMedium(APool: PThreadPool; const APointer: Pointer;
  const ASizeAndFlags, ASize: NativeUInt);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogEFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' EReallocMem M:', Length(' EReallocMem M:'), Ptr);
  Ptr := AppendToBuffer(' pool: ', Length(' pool: '), Ptr);
  Ptr := AppendToBuffer(APool.Index shr CMediumSlotIndexShift, Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(' flag: ', Length(' flag: '), Ptr);
  Ptr := AppendToBuffer(ASizeAndFlags, Ptr);
  Ptr := AppendToBuffer(' av-size: ', Length(' av-size: '), Ptr);
  Ptr := AppendToBuffer(ASizeAndFlags and CExtractMediumSizeMask, Ptr);
  Ptr := AppendToBuffer(' size: ', Length(' size: '), Ptr);
  Ptr := AppendToBuffer(ASize, Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogEFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteGetMemSmall(APool: PThreadPool; const APointer: Pointer;
  const ABlockPool: PSmallBlockPoolHeader; ABlockType: PSmallBlockType; const ASize: NativeUInt);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogGFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' GetMem S:', Length(' GetMem S:'), Ptr);
  Ptr := AppendToBuffer(' pool: ', Length(' pool: '), Ptr);
  Ptr := AppendToBuffer(APool.Index shr CMediumSlotIndexShift, Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(' block: ', Length(' block: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(ABlockPool), Ptr);
  Ptr := AppendToBuffer(' inuse: ', Length(' inuse: '), Ptr);
  Ptr := AppendToBuffer(ABlockPool.BlocksInUse, Ptr);
  Ptr := AppendToBuffer(' flag: ', Length(' flag: '), Ptr);
  Ptr := AppendToBuffer(ABlockPool.FirstBlockPoolPointerAndFlags, Ptr);

  Ptr := AppendToBuffer(' type: ', Length(' type: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(ABlockType), Ptr);

  Ptr := AppendToBuffer(' size: ', Length(' size: '), Ptr);
  Ptr := AppendToBuffer(ASize, Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogGFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteGetMemErrorSmall(APool: PThreadPool; const APointer: Pointer;
  const ABlockPool: PSmallBlockPoolHeader; ABlockType: PSmallBlockType;
  const ASize: NativeUInt);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogEFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' EGetMem S:', Length(' EGetMem S:'), Ptr);
  Ptr := AppendToBuffer(' pool: ', Length(' pool: '), Ptr);
  Ptr := AppendToBuffer(APool.Index shr CMediumSlotIndexShift, Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(' block: ', Length(' block: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(ABlockPool), Ptr);
  Ptr := AppendToBuffer(' inuse: ', Length(' inuse: '), Ptr);
  Ptr := AppendToBuffer(ABlockPool.BlocksInUse, Ptr);
  Ptr := AppendToBuffer(' flag: ', Length(' flag: '), Ptr);
  Ptr := AppendToBuffer(ABlockPool.FirstBlockPoolPointerAndFlags, Ptr);

  Ptr := AppendToBuffer(' type: ', Length(' type: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(ABlockType), Ptr);

  Ptr := AppendToBuffer(' size: ', Length(' size: '), Ptr);
  Ptr := AppendToBuffer(ASize, Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogEFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteFreeMemErrorSmall(APool: PThreadPool; const APointer: Pointer;
  const ABlockPool: PSmallBlockPoolHeader; ABlockType: PSmallBlockType);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogEFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' EFreeMem S:', Length(' EFreeMem S:'), Ptr);
  Ptr := AppendToBuffer(' pool: ', Length(' pool: '), Ptr);
  Ptr := AppendToBuffer(APool.Index shr CMediumSlotIndexShift, Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(' block: ', Length(' block: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(ABlockPool), Ptr);
  Ptr := AppendToBuffer(' inuse: ', Length(' inuse: '), Ptr);
  Ptr := AppendToBuffer(ABlockPool.BlocksInUse, Ptr);
  Ptr := AppendToBuffer(' flag: ', Length(' flag: '), Ptr);
  Ptr := AppendToBuffer(ABlockPool.FirstBlockPoolPointerAndFlags, Ptr);

  Ptr := AppendToBuffer(' type: ', Length(' type: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(ABlockType), Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogEFileHandle, Line[0], Ptr - @Line[0]);
end;

procedure WriteReallocMemErrorSmall(APool: PThreadPool; const APointer: Pointer;
  const ABlockPool: PSmallBlockPoolHeader; const ABlockType: PSmallBlockType;
  const ALOldAvailableSize, ASize: NativeUInt);
var
  Line: TLogLine;
  Ptr: PAnsiChar;
begin
  if LogEFileHandle = 0 then
    Exit;

  Ptr := AppendToBuffer(LockxchgInc(@LogSequence), @Line[0]);
  Ptr := AppendToBuffer(' EReallocMem S:', Length(' EReallocMem S:'), Ptr);
  Ptr := AppendToBuffer(' pool: ', Length(' pool: '), Ptr);
  Ptr := AppendToBuffer(APool.Index shr CMediumSlotIndexShift, Ptr);
  Ptr := AppendToBuffer(' ptr: ', Length(' ptr: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(APointer), Ptr);

  Ptr := AppendToBuffer(' block: ', Length(' block: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(ABlockPool), Ptr);
  Ptr := AppendToBuffer(' inuse: ', Length(' inuse: '), Ptr);
  Ptr := AppendToBuffer(ABlockPool.BlocksInUse, Ptr);
  Ptr := AppendToBuffer(' flag: ', Length(' flag: '), Ptr);
  Ptr := AppendToBuffer(ABlockPool.FirstBlockPoolPointerAndFlags, Ptr);

  Ptr := AppendToBuffer(' type: ', Length(' type: '), Ptr);
  Ptr := AppendToBuffer(NativeUInt(ABlockType), Ptr);

  Ptr := AppendToBuffer(' av-size: ', Length(' av-size: '), Ptr);
  Ptr := AppendToBuffer(ALOldAvailableSize, Ptr);
  Ptr := AppendToBuffer(' size: ', Length(' size: '), Ptr);
  Ptr := AppendToBuffer(ASize, Ptr);

  Ptr := AppendToBuffer(CNL, Length(CNL), Ptr);
  FileWrite(LogEFileHandle, Line[0], Ptr - @Line[0]);
end;

initialization
  InitializeDebug;

finalization
  FinalizeDebug;

end.
