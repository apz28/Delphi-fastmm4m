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

unit FTrackAlloc;

interface

{$include FOption.inc}
{.$define F4mTrackAlloc}

uses
  {$ifndef F4mTrackAlloc}Classes,{$endif}
  FTypeLib, FUtil;  // System

{$MINENUMSIZE 1}

const
  CTrackAllocFileName: PAnsiChar = 'c:\FTrackMemoryAlloc.log';

type
  TTrackAllocCaller = (tacGetMem = 1, tacAllocMem, tacReallocMem, tacFreeMem);

  PTrackMemoryEntry = ^TTrackMemoryEntry;
  TTrackMemoryEntry = packed record
    Ptr: NativeUInt; // Pointer of successfull allocation
    Sequence: NativeUInt;
    Size: NativeUInt;
    Caller: TTrackAllocCaller;
  end;

  TTrackMemoryEntryArray = array of TTrackMemoryEntry;

{$ifndef F4mTrackAlloc}
function TrackAllocIndexOf(ASortedItems: TList; const AItem: PTrackMemoryEntry): NativeInt;

function TrackAllocRead(var Items: TTrackMemoryEntryArray; const ASortedItems: TList): NativeUInt;
{$endif}

{$ifdef F4mTrackAlloc}
var
  StartTrack: Boolean = True;
{$endif}

implementation

{$ifdef F4mTrackAlloc}
var
  OldMemoryManager: TMemoryManagerEx;
  TrackSequence: NativeUInt;
  TrackHandle: THandle;
{$endif}

function CompareNativeUIntItems(AItem1, AItem2: NativeUInt): Integer;
begin
  if AItem1 > AItem2 then
    Result := 1
  else if AItem1 < AItem2 then
    Result := -1
  else
    Result := 0;
end;

function CompareTrackMemoryEntryItems(AItem1, AItem2: Pointer): Integer;
begin
  Result := CompareNativeUIntItems(PTrackMemoryEntry(AItem1).Ptr, PTrackMemoryEntry(AItem2).Ptr);
  if Result = 0 then
    Result := CompareNativeUIntItems(PTrackMemoryEntry(AItem1).Sequence, PTrackMemoryEntry(AItem2).Sequence);
end;

{$ifndef F4mTrackAlloc}
function TrackAllocIndexOf(ASortedItems: TList; const AItem: PTrackMemoryEntry): NativeInt;
var
  P: PTrackMemoryEntry;
  L, M, R: NativeInt;
  E: Integer;
begin
  L := 0;
  R := ASortedItems.Count - 1;
  while L <= R do
  begin
    M := (L + R) shr 1;
    P := ASortedItems[M];
    E := CompareNativeUIntItems(P.Ptr, AItem.Ptr);
    if E = 0 then
    begin
      Result := M;

      // Move to the end
      Inc(M);
      while M < ASortedItems.Count do
      begin
        P := ASortedItems[M];
        if (P.Ptr <> AItem.Ptr) or (P.Sequence >= AItem.Sequence) then
          Break;
        Inc(M);
      end;

      // Get the one that sequencely smaller than AItem.Sequence
      Dec(M);
      while M >= 0 do
      begin
        P := ASortedItems[M];
        if P.Ptr <> AItem.Ptr then
          Exit;

        if P.Sequence < AItem.Sequence then
        begin
          Result := M;
          Exit;
        end;

        Dec(M);
      end;

      Exit;
    end
    else if E > 0 then
      R := M - 1
    else
      L := M + 1;
  end;
  Result := -1;
end;
{$endif}

{$ifndef F4mTrackAlloc}
function TrackAllocRead(var Items: TTrackMemoryEntryArray; const ASortedItems: TList): NativeUInt;
var
  Item: TTrackMemoryEntry;
  Handle: THandle;
  Count: UInt32;
begin
  if not FileOpen(CTrackAllocFileName, Handle) then
  begin
    Result := 0;
    Exit;
  end;

  try
    Count := FileSize(Handle);
    Result := Count div SizeOf(TTrackMemoryEntry);
    if Result <> 0 then
    begin
      SetLength(Items, Result);
      ASortedItems.Capacity := (Result div 3) * 2;

      Count := 0;
      while FileRead(Handle, Item, SizeOf(TTrackMemoryEntry)) = SizeOf(TTrackMemoryEntry) do
      begin
        Items[Count] := Item;
        if Item.Caller <> tacFreeMem then
          ASortedItems.Add(@Items[Count]);
        Inc(Count);
      end;

      ASortedItems.Sort(CompareTrackMemoryEntryItems);
    end;
  finally
    FileClose(Handle);
  end;
end;
{$endif}

{$ifdef F4mTrackAlloc}
function TGetMem(Size: NativeInt): Pointer;
var
  TrackItem: TTrackMemoryEntry;
begin
  Result := OldMemoryManager.GetMem(Size);
  if (Result <> nil) and StartTrack then
  begin
    TrackItem.Caller := tacGetMem;
    TrackItem.Ptr := NativeUInt(Result);
    TrackItem.Size := Size;
    TrackItem.Sequence := LockxchgInc(@TrackSequence);
    FileWrite(TrackHandle, TrackItem, SizeOf(TrackItem));
  end;
end;
{$endif}

{$ifdef F4mTrackAlloc}
function TFreeMem(P: Pointer): Integer;
var
  TrackItem: TTrackMemoryEntry;
begin
  if P <> nil then
  begin
    Result := OldMemoryManager.FreeMem(P);
    if (Result = CResultOK) and StartTrack then
    begin
      TrackItem.Caller := tacFreeMem;
      TrackItem.Ptr := NativeUInt(P);
      TrackItem.Size := 0;
      TrackItem.Sequence := LockxchgInc(@TrackSequence);
      FileWrite(TrackHandle, TrackItem, SizeOf(TrackItem));
    end;
  end
  else
    Result := CResultOK;
end;
{$endif}

{$ifdef F4mTrackAlloc}
function TReallocMem(P: Pointer; Size: NativeInt): Pointer;
var
  TrackItem: TTrackMemoryEntry;
begin
  if P <> nil then
  begin
    Result := OldMemoryManager.ReallocMem(P, Size);
    if (Result <> nil) and StartTrack then
    begin
      TrackItem.Caller := tacReallocMem;
      TrackItem.Ptr := NativeUInt(Result);
      TrackItem.Size := Size;
      TrackItem.Sequence := LockxchgInc(@TrackSequence);
      FileWrite(TrackHandle, TrackItem, SizeOf(TrackItem));
    end;
  end
  else
  begin
    if Size > 0 then
      Result := TGetMem(Size)
    else
      Result := nil;
  end;
end;
{$endif}

{$if CompilerVersion > 15} // Delphi 7
{$ifdef F4mTrackAlloc}
function TAllocMem(Size: NativeInt): Pointer;
var
  TrackItem: TTrackMemoryEntry;
begin
  Result := OldMemoryManager.AllocMem(Size);
  if (Result <> nil) and StartTrack then
  begin
    TrackItem.Caller := tacAllocMem;
    TrackItem.Ptr := NativeUInt(Result);
    TrackItem.Size := Size;
    TrackItem.Sequence := LockxchgInc(@TrackSequence);
    FileWrite(TrackHandle, TrackItem, SizeOf(TrackItem));
  end;
end;
{$endif}
{$ifend}

{$ifdef F4mTrackAlloc}
procedure InstallTrackMemoryManager;
var
  NewMemoryManager: TMemoryManagerEx;
begin
  GetMemoryManager(OldMemoryManager);

  NewMemoryManager := OldMemoryManager;
  NewMemoryManager.GetMem := TGetMem;
  NewMemoryManager.FreeMem := TFreeMem;
  NewMemoryManager.ReallocMem := TReallocMem;
{$if CompilerVersion > 15} // Delphi 7
  NewMemoryManager.AllocMem := TAllocMem;
{$ifend}

  SetMemoryManager(NewMemoryManager);
end;
{$endif}

{$ifdef F4mTrackAlloc}
procedure UnInstallTrackMemoryManager;
begin
  SetMemoryManager(OldMemoryManager);
end;
{$endif}

{$ifdef F4mTrackAlloc}
initialization
  if FileCreate(CTrackAllocFileName, TrackHandle) then
    InstallTrackMemoryManager;
{$endif}

{$ifdef F4mTrackAlloc}
finalization
  if TrackHandle <> 0 then
  begin
    UnInstallTrackMemoryManager;
    FileClose(TrackHandle);
    TrackHandle := 0;
  end;
{$endif}

end.
