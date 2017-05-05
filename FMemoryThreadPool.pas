{
License:
 This work is copyright Professional Software Development / An Pham. It
 is released under a dual license, and you may choose to use it under either the
 Mozilla Public License 1.1 (MPL 1.1, available from
 http://www.mozilla.org/MPL/MPL-1.1.html)
 or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
 http://www.opensource.org/licenses/lgpl-license.php).


Completed Change Log:
 Please see FChange.log file
}

unit FMemoryThreadPool;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil; // System

procedure FFinalizeThreadPool; 
function GetThreadPool: PThreadPool;

implementation

uses
  Windows, FVirtual;

function FindFreeThreadPool: PThreadPool;
var
  ThreadId: TThreadId;
  I, MinIndex, ThreadIndex: Int32;
begin
  Result := nil;
  ThreadId := GetCurrentThreadId;
  ThreadIndex := ThreadId mod MaximumThreadPool;
  MinIndex := ThreadIndex;

  LockAcquire(@ThreadPoolsLocked);

  for I := ThreadIndex to (MaximumThreadPool - 1) do
  begin
{$ifdef F4mDebugManager}
    if I = 0 then
      Continue;
{$endif}

    if ThreadPools[I].ThreadIdc = 0 then
    begin
      Result := @ThreadPools[I];
      Break;
    end;

    if ThreadPools[I].ThreadIdc < ThreadPools[MinIndex].ThreadIdc then
      MinIndex := I;
  end;

  if (Result = nil) and (ThreadIndex <> 0) then
  begin
    for I := 0 to (ThreadIndex - 1) do
    begin
{$ifdef F4mDebugManager}
      if I = 0 then
        Continue;
{$endif}

      if ThreadPools[I].ThreadIdc = 0 then
      begin
        Result := @ThreadPools[I];
        Break;
      end;

      if ThreadPools[I].ThreadIdc < ThreadPools[MinIndex].ThreadIdc then
        MinIndex := I;
    end;
  end;

  if Result = nil then
    Result := @ThreadPools[MinIndex];

  Assert(Result.ThreadIdc < MaximumThreadPerPool);

  Result.ThreadIds[Result.ThreadIdc] := ThreadId;
  Inc(Result.ThreadIdc);

  LockRelease(@ThreadPoolsLocked);
end;

procedure FFinalizeThreadPool;
  procedure FFinalizeThreadPoolAt(APool: PThreadPool; const AIndex: Int32);
{$ifdef F4mCacheThreadOSAlloc}
  var
    P: Pointer;
{$endif}
  begin
    APool.ThreadIds[AIndex] := 0;
    Dec(APool.ThreadIdc);
    if AIndex < APool.ThreadIdc then
      Move(APool.ThreadIds[AIndex + 1], APool.ThreadIds[AIndex], (APool.ThreadIdc - AIndex) * SizeOf(TThreadId));

{$ifdef F4mCacheThreadOSAlloc}
    if (Shutdown <> 0) or (APool.ThreadIdc = 0) then
    begin
      P := UnlinkIf(APool.MediumCachedOSAlloc);
      if P <> nil then
        OSFree(P);
    end;
{$endif}      
  end;
var
  ThreadId: TThreadId;
  I, J, ThreadIndex, FoundIndex: Int32;
begin
  FoundIndex := -1;
  ThreadId := GetCurrentThreadId;
  ThreadIndex := ThreadId mod MaximumThreadPool;

  if Shutdown = 0 then
    LockAcquire(@ThreadPoolsLocked);

  for I := ThreadIndex to (MaximumThreadPool - 1) do
  begin
    with ThreadPools[I] do
    begin
      for J := 0 to (ThreadIdc - 1) do
      begin
        if ThreadIds[J] = ThreadId then
        begin
          FoundIndex := I;
          FFinalizeThreadPoolAt(@ThreadPools[I], J);
          Break;
        end;
      end;
    end;

    if FoundIndex >= 0 then
      Break;
  end;

  if (FoundIndex < 0) and (ThreadIndex <> 0) then
    for I := 0 to (ThreadIndex - 1) do
    begin
      with ThreadPools[I] do
      begin
        for J := 0 to (ThreadIdc - 1) do
        begin
          if ThreadIds[J] = ThreadId then
          begin
            FoundIndex := I;
            FFinalizeThreadPoolAt(@ThreadPools[I], J);
            Break;
          end;
        end;
      end;

      if FoundIndex >= 0 then
        Break;
    end;

  if Shutdown = 0 then
    LockRelease(@ThreadPoolsLocked);
end;

procedure InitializeMemoryThreadPool;
var
  I: Int32;
begin
  // Initialize pool-index
  for I := 0 to (MaximumThreadPool - 1) do
    ThreadPools[I].IndexFlag := UInt32(I) shl MediumSlotIndexShift;
end;

{$ifndef F4mTestThreadPool}
threadvar
  ThreadPool: PThreadPool;
{$endif}

function GetThreadPool: PThreadPool;
begin
{$ifdef F4mTestThreadPool}
  Result := @ThreadPools[0];
{$else}
  Result := ThreadPool;
  if Result = nil then
  begin
    Result := FindFreeThreadPool;
    ThreadPool := Result;
  end;
{$endif}
end;

initialization
  InitializeMemoryThreadPool;
  
end.
