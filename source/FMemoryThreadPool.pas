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

{$ifndef F4mTestThreadPool}
threadvar
  ThreadPool: PThreadPool;
{$endif}

function FindFreeThreadPool: PThreadPool;
var
  MinP, P: PThreadPool;
  ThreadId: TThreadId;
  I, ThreadIndex: Int32;
begin
  Result := nil;
  
  ThreadId := GetCurrentThreadId;
  while (ThreadId and $ff) = 0 do
    ThreadId := ThreadId shr 8;
  ThreadIndex := ThreadId mod CMaximumThreadPool;
  MinP := @ThreadPools[ThreadIndex];

  LockAcquire(@ThreadPoolsLocked);

  for I := ThreadIndex to (CMaximumThreadPool - 1) do
  begin
{$ifdef F4mDebugManager}
    if I = 0 then
      Continue;
{$endif}

    P := @ThreadPools[I];

    if P.UsedCount = 0 then
    begin
      Result := P;
      Break;
    end;

    if MinP.UsedCount > P.UsedCount then
      MinP := P;
  end;

  if (Result = nil) and (ThreadIndex <> 0) then
  begin
    for I := 0 to (ThreadIndex - 1) do
    begin
{$ifdef F4mDebugManager}
      if I = 0 then
        Continue;
{$endif}

      P := @ThreadPools[I];

      if P.UsedCount = 0 then
      begin
        Result := P;
        Break;
      end;

      if MinP.UsedCount > P.UsedCount then
        MinP := P;
    end;
  end;

  if Result = nil then
    Result := MinP;

  Inc(Result.UsedCount);

{$ifdef F4mTrackThreadLife}
  for I := 0 to (CMaximumThreadPerPool - 1) do
  begin
    if Result.ThreadIds[I] = 0 then
    begin
      Result.ThreadIds[I] := ThreadId;
{$ifopt C+}
      ThreadId := 0;
{$endif}
      Break;
    end;
  end;
  Assert(ThreadId = 0, 'Not found slot to store thread-id from threadpool');
{$endif}

{$ifdef F4mStatisticInfo}
  if Result.UsedCount > 1 then
    LockInc(@StatisticCalls.PoolCollision);
{$endif}

  //LockRelease(@ThreadPoolsLocked);
  ThreadPoolsLocked := 0;
end;

procedure FFinalizeThreadPool;
var
  FoundPool: PThreadPool;
{$ifdef F4mTrackThreadLife}
  ThreadId: TThreadId;
  I: Int32;
{$endif}
begin
{$ifdef F4mTestThreadPool}
  FoundPool := @ThreadPools[0];
{$else}
  FoundPool := ThreadPool;
  //ThreadPool := nil;
{$endif}

  if FoundPool = nil then
    Exit;

  if Shutdown = 0 then
    LockAcquire(@ThreadPoolsLocked);

  Assert(FoundPool.UsedCount > 0, 'Invalid threadpool counter on shutdown');

  Dec(FoundPool.UsedCount);

{$ifdef F4mTrackThreadLife}
  ThreadId := GetCurrentThreadId;
  for I := 0 to (CMaximumThreadPerPool - 1) do
  begin
    if FoundPool.ThreadIds[I] = ThreadId then
    begin
      FoundPool.ThreadIds[I] := 0;
{$ifopt C+}
      ThreadId := 0; // Indicate found
{$endif}      
      Break;
    end;
  end;
  Assert(ThreadId = 0, 'Not found thread-id from threadpool');
{$endif}

  if Shutdown = 0 then
    //LockRelease(@ThreadPoolsLocked);
    ThreadPoolsLocked := 0;
end;

procedure InitializeMemoryThreadPool;
var
  I: UInt32;
begin
  // Initialize pool-index
  for I := 0 to (CMaximumThreadPool - 1) do
    ThreadPools[I].Index := I shl CMediumSlotIndexShift;
end;

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
