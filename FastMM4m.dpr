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

library FastMM4m;

uses
  Windows,
  FTypeLib in 'FTypeLib.pas',
  FType in 'FType.pas',
  FUtil in 'FUtil.pas',
  FShare in 'FShare.pas',
  FVirtual in 'FVirtual.pas',
  FMemoryThreadPool in 'FMemoryThreadPool.pas',
  FMemory in 'FMemory.pas',
  FMemoryLarge in 'FMemoryLarge.pas',
  FMemoryMedium in 'FMemoryMedium.pas',
  FMemorySmall in 'FMemorySmall.pas',
  FDebug in 'FDebug.pas',
  FTrackLeak in 'FTrackLeak.pas',
  FState in 'FState.pas';

{$R *.res}

function FreeMem(P: Pointer): Integer; stdcall;
begin
  Result := FMemory.FFreeMem(P);
end;

function GetMem(Size: NativeInt): Pointer; stdcall;
begin
  Result := FMemory.FGetMem(Size);
end;

function ReallocMem(P: Pointer; Size: NativeInt): Pointer; stdcall;
begin
  Result := FMemory.FReallocMem(P, Size);
end;

function AllocMem(Size: NativeInt): Pointer; stdcall;
begin
  Result := FMemory.FAllocMem(Size);
end;

function RegisterExpectedMemoryLeak(P: Pointer): Boolean; stdcall;
begin
  Result := FMemory.FRegisterExpectedMemoryLeak(P);
end;

function UnregisterExpectedMemoryLeak(P: Pointer): Boolean; stdcall;
begin
  Result := FMemory.FUnregisterExpectedMemoryLeak(P);
end;

exports
  // stdcall calling convention
  FreeMem,
  GetMem,
  ReallocMem,
  AllocMem,
  RegisterExpectedMemoryLeak,
  UnregisterExpectedMemoryLeak,
  
  // delphi calling convention
  FGetMem,
  FFreeMem,
  FReallocMem,
  FAllocMem,
  FRegisterExpectedMemoryLeak,
  FUnregisterExpectedMemoryLeak,
  FScanForMemoryLeaks,
  FFinalizeThreadPool,
  FFinalizeMemoryManager,
  FShareMemoryManager,
  FAttemptToUseSharedMemoryManager,
  FGetMemoryManagerState,
  FGetMemoryMap,
  FGetHeapStatus
  ;

var
  SaveDLLProc: procedure(AReason: Integer);

procedure FMemoryDLLProc(AReason: Integer);
begin
  if Assigned(SaveDLLProc) then
    SaveDLLProc(AReason);

  if AReason = DLL_THREAD_DETACH then
    FFinalizeThreadPool
  else if AReason = DLL_PROCESS_DETACH then
    FFinalizeMemoryManager;
end;

begin
  SaveDLLProc := DLLProc;
  DLLProc := @FMemoryDLLProc;
end.
 