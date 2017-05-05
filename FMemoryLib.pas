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

unit FMemoryLib;

interface

{$include FOption.inc}

uses
  FTypeLib;

const
  lib = 'FastMM4m.dll';

function FMemoryManager: PMemoryManagerEx;

function FGetMem(Size: NativeInt): Pointer; external lib name 'FGetMem';
function FFreeMem(P: Pointer): Integer; external lib name 'FFreeMem';
function FReallocMem(P: Pointer; Size: NativeInt): Pointer; external lib name 'FReallocMem';
function FAllocMem(Size: NativeInt): Pointer; external lib name 'FAllocMem';

function FRegisterExpectedMemoryLeak(P: Pointer): Boolean; external lib name 'FRegisterExpectedMemoryLeak';
function FUnregisterExpectedMemoryLeak(P: Pointer): Boolean; external lib name 'FUnregisterExpectedMemoryLeak';
procedure FScanForMemoryLeaks; external lib name 'FScanForMemoryLeaks';

function FShareMemoryManager: TSharedMemory; external lib name 'FShareMemoryManager';
function FAttemptToUseSharedMemoryManager: TSharedMemory; external lib name 'FAttemptToUseSharedMemoryManager';

procedure FGetMemoryManagerState(out AMemoryManagerState: TMemoryManagerState); external lib name 'FGetMemoryManagerState';
procedure FGetMemoryMap(out AMemoryMap: TMemoryMap); external lib name 'FGetMemoryMap';
function FGetHeapStatus: THeapStatus; external lib name 'FGetHeapStatus';

implementation

var
  NewMemoryManager, OldMemoryManager: TMemoryManagerEx;

function FMemoryManager: PMemoryManagerEx;
begin
  // Use the last setting item as check
  if not Assigned(NewMemoryManager.ReallocMem) then
  begin
{$if CompilerVersion > 15} // Delphi 7
    NewMemoryManager.AllocMem := FAllocMem;
    NewMemoryManager.RegisterExpectedMemoryLeak := FRegisterExpectedMemoryLeak;
    NewMemoryManager.UnregisterExpectedMemoryLeak := FUnregisterExpectedMemoryLeak;
{$ifend}

    NewMemoryManager.GetMem := FGetMem;
    NewMemoryManager.FreeMem := FFreeMem;
    NewMemoryManager.ReallocMem := FReallocMem;
  end;

  Result := @NewMemoryManager;
end;

procedure InstallLib;
begin
  GetMemoryManager(OldMemoryManager);
{$ifndef F4mTestManager}
  SetMemoryManager(FMemoryManager^);
{$endif}
end;

procedure UninstallLib;
begin
{$ifndef F4mTestManager}
  SetMemoryManager(OldMemoryManager);
{$endif}
end;

initialization
  InstallLib;

finalization
  UninstallLib;  

end.
