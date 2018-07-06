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

unit FVirtual;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil;

// Returns the class for a memory block. Returns nil if it is not a valid class
function GetObjectClass(const APointer: Pointer): TClass;

// Checks whether the given address is a valid address for a VMT entry.
function IsValidVMTAddress(const APAddress: Pointer; var MemSegmentSize: NativeUInt; var MemSegmentBase: Pointer): Boolean;

function OSAlloc(const Size: NativeUInt): Pointer;
function OSAllocTopDown(const Size: NativeUInt): Pointer;
function OSAllocTryFrom(const BasedAddress: Pointer; const Size: NativeUInt): Pointer;

function OSFree(const P: Pointer): Boolean;
function OSQuery(const P: Pointer; out Size: NativeUInt; out BaseAddress: Pointer): TChunkStatus;

implementation

uses
  Windows;

// Returns the class for a memory block. Returns nil if it is not a valid class
function GetObjectClass(const APointer: Pointer): TClass;
var
  MemSegmentBase: Pointer;
  MemSegmentSize: NativeUInt;

  // Returns true if AClassPointer points to a class VMT
  function InternalIsValidClass(const AClassPointer: Pointer; const ADepth: Int32 = 0): Boolean;
  var
    LParentClassSelfPointer: PPointer;
  begin
    // Check that the self pointer as well as parent class self pointer addresses are valid
    if (ADepth < 1000)
      and IsValidVMTAddress(Pointer(NativeInt(AClassPointer) + vmtSelfPtr), MemSegmentSize, MemSegmentBase)
      and IsValidVMTAddress(Pointer(NativeInt(AClassPointer) + vmtParent), MemSegmentSize, MemSegmentBase) then
    begin
      // Get a pointer to the parent class' self pointer
      LParentClassSelfPointer := PPointer(NativeInt(AClassPointer) + vmtParent)^;

      // Check that the self pointer as well as the parent class is valid
      Result := (PPointer(NativeInt(AClassPointer) + vmtSelfPtr)^ = AClassPointer)
        and ((LParentClassSelfPointer = nil)
          or (IsValidVMTAddress(LParentClassSelfPointer, MemSegmentSize, MemSegmentBase)
            and InternalIsValidClass(LParentClassSelfPointer^, ADepth + 1)));
    end
    else
      Result := False;
  end;

begin
  // Get the class pointer from the (suspected) object
  Result := TClass(PNativeUInt(APointer)^);

  // No VM info yet
  MemSegmentSize := 0;
  MemSegmentBase := nil;

  // Check the block
  if not InternalIsValidClass(Pointer(Result), 0) then
    Result := nil;
end;

// Checks whether the given address is a valid address for a VMT entry.
function IsValidVMTAddress(const APAddress: Pointer; var MemSegmentSize: NativeUInt;
  var MemSegmentBase: Pointer): Boolean;
var
  LMemInfo: TMemoryBasicInformation;
begin
  // Do some basic pointer checks: Must be dword aligned and beyond 64K
  if (NativeUInt(APAddress) > 65535) and ((NativeUInt(APAddress) and 3) = 0) then
  begin
    // Do we need to recheck the OS memory?
    if (NativeUInt(MemSegmentBase) > NativeUInt(APAddress))
        or ((NativeUInt(MemSegmentBase) + MemSegmentSize) < (NativeUInt(APAddress) + SizeOf(Pointer))) then
    begin
      // Get the VM status for the pointer
      FillChar(LMemInfo, SizeOf(LMemInfo), 0);
      VirtualQuery(APAddress, LMemInfo, SizeOf(LMemInfo));
      MemSegmentSize := LMemInfo.RegionSize;
      MemSegmentBase := LMemInfo.BaseAddress;
    end;

    // Check the readability of the memory address
    Result := (MemSegmentSize >= SizeOf(Pointer))
      and (LMemInfo.State = MEM_COMMIT)
      and (LMemInfo.Protect and (PAGE_READONLY or PAGE_READWRITE
        or PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0)
      and ((LMemInfo.Protect and PAGE_GUARD) = 0);
  end
  else
    Result := False;
end;

function OSAlloc(const Size: NativeUInt): Pointer;
begin
  Result := VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_READWRITE);

{$ifdef F4mStatisticInfo}
  LockInc(@StatisticCalls.OSAllocCount);
  if Result = nil then
    LockInc(@StatisticCalls.OSAllocFailCount);
{$endif}
end;

function OSAllocTopDown(const Size: NativeUInt): Pointer;
begin
  Result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);

{$ifdef F4mStatisticInfo}
  LockInc(@StatisticCalls.OSAllocCount);
  if Result = nil then
    LockInc(@StatisticCalls.OSAllocFailCount);
{$endif}
end;

function OSAllocTryFrom(const BasedAddress: Pointer; const Size: NativeUInt): Pointer;
begin
  Result := VirtualAlloc(BasedAddress, Size, MEM_RESERVE, PAGE_READWRITE);
  if Result <> nil then
    Result := VirtualAlloc(BasedAddress, Size, MEM_COMMIT, PAGE_READWRITE);

{$ifdef F4mStatisticInfo}
  LockInc(@StatisticCalls.OSAllocExtCount);
  if Result = nil then
    LockInc(@StatisticCalls.OSAllocFailCount);
{$endif}
end;

function OSFree(const P: Pointer): Boolean;
begin
  Result := VirtualFree(P, 0, MEM_RELEASE);

{$ifdef F4mStatisticInfo}
  LockInc(@StatisticCalls.OSFreeCount);
  if not Result then
    LockInc(@StatisticCalls.OSFreeFailCount);
{$endif}
end;

function OSQuery(const P: Pointer; out Size: NativeUInt; out BaseAddress: Pointer): TChunkStatus;
var
  LMemInfo: TMemoryBasicInformation;
begin
  FillChar(LMemInfo, SizeOf(LMemInfo), 0);
  VirtualQuery(P, LMemInfo, SizeOf(LMemInfo));
  Size := LMemInfo.RegionSize;
  BaseAddress := LMemInfo.BaseAddress;

  case LMemInfo.State of
    MEM_COMMIT: Result := csSysAllocated;
    MEM_RESERVE: Result := csSysReserved;
    //MEM_FREE: Result := csUnallocated;
    else Result := csUnallocated;
  end;
end;

end.
