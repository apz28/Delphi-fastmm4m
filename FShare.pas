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

unit FShare;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil; // System

// Starts sharing this memory manager with other modules in the current process.
// Only one memory manager may be shared per process, so this function may fail.
function FShareMemoryManager: TSharedMemory;

// Searches the current process for a shared memory manager. If no memory has
// been allocated using this memory manager it will switch to using the shared
// memory manager instead. Returns true if another memory manager was found and
// it could be shared. If this memory manager instance *is* the shared memory
// manager, it will do nothing and return true.
function FAttemptToUseSharedMemoryManager: TSharedMemory;


implementation

uses
  Windows, FMemory;

var
  // A string uniquely identifying the current process (for sharing the memory
  // manager between DLLs and the main application).
  MappingObjectName: array[0..26] of AnsiChar = ('L', 'o', 'c', 'a', 'l', '\',
    'F', 'a', 's', 't', 'M', 'M', '4', '_',
    'P', 'I', 'D', '_',
    '?', '?', '?', '?', '?', '?', '?', '?', #0);

  // The handle of the memory mapped file that contains the pointer to the
  // TMemoryManagerEx structure of the shared memory manager.
  MappingObjectHandle: THandle;

  // Indicate if Memory Manager had been set
  IsMemoryFManagerSet: Boolean;


// Generates a string identifying the process
procedure BuildMappingObjectName;
var
  LProcessID: UInt32;
  I: Int32;
begin
  LProcessID := GetCurrentProcessId;
  for I := 0 to 7 do
  begin
    MappingObjectName[(High(MappingObjectName) - 1) - I] :=
      HexTable[((LProcessID shr (I * 4)) and $F)];
  end;
end;

// Searches the current process for a shared memory manager
function FindSharedMemoryManager: PMemoryManagerEx;
var
  LPMapAddress: Pointer;
  LLocalMappingObjectHandle: THandle;
begin
  // Try to open the shared memory manager file mapping
  LLocalMappingObjectHandle := OpenFileMappingA(FILE_MAP_READ, False, MappingObjectName);

  // Is a memory manager in this process sharing its memory manager?
  if LLocalMappingObjectHandle = 0 then
    // No shared memory manager in the process
    Result := nil
  else
  begin
    // Map a view of the memory
    LPMapAddress := MapViewOfFile(LLocalMappingObjectHandle, FILE_MAP_READ, 0, 0, 0);

    // Get the address of the shared memory manager
    Result := PPointer(LPMapAddress)^;

    // Unmap the file
    UnmapViewOfFile(LPMapAddress);

    // Close the file mapping
    CloseHandle(LLocalMappingObjectHandle);
  end;
end;

procedure FinalizeShare;
begin
  // Destroy the memory mapped file handle used for sharing the memory manager.
  if MappingObjectHandle <> 0 then
  begin
    CloseHandle(MappingObjectHandle);
    MappingObjectHandle := 0;
  end;
end;

// Starts sharing this memory manager with other modules in the current process.
// Only one memory manager may be shared per process, so this function may fail.
function FShareMemoryManager: TSharedMemory;
var
  LPMapAddress: Pointer;
begin
  // Either another memory manager has been set or this memory manager is already being shared
  Result := smAlreadySet;

  if MappingObjectHandle = 0 then
  begin
    // Is any other module already sharing its Memory Manager?
    if FindSharedMemoryManager = nil then
    begin
      // Create the memory mapped file
      MappingObjectHandle := CreateFileMappingA(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0,
        SizeOf(Pointer), MappingObjectName);

      if MappingObjectHandle = 0 then
      begin
        Result := smFailed;
        Exit;
      end;
        
      // Map a view of the memory
      LPMapAddress := MapViewOfFile(MappingObjectHandle, FILE_MAP_WRITE, 0, 0, 0);

      // Set a pointer to the new memory manager
      PPointer(LPMapAddress)^ := @FMemoryManager;

      // Unmap the file
      UnmapViewOfFile(LPMapAddress);

      // Success
      Result := smOK;
    end
    // Another module is already sharing its memory manager
    else
      Result := smAlreadyShared;
  end;
end;

// Searches the current process for a shared memory manager. If no memory has
// been allocated using this memory manager it will switch to using the shared
// memory manager instead. Returns true if another memory manager was found and
// it could be shared. If this memory manager instance *is* the shared memory
// manager, it will do nothing and return true.
function FAttemptToUseSharedMemoryManager: TSharedMemory;
var
  LPMemoryManagerEx: PMemoryManagerEx;
begin
  if IsMemoryFManagerSet then
  begin
    Result := smAlreadySet;
    Exit;
  end;

  // Is this Memory Manager being shared?
  // If so, switching to another Memory Manager is not allowed
  if MappingObjectHandle = 0 then
  begin
    // May not switch memory manager after memory has been allocated
    if IsMemoryAllocated then
      Result := smAlreadyUsed
    else
    begin
      LPMemoryManagerEx := FindSharedMemoryManager;
      if LPMemoryManagerEx <> nil then
      begin
        // Set to the new memory manager
        SetMemoryManager(LPMemoryManagerEx^);
        IsMemoryFManagerSet := True;

        Result := smOK;
      end
      else
        Result := smFailed;
    end;
  end
  else
    Result := smAlreadyShared;
end;

initialization
  BuildMappingObjectName;

finalization
  FinalizeShare;

end.
