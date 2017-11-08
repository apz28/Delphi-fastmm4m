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

unit FMemoryLarge;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil; // System

// Allocates a Large block of at least Size (actual size may be larger to
// allow for alignment etc.). Size must be the actual user requested size. This
// procedure will pad it to the appropriate page boundary and also add the space
// required by the header.
function GetMemLarge(const APool: PThreadPool; const Size: NativeUInt): Pointer;

// Frees a large block, returning ResultOK on success, ResultError otherwise
function FreeMemLarge(const P: Pointer): Integer;

// Reallocates a large block to at least the requested Size. Returns the new pointer, or nil on error
function ReallocMemLarge(const P: Pointer; const Size: NativeUInt): Pointer;

procedure FreeAllMemoryLarge;

implementation

uses
  FVirtual, FMemory;

// Allocates a Large block of at least ASize (actual size may be Larger to
// allow for alignment etc.). ASize must be the actual user requested size. This
// procedure will pad it to the appropriate page boundary and also add the space
// required by the header.
function GetMemLarge(const APool: PThreadPool; const Size: NativeUInt): Pointer;
var
  LOldFirstLargeBlock: PLargeBlockHeader;
  LLargeUsedBlockSize: NativeUInt;
begin
  Assert(APool <> nil);
  Assert(Size > CMaximumMediumBlockUserSize);

  // Pad the block size to include the header and granularity. We also add a
  // SizeOf(Pointer) overhead so a huge block size is a multiple of 16 bytes less
  // SizeOf(Pointer) (so we can use a single move function for reallocating all block types)
  LLargeUsedBlockSize := (Size + (CLargeBlockHeaderSize + CLargeBlockGranularity - 1 + CBlockHeaderSize))
    and -CLargeBlockGranularity;

  // Get the Large block
  Result := OSAllocTopDown(LLargeUsedBlockSize);

  // Set the Large block fields
  if Result <> nil then
  begin
    // Set the large block size and flags
    PLargeBlockHeader(Result).ThreadPool := APool;
    PLargeBlockHeader(Result).UserAllocatedSize := Size;
    PLargeBlockHeader(Result).BlockSizeAndFlags := LLargeUsedBlockSize or CIsLargeBlockFlag;

    // Insert the large block into the linked list of large blocks
    LockAcquire(@APool.LargeBlocksLocked);

    LOldFirstLargeBlock := APool.LargeBlocksCircularList.NextLargeBlockHeader;
    PLargeBlockHeader(Result).PreviousLargeBlockHeader := @APool.LargeBlocksCircularList;
    APool.LargeBlocksCircularList.NextLargeBlockHeader := Result;
    PLargeBlockHeader(Result).NextLargeBlockHeader := LOldFirstLargeBlock;
    LOldFirstLargeBlock.PreviousLargeBlockHeader := Result;

    //LockRelease(@APool.LargeBlocksLocked);
    APool.LargeBlocksLocked := 0;

    // Add the size of the header
    Inc(NativeUInt(Result), CLargeBlockHeaderSize);
  end;
end;

// Frees a large block, returning ResultOK on success, ResultError otherwise
function FreeMemLarge(const P: Pointer): Integer;
var
  Pool: PThreadPool;
  LargeBlockHeader, LPreviousLargeBlockHeader, LNextLargeBlockHeader: PLargeBlockHeader;
  LCurrentSegment, MemSegmentBase: Pointer;
  LRemainingSize, MemSegmentSize: NativeUInt;
begin                                                      
  Assert(P <> nil);

  // Point to the start of the large block
  LargeBlockHeader := Pointer(NativeUInt(P) - CLargeBlockHeaderSize);

  // Get ThreadPool where the memory was allocated
  Pool := LargeBlockHeader.ThreadPool;
  Assert(Pool <> nil);

{$ifdef F4mTestThreadPool}
  Assert(Pool = @ThreadPools[0]);
{$endif}

  // Get the previous and next large blocks
  LockAcquire(@Pool.LargeBlocksLocked);
  
  LPreviousLargeBlockHeader := LargeBlockHeader.PreviousLargeBlockHeader;
  LNextLargeBlockHeader := LargeBlockHeader.NextLargeBlockHeader;

  // Is the large block segmented?
  if (LargeBlockHeader.BlockSizeAndFlags and CLargeBlockIsSegmentedFlag) = 0 then
  begin
    // Single segment large block: Try to free it
    if OSFree(LargeBlockHeader) then
      Result := CResultOK
    else
      Result := CResultError;
  end
  else
  begin
    // The large block is segmented - free all segments
    LCurrentSegment := LargeBlockHeader;
    LRemainingSize := LargeBlockHeader.BlockSizeAndFlags and CExtractLargeSizeMask;
    Result := CResultOK;
    while True do
    begin
      // Get the size of the current segment
      OSQuery(LCurrentSegment, MemSegmentSize, MemSegmentBase);

      // Free the segment
      if not OSFree(LCurrentSegment) then
      begin
        Result := CResultError;
        Break;
      end;

      // Done?
      if MemSegmentSize >= LRemainingSize then
        Break;

      // Decrement the remaining size
      Dec(LRemainingSize, MemSegmentSize);
      Inc(NativeUInt(LCurrentSegment), MemSegmentSize);
    end;
  end;

  // Success?
  if Result = CResultOK then
  begin
    // Remove the large block from the linked list
    LNextLargeBlockHeader.PreviousLargeBlockHeader := LPreviousLargeBlockHeader;
    LPreviousLargeBlockHeader.NextLargeBlockHeader := LNextLargeBlockHeader;
  end;

  // Unlock the large blocks
  //LockRelease(@Pool.LargeBlocksLocked);
  Pool.LargeBlocksLocked := 0;
end;

// Reallocates a large block to at least the requested size. Returns the new
// pointer, or nil on error
function ReallocMemLarge(const P: Pointer; const Size: NativeUInt): Pointer;
var
  LNextSegmentPointer, MemSegmentBase: Pointer;
  LargeBlockHeader: PLargeBlockHeader;
  LBlockSizeAndFlags, LOldAvailableSize, LOldUserSize, LNewAllocSize, LNewSegmentSize: NativeUInt;
  MemSegmentSize: NativeUInt;
{$ifdef F4mReallocUpsize}
  LMinimumUpsize: NativeUInt;
{$endif}
begin
  Assert(P <> nil);

  // Point to the start of the large block
  LargeBlockHeader := Pointer(NativeUInt(P) - CLargeBlockHeaderSize);

  // Get the block header
  LBlockSizeAndFlags := PNativeUInt(NativeUInt(P) - CBlockHeaderSize)^;

  // Subtract the overhead to determine the useable size in the large block.
  LOldAvailableSize := (LBlockSizeAndFlags and CExtractLargeSizeMask) - (CLargeBlockHeaderSize + CBlockHeaderSize);

  // Is it an upsize or a downsize?
  if Size > LOldAvailableSize then
  begin
{$ifdef F4mReallocUpsize}
    // This pointer is being reallocated to a larger block and therefore it is
    // logical to assume that it may be enlarged again. Since reallocations are
    // expensive, there is a minimum upsize percentage to avoid unnecessary
    // future move operations.

    // Add 25% for large block upsizes
    LNewAllocSize := Size;
    LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if LNewAllocSize < LMinimumUpsize then
      LNewAllocSize := LMinimumUpsize;
{$else}
    // Need round up for the special move
    LNewAllocSize := (Size + (CMinimumBlockAlignment - 1)) and -CMinimumBlockAlignment;
{$endif}

    // Can another large block segment be allocated directly after this segment,
    // thus negating the need to move the data?
    LNextSegmentPointer := Pointer(NativeUInt(P) - CLargeBlockHeaderSize + (LBlockSizeAndFlags and CExtractLargeSizeMask));
    if OSQuery(LNextSegmentPointer, MemSegmentSize, MemSegmentBase) = csUnallocated then
    begin
      // Round the region size to the previous 64K
      MemSegmentSize := MemSegmentSize and -CLargeBlockGranularity;

      // Enough space to grow in place?
      if MemSegmentSize > (Size - LOldAvailableSize) then
      begin
        // There is enough space after the block to extend it - determine by how much
        LNewSegmentSize := (LNewAllocSize - LOldAvailableSize + (CLargeBlockGranularity - 1)) and -CLargeBlockGranularity;
        if LNewSegmentSize > MemSegmentSize then
          LNewSegmentSize := MemSegmentSize;
          
        if OSAllocTryFrom(LNextSegmentPointer, LNewSegmentSize) <> nil then
        begin
          // Update the requested size
          LargeBlockHeader.UserAllocatedSize := Size;
          LargeBlockHeader.BlockSizeAndFlags := (LargeBlockHeader.BlockSizeAndFlags + LNewSegmentSize) or CLargeBlockIsSegmentedFlag;

          // Success
          Result := P;
          Exit;
        end;
      end;
    end;

    // Could not resize in place: Allocate the new block
    Result := GetMemLarge(LargeBlockHeader.ThreadPool, LNewAllocSize); 

    if Result <> nil then
    begin
      Assert(Result <> P);
      
      // If it's a large block - store the actual user requested size (it may
      // not be if the block that is being reallocated from was previously downsized)
      if LNewAllocSize > CMaximumMediumBlockUserSize then
        PLargeBlockHeader(NativeUInt(Result) - CLargeBlockHeaderSize).UserAllocatedSize := Size;

      // The user allocated size is stored for large blocks
      LOldUserSize := PLargeBlockHeader(NativeUInt(P) - CLargeBlockHeaderSize).UserAllocatedSize;

      // The number of bytes to move is the old user size.
{$ifdef F4mUseCustomVariableSizeMoveRoutines}
      MoveX16LP(P^, Result^, LOldUserSize);
{$else}
      Move(P^, Result^, LOldUserSize);
{$endif}

      // Free the old block
      FreeMemLarge(P);
    end;
  end
  else
  begin
    // Must be less than half the current size or we don't bother resizing.
    if Size > (LOldAvailableSize shr 1) then
    begin
      // Update the requested size
      LargeBlockHeader.UserAllocatedSize := Size;

      Result := P;
      Exit;
    end;

    // Need round up for the special move
    LNewAllocSize := (Size + (CMinimumBlockAlignment - 1)) and -CMinimumBlockAlignment;

    // The block is less than half the old size: reallocate
    Result := FGetMemPool(LargeBlockHeader.ThreadPool, LNewAllocSize);
    if Result <> nil then
    begin
      Assert(Result <> P);
      
      // Move the data across
{$ifdef F4mUseCustomVariableSizeMoveRoutines}
  {$ifdef CPU386}
      MoveX8LP(P^, Result^, Size);
  {$else}
      // Blocks are always 16-byte aligned under 64-bit.
      MoveX16LP(P^, Result^, Size);
  {$endif}
{$else}
      Move(P^, Result^, Size);
{$endif}

      // Free the old block
      FreeMemLarge(P);
    end;
  end;
end;

procedure InitializeMemoryLargePool(const APool: PThreadPool);
begin
  APool.LargeBlocksCircularList.PreviousLargeBlockHeader := @APool.LargeBlocksCircularList;
  APool.LargeBlocksCircularList.NextLargeBlockHeader := @APool.LargeBlocksCircularList;
end;

procedure FreeAllMemoryLarge;
  procedure FreeAllMemoryLargePool(const APool: PThreadPool);
  var
    LPLargeBlockHeader, LPNextLargeBlockHeader: PLargeBlockHeader;
  begin
    LPLargeBlockHeader := APool.LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlockHeader <> @APool.LargeBlocksCircularList do
    begin
      // Get the next large block
      LPNextLargeBlockHeader := LPLargeBlockHeader.NextLargeBlockHeader;

      // Free this large block
      FreeMemLarge(LPLargeBlockHeader);

      // Next large block
      LPLargeBlockHeader := LPNextLargeBlockHeader;
    end;

    InitializeMemoryLargePool(APool);
  end;
var
  I: Int32;
begin
  for I := 0 to High(ThreadPools) do
    FreeAllMemoryLargePool(@ThreadPools[I]);
end;

procedure InitializeMemoryLarge;
var
  I: Int32;
begin
  for I := 0 to High(ThreadPools) do
    InitializeMemoryLargePool(@ThreadPools[I]);
end;

initialization
  InitializeMemoryLarge;

end.
