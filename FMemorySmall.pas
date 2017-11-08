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

unit FMemorySmall;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil; // System

// Allocates a small block of at least Size (actual size may be larger to
// allow for alignment etc.). Size must be the actual user requested size. This
// procedure will pad it to the appropriate small size boundary and also add the space
// required by the header.
function GetMemSmall(const APool: PThreadPool; const Size: UInt32): Pointer;

// Frees a small block, returning ResultOK on success, ResultError otherwise
function FreeMemSmall(const P: Pointer): Integer;

// Reallocates a small block to at least the requested size. Returns the new pointer, or nil on error
function ReallocMemSmall(const P: Pointer; const Size: NativeUInt): Pointer;

procedure FreeAllMemorySmall;

// Locks all small block types
procedure LockAllSmallBlockTypes(const APool: PThreadPool);

// Unlock all the small block types
procedure UnlockAllSmallBlockTypes(const APool: PThreadPool);

implementation

uses
  FMemoryMedium, FMemory 
{$ifdef F4mDebugManager}
  , FDebug
{$endif}
  ;

// Locks all small block types
procedure LockAllSmallBlockTypes(const APool: PThreadPool);
var
  I: Int32;
begin
  for I := 0 to (CNumSmallBlockTypes - 1) do
    LockAcquire(@APool.SmallBlockTypes[I].BlockTypeLocked);
end;

// Unlock all the small block types
procedure UnlockAllSmallBlockTypes(const APool: PThreadPool);
var
  I: Int32;
begin
  for I := 0 to (CNumSmallBlockTypes - 1) do
    //LockRelease(@APool.SmallBlockTypes[I].BlockTypeLocked);
    APool.SmallBlockTypes[I].BlockTypeLocked := 0;
end;

function GetMemSmall(const APool: PThreadPool; const Size: UInt32): Pointer;
var
  LMediumBlock, LNextFreeBlock, LSecondSplit, LPMediumBin: PMediumFreeBlock;
  LPSmallBlockPool, LPNewFirstPool: PSmallBlockPoolHeader;
  LPSmallBlockType: PSmallBlockType;
  LNextMediumBlockSizeAndFlags: PNativeUInt;
  LNewFirstFreeBlock: Pointer;
  LBlockSize, LSecondSplitSize, LSequentialFeedFreeSize, LBinNumber, LBinGroupsMasked, LBinGroupNumber: UInt32;
  SpinCounter: NativeUInt;
begin
  Assert(APool <> nil);
  Assert(Size > 0);
  Assert(Size <= CMaximumSmallBlockUserSize);

  // Get the block type from the size
  LPSmallBlockType := PSmallBlockType(
    APool.AllocSize2SmallBlockTypeIndX4[(Size + (CBlockHeaderSize - 1)) div CSmallBlockGranularity]
    * (SizeOf(TSmallBlockType) div 4)
    + NativeUInt(@APool.SmallBlockTypes));

  // Lock the block type
  SpinCounter := CDefaultSpinCounter;
  while True do
  begin
    // Try to lock the small block type
    if TryLockAcquire(@LPSmallBlockType.BlockTypeLocked) then
      Break;

    // Try the next block type
    Inc(NativeUInt(LPSmallBlockType), SizeOf(TSmallBlockType));
    if TryLockAcquire(@LPSmallBlockType.BlockTypeLocked) then
      Break;
        
    // Try up to two sizes past the requested size
    Inc(NativeUInt(LPSmallBlockType), SizeOf(TSmallBlockType));
    if TryLockAcquire(@LPSmallBlockType.BlockTypeLocked) then
      Break;

    // All three sizes locked - given up and sleep
    Dec(NativeUInt(LPSmallBlockType), 2 * SizeOf(TSmallBlockType));
    ThreadYield(SpinCounter);
  end;

  // Get the first pool with free blocks
  LPSmallBlockPool := LPSmallBlockType.NextPartiallyFreePool;

  // Is the pool valid?
  if NativeUInt(LPSmallBlockPool) <> NativeUInt(LPSmallBlockType) then
  begin
    // Get the first free offset
    Result := LPSmallBlockPool.FirstFreeBlock;

    // Get the new first free block
    LNewFirstFreeBlock := PPointer(NativeUInt(Result) - CBlockHeaderSize)^;
    LNewFirstFreeBlock := Pointer(NativeUInt(LNewFirstFreeBlock) and CDropSmallFlagsMask);

    // Increment the number of used blocks
    Inc(LPSmallBlockPool.BlocksInUse);

    // Set the new first free block
    LPSmallBlockPool.FirstFreeBlock := LNewFirstFreeBlock;

    // Is the pool now full?}
    if LNewFirstFreeBlock = nil then
    begin
      // Pool is full - remove it from the partially free list
      LPNewFirstPool := LPSmallBlockPool.NextPartiallyFreePool;
      LPSmallBlockType.NextPartiallyFreePool := LPNewFirstPool;
      LPNewFirstPool.PreviousPartiallyFreePool := PSmallBlockPoolHeader(LPSmallBlockType);
    end;

{$ifdef F4mDebugManager}
    WriteGetMemSmall(@ThreadPools[0], Result, LPSmallBlockPool, LPSmallBlockType, Size);
    if not MarkMemoryUsed(@ThreadPools[0], Result) then
      WriteGetMemErrorSmall(@ThreadPools[0], Result, LPSmallBlockPool, LPSmallBlockType, Size);
{$endif}
  end
  else
  begin
    // Try to feed a small block sequentially
    Result := LPSmallBlockType.NextSequentialFeedBlockAddress;

    // Can another block fit?
    if NativeUInt(Result) <= NativeUInt(LPSmallBlockType.MaxSequentialFeedBlockAddress) then
    begin
      // Get the sequential feed block pool
      LPSmallBlockPool := LPSmallBlockType.CurrentSequentialFeedPool;

      // Increment the number of used blocks in the sequential feed pool
      Inc(LPSmallBlockPool.BlocksInUse);

      // Store the next sequential feed block address
      LPSmallBlockType.NextSequentialFeedBlockAddress := Pointer(NativeUInt(Result) + LPSmallBlockType.BlockSize);

{$ifdef F4mDebugManager}
      WriteGetMemSmall(@ThreadPools[0], Result, LPSmallBlockPool, LPSmallBlockType, Size);
      if not MarkMemoryUsed(@ThreadPools[0], Result) then
        WriteGetMemErrorSmall(@ThreadPools[0], Result, LPSmallBlockPool, LPSmallBlockType, Size);
{$endif}
    end
    else
    begin
      // Need to allocate a pool: Lock the medium blocks
      LockAcquire(@APool.MediumBlocksLocked);

      // Are there any available blocks of a suitable size?
      LBinGroupsMasked := APool.MediumBlockBinGroupBitmap and ($ffffff00 or LPSmallBlockType.AllowedGroupsForBlockPoolBitmap);
      if LBinGroupsMasked <> 0 then
      begin
        // Get the bin group with free blocks
        LBinGroupNumber := FindFirstSetBit(LBinGroupsMasked);

        // Get the bin in the group with free blocks
        //LBinNumber := FindFirstSetBit(APool.MediumBlockBinBitmaps[LBinGroupNumber]) + (LBinGroupNumber * CMediumBlockBinsPerGroup);
        LBinNumber := FindFirstSetBit(APool.MediumBlockBinBitmaps[LBinGroupNumber]) + (LBinGroupNumber shl CMediumBlockBinsPerGroupShift);
        LPMediumBin := @APool.MediumBlockBins[LBinNumber];

        // Get the first block in the bin
        LMediumBlock := LPMediumBin.NextFreeBlock;

        // Remove the first block from the linked list (LIFO)
        LNextFreeBlock := LMediumBlock.NextFreeBlock;
        LPMediumBin.NextFreeBlock := LNextFreeBlock;
        LNextFreeBlock.PreviousFreeBlock := LPMediumBin;

        // Is this bin now empty?
        if LNextFreeBlock = LPMediumBin then
        begin
          // Flag this bin as empty
          APool.MediumBlockBinBitmaps[LBinGroupNumber] := APool.MediumBlockBinBitmaps[LBinGroupNumber]
            and (not (1 shl (LBinNumber and 31)));

          // Is the group now entirely empty?
          if APool.MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
          begin
            // Flag this group as empty
            APool.MediumBlockBinGroupBitmap := APool.MediumBlockBinGroupBitmap
              and (not (1 shl LBinGroupNumber));
          end;
        end;

        // Get the size of the available medium block
        LBlockSize := PNativeUInt(NativeUInt(LMediumBlock) - CBlockHeaderSize)^ and CExtractMediumSizeMask;

        // Medium blocks are never split or coalesced in full debug mode

        // Should the block be split?
        if LBlockSize >= CMaximumSmallBlockPoolSize then
        begin
          // Get the size of the second split
          LSecondSplitSize := LBlockSize - LPSmallBlockType.OptimalBlockPoolSize;

          // Adjust the block size
          LBlockSize := LPSmallBlockType.OptimalBlockPoolSize;

          // Split the block in two
          LSecondSplit := PMediumFreeBlock(NativeUInt(LMediumBlock) + LBlockSize);
          PNativeUInt(NativeUInt(LSecondSplit) - CBlockHeaderSize)^ :=
            APool.Index or LSecondSplitSize or CIsMediumBlockFlag or CIsFreeBlockFlag;

          // Store the size of the second split as the second last dword/qword
          PNativeUInt(NativeUInt(LSecondSplit) + LSecondSplitSize - 2 * CBlockHeaderSize)^ :=
            APool.Index or LSecondSplitSize;

          // Put the remainder in a bin (it will be big enough)
          InsertMediumBlockIntoBin(APool, LSecondSplit, LSecondSplitSize);
        end
        else
        begin
          // Mark this block as used in the block following it
          LNextMediumBlockSizeAndFlags := PNativeUInt(NativeUInt(LMediumBlock) + LBlockSize - CBlockHeaderSize);
          LNextMediumBlockSizeAndFlags^ := LNextMediumBlockSizeAndFlags^ and (not CPreviousMediumBlockIsFreeFlag);
        end;
      end
      else
      begin
        // Check the sequential feed medium block pool for space
        LSequentialFeedFreeSize := APool.MediumSequentialFeedBytesLeft;
        if LSequentialFeedFreeSize >= LPSmallBlockType.MinimumBlockPoolSize then
        begin
          // Enough sequential feed space: Will the remainder be usable?
          if LSequentialFeedFreeSize >= (LPSmallBlockType.OptimalBlockPoolSize + CMinimumMediumBlockSize) then
            LBlockSize := LPSmallBlockType.OptimalBlockPoolSize
          else
            LBlockSize := LSequentialFeedFreeSize;

          // Get the block
          LMediumBlock := Pointer(NativeUInt(APool.LastSequentiallyFedMediumBlock) - LBlockSize);

          // Update the sequential feed parameters
          APool.LastSequentiallyFedMediumBlock := LMediumBlock;
          APool.MediumSequentialFeedBytesLeft := LSequentialFeedFreeSize - LBlockSize;
        end
        else
        begin
          // Need to allocate a new sequential feed medium block pool: use the
          // optimal size for this small block pool
          LBlockSize := LPSmallBlockType.OptimalBlockPoolSize;

          // Allocate the medium block pool
          LMediumBlock := AllocNewSequentialFeedMediumPool(APool, LBlockSize);

          // Out of memory?
          if LMediumBlock = nil then
          begin
            // Unlock the medium blocks
            //LockRelease(@APool.MediumBlocksLocked);
            APool.MediumBlocksLocked := 0;

            // Unlock the block type
            //LockRelease(@LPSmallBlockType.BlockTypeLocked);
            LPSmallBlockType.BlockTypeLocked := 0;

            // Failed
            Result := nil;
            Exit;
          end;

{$ifdef F4mDebugManager}
          if not MarkMemoryUsed(APool, LMediumBlock) then
            WriteError(LMediumBlock, 'AllocNewSequentialFeedMediumPool-Small');
{$endif}
        end;
      end;

      // Mark this block as in use

      // Set the size and flags for this block
      PNativeUInt(NativeUInt(LMediumBlock) - CBlockHeaderSize)^ :=
        APool.Index or LBlockSize or CIsMediumBlockFlag or CIsSmallBlockPoolInUseFlag;

      // Unlock medium blocks
      //LockRelease(@APool.MediumBlocksLocked);
      APool.MediumBlocksLocked := 0;

      // Set up the block pool
      LPSmallBlockPool := PSmallBlockPoolHeader(LMediumBlock);
      LPSmallBlockPool.BlockType := LPSmallBlockType;
      LPSmallBlockPool.FirstFreeBlock := nil;
      LPSmallBlockPool.BlocksInUse := 1;

      // Set it up for sequential block serving
      LPSmallBlockType.CurrentSequentialFeedPool := LPSmallBlockPool;
      Result := Pointer(NativeUInt(LPSmallBlockPool) + CSmallBlockPoolHeaderSize);
      LPSmallBlockType.NextSequentialFeedBlockAddress :=
        Pointer(NativeUInt(Result) + LPSmallBlockType.BlockSize);
      LPSmallBlockType.MaxSequentialFeedBlockAddress :=
        Pointer(NativeUInt(LPSmallBlockPool) + LBlockSize - LPSmallBlockType.BlockSize);

{$ifdef F4mDebugManager}
      WriteGetMemSmall(@ThreadPools[0], Result, LPSmallBlockPool, LPSmallBlockType, Size);
      if not MarkMemoryUsed(@ThreadPools[0], Result) then
        WriteGetMemErrorSmall(@ThreadPools[0], Result, LPSmallBlockPool, LPSmallBlockType, Size);
{$endif}
    end;
  end;

  // Unlock the block type
  //LockRelease(@LPSmallBlockType.BlockTypeLocked);
  LPSmallBlockType.BlockTypeLocked := 0;

  // Set the block header
  PNativeUInt(NativeUInt(Result) - CBlockHeaderSize)^ := NativeUInt(LPSmallBlockPool);
end;

function FreeMemSmall(const P: Pointer): Integer;
var
  LPSmallBlockPool, LPPreviousPool, LPNextPool, LPOldFirstPool: PSmallBlockPoolHeader;
  LPSmallBlockType: PSmallBlockType;
  LOldFirstFreeBlock: Pointer;
begin
  Assert(P <> nil);

{$ifdef F4mDebugManager}
  WriteTrace(P, 'FreeMemSmall');
{$endif}

  // Get a pointer to the block pool
  LPSmallBlockPool := PSmallBlockPoolHeader(PNativeUInt(NativeUInt(P) - CBlockHeaderSize)^);

  // Get the block type
  LPSmallBlockType := LPSmallBlockPool.BlockType;

{$ifdef F4mDebugManager}
  if not UnmarkMemoryUsed(@ThreadPools[0], P) then
    WriteFreeMemErrorSmall(@ThreadPools[0], P, LPSmallBlockPool, LPSmallBlockType);
{$endif}

  // Lock the block type
  LockAcquire(@LPSmallBlockType.BlockTypeLocked);

  // Get the old first free block
  LOldFirstFreeBlock := LPSmallBlockPool.FirstFreeBlock;

  // Was the pool manager previously full?
  if LOldFirstFreeBlock = nil then
  begin
    // Insert this as the first partially free pool for the block size
    LPOldFirstPool := LPSmallBlockType.NextPartiallyFreePool;
    LPSmallBlockPool.NextPartiallyFreePool := LPOldFirstPool;
    LPOldFirstPool.PreviousPartiallyFreePool := LPSmallBlockPool;
    LPSmallBlockPool.PreviousPartiallyFreePool := PSmallBlockPoolHeader(LPSmallBlockType);
    LPSmallBlockType.NextPartiallyFreePool := LPSmallBlockPool;
  end;

  // Store the old first free block
  PNativeUInt(NativeUInt(P) - CBlockHeaderSize)^ := NativeUInt(LOldFirstFreeBlock) or CIsFreeBlockFlag;

  // Store this as the new first free block
  LPSmallBlockPool.FirstFreeBlock := P;

  // Decrement the number of allocated blocks
  Dec(LPSmallBlockPool.BlocksInUse);

  // Is the entire pool now free? -> Free it.
  if LPSmallBlockPool.BlocksInUse = 0 then
  begin
    // Get the previous and next chunk managers
    LPPreviousPool := LPSmallBlockPool.PreviousPartiallyFreePool;
    LPNextPool := LPSmallBlockPool.NextPartiallyFreePool;

    // Remove this manager
    LPPreviousPool.NextPartiallyFreePool := LPNextPool;
    LPNextPool.PreviousPartiallyFreePool := LPPreviousPool;

    // Is this the sequential feed pool? If so, stop sequential feeding
    if LPSmallBlockType.CurrentSequentialFeedPool = LPSmallBlockPool then
      LPSmallBlockType.MaxSequentialFeedBlockAddress := nil;

    // Unlock this block type
    //LockRelease(@LPSmallBlockType.BlockTypeLocked);
    LPSmallBlockType.BlockTypeLocked := 0;

    // No longer a small block pool in use (the flag must be reset in the
    // pascal version, since IsSmallBlockPoolInUseFlag = IsLargeBlockFlag)
    // Can skip this setting if calling FreeMemMedium without checking the flag
    PNativeUInt(NativeUInt(LPSmallBlockPool) - CBlockHeaderSize)^ :=
      PNativeUInt(NativeUInt(LPSmallBlockPool) - CBlockHeaderSize)^ and (not CIsSmallBlockPoolInUseFlag);

    // Release this pool
    Result := FFreeMem(LPSmallBlockPool);
    //Result := FreeMemMedium(LPSmallBlockPool);
  end
  else
  begin
    // Unlock this block type
    //LockRelease(@LPSmallBlockType.BlockTypeLocked);
    LPSmallBlockType.BlockTypeLocked := 0;

    // No error
    Result := CResultOK;
  end;
end;

function ReallocMemSmall(const P: Pointer; const Size: NativeUInt): Pointer;
var
  LPSmallBlockPool: PSmallBlockPoolHeader;
  LPSmallBlockType: PSmallBlockType;
  LNewAllocSize: NativeUInt;
  LOldAvailableSize: UInt32;
begin
  Assert(P <> nil);

  // Get a pointer to the block pool
  LPSmallBlockPool := PSmallBlockPoolHeader(PNativeUInt(NativeUInt(P) - CBlockHeaderSize)^);

  // Get the block type
  LPSmallBlockType := LPSmallBlockPool.BlockType;

  // Get the available size inside blocks of this type.
  LOldAvailableSize := LPSmallBlockType.BlockSize - CBlockHeaderSize;

{$ifdef F4mDebugManager}
  if not IsMemoryUsed(@ThreadPools[0], P) then
    WriteReallocMemErrorSmall(@ThreadPools[0], P, LPSmallBlockPool, LPSmallBlockType, LOldAvailableSize, Size);
{$endif}

  // Is it an upsize?
  if Size > LOldAvailableSize then
  begin
{$ifdef F4mReallocUpsize}
    // This pointer is being reallocated to a larger block and therefore it is
    // logical to assume that it may be enlarged again. Since reallocations are
    // expensive, there is a minimum upsize percentage to avoid unnecessary
    // future move operations.

    // Must grow with at least 100% + x bytes
    LNewAllocSize := (LOldAvailableSize shl 1) + CSmallBlockUpsizeAdder;

    // Still not large enough?
    if LNewAllocSize < Size then
      LNewAllocSize := Size;
{$else}
    // Need round up for the special move
    LNewAllocSize := (Size + (CMinimumBlockAlignment - 1)) and -CMinimumBlockAlignment;
{$endif}

    // Allocate the new block
    Result := FGetMem(LNewAllocSize);

    // Allocated OK?
    if Result <> nil then
    begin
      Assert(Result <> P);
      Assert((PNativeUInt(NativeUInt(Result) - CBlockHeaderSize)^ and CIsFreeBlockFlag) = 0);

      // Move the data across
{$ifdef F4mUseCustomFixedSizeMoveRoutines}
      LPSmallBlockType.UpsizeMoveProcedure(P^, Result^, LOldAvailableSize);
{$else}
      Move(P^, Result^, LOldAvailableSize);
{$endif}

      // Free the old pointer
      FreeMemSmall(P);
    end;
  end
  else
  begin
    // Must be less than half the current size + SmallBlockDownsizeCheckAdder or we
    // don't bother resizing.
{$ifdef F4mReallocUpsize}
    if (Size + CSmallBlockDownsizeCheckAdder) > (LOldAvailableSize shr 1) then
    begin
      Result := P;
      Exit;
    end;
{$else}
    if Size > (LOldAvailableSize shr 1) then
    begin
      Result := P;
      Exit;
    end;
{$endif}

    // Need round up for the special move
    LNewAllocSize := (Size + (CMinimumBlockAlignment - 1)) and -CMinimumBlockAlignment;

    // Allocate a smaller block
    Result := FGetMem(LNewAllocSize);

    // Allocated OK?
    if Result <> nil then
    begin
      Assert(Result <> P);
      Assert((PNativeUInt(NativeUInt(Result) - CBlockHeaderSize)^ and CIsFreeBlockFlag) = 0);

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

      // Free the old pointer
      FreeMemSmall(P);
    end;
  end;
end;

procedure FreeAllMemorySmall;
  procedure FreeAllMemorySmallPool(const APool: PThreadPool);
  var
    J: Int32;
  begin
    for J := 0 to High(APool.SmallBlockTypes) do
    begin
      APool.SmallBlockTypes[J].PreviousPartiallyFreePool := Pointer(@APool.SmallBlockTypes[J]);
      APool.SmallBlockTypes[J].NextPartiallyFreePool := Pointer(@APool.SmallBlockTypes[J]);
      APool.SmallBlockTypes[J].NextSequentialFeedBlockAddress := Pointer(1);
      APool.SmallBlockTypes[J].MaxSequentialFeedBlockAddress := nil;
    end;
  end;
var
  I: Int32;
begin
  for I := 0 to High(ThreadPools) do
    FreeAllMemorySmallPool(@ThreadPools[I]);
end;

procedure InitializeMemorySmall;
  procedure BuildBlockTypeLookupTablesPool(const APool: PThreadPool);
  var
    LStartIndex, LNextStartIndex: UInt32;
    J: Int32;
    LBlockTypeVal: Byte;
  begin
    LStartIndex := 0;
    for J := 0 to High(APool.SmallBlockTypes) do
    begin
      APool.SmallBlockTypes[J].BlockSize := CSmallBlockSizes[J].BlockSize;
      
      // Is this a valid block type for the alignment restriction?
      if (CMinimumBlockAlignment = 8) or ((APool.SmallBlockTypes[J].BlockSize and 15) = 0) then
      begin
        LNextStartIndex := APool.SmallBlockTypes[J].BlockSize div CSmallBlockGranularity;

        // Store the block type index * 4 in the appropriate slots.
        LBlockTypeVal := J * 4;
        while LStartIndex < LNextStartIndex do
        begin
          APool.AllocSize2SmallBlockTypeIndX4[LStartIndex] := LBlockTypeVal;
          Inc(LStartIndex);
        end;

        // Set the start of the next block type
        LStartIndex := LNextStartIndex;
      end;
    end;
  end;

  procedure InitializeMemorySmallPool(const APool: PThreadPool);
  var
    LMinimumPoolSize, LOptimalPoolSize, LGroupNumber, LBlocksPerPool: UInt32;
    J: Int32;
  begin
    for J := 0 to High(APool.SmallBlockTypes) do
    begin
      // The upsize move procedure may move chunks in 16 bytes even with 8-byte
      // alignment, since the new size will always be at least 8 bytes bigger than
      // the old size.

{$ifdef F4mUseCustomFixedSizeMoveRoutines}
      APool.SmallBlockTypes[J].UpsizeMoveProcedure := CSmallBlockSizes[J].UpsizeMoveProcedure;
      if not Assigned(APool.SmallBlockTypes[J].UpsizeMoveProcedure) then
  {$ifdef F4mUseCustomVariableSizeMoveRoutines}
        APool.SmallBlockTypes[J].UpsizeMoveProcedure := MoveX16LP;
  {$else}
        APool.SmallBlockTypes[J].UpsizeMoveProcedure := Move;
  {$endif}
{$endif}

      // Set the first "available pool" to the block type itself, so that the
      // allocation routines know that there are currently no pools with free
      // blocks of this size.
      APool.SmallBlockTypes[J].PreviousPartiallyFreePool := Pointer(@APool.SmallBlockTypes[J]);
      APool.SmallBlockTypes[J].NextPartiallyFreePool := Pointer(@APool.SmallBlockTypes[J]);

      // Cannot sequential feed yet: Ensure that the next address is greater than the maximum address
      APool.SmallBlockTypes[J].MaxSequentialFeedBlockAddress := Pointer(0);
      APool.SmallBlockTypes[J].NextSequentialFeedBlockAddress := Pointer(1);

      // Get the mask to use for finding a medium block suitable for a block pool
      LMinimumPoolSize :=
        ((APool.SmallBlockTypes[J].BlockSize * CMinimumSmallBlocksPerPool
          + (CSmallBlockPoolHeaderSize + CMediumBlockGranularity - 1 - CMediumBlockSizeOffset))
        and -CMediumBlockGranularity) + CMediumBlockSizeOffset;
      if LMinimumPoolSize < CMinimumMediumBlockSize then
        LMinimumPoolSize := CMinimumMediumBlockSize;

      // Get the closest group number for the minimum pool size - todo check for expression
      LGroupNumber := (LMinimumPoolSize + (- CMinimumMediumBlockSize + CMediumBlockBinsPerGroup * CMediumBlockGranularity div 2))
        div (CMediumBlockBinsPerGroup * CMediumBlockGranularity);

      // Too large?
      if LGroupNumber > 7 then
        LGroupNumber := 7;

      // Set the bitmap
      APool.SmallBlockTypes[J].AllowedGroupsForBlockPoolBitmap := Byte(Byte(-1) shl LGroupNumber);

      // Set the minimum pool size
      APool.SmallBlockTypes[J].MinimumBlockPoolSize := CMinimumMediumBlockSize + LGroupNumber *
        (CMediumBlockBinsPerGroup * CMediumBlockGranularity);

      // Get the optimal block pool size
      LOptimalPoolSize := ((APool.SmallBlockTypes[J].BlockSize * CTargetSmallBlocksPerPool
          + (CSmallBlockPoolHeaderSize + CMediumBlockGranularity - 1 - CMediumBlockSizeOffset))
        and -CMediumBlockGranularity) + CMediumBlockSizeOffset;

      // Limit the optimal pool size to within range
      if LOptimalPoolSize < COptimalSmallBlockPoolSizeLowerLimit then
        LOptimalPoolSize := COptimalSmallBlockPoolSizeLowerLimit;
      if LOptimalPoolSize > COptimalSmallBlockPoolSizeUpperLimit then
        LOptimalPoolSize := COptimalSmallBlockPoolSizeUpperLimit;

      // How many blocks will fit in the adjusted optimal size?
      LBlocksPerPool := (LOptimalPoolSize - CSmallBlockPoolHeaderSize) div APool.SmallBlockTypes[J].BlockSize;

      // Recalculate the optimal pool size to minimize wastage due to a partial last block.
      APool.SmallBlockTypes[J].OptimalBlockPoolSize :=
        ((LBlocksPerPool * APool.SmallBlockTypes[J].BlockSize + (CSmallBlockPoolHeaderSize + CMediumBlockGranularity - 1 - CMediumBlockSizeOffset))
          and -CMediumBlockGranularity) + CMediumBlockSizeOffset;
    end;
  end;

var
  I: Int32;
begin
  for I := 0 to High(ThreadPools) do
  begin
    BuildBlockTypeLookupTablesPool(@ThreadPools[I]);
    InitializeMemorySmallPool(@ThreadPools[I]);
  end;
end;

initialization
  InitializeMemorySmall;

end.
