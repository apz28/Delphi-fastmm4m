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

unit FMemoryMedium;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil; // System

// Allocates a medium block of at least Size (actual size may be larger to
// allow for alignment etc.). Size must be the actual user requested size. This
// procedure will pad it to the appropriate medium size boundary and also add the space
// required by the header.
function GetMemMedium(const APool: PThreadPool; const Size: UInt32): Pointer;

// Frees a medium block, returning ResultOK on success, ResultError otherwise
function FreeMemMedium(P: Pointer): Integer;

// Reallocates a medium block to at least the requested size. Returns the new pointer,
// or nil on error
function ReallocMemMedium(const P: Pointer; Size: NativeUInt): Pointer;

procedure FreeAllMemoryMedium;

implementation

uses
  FVirtual, FMemory
{$ifdef F4mDebugManager}
  , FDebug
{$endif}
  ;

function GetMemMedium(const APool: PThreadPool; const Size: UInt32): Pointer;
var
  LPMediumBin, LSecondSplit: PMediumFreeBlock;
  LNextMediumBlockSizeAndFlags: PNativeUInt;
  LBlockSize, LAvailableBlockSize, LSecondSplitSize, LSequentialFeedFreeSize,
  LBinNumber, LBinGroupsMasked, LBinGroupMasked, LBinGroupNumber: UInt32;
begin
  Assert(APool <> nil);
  Assert(Size > 0);
  Assert(Size <= MaximumMediumBlockUserSize);

  // Get the block size and bin number for this block size. Block sizes are
  // rounded up to the next bin size.
  LBlockSize := ((Size + (MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset))
    and -MediumBlockGranularity) + MediumBlockSizeOffset;

  // Get the bin number
  //LBinNumber := (LBlockSize - MinimumMediumBlockSize) div MediumBlockGranularity;
  LBinNumber := (LBlockSize - MinimumMediumBlockSize) shr MediumBlockGranularityShift;

  // Calculate the bin group
  //LBinGroupNumber := LBinNumber div MediumBlockBinGroupCount;
  LBinGroupNumber := LBinNumber shr MediumBlockBinGroupCountShift;

  // Lock the medium blocks
  LockAcquire(@APool.MediumBlocksLocked);

  // Is there a suitable block inside this group?
  LBinGroupMasked := APool.MediumBlockBinBitmaps[LBinGroupNumber] and -(1 shl (LBinNumber and 31));
  if LBinGroupMasked <> 0 then
  begin
    // Get the actual bin number
    //LBinNumber := FindFirstSetBit(LBinGroupMasked) + (LBinGroupNumber * MediumBlockBinsPerGroup);
    LBinNumber := FindFirstSetBit(LBinGroupMasked) + (LBinGroupNumber shl MediumBlockBinsPerGroupShift);
  end
  else
  begin
    // Try all groups greater than this group
    LBinGroupsMasked := APool.MediumBlockBinGroupBitmap and -(2 shl LBinGroupNumber);
    if LBinGroupsMasked <> 0 then
    begin
      // There is a suitable group with space: get the bin number
      LBinGroupNumber := FindFirstSetBit(LBinGroupsMasked);

      // Get the bin in the group with free blocks
      //LBinNumber := FindFirstSetBit(APool.MediumBlockBinBitmaps[LBinGroupNumber]) + (LBinGroupNumber * MediumBlockBinsPerGroup);
      LBinNumber := FindFirstSetBit(APool.MediumBlockBinBitmaps[LBinGroupNumber]) + (LBinGroupNumber shl MediumBlockBinsPerGroupShift);
    end
    else
    begin
      // There are no bins with a suitable block: Sequentially feed the required block
      LSequentialFeedFreeSize := APool.MediumSequentialFeedBytesLeft;
      if LSequentialFeedFreeSize >= LBlockSize then
      begin
        // Block can be fed sequentially
        Result := Pointer(NativeUInt(APool.LastSequentiallyFedMediumBlock) - LBlockSize);

        // Store the last sequentially fed block
        APool.LastSequentiallyFedMediumBlock := Result;

        // Store the remaining bytes
        APool.MediumSequentialFeedBytesLeft := LSequentialFeedFreeSize - LBlockSize;

        // Set the Pool and flags for the block
        PNativeUInt(NativeUInt(Result) - BlockHeaderSize)^ :=
          APool.IndexFlag or LBlockSize or IsMediumBlockFlag;
      end
      else
      begin
        // Need to allocate a new sequential feed block
        Result := AllocNewSequentialFeedMediumPool(APool, LBlockSize);
      end;

{$ifdef F4mDebugManager}
      if Result <> nil then
      begin
        WriteGetMemMedium(APool, Result, LBinGroupMasked, LBinNumber, LBlockSize, Size);
        if not MarkMemoryUsed(APool, Result) then
          WriteGetMemErrorMedium(APool, Result, LBinGroupMasked, LBinNumber, LBlockSize, Size);
      end;
{$endif}

      // Done
      LockRelease(@APool.MediumBlocksLocked);
      Exit;
    end;
  end;

  // If we get here we have a valid LBinGroupNumber and LBinNumber:
  // Use the first block in the bin, splitting it if necessary

  // Get a pointer to the bin
  LPMediumBin := @APool.MediumBlockBins[LBinNumber];

  // Get the result
  Result := LPMediumBin.NextFreeBlock;

  // Remove the block from the bin containing it
  RemoveMediumFreeBlock(APool, Result);

  // Get the block size
  LAvailableBlockSize := PNativeUInt(NativeUInt(Result) - BlockHeaderSize)^ and ExtractMediumSizeMask;

  // Is it an exact fit or not?
  LSecondSplitSize := LAvailableBlockSize - LBlockSize;
  if LSecondSplitSize <> 0 then
  begin
    // Split the block in two
    LSecondSplit := PMediumFreeBlock(NativeUInt(Result) + LBlockSize);

    // Set the size of the second split
    PNativeUInt(NativeUInt(LSecondSplit) - BlockHeaderSize)^ :=
      APool.IndexFlag or LSecondSplitSize or IsMediumBlockFlag or IsFreeBlockFlag;

    // Store the size of the second split
    PNativeUInt(NativeUInt(LSecondSplit) + LSecondSplitSize - 2 * BlockHeaderSize)^ :=
      APool.IndexFlag or LSecondSplitSize;

    // Put the remainder in a bin if it is big enough
    if LSecondSplitSize >= MinimumMediumBlockSize then
      InsertMediumBlockIntoBin(APool, LSecondSplit, LSecondSplitSize);
  end
  else
  begin
    // Mark this block as used in the block following it
    LNextMediumBlockSizeAndFlags := Pointer(NativeUInt(Result) + LBlockSize - BlockHeaderSize);
    LNextMediumBlockSizeAndFlags^ := LNextMediumBlockSizeAndFlags^ and (not PreviousMediumBlockIsFreeFlag);
  end;

  // Set the size and flags for this block
  PNativeUInt(NativeUInt(Result) - BlockHeaderSize)^ :=
    APool.IndexFlag or LBlockSize or IsMediumBlockFlag;

  // Unlock the medium blocks
  LockRelease(@APool.MediumBlocksLocked);

{$ifdef F4mDebugManager}
  WriteGetMemMedium(APool, Result, LBinGroupMasked, LBinNumber, LBlockSize, Size);
  if not MarkMemoryUsed(APool, Result) then
    WriteGetMemErrorMedium(APool, Result, LBinGroupMasked, LBinNumber, LBlockSize, Size);
{$endif}
end;

function FreeMemMedium(P: Pointer): Integer;
var
  Pool: PThreadPool;
  LNextMediumBlock, LPreviousMediumBlock: PMediumFreeBlock;
  LPPreviousMediumBlockPoolHeader, LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LNextMediumBlockSizeAndFlags, LBlockHeaderFlags: NativeUInt;
  LBlockSize, LPreviousMediumBlockSize: UInt32;
begin
  Assert(P <> nil);

{$ifdef F4mDebugManager}
  WriteTrace(P, 'FreeMemMedium');
{$endif}

  // Get the small block header: Is it actually a small block?
  LBlockHeaderFlags := PNativeUInt(NativeUInt(P) - BlockHeaderSize)^;
  Pool := @ThreadPools[LBlockHeaderFlags shr MediumSlotIndexShift];
  Assert(Pool <> nil);

{$ifdef F4mTestThreadPool}
  Assert(Pool = @ThreadPools[0]);
{$endif}

  // Get the medium block size
  LBlockSize := LBlockHeaderFlags and ExtractMediumSizeMask;

{$ifdef F4mDebugManager}
  if not UnmarkMemoryUsed(Pool, P) then
    WriteFreeMemErrorMedium(Pool, P, LBlockHeaderFlags);
{$endif}

  // Lock the medium blocks
  LockAcquire(@Pool.MediumBlocksLocked);

  // Can we combine this block with the next free block?
  LNextMediumBlock := PMediumFreeBlock(NativeUInt(P) + LBlockSize);
  LNextMediumBlockSizeAndFlags := PNativeUInt(NativeUInt(LNextMediumBlock) - BlockHeaderSize)^;
  if (LNextMediumBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
  begin
    // Increase the size of this block
    Inc(LBlockSize, LNextMediumBlockSizeAndFlags and ExtractMediumSizeMask);

    // Remove the next block as well
    if (LNextMediumBlockSizeAndFlags and ExtractMediumSizeMask) >= MinimumMediumBlockSize then
      RemoveMediumFreeBlock(Pool, LNextMediumBlock);
  end
  else
  begin
    // Reset the "previous in use" flag of the next block
    PNativeUInt(NativeUInt(LNextMediumBlock) - BlockHeaderSize)^ :=
      LNextMediumBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
  end;

  // Can we combine this block with the previous free block? We need to
  // re-read the flags since it could have changed before we could lock the medium blocks.
  if (PNativeUInt(NativeUInt(P) - BlockHeaderSize)^ and PreviousMediumBlockIsFreeFlag) <> 0 then
  begin
    // Get the size of the free block just before this one
    LPreviousMediumBlockSize := PNativeUInt(NativeUInt(P) - 2 * BlockHeaderSize)^ and ExtractMediumSizeMask;

    // Get the start of the previous block
    LPreviousMediumBlock := PMediumFreeBlock(NativeUInt(P) - LPreviousMediumBlockSize);

    // Set the new block size
    Inc(LBlockSize, LPreviousMediumBlockSize);

    // This is the new current block
    P := LPreviousMediumBlock;

    // Remove the previous block from the linked list
    if LPreviousMediumBlockSize >= MinimumMediumBlockSize then
      RemoveMediumFreeBlock(Pool, LPreviousMediumBlock);
  end;

  // Is the entire medium block pool free, and there are other free blocks
  // that can fit the largest possible medium block? -> free it.
  if LBlockSize <> (MediumBlockPoolSize - MediumBlockPoolHeaderSize) then
  begin
    // Store the size of the block as well as the flags
    PNativeUInt(NativeUInt(P) - BlockHeaderSize)^ :=
      Pool.IndexFlag or LBlockSize or IsMediumBlockFlag or IsFreeBlockFlag;

    // Store the trailing size marker
    PNativeUInt(NativeUInt(P) + LBlockSize - 2 * BlockHeaderSize)^ :=
      Pool.IndexFlag or LBlockSize;

    // Insert this block back into the bins: Size check not required here,
    // since medium blocks that are in use are not allowed to be
    // shrunk smaller than MinimumMediumBlockSize
    InsertMediumBlockIntoBin(Pool, P, LBlockSize);

    // Unlock medium blocks
    LockRelease(@Pool.MediumBlocksLocked);

    // All OK
    Result := ResultOK;
  end
  else
  begin
    // Should this become the new sequential feed?
    if Pool.MediumSequentialFeedBytesLeft <> (MediumBlockPoolSize - MediumBlockPoolHeaderSize) then
    begin
      // Bin the current sequential feed
      BinMediumSequentialFeedRemainder(Pool);

      // Set this medium pool up as the new sequential feed pool:
      // Store the sequential feed pool trailer
      PNativeUInt(NativeUInt(P) + LBlockSize - BlockHeaderSize)^ := IsMediumBlockFlag;

      // Store the number of bytes available in the sequential feed chunk
      Pool.MediumSequentialFeedBytesLeft := MediumBlockPoolSize - MediumBlockPoolHeaderSize;

      // Set the last sequentially fed block
      Pool.LastSequentiallyFedMediumBlock := Pointer(NativeUInt(P) + LBlockSize);

      // Unlock medium blocks
      LockRelease(@Pool.MediumBlocksLocked);

      // Success
      Result := ResultOK;
    end
    else
    begin
      // Remove this medium block pool from the linked list
      Dec(NativeUInt(P), MediumBlockPoolHeaderSize);
      LPPreviousMediumBlockPoolHeader := PMediumBlockPoolHeader(P).PreviousMediumBlockPoolHeader;
      LPNextMediumBlockPoolHeader := PMediumBlockPoolHeader(P).NextMediumBlockPoolHeader;
      LPPreviousMediumBlockPoolHeader.NextMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
      LPNextMediumBlockPoolHeader.PreviousMediumBlockPoolHeader := LPPreviousMediumBlockPoolHeader;

      // Unlock medium blocks
      LockRelease(@Pool.MediumBlocksLocked);

      Result := FreeMediumBlockPool(Pool, P);
    end;
  end;
end;

function ReallocMemMedium(const P: Pointer; Size: NativeUInt): Pointer;
var
  Pool: PThreadPool;
  LPNextBlock, LPNextBlockHeader: Pointer;
  LBlockSizeAndFlags, LNextBlockSizeAndFlags, LBlockFlags: NativeUInt;
  LNewAllocSize, LNewAvailableSize, LNewBlockSize: NativeUInt;
  LOldAvailableSize, LNextBlockSize, LSecondSplitSize: UInt32;
{$ifdef F4mReallocUpsize}
  LMinimumUpsize: NativeUInt;
{$endif}

  // Upsizes a large block in-place. The following variables are assumed correct:
  // LBlockFlags, LOldAvailableSize, LPNextBlock, LNextBlockSizeAndFlags,
  // LNextBlockSize, LNewAvailableSize. Medium blocks must be locked on entry if required.
  procedure MediumBlockInPlaceUpsize;
  begin
    // Remove the next block
    if (LNextBlockSizeAndFlags and ExtractMediumSizeMask) >= MinimumMediumBlockSize then
      RemoveMediumFreeBlock(Pool, LPNextBlock);

    LNewAllocSize := Size;
{$ifdef F4mReallocUpsize}
    // Add 25% for medium block in-place upsizes
    LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if LNewAllocSize < LMinimumUpsize then
      LNewAllocSize := LMinimumUpsize;
{$endif}

    // Round up to the nearest block size granularity
    LNewBlockSize := ((LNewAllocSize + (BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
      and -MediumBlockGranularity) + MediumBlockSizeOffset;

    // Calculate the size of the second split
    LSecondSplitSize := LNewAvailableSize + BlockHeaderSize - LNewBlockSize;

    // Does it fit?
    if Int32(LSecondSplitSize) <= 0 then
    begin
      // The block size is the full available size plus header
      LNewBlockSize := LNewAvailableSize + BlockHeaderSize;

      // Grab the whole block: Mark it as used in the block following it
      LPNextBlockHeader := Pointer(NativeUInt(P) + LNewAvailableSize);
      PNativeUInt(LPNextBlockHeader)^ := PNativeUInt(LPNextBlockHeader)^ and (not PreviousMediumBlockIsFreeFlag);
    end
    else
    begin
      // Split the block in two
      LPNextBlock := PMediumFreeBlock(NativeUInt(P) + LNewBlockSize);

      // Set the size of the second split
      PNativeUInt(NativeUInt(LPNextBlock) - BlockHeaderSize)^ :=
        Pool.IndexFlag or LSecondSplitSize or IsMediumBlockFlag or IsFreeBlockFlag;

      // Store the size of the second split before the header of the next block
      PNativeUInt(NativeUInt(LPNextBlock) + LSecondSplitSize - 2 * BlockHeaderSize)^ :=
        Pool.IndexFlag or LSecondSplitSize;

      // Put the remainder in a bin if it is big enough
      if LSecondSplitSize >= MinimumMediumBlockSize then
        InsertMediumBlockIntoBin(Pool, LPNextBlock, LSecondSplitSize);
    end;

    // Set the size and flags for this block
    PNativeUInt(NativeUInt(P) - BlockHeaderSize)^ :=
      Pool.IndexFlag or LNewBlockSize or LBlockFlags;
  end;

  // In-place downsize of a medium block. On entry Size must be less than half of LOldAvailableSize.
  procedure MediumBlockInPlaceDownsize;
  begin
    // Round up to the next medium block size
    LNewBlockSize := ((Size + (BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
      and -MediumBlockGranularity) + MediumBlockSizeOffset;

    // Get the size of the second split
    LSecondSplitSize := (LOldAvailableSize + BlockHeaderSize) - LNewBlockSize;

    // Lock the medium blocks
    LockAcquire(@Pool.MediumBlocksLocked);

    // Set the new size
    PNativeUInt(NativeUInt(P) - BlockHeaderSize)^ :=
      (PNativeUInt(NativeUInt(P) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask)
      or Pool.IndexFlag or LNewBlockSize;

    // Is the next block in use?
    LPNextBlock := PNativeUInt(NativeUInt(P) + LOldAvailableSize + BlockHeaderSize);
    LNextBlockSizeAndFlags := PNativeUInt(NativeUInt(LPNextBlock) - BlockHeaderSize)^;
    if (LNextBlockSizeAndFlags and IsFreeBlockFlag) = 0 then
    begin
      // The next block is in use: flag its previous block as free
      PNativeUInt(NativeUInt(LPNextBlock) - BlockHeaderSize)^ :=
        LNextBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
    end
    else
    begin
      // The next block is free: combine it
      LNextBlockSize := LNextBlockSizeAndFlags and ExtractMediumSizeMask;
      Inc(LSecondSplitSize, LNextBlockSize);
      if LNextBlockSize >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(Pool, LPNextBlock);
    end;

    // Set the split
    LPNextBlock := PNativeUInt(NativeUInt(P) + LNewBlockSize);

    // Store the free part's header
    PNativeUInt(NativeUInt(LPNextBlock) - BlockHeaderSize)^ :=
      Pool.IndexFlag or LSecondSplitSize or IsMediumBlockFlag or IsFreeBlockFlag;

    // Store the trailing size field
    PNativeUInt(NativeUInt(LPNextBlock) + LSecondSplitSize - 2 * BlockHeaderSize)^ :=
      Pool.IndexFlag or LSecondSplitSize;

    // Bin this free block
    if LSecondSplitSize >= MinimumMediumBlockSize then
      InsertMediumBlockIntoBin(Pool, LPNextBlock, LSecondSplitSize);

    // Unlock the medium blocks
    LockRelease(@Pool.MediumBlocksLocked);
  end;

begin
  Assert(P <> nil);

  LBlockSizeAndFlags := PNativeUInt(NativeUInt(P) - BlockHeaderSize)^;
  Pool := @ThreadPools[LBlockSizeAndFlags shr MediumSlotIndexShift];
  Assert(Pool <> nil);

{$ifdef F4mTestThreadPool}
  Assert(Pool = @ThreadPools[0]);
{$endif}

  // What is the available size in the block being reallocated?
  LOldAvailableSize := LBlockSizeAndFlags and ExtractMediumSizeMask;

{$ifdef F4mDebugManager}
  if not IsMemoryUsed(Pool, P) then
    WriteReallocMemErrorMedium(Pool, P, LBlockSizeAndFlags, Size);
{$endif}

  // Get a pointer to the next block
  LPNextBlock := PNativeUInt(NativeUInt(P) + LOldAvailableSize);

  // Subtract the block header size from the old available size
  Dec(LOldAvailableSize, BlockHeaderSize);

  // Is it an upsize?
  if Size > LOldAvailableSize then
  begin
    // Can we do an in-place upsize?
    LNextBlockSizeAndFlags := PNativeUInt(NativeUInt(LPNextBlock) - BlockHeaderSize)^;

    // Is the next block free?
    if (LNextBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
    begin
      LNextBlockSize := LNextBlockSizeAndFlags and ExtractMediumSizeMask;

      // The available size including the next block
      LNewAvailableSize := LOldAvailableSize + LNextBlockSize;

      // Can the block fit?
      if Size <= LNewAvailableSize then
      begin
        // The next block is free and there is enough space to grow this block in place.

        // Multi-threaded application - lock medium blocks and re-read the information on the blocks.
        LockAcquire(@Pool.MediumBlocksLocked);

        // Re-read the info for this block
        LBlockFlags := PNativeUInt(NativeUInt(P) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask;

        // Re-read the info for the next block
        LNextBlockSizeAndFlags := PNativeUInt(NativeUInt(LPNextBlock) - BlockHeaderSize)^;

        // Recalculate the next block size
        LNextBlockSize := LNextBlockSizeAndFlags and ExtractMediumSizeMask;

        // The available size including the next block
        LNewAvailableSize := LOldAvailableSize + LNextBlockSize;

        // Is the next block still free and the size still sufficient?
        if ((LNextBlockSizeAndFlags and IsFreeBlockFlag) <> 0) and (Size <= LNewAvailableSize) then
        begin
          // Upsize the block in-place
          MediumBlockInPlaceUpsize;

          // Unlock the medium blocks
          LockRelease(@Pool.MediumBlocksLocked);

          // Return the result
          Result := P;

          Assert(Result <> nil);
          Assert((PNativeUInt(NativeUInt(Result) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0);

          // Done
          Exit;
        end;

        // Couldn't use the block: Unlock the medium blocks
        LockRelease(@Pool.MediumBlocksLocked);
      end;
    end;

{$ifdef F4mReallocUpsize}
    // Couldn't upsize in place. Grab a new block and move the data across:
    // If we have to reallocate and move medium blocks, we grow by at least 25%
    LNewAllocSize := Size;

    // Still not large enough?
    LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if Size < LMinimumUpsize then
      LNewAllocSize := LMinimumUpsize;
{$else}
    // Need round up for the special move
    LNewAllocSize := (Size + (MinimumBlockAlignment - 1)) and -MinimumBlockAlignment;
{$endif}

    // Allocate the new block
    Result := FGetMemPool(Pool, LNewAllocSize);

    // Allocated OK?
    if Result <> nil then
    begin
      Assert(Result <> P);
      Assert((PNativeUInt(NativeUInt(Result) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0);

      // Move the data across
{$ifdef F4mUseCustomVariableSizeMoveRoutines}
      MoveX16LP(P^, Result^, LOldAvailableSize);
{$else}
      Move(P^, Result^, LOldAvailableSize);
{$endif}

      // Free the old block
      FreeMemMedium(P);
    end;
  end
  else
  begin
    // Must be less than half the current size or we don't bother resizing.
    if Size > (LOldAvailableSize shr 1) then
    begin
      Result := P;
      Exit;
    end;

    // In-place downsize? Balance the cost of moving the data vs. the cost of
    // fragmenting the memory pool. Medium blocks in use may never be smaller
    // than MinimumMediumBlockSize.
    if Size >= (MinimumMediumBlockSize - BlockHeaderSize) then
    begin
      MediumBlockInPlaceDownsize;
      Result := P;

      Assert(Result <> nil);
      Assert((PNativeUInt(NativeUInt(Result) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0);

      Exit;
    end;

    // The requested size is less than the minimum medium block size. If
    // the requested size is less than the threshold value (currently a
    // quarter of the minimum medium block size), move the data to a small
    // block, otherwise shrink the medium block to the minimum allowable
    // medium block size.
    if Size >= MediumInPlaceDownsizeLimit then
    begin
      // The request is for a size smaller than the minimum medium block
      // size, but not small enough to justify moving data: Reduce the
      // block size to the minimum medium block size
      Size := MinimumMediumBlockSize - BlockHeaderSize;

      // Is it already at the minimum medium block size?
      if LOldAvailableSize > Size then
        MediumBlockInPlaceDownsize;

      Result := P;

      Assert(Result <> nil);
      Assert((PNativeUInt(NativeUInt(Result) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0);

      Exit;
    end;

    // Need round up for the special move
    LNewAllocSize := (Size + (MinimumBlockAlignment - 1)) and -MinimumBlockAlignment;

    // Allocate the new block
    Result := FGetMemPool(Pool, LNewAllocSize);

    // Allocated OK?
    if Result <> nil then
    begin
      Assert(Result <> P);
      Assert((PNativeUInt(NativeUInt(Result) - BlockHeaderSize)^ and IsFreeBlockFlag) = 0);

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
      FreeMemMedium(P);
    end;
  end;
end;

procedure FreeAllMemoryMedium;
  procedure FreeAllMemoryMediumPool(APool: PThreadPool);
  var
    LPMediumBlockPoolHeader, LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
    LPMediumFreeBlock: PMediumFreeBlock;
    J: Int32;
  begin
{$ifdef F4mCacheThreadOSAlloc}
    if APool.MediumCachedOSAlloc <> nil then
    begin
      OSFree(APool.MediumCachedOSAlloc);
      APool.MediumCachedOSAlloc := nil;
    end;
{$endif}    

    // Free all block pools
    LPMediumBlockPoolHeader := APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @APool.MediumBlockPoolsCircularList do
    begin
      // Get the next medium block pool so long
      LPNextMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;

      // Free this pool
      OSFree(LPMediumBlockPoolHeader);

      // Next pool
      LPMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
    end;

    // Clear all medium block pools
    APool.MediumBlockPoolsCircularList.PreviousMediumBlockPoolHeader := @APool.MediumBlockPoolsCircularList;
    APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := @APool.MediumBlockPoolsCircularList;

    // All medium bins are empty
    for J := 0 to High(APool.MediumBlockBins) do
    begin
      LPMediumFreeBlock := @APool.MediumBlockBins[J];
      LPMediumFreeBlock.PreviousFreeBlock := LPMediumFreeBlock;
      LPMediumFreeBlock.NextFreeBlock := LPMediumFreeBlock;
    end;
    FillChar(APool.MediumBlockBinBitmaps, SizeOf(APool.MediumBlockBinBitmaps), 0);
    APool.MediumBlockBinGroupBitmap := 0;
    APool.MediumSequentialFeedBytesLeft := 0;
  end;
var
  I: Int32;
begin
  for I := 0 to High(ThreadPools) do
    FreeAllMemoryMediumPool(@ThreadPools[I]);
end;

procedure InitializeMemoryMedium;
  procedure InitializeMemoryMediumPool(APool: PThreadPool);
  var
    LPMediumFreeBlock: PMediumFreeBlock;
    J: Int32;
  begin
    // There are currently no medium block pools
    APool.MediumBlockPoolsCircularList.PreviousMediumBlockPoolHeader := @APool.MediumBlockPoolsCircularList;
    APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := @APool.MediumBlockPoolsCircularList;

    // All medium bins are empty
    for J := 0 to High(APool.MediumBlockBins) do
    begin
      LPMediumFreeBlock := @APool.MediumBlockBins[J];
      LPMediumFreeBlock.PreviousFreeBlock := LPMediumFreeBlock;
      LPMediumFreeBlock.NextFreeBlock := LPMediumFreeBlock;
    end;
  end;
var
  I: Int32;
begin
  for I := 0 to High(ThreadPools) do
    InitializeMemoryMediumPool(@ThreadPools[I]);
end;

initialization
  InitializeMemoryMedium;

end.
