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

unit FState;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil; // System

// Returns statistics about the current state of the memory manager
procedure FGetMemoryManagerState(out AMemoryManagerState: TMemoryManagerState);

// Gets the state of every 64K block in the 4GB address space for 32-bit, and the
// low 4GB of the address space under 64-bit.
procedure FGetMemoryMap(out AMemoryMap: TMemoryMap);

// Returns summarised information about the state of the memory manager.
function FGetHeapStatus: THeapStatus;


implementation

uses
  FVirtual, FMemory, FMemorySmall;

// Returns statistics about the current state of the memory manager
procedure FGetMemoryManagerState(out AMemoryManagerState: TMemoryManagerState);
  procedure GetMemoryManagerStatePool(APool: PThreadPool);
  var
    LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
    LPMediumBlock: Pointer;
    LPLargeBlock: PLargeBlockHeader;
    LBlockSizeAndFlags, LBlockSize: NativeUInt;
    LBlockTypeIndex: UInt32;
  begin
    // Lock all small block types
    LockAllSmallBlockTypes(APool);

    // Lock the medium blocks
    LockAcquire(@APool.MediumBlocksLocked);

    // Step through all the medium block pools
    LPMediumBlockPoolHeader := APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @APool.MediumBlockPoolsCircularList do
    begin
      // Add to the medium block used space
      Inc(AMemoryManagerState.ReservedMediumBlockAddressSpace, MediumBlockPoolSize);
      LPMediumBlock := GetFirstMediumBlockInPool(APool, LPMediumBlockPoolHeader);
      while LPMediumBlock <> nil do
      begin
        LBlockSizeAndFlags := PNativeUInt(NativeUInt(LPMediumBlock) - BlockHeaderSize)^;

        // Is the block in use?
        if LBlockSizeAndFlags and IsFreeBlockFlag = 0 then
        begin
          // Get the block size
          LBlockSize := LBlockSizeAndFlags and ExtractMediumSizeMask;
          if (LBlockSizeAndFlags and IsSmallBlockPoolInUseFlag) <> 0 then
          begin
            // Get the block type index
            LBlockTypeIndex := (NativeUInt(PSmallBlockPoolHeader(LPMediumBlock).BlockType)
              - NativeUInt(@APool.SmallBlockTypes[0])) div SizeOf(TSmallBlockType);

            // Subtract from medium block usage
            Dec(AMemoryManagerState.ReservedMediumBlockAddressSpace, LBlockSize);

            // Add it to the reserved space for the block size
            Inc(AMemoryManagerState.SmallBlockTypeStates[LBlockTypeIndex].ReservedAddressSpace, LBlockSize);

            // Add the usage for the pool
            Inc(AMemoryManagerState.SmallBlockTypeStates[LBlockTypeIndex].AllocatedBlockCount,
              PSmallBlockPoolHeader(LPMediumBlock).BlocksInUse);
          end
          else
          begin
            Inc(AMemoryManagerState.AllocatedMediumBlockCount);
            Inc(AMemoryManagerState.TotalAllocatedMediumBlockSize, LBlockSize - BlockHeaderSize);
          end;
        end;

        // Next medium block
        LPMediumBlock := NextMediumBlock(LPMediumBlock);
      end;

      // Get the next medium block pool
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
    end;

    // Unlock medium blocks
    LockRelease(@APool.MediumBlocksLocked);

    // Unlock all the small block types
    UnlockAllSmallBlockTypes(APool);

    // Step through all the large blocks
    LockAcquire(@APool.LargeBlocksLocked);

    LPLargeBlock := APool.LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @APool.LargeBlocksCircularList do
    begin
      LBlockSize := LPLargeBlock.BlockSizeAndFlags and ExtractLargeSizeMask;
      Inc(AMemoryManagerState.AllocatedLargeBlockCount);
      Inc(AMemoryManagerState.ReservedLargeBlockAddressSpace, LBlockSize);
      Inc(AMemoryManagerState.TotalAllocatedLargeBlockSize, LPLargeBlock.UserAllocatedSize);

      // Get the next large block
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;

    LockRelease(@APool.LargeBlocksLocked);
  end;
var
  I: Int32;
begin
  // Clear the results
  FillChar(AMemoryManagerState, SizeOf(AMemoryManagerState), 0);

  // Set the small block size stats
  for I := 0 to (NumSmallBlockTypes - 1) do
  begin
    AMemoryManagerState.SmallBlockTypeStates[I].InternalBlockSize := CSmallBlockSizes[I].BlockSize;
    AMemoryManagerState.SmallBlockTypeStates[I].UseableBlockSize := CSmallBlockSizes[I].BlockSize - BlockHeaderSize;
    if Int32(AMemoryManagerState.SmallBlockTypeStates[I].UseableBlockSize) < 0 then
      AMemoryManagerState.SmallBlockTypeStates[I].UseableBlockSize := 0;
  end;

  for I := 0 to High(ThreadPools) do
    GetMemoryManagerStatePool(@ThreadPools[I]);
end;


// Gets the state of every 64K block in the 4GB address space for 32-bit, and the
// low 4GB of the address space under 64-bit.
procedure FGetMemoryMap(out AMemoryMap: TMemoryMap);
  procedure GetMemoryMapPool(APool: PThreadPool);
  var
    LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
    LPLargeBlock: PLargeBlockHeader;
    LBlockSize: NativeUInt;
    LChunkIndex, LInd: UInt32;
  begin
    // Step through all the medium block pools
    LockAcquire(@APool.MediumBlocksLocked);

    LPMediumBlockPoolHeader := APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @APool.MediumBlockPoolsCircularList do
    begin
      // Add to the medium block used space
      LChunkIndex := NativeUInt(LPMediumBlockPoolHeader) shr 16;
      for LInd := LChunkIndex to (LChunkIndex + (MediumBlockPoolSize - 1) shr 16) do
      begin
        if LChunkIndex > High(AMemoryMap) then
          Break;
        AMemoryMap[LInd] := csAllocated;
      end;

      // Get the next medium block pool
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
    end;

    LockRelease(@APool.MediumBlocksLocked);

    // Step through all the large blocks
    LockAcquire(@APool.LargeBlocksLocked);

    LPLargeBlock := APool.LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @APool.LargeBlocksCircularList do
    begin
      LChunkIndex := NativeUInt(LPLargeBlock) shr 16;
      LBlockSize := LPLargeBlock.BlockSizeAndFlags and ExtractLargeSizeMask;
      for LInd := LChunkIndex to (LChunkIndex + (LBlockSize - 1) shr 16) do
      begin
        if LChunkIndex > High(AMemoryMap) then
          Break;
        AMemoryMap[LInd] := csAllocated;
      end;

      // Get the next large block
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;

    LockRelease(@APool.LargeBlocksLocked);
  end;

var
  MemSegmentBase: Pointer;
  MemSegmentSize: NativeUInt;
  I: Int32;
begin
  // Clear the map
  FillChar(AMemoryMap, SizeOf(AMemoryMap), Ord(csUnallocated));

  for I := 0 to High(ThreadPools) do
    GetMemoryMapPool(@ThreadPools[I]);

  // Fill in the rest of the map
  for I := 0 to High(TMemoryMap) do
  begin
    // If the chunk is not allocated by this Memory Manager, what is its status?
    if AMemoryMap[I] = csUnallocated then
    begin
      // Get all the reserved memory blocks and allocated memory blocks, etc.
      AMemoryMap[I] := OSQuery(Pointer(I * 65536), MemSegmentSize, MemSegmentBase);
    end;
  end;
end;

// Returns summarised information about the state of the memory manager.
function FGetHeapStatus: THeapStatus;
  procedure GetHeapStatusPool(APool: PThreadPool; var Status: THeapStatus);
  var
    LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
    LPMediumBlock: Pointer;
    LPLargeBlock: PLargeBlockHeader;
    LBlockSizeAndFlags, LBlockSize: NativeUInt;
    LBlockTypeIndex, LSmallBlockUsage, LSmallBlockOverhead: UInt32;
  begin
    // Lock all small block types
    LockAllSmallBlockTypes(APool);

    // Lock the medium blocks
    LockAcquire(@APool.MediumBlocksLocked);

    // Step through all the medium block pools
    LPMediumBlockPoolHeader := APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @APool.MediumBlockPoolsCircularList do
    begin
      // Add to the total and committed address space
      Inc(Status.TotalAddrSpace, ((MediumBlockPoolSize + $ffff) and $ffff0000));
      Inc(Status.TotalCommitted, ((MediumBlockPoolSize + $ffff) and $ffff0000));

      // Add the medium block pool overhead
      Inc(Status.Overhead, (((MediumBlockPoolSize + $ffff) and $ffff0000)
        - MediumBlockPoolSize + MediumBlockPoolHeaderSize));

      // Get the first medium block in the pool
      LPMediumBlock := GetFirstMediumBlockInPool(APool, LPMediumBlockPoolHeader);
      while LPMediumBlock <> nil do
      begin
        // Get the block header
        LBlockSizeAndFlags := PNativeUInt(NativeUInt(LPMediumBlock) - BlockHeaderSize)^;

        // Get the block size
        LBlockSize := LBlockSizeAndFlags and ExtractMediumSizeMask;

        // Is the block in use?
        if (LBlockSizeAndFlags and IsFreeBlockFlag) = 0 then
        begin
          if (LBlockSizeAndFlags and IsSmallBlockPoolInUseFlag) <> 0 then
          begin
            // Get the block type index
            LBlockTypeIndex := (NativeUInt(PSmallBlockPoolHeader(LPMediumBlock).BlockType)
              - NativeUInt(@APool.SmallBlockTypes[0])) div SizeOf(TSmallBlockType);

            // Get the usage in the block
            LSmallBlockUsage := PSmallBlockPoolHeader(LPMediumBlock).BlocksInUse
              * APool.SmallBlockTypes[LBlockTypeIndex].BlockSize;

            // Get the total overhead for all the small blocks
            LSmallBlockOverhead := PSmallBlockPoolHeader(LPMediumBlock).BlocksInUse
              * BlockHeaderSize;

            // Add to the totals
            Inc(Status.FreeSmall, LBlockSize - LSmallBlockUsage - BlockHeaderSize);
            Inc(Status.Overhead, LSmallBlockOverhead + BlockHeaderSize);
            Inc(Status.TotalAllocated, LSmallBlockUsage - LSmallBlockOverhead);
          end
          else
          begin
            // Add to the result
            Inc(Status.TotalAllocated, LBlockSize - BlockHeaderSize);
            Inc(Status.Overhead, BlockHeaderSize);
          end;
        end
        else
        begin
          // The medium block is free
          Inc(Status.FreeBig, LBlockSize);
        end;

        // Next medium block
        LPMediumBlock := NextMediumBlock(LPMediumBlock);
      end;

      // Get the next medium block pool
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
    end;

    // Add the sequential feed unused space
    Inc(Status.Unused, APool.MediumSequentialFeedBytesLeft);

    // Unlock the medium blocks
    LockRelease(@APool.MediumBlocksLocked);

    // Unlock all the small block types
    UnlockAllSmallBlockTypes(APool);

    // Step through all the large blocks
    LockAcquire(@APool.LargeBlocksLocked);

    LPLargeBlock := APool.LargeBlocksCircularList.NextLargeBlockHeader;
    while (LPLargeBlock <> @APool.LargeBlocksCircularList) do
    begin
      LBlockSize := LPLargeBlock.BlockSizeAndFlags and ExtractLargeSizeMask;
      Inc(Status.TotalAddrSpace, LBlockSize);
      Inc(Status.TotalCommitted, LBlockSize);
      Inc(Status.TotalAllocated, LPLargeBlock.UserAllocatedSize);
      Inc(Status.Overhead, LBlockSize - LPLargeBlock.UserAllocatedSize);

      // Get the next large block
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;

    LockRelease(@APool.LargeBlocksLocked);
  end;

var
  I: Int32;
begin
  // Clear the structure
  FillChar(Result, SizeOf(Result), 0);

  for I := 0 to High(ThreadPools) do
    GetHeapStatusPool(@ThreadPools[I], Result);

  // Set the total number of free bytes
  Result.TotalFree := Result.FreeSmall + Result.FreeBig + Result.Unused;
end;

end.
