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

unit FMemory;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil; // System

const
  {-----------------------Small block management--------------------------}

  // The small block types. Sizes include the leading block header. Sizes are
  // picked to limit maximum wastage to about 10% or 256 bytes (whichever is less) where possible.
  CSmallBlockSizes: TSmallBlockSizes = (
    // 8 or 16 byte jumps
    (BlockSize: 16{$ifdef F4mUseCustomFixedSizeMoveRoutines};UpsizeMoveProcedure: {$ifdef CPU386)}Move12{$else}Move8{$endif}{$endif}),
{$ifdef CPU386}
    (BlockSize: 24{$ifdef F4mUseCustomFixedSizeMoveRoutines};UpsizeMoveProcedure: Move20{$endif}),
{$endif}
    (BlockSize: 32{$ifdef F4mUseCustomFixedSizeMoveRoutines};UpsizeMoveProcedure: {$ifdef CPU386}Move28{$else}Move24{$endif}{$endif}),
{$ifdef CPU386}
    (BlockSize: 40{$ifdef F4mUseCustomFixedSizeMoveRoutines};UpsizeMoveProcedure: Move36{$endif}),
{$endif}
    (BlockSize: 48{$ifdef F4mUseCustomFixedSizeMoveRoutines};UpsizeMoveProcedure: {$ifdef CPU386}Move44{$else}Move40{$endif}{$endif}),
{$ifdef CPU386}
    (BlockSize: 56{$ifdef F4mUseCustomFixedSizeMoveRoutines};UpsizeMoveProcedure: Move52{$endif}),
{$endif}
    (BlockSize: 64{$ifdef F4mUseCustomFixedSizeMoveRoutines};UpsizeMoveProcedure: {$ifdef CPU386}Move60{$else}Move56{$endif}{$endif}),
{$ifdef CPU386}
    (BlockSize: 72{$ifdef F4mUseCustomFixedSizeMoveRoutines};UpsizeMoveProcedure: Move68{$endif}),
{$endif}
    (BlockSize: 80),
{$ifdef CPU386}
    (BlockSize: 88),
{$endif}
    (BlockSize: 96),
{$ifdef CPU386}
    (BlockSize: 104),
{$endif}
    (BlockSize: 112),
{$ifdef CPU386}
    (BlockSize: 120),
{$endif}
    (BlockSize: 128),
{$ifdef CPU386}
    (BlockSize: 136),
{$endif}
    (BlockSize: 144),
{$ifdef CPU386}
    (BlockSize: 152),
{$endif}
    (BlockSize: 160),

    // 16 byte jumps
    (BlockSize: 176),
    (BlockSize: 192),
    (BlockSize: 208),
    (BlockSize: 224),
    (BlockSize: 240),
    (BlockSize: 256),
    (BlockSize: 272),
    (BlockSize: 288),
    (BlockSize: 304),
    (BlockSize: 320),

    // 32 byte jumps
    (BlockSize: 352),
    (BlockSize: 384),
    (BlockSize: 416),
    (BlockSize: 448),
    (BlockSize: 480),

    // 48 byte jumps
    (BlockSize: 528),
    (BlockSize: 576),
    (BlockSize: 624),
    (BlockSize: 672),

    // 64 byte jumps
    (BlockSize: 736),
    (BlockSize: 800),

    // 80 byte jumps
    (BlockSize: 880),
    (BlockSize: 960),

    // 96 byte jumps
    (BlockSize: 1056),
    (BlockSize: 1152),

    // 112 byte jumps
    (BlockSize: 1264),
    (BlockSize: 1376),

    // 128 byte jumps
    (BlockSize: 1504),

    // 144 byte jumps
    (BlockSize: 1648),

    // 160 byte jumps
    (BlockSize: 1808),

    // 176 byte jumps
    (BlockSize: 1984),

    // 192 byte jumps
    (BlockSize: 2176),

    // 208 byte jumps
    (BlockSize: 2384),

    // 224 byte jumps
    (BlockSize: CMaximumSmallBlockSize),

    // The last block size occurs three times. If, during a GetMem call, the
    // requested block size is already locked by another thread then up to two
    // larger block sizes may be used instead. Having the last block size occur
    // three times avoids the need to have a size overflow check.
    (BlockSize: CMaximumSmallBlockSize),
    (BlockSize: CMaximumSmallBlockSize)
  );

{----------------------Main Memory Manager Functions----------------------}

function FGetMem(Size: NativeInt): Pointer;
function FFreeMem(P: Pointer): Integer;
function FReallocMem(P: Pointer; Size: NativeInt): Pointer;
function FAllocMem(Size: NativeInt): Pointer;

function FGetMemPool(const APool: PThreadPool; const ASize: NativeUInt): Pointer;

function FRegisterExpectedMemoryLeak(P: Pointer): Boolean;
function FUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
procedure FScanForMemoryLeaks;

procedure FFinalizeMemoryManager;

function FGetOSAllocCachedCount: UInt32;


{-------------------------Medium Block Management-------------------------}

// Allocates a new sequential feed medium block pool and immediately splits off a
// block of the requested size. The block size must be a multiple of 16.
// Medium blocks must be locked.
function AllocNewSequentialFeedMediumPool(const APool: PThreadPool; const AFirstBlockSize: UInt32): Pointer;

// Bins what remains in the current sequential feed medium block pool.
// Medium blocks must be locked.
// APool.MediumSequentialFeedBytesLeft must not be zero
procedure BinMediumSequentialFeedRemainder(const APool: PThreadPool);

// Frees a medium block pool
// Medium blocks must be locked.
function FreeMediumBlockPool(const APool: PThreadPool; AMediumBlockPool: PLinkNode): Integer;

// Gets the first and last block pointer for a small block pool
procedure GetFirstAndLastSmallBlockInPool(const APSmallBlockPool: PSmallBlockPoolHeader;
  out FirstPtr, LastPtr: Pointer);

// Gets the first medium block in the medium block pool
// Medium blocks must be locked.
function GetFirstMediumBlockInPool(const APool: PThreadPool; const APMediumBlockPoolHeader: PMediumBlockPoolHeader): Pointer;

// Inserts a medium block into the appropriate medium block bin.
// Medium blocks must be locked.
procedure InsertMediumBlockIntoBin(const APool: PThreadPool; const APMediumFreeBlock: PMediumFreeBlock;
  const AMediumBlockSize: UInt32);

// Memory manager has been allocated/used
function IsMemoryAllocated: Boolean;

// Advances to the next medium block. Returns nil if the end of the medium block pool
// has been reached
function NextMediumBlock(const APMediumBlock: Pointer): Pointer;

// Removes a medium block from the circular linked list of free blocks.
// Does not change any header flags.
// Medium blocks must be locked.
procedure RemoveMediumFreeBlock(const APool: PThreadPool; const APMediumFreeBlock: PMediumFreeBlock);


const
  // This memory manager
  FMemoryManager: TMemoryManagerEx = (
    GetMem: FGetMem;
    FreeMem: FFreeMem;
    ReallocMem: FReallocMem;
{$if CompilerVersion > 15} // Delphi 7
    AllocMem: FAllocMem;
    RegisterExpectedMemoryLeak: FRegisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: FUnregisterExpectedMemoryLeak
{$ifend}
  );

implementation

uses
  FMemoryThreadPool, FVirtual, FMemoryLarge, FMemoryMedium, FMemorySmall
{$ifdef F4mShareMemoryManager}
  , FShare
{$endif}
{$ifdef F4mDebugManager}
  , FDebug
{$endif}
{$ifdef F4mIncludeMemoryLeakTrackingCode}
  , FTrackLeak
{$endif}
  ;

{$ifdef F4mInstallMemoryManager}
var
  OldMemoryManager: TMemoryManagerEx;
  SharedMemory, UseSharedMemory: TSharedMemory:
  IsMemoryFManagerSet: Boolean;
{$endif}

{----------------------Main Memory Manager Functions----------------------}

function FGetMem(Size: NativeInt): Pointer;
begin
  if Size > 0 then
    Result := FGetMemPool(GetThreadPool, Size)
  else
    Result := nil;
end;

function FGetMemPool(const APool: PThreadPool; const ASize: NativeUInt): Pointer;
begin
  Assert(APool <> nil);
  Assert(ASize > 0);

  // Take the header size into account when determining the required block size
  // Is it a small block?
  if ASize <= CMaximumSmallBlockUserSize then
    Result := GetMemSmall(APool, ASize)
  // Is it a medium block?
  else if ASize <= CMaximumMediumBlockUserSize then
    Result := GetMemMedium(APool, ASize)
  // Allocate a large block
  else
    Result := GetMemLarge(APool, ASize)
end;

function FFreeMem(P: Pointer): Integer;
var
  LBlockSizeAndFlags: NativeUInt;
begin
{$ifdef F4mDebugManager}
  WriteTrace(P, 'FFreeMem');
{$endif}

  if P <> nil then
  begin
    // Get the block header
    LBlockSizeAndFlags := PNativeUInt(NativeUInt(P) - CBlockHeaderSize)^;

    // Is it a valid small block?
    if (LBlockSizeAndFlags and (CIsFreeBlockFlag or CIsMediumBlockFlag or CIsLargeBlockFlag)) = 0 then
      Result := FreeMemSmall(P)
    // Is this a valid medium block?
    else if (LBlockSizeAndFlags and (CIsFreeBlockFlag or CIsLargeBlockFlag)) = 0 then
      Result := FreeMemMedium(P)
    // Is this a valid large block?
    else if (LBlockSizeAndFlags and (CIsFreeBlockFlag or CIsMediumBlockFlag)) = 0 then
      Result := FreeMemLarge(P)
    else
      Result := CResultError;

{$ifdef F4mDebugManager}
    if Result = CResultError then
      if (LBlockSizeAndFlags and CIsMediumBlockFlag) <> 0 then
        WriteError(P, 'FFreeMem-Medium')
      else if (LBlockSizeAndFlags and CIsLargeBlockFlag) <> 0 then
        WriteError(P, 'FFreeMem-Large')
      else
        WriteError(P, 'FFreeMem-Small');
{$endif}
  end
  else
    Result := CResultOK;
end;

function FReallocMem(P: Pointer; Size: NativeInt): Pointer;
var
  LBlockSizeAndFlags: NativeUInt;
begin
{$ifdef F4mDebugManager}
  WriteTrace(P, 'FReallocMem-Old');
{$endif}

  if P <> nil then
  begin
    if Size > 0 then
    begin
      // Get the block header
      LBlockSizeAndFlags := PNativeUInt(NativeUInt(P) - CBlockHeaderSize)^;

      // Is it a valid small block?
      if (LBlockSizeAndFlags and (CIsFreeBlockFlag or CIsMediumBlockFlag or CIsLargeBlockFlag)) = 0 then
        Result := ReallocMemSmall(P, Size)
      // Is this a valid medium block?
      else if (LBlockSizeAndFlags and (CIsFreeBlockFlag or CIsLargeBlockFlag)) = 0 then
        Result := ReallocMemMedium(P, Size)
      // Is this a valid large block?
      else if (LBlockSizeAndFlags and (CIsFreeBlockFlag or CIsMediumBlockFlag)) = 0 then
        Result := ReallocMemLarge(P, Size)
      // Bad pointer: probably an attempt to reallocate a free memory block.
      else
        Result := nil;
    end
    else
    begin
      FFreeMem(P);
      Result := nil;
    end;
  end
  else
    Result := FGetMem(Size);

{$ifdef F4mDebugManager}
  WriteTrace(Result, 'FReallocMem-New');
{$endif}
end;

// Allocates a block and fills it with zeroes
function FAllocMem(Size: NativeInt): Pointer;
begin
  Result := FGetMem(Size);

  // Large blocks are already zero filled
  if (Result <> nil) and (Size <= CMaximumMediumBlockUserSize) then
    FillChar(Result^, Size, 0);
end;

// Registers expected memory leaks. Returns True on success. The list of leaked
// blocks is limited, so failure is possible if the list is full.
function FRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
{$ifdef F4mIncludeMemoryLeakTrackingCode}
  Result := FTrackLeak.RegisterExpectedMemoryLeak(P);
{$else}
  Result := False;
{$endif}
end;

function FUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
{$ifdef F4mIncludeMemoryLeakTrackingCode}
  Result := FTrackLeak.UnregisterExpectedMemoryLeak(P);
{$else}
  // Default to error
  Result := False;
{$endif}
end;

procedure FScanForMemoryLeaks;
begin
{$ifdef F4mIncludeMemoryLeakTrackingCode}
  FTrackLeak.ScanForMemoryLeaks;
{$endif}
end;

procedure FFinalizeMemoryManager;
begin
  Inc(Shutdown);

  FFinalizeThreadPool;

{$ifdef F4mIncludeMemoryLeakTrackingCode}
  FTrackLeak.ScanForMemoryLeaks;
{$endif}

  // Clean up: Free all memory allocated through this memory manager. If this is
  // a library that is frequently loaded and unloaded then it is necessary to
  // prevent the process from running out of address space.

  // Clear all small block types
  FreeAllMemorySmall;

  // Free all block pools
  FreeAllMemoryMedium;

  // Free all large blocks
  FreeAllMemoryLarge;

  Dec(Shutdown);
end;

function FGetOSAllocCachedCount: UInt32;
  function GetOSAllocCachedCountPool(const APool: PThreadPool): UInt32;
  var
    LPLargeBlock: PLargeBlockHeader;
    LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  begin
    Result := 0;
    
    LPMediumBlockPoolHeader := APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @APool.MediumBlockPoolsCircularList do
    begin
      Inc(Result);
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
    end;

    LPLargeBlock := APool.LargeBlocksCircularList.NextLargeBlockHeader;
    while LPLargeBlock <> @APool.LargeBlocksCircularList do
    begin
      Inc(Result);
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;
  end;
var
  Count: UInt32;
  I: Int32;
begin
  Count := 0;
  
  for I := 0 to High(ThreadPools) do
    Inc(Count, GetOSAllocCachedCountPool(@ThreadPools[I]));

{$ifdef F4mCacheThreadOSAlloc}
  Inc(Count, MediumBlockPoolCachedsCount);
  for I := 0 to High(ThreadPools) do
    Inc(Count, ThreadPools[I].MediumBlockPoolCachedsCount);
{$endif}

  Result := Count;
end;

{-------------------------Medium Block Management-------------------------}

// Allocates a new sequential feed medium block pool and immediately splits off a
// block of the requested size. The block size must be a multiple of 16 and
// medium blocks must be locked.
function AllocNewSequentialFeedMediumPool(const APool: PThreadPool; const AFirstBlockSize: UInt32): Pointer;
var
  LOldFirstMediumBlockPool: PMediumBlockPoolHeader;
  LNewPool: PLinkNode;
begin
  Assert(APool.MediumBlocksLocked <> 0, 'medium-block-locked must be on before call');

  // Bin the current sequential feed remainder
  if APool.MediumSequentialFeedBytesLeft <> 0 then
    BinMediumSequentialFeedRemainder(APool);

  // Allocate a new sequential feed block pool
{$ifdef F4mCacheThreadOSAlloc}
  // Is thread cache available?
  LNewPool := APool.MediumBlockPoolCacheds;
  if LNewPool <> nil then
  begin
    APool.MediumBlockPoolCacheds := LNewPool.Next;
    LNewPool.Next := nil;
    Dec(APool.MediumBlockPoolCachedsCount);
  end;

  if LNewPool = nil then
  begin
    // Is global cache available?
    LockAcquire(@MediumBlockPoolCachedsLocked);

    LNewPool := MediumBlockPoolCacheds;
    if LNewPool <> nil then
    begin
      MediumBlockPoolCacheds := LNewPool.Next;
      LNewPool.Next := nil;
      Dec(MediumBlockPoolCachedsCount);
    end;

    //LockRelease(@MediumBlockPoolCachedsLocked);
    MediumBlockPoolCachedsLocked := 0;
  end;

  if LNewPool = nil then
    LNewPool := OSAlloc(CMediumBlockPoolSize);
{$else}
  LNewPool := OSAlloc(CMediumBlockPoolSize);
{$endif}

  if LNewPool <> nil then
  begin
    // Insert this block pool into the list of block pools
    LOldFirstMediumBlockPool := APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    PMediumBlockPoolHeader(LNewPool).PreviousMediumBlockPoolHeader := @APool.MediumBlockPoolsCircularList;
    APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := Pointer(LNewPool);
    PMediumBlockPoolHeader(LNewPool).NextMediumBlockPoolHeader := LOldFirstMediumBlockPool;
    LOldFirstMediumBlockPool.PreviousMediumBlockPoolHeader := Pointer(LNewPool);

    // Store the sequential feed pool trailer
    PNativeUInt(NativeUInt(LNewPool) + CMediumBlockPoolSize - CBlockHeaderSize)^ :=
      APool.Index or CIsMediumBlockFlag;

    // Get the number of bytes still available
    APool.MediumSequentialFeedBytesLeft := (CMediumBlockPoolSize - CMediumBlockPoolHeaderSize) - AFirstBlockSize;

    // Get the result
    Result := Pointer(NativeUInt(LNewPool) + CMediumBlockPoolSize - AFirstBlockSize);
    APool.LastSequentiallyFedMediumBlock := Result;

    // Store the block header
    PNativeUInt(NativeUInt(Result) - CBlockHeaderSize)^ :=
      APool.Index or AFirstBlockSize or CIsMediumBlockFlag;
  end
  else
  begin
    // Out of memory
    APool.MediumSequentialFeedBytesLeft := 0;
    Result := nil;
  end;
end;

// Bins what remains in the current sequential feed medium block pool.
// Medium blocks must be locked.
procedure BinMediumSequentialFeedRemainder(const APool: PThreadPool);
var
  LPRemainderBlock, LNextMediumBlock: Pointer;
  LSequentialFeedFreeSize, LNextBlockSizeAndFlags: NativeUInt;
begin
  Assert(APool.MediumBlocksLocked <> 0, 'medium-block-locked must be on before call');
  Assert(APool.MediumSequentialFeedBytesLeft <> 0);

  LSequentialFeedFreeSize := APool.MediumSequentialFeedBytesLeft;
  
  // Get the block after the open space
  LNextMediumBlock := APool.LastSequentiallyFedMediumBlock;
  LNextBlockSizeAndFlags := PNativeUInt(NativeUInt(LNextMediumBlock) - CBlockHeaderSize)^;

  // Point to the remainder
  LPRemainderBlock := Pointer(NativeUInt(LNextMediumBlock) - LSequentialFeedFreeSize);

  // Can the next block be combined with the remainder?
  if (LNextBlockSizeAndFlags and CIsFreeBlockFlag) <> 0 then
  begin
    // Increase the size of this block
    Inc(LSequentialFeedFreeSize, LNextBlockSizeAndFlags and CExtractMediumSizeMask);

    // Remove the next block as well
    if (LNextBlockSizeAndFlags and CExtractMediumSizeMask) >= CMinimumMediumBlockSize then
      RemoveMediumFreeBlock(APool, LNextMediumBlock);
  end
  else
  begin
    // Set the "previous block is free" flag of the next block
    PNativeUInt(NativeUInt(LNextMediumBlock) - CBlockHeaderSize)^ :=
      LNextBlockSizeAndFlags or CPreviousMediumBlockIsFreeFlag;
  end;

  // Store the size of the block as well as the flags
  PNativeUInt(NativeUInt(LPRemainderBlock) - CBlockHeaderSize)^ :=
    APool.Index or LSequentialFeedFreeSize or CIsMediumBlockFlag or CIsFreeBlockFlag;

  // Store the trailing size marker
  PNativeUInt(NativeUInt(LPRemainderBlock) + LSequentialFeedFreeSize - CBlockHeaderSize * 2)^ :=
    APool.Index or LSequentialFeedFreeSize;

  // Bin this medium block
  if LSequentialFeedFreeSize >= CMinimumMediumBlockSize then
    InsertMediumBlockIntoBin(APool, LPRemainderBlock, LSequentialFeedFreeSize);
end;

function FreeMediumBlockPool(const APool: PThreadPool; AMediumBlockPool: PLinkNode): Integer;
begin
  Assert(APool.MediumBlocksLocked <> 0, 'medium-block-locked must be on before call');

{$ifdef F4mCacheThreadOSAlloc}
  if Shutdown = 0 then
  begin
    // Reset it here while it is still hot?
    //FillChar(Pointer(AMediumBlockPool)^, CMediumBlockPoolSize, 0);

    if APool.MediumBlockPoolCachedsCount < CMediumBlockThreadPoolCachedsMax then
    begin
      AMediumBlockPool.Next := APool.MediumBlockPoolCacheds;
      APool.MediumBlockPoolCacheds := AMediumBlockPool;
      AMediumBlockPool := nil;
      Inc(APool.MediumBlockPoolCachedsCount);
    end;

    if (AMediumBlockPool <> nil) and (MediumBlockPoolCachedsCount < CMediumBlockPoolCachedsMax) then
    begin
      LockAcquire(@MediumBlockPoolCachedsLocked);

      if MediumBlockPoolCachedsCount < CMediumBlockPoolCachedsMax then
      begin
        AMediumBlockPool.Next := MediumBlockPoolCacheds;
        MediumBlockPoolCacheds := AMediumBlockPool;
        AMediumBlockPool := nil;
        Inc(MediumBlockPoolCachedsCount);
      end;

      //LockRelease(@MediumBlockPoolCachedsLocked);
      MediumBlockPoolCachedsLocked := 0;
    end;
  end;
{$endif}

  // Free the medium block pool
  if (AMediumBlockPool = nil) or OSFree(AMediumBlockPool) then
    Result := CResultOK
  else
    Result := CResultError;
end;

// Gets the first and last block pointer for a small block pool
procedure GetFirstAndLastSmallBlockInPool(const APSmallBlockPool: PSmallBlockPoolHeader;
  out FirstPtr, LastPtr: Pointer);
var
  LBlockSize: UInt32;
begin
  // Get the pointer to the first block
  FirstPtr := Pointer(NativeUInt(APSmallBlockPool) + CSmallBlockPoolHeaderSize);

  // Get a pointer to the last block
  if (APSmallBlockPool.BlockType.CurrentSequentialFeedPool <> APSmallBlockPool)
      or (NativeUInt(APSmallBlockPool.BlockType.NextSequentialFeedBlockAddress) > NativeUInt(APSmallBlockPool.BlockType.MaxSequentialFeedBlockAddress)) then
  begin
    // Not the sequential feed - point to the end of the block
    LBlockSize := PNativeUInt(NativeUInt(APSmallBlockPool) - CBlockHeaderSize)^ and CExtractMediumSizeMask;
    LastPtr := Pointer(NativeUInt(APSmallBlockPool) + LBlockSize - APSmallBlockPool.BlockType.BlockSize);
  end
  else
  begin
    // The sequential feed pool - point to before the next sequential feed block
    LastPtr := Pointer(NativeUInt(APSmallBlockPool.BlockType.NextSequentialFeedBlockAddress) - 1);
  end;
end;

// Gets the first medium block in the medium block pool
function GetFirstMediumBlockInPool(const APool: PThreadPool; const APMediumBlockPoolHeader: PMediumBlockPoolHeader): Pointer;
begin
  Assert(APool.MediumBlocksLocked <> 0, 'medium-block-locked must be on before call');

  if (APool.MediumSequentialFeedBytesLeft = 0)
      or (NativeUInt(APool.LastSequentiallyFedMediumBlock) < NativeUInt(APMediumBlockPoolHeader))
      or (NativeUInt(APool.LastSequentiallyFedMediumBlock) > NativeUInt(APMediumBlockPoolHeader) + CMediumBlockPoolSize) then
  begin
    Result := Pointer(NativeUInt(APMediumBlockPoolHeader) + CMediumBlockPoolHeaderSize);
  end
  else
  begin
    // Is the sequential feed pool empty?
    if APool.MediumSequentialFeedBytesLeft <> CMediumBlockPoolSize - CMediumBlockPoolHeaderSize then
      Result := APool.LastSequentiallyFedMediumBlock
    else
      Result := nil;
  end;
end;

// Inserts a medium block into the appropriate medium block bin.
// Medium blocks must be locked.
procedure InsertMediumBlockIntoBin(const APool: PThreadPool; const APMediumFreeBlock: PMediumFreeBlock;
  const AMediumBlockSize: UInt32);
var
  LPBin, LPFirstFreeBlock: PMediumFreeBlock;
  LBinNumber, LBinGroupNumber: UInt32;
begin
  Assert(APool.MediumBlocksLocked <> 0, 'medium-block-locked must be on before call');

  // Get the bin number for this block size. Get the bin that holds blocks of at least this size.
  //LBinNumber := (AMediumBlockSize - CMinimumMediumBlockSize) div CMediumBlockGranularity;
  LBinNumber := (AMediumBlockSize - CMinimumMediumBlockSize) shr CMediumBlockGranularityShift;
  if LBinNumber >= CMediumBlockBinCount then
    LBinNumber := CMediumBlockBinCount - 1;

  // Get the bin
  LPBin := @APool.MediumBlockBins[LBinNumber];

  // Bins are LIFO, se we insert this block as the first free block in the bin
  LPFirstFreeBlock := LPBin.NextFreeBlock;
  APMediumFreeBlock.PreviousFreeBlock := LPBin;
  APMediumFreeBlock.NextFreeBlock := LPFirstFreeBlock;
  LPFirstFreeBlock.PreviousFreeBlock := APMediumFreeBlock;
  LPBin.NextFreeBlock := APMediumFreeBlock;

  // Was this bin empty?
  if LPFirstFreeBlock = LPBin then
  begin
    // Get the group number
    //LBinGroupNumber := LBinNumber div MediumBlockBinGroupCount;
    LBinGroupNumber := LBinNumber shr CMediumBlockBinGroupCountShift;

    // Flag this bin as used
    APool.MediumBlockBinBitmaps[LBinGroupNumber] := APool.MediumBlockBinBitmaps[LBinGroupNumber]
      or (1 shl (LBinNumber and 31));

    // Flag the group as used
    APool.MediumBlockBinGroupBitmap := APool.MediumBlockBinGroupBitmap or (1 shl LBinGroupNumber);
  end;
end;

// Memory manager has been allocated/used
function IsMemoryAllocated: Boolean;
  function IsMemoryAllocatedPool(APool: PThreadPool): Boolean;
  begin
    Result := (APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader <> @APool.MediumBlockPoolsCircularList)
      or (APool.LargeBlocksCircularList.NextLargeBlockHeader <> @APool.LargeBlocksCircularList);
  end;
var
  I: Int32;
begin
  for I := 0 to (CMaximumThreadPool - 1) do
  begin
    if IsMemoryAllocatedPool(@ThreadPools[I]) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

// Advances to the next medium block. Returns nil if the end of the medium block pool
// has been reached
function NextMediumBlock(const APMediumBlock: Pointer): Pointer;
begin
  // Advance the pointer
  Result := Pointer(NativeUInt(APMediumBlock) +
    (PNativeUInt(NativeUInt(APMediumBlock) - CBlockHeaderSize)^ and CExtractMediumSizeMask));

  // Is the next block the end of medium pool marker?
  if (PNativeUInt(NativeUInt(Result) - CBlockHeaderSize)^ and CExtractMediumSizeMask) = 0 then
    Result := nil;
end;

// Removes a medium block from the circular linked list of free blocks.
// Does not change any header flags.
// Medium blocks should be locked.
procedure RemoveMediumFreeBlock(const APool: PThreadPool; const APMediumFreeBlock: PMediumFreeBlock);
var
  LPreviousFreeBlock, LNextFreeBlock: PMediumFreeBlock;
  LBinNumber, LBinGroupNumber: UInt32;
begin
  Assert(APool.MediumBlocksLocked <> 0, 'medium-block-locked must be on before call');

  // Get the current previous and next blocks
  LNextFreeBlock := APMediumFreeBlock.NextFreeBlock;
  LPreviousFreeBlock := APMediumFreeBlock.PreviousFreeBlock;

  // Remove this block from the linked list
  LPreviousFreeBlock.NextFreeBlock := LNextFreeBlock;
  LNextFreeBlock.PreviousFreeBlock := LPreviousFreeBlock;

  // Is this bin now empty? If the previous and next free block pointers are
  // equal, they must point to the bin.
  if LPreviousFreeBlock = LNextFreeBlock then
  begin
    // Get the bin number for this block size
    LBinNumber := (NativeUInt(LNextFreeBlock) - NativeUInt(@APool.MediumBlockBins)) div SizeOf(TMediumFreeBlock);
    //LBinGroupNumber := LBinNumber div MediumBlockBinGroupCount;
    LBinGroupNumber := LBinNumber shr CMediumBlockBinGroupCountShift;

    // Flag this bin as empty
    APool.MediumBlockBinBitmaps[LBinGroupNumber] := APool.MediumBlockBinBitmaps[LBinGroupNumber]
      and (not (1 shl (LBinNumber and 31)));

    // Is the group now entirely empty?
    if APool.MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
    begin
      // Flag this group as empty
      APool.MediumBlockBinGroupBitmap := APool.MediumBlockBinGroupBitmap and (not (1 shl LBinGroupNumber));
    end;
  end;
end;

{$ifdef F4mInstallMemoryManager}
procedure FInstallMemoryManager;
begin
  GetMemoryManager(OldMemoryManager);

{$ifdef F4mUseSharedMemoryManager}
  UseSharedMemory := FAttemptToUseSharedMemoryManager;
  if UseSharedMemory = smOK then
    Exit;
{$endif}

  SetMemoryManager(FMemoryManager);
  IsMemoryFManagerSet := True;
  
{$ifdef F4mShareMemoryManager}
  SharedMemory := FShareMemoryManager;
{$endif}
end;
{$endif}

{$ifdef F4mInstallMemoryManager}
procedure FUninstallMemoryManager;
begin
  if IsMemoryFManagerSet then
  begin
    IsMemoryFManagerSet := False;
    SetMemoryManager(OldMemoryManager);
  end;
end;
{$endif}

initialization
{$ifdef F4mInstallMemoryManager}
  FInstallMemoryManager;
{$endif}

finalization
  FFinalizeMemoryManager;
{$ifdef F4mInstallMemoryManager}
  FUninstallMemoryManager;
{$endif}

end.
