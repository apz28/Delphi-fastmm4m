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

unit FType;

interface

{$include FOption.inc}

uses
  FTypeLib;
  
type
  // Move procedure type
  TMoveProc = procedure(const ASource; var Dest; ACount: NativeInt);

const
  // Maximum number of thread pools. Must not be greater than 255
  // Prefer to be a prim number
  CMaximumThreadPool = 251;

  // For F4mTrackThreadLife, Maximum number of threads per pool. Must not be greater than 255
  CMaximumThreadPerPool = 255;

  // The minimum block alignment. Under 64-bit blocks are always aligned to a 16 byte boundary.
  // For 32 bit, it can be 8 or 16
  CMinimumBlockAlignment = {$ifdef F4mAlign16Bytes}16{$else}8{$endif};

  // The size of a medium block pool. This is allocated through OSAlloc and
  // is used to serve medium blocks. The size must be a multiple of 16 and at
  // least 4 bytes less than a multiple of 4K (the page size) to prevent a
  // possible read access violation when reading past the end of a memory block
  // in the optimized move routine (MoveX16LP).
  CMediumBlockPoolSize = 20 * 64 * 1024 - 16;

  // Maximum number of global cached medium blocks. Must not be greater than 254
  CMediumBlockPoolCachedsMax = 200;

  // Maximum number of thread cached medium blocks. Must not be greater than 254
  // and smaller than CMediumBlockPoolCachedsMax
  CMediumBlockThreadPoolCachedsMax = 2;

  // The granularity of small blocks
  CSmallBlockGranularity = {$ifdef F4mAlign16Bytes}16{$else}8{$endif};

  // The granularity of medium blocks. Newly allocated medium blocks are
  // a multiple of this size plus MediumBlockSizeOffset, to avoid cache line conflicts
  CMediumBlockGranularity = 256;
  CMediumBlockGranularityShift = 8; // Used inplace of div/mul for speed
  CMediumBlockSizeOffset = 48;

  // The granularity of large blocks
  CLargeBlockGranularity = 65536;

  // The maximum size of a small block. Blocks Larger than this are either
  // medium or large blocks.
  CMaximumSmallBlockSize = 2608;

  // The smallest medium block size. (Medium blocks are rounded up to the nearest
  // multiple of MediumBlockGranularity plus MediumBlockSizeOffset)
  CMinimumMediumBlockSize = 11 * 256 + CMediumBlockSizeOffset;

  // The number of bins reserved for medium blocks
  CMediumBlockBinsPerGroup = 32;
  CMediumBlockBinsPerGroupShift = 5; // Used inplace of div/mul for speed
  CMediumBlockBinGroupCount = 32;
  CMediumBlockBinGroupCountShift = 5; // Used inplace of div/mul for speed
  CMediumBlockBinCount = CMediumBlockBinGroupCount * CMediumBlockBinsPerGroup;

  // The maximum size allocatable through medium blocks. Blocks larger than this
  // fall through to OSAlloc which is large block.
  CMaximumMediumBlockSize = CMinimumMediumBlockSize + (CMediumBlockBinCount - 1) * CMediumBlockGranularity;

  // The target number of small blocks per pool. The actual number of blocks per
  // pool may be much greater for very small sizes and less for larger sizes. The
  // cost of allocating the small block pool is amortized across all the small
  // blocks in the pool, however the blocks may not all end up being used so they
  // may be lying idle.
  CTargetSmallBlocksPerPool = 48;

  // The minimum number of small blocks per pool. Any available medium block must
  // have space for roughly this many small blocks (or more) to be useable as a
  // small block pool.
  CMinimumSmallBlocksPerPool = 12;

  // The lower and upper limits for the optimal small block pool size
  COptimalSmallBlockPoolSizeLowerLimit = 29 * 1024 - CMediumBlockGranularity + CMediumBlockSizeOffset;
  COptimalSmallBlockPoolSizeUpperLimit = 64 * 1024 - CMediumBlockGranularity + CMediumBlockSizeOffset;

  // The maximum small block pool size. If a free block is this size or larger then it will be split.
  CMaximumSmallBlockPoolSize = COptimalSmallBlockPoolSizeUpperLimit + CMinimumMediumBlockSize;

  {----------------------------Block type flags---------------------------}

  // The lower 3 bits in the pointer size header of small & medium blocks (4 bits in medium and
  // large blocks) are used as flags to indicate the state of the block

  // First bit. Set if the block is not in use
  CIsFreeBlockFlag = 1;

  // Second bit. Set if this is a medium block
  CIsMediumBlockFlag = 2;

  // Third bit.Set if it is a medium block being used as a small block pool.
  // Only valid if IsMediumBlockFlag is set.
  CIsSmallBlockPoolInUseFlag = 4;

  // Set if it is a large block. Only valid if IsMediumBlockFlag is not set.
  CIsLargeBlockFlag = 4;

  // Fourth bit -> Medium & Large block must be align at least at 16 bytes
  // Is the medium block preceding this block available?
  CPreviousMediumBlockIsFreeFlag = 8;

  // Is this large block segmented? I.e. is it actually built up from more than
  // one chunk allocated through OSAlloc? (Only used by large blocks.)
  CLargeBlockIsSegmentedFlag = 8;

  // The flags masks for small blocks
  CDropSmallFlagsMask = NativeUInt(-8);
  CExtractSmallFlagsMask = 7;

  // The flags masks for medium and large blocks
  CExtractMediumAndLargeFlagsMask = 15;

  // The flag mask for large block size
  CExtractLargeSizeMask = NativeUInt(-16);

  // The flag mask for medium block size
  CExtractMediumSizeMask = $fffff0;

  // The flag for medium pool index
  CMediumSlotIndexShift = 24;

  {-------------------------Block resizing constants----------------------}

  CSmallBlockDownsizeCheckAdder = 64;
  CSmallBlockUpsizeAdder = 32;

  // When a medium block is reallocated to a size smaller than this, then it must
  // be reallocated to a small block and the data moved. If not, then it is
  // shrunk in place down to MinimumMediumBlockSize. Currently the limit is set
  // at a quarter of the minimum medium block size.
  CMediumInPlaceDownsizeLimit = CMinimumMediumBlockSize div 4;

type
  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..Maxint div SizeOf(Pointer) - 1] of Pointer;

  PLinkNode = ^TLinkNode;
  TLinkNode = packed record
    Next: PLinkNode;
  end;

  {-----------------------Small block structures--------------------------}

  // Pointer to the header of a small block pool
  PSmallBlockPoolHeader = ^TSmallBlockPoolHeader;

  TSmallBlockSize = packed record
    // The block size for this block type
    BlockSize: UInt16;
{$ifdef F4mUseCustomFixedSizeMoveRoutines}
    // The fixed size move procedure used to move data for this block size when
    // it is upsized. When a block is downsized (which usually does not occur
    // that often) the variable size move routine is used.
    UpsizeMoveProcedure: TMoveProc;
{$endif}
  end;

  // Small block type (Size = 32 bytes for 32-bit, 64 bytes for 64-bit).
  PSmallBlockType = ^TSmallBlockType;
  TSmallBlockType = packed record
    // True = Block type is locked
    BlockTypeLocked: Byte;

    // Bitmap indicating which of the first 8 medium block groups contain blocks
    // of a suitable size for a block pool.
    AllowedGroupsForBlockPoolBitmap: Byte;

    // The block size for this block type
    BlockSize: UInt16;

    // The minimum and optimal size of a small block pool for this block type
    MinimumBlockPoolSize: UInt16;
    OptimalBlockPoolSize: UInt16;

    // The first partially free pool for the given small block. This field must
    // be at the same offset as TSmallBlockPoolHeader.NextPartiallyFreePool.
    NextPartiallyFreePool: PSmallBlockPoolHeader;

    // The last partially free pool for the small block type. This field must
    // be at the same offset as TSmallBlockPoolHeader.PreviousPartiallyFreePool.
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;

    // The offset of the last block that was served sequentially. The field must
    // be at the same offset as TSmallBlockPoolHeader.FirstFreeBlock.
    NextSequentialFeedBlockAddress: Pointer;

    // The last block that can be served sequentially.
    MaxSequentialFeedBlockAddress: Pointer;

    // The pool that is current being used to serve blocks in sequential order
    CurrentSequentialFeedPool: PSmallBlockPoolHeader;

{$ifdef F4mUseCustomFixedSizeMoveRoutines}
    // The fixed size move procedure used to move data for this block size when
    // it is upsized. When a block is downsized (which usually does not occur
    // that often) the variable size move routine is used.
    UpsizeMoveProcedure: TMoveProc;
{$else}
    Reserved1: Pointer;
{$endif}

{$if SizeOf(Pointer) = 8}
    // Pad to 64 bytes for 64-bit
    Reserved2: Pointer;
{$ifend}
  end;

  TSmallBlockSizes = array[0..CNumSmallBlockTypes - 1] of TSmallBlockSize;

  TSmallBlockTypes = array[0..CNumSmallBlockTypes - 1] of TSmallBlockType;

  // Small block pool (Size = 32 bytes for 32-bit, 48 bytes for 64-bit).
  TSmallBlockPoolHeader = packed record
    // BlockType
    BlockType: PSmallBlockType;

{$if SizeOf(Pointer) = 4}
    // Align the next fields to the same fields in TSmallBlockType and pad this
    // structure to 32 bytes for 32-bit
    Reserved1: UInt32;
{$ifend}

    // The next and previous pool that has free blocks of this size. Do not
    // change the position of these two fields: They must be at the same offsets
    // as the fields in TSmallBlockType of the same name.
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;

    // Pointer to the first free block inside this pool. This field must be at
    // the same offset as TSmallBlockType.NextSequentialFeedBlockAddress.
    FirstFreeBlock: Pointer;

    // The number of blocks allocated in this pool.
    BlocksInUse: UInt32;

    // Small block pool signature. Used by the leak checking mechanism to
    // determine whether a medium block is a small block pool or a regular medium block.
    SmallBlockPoolSignature: UInt32;

    // The pool pointer and flags of the first block
    FirstBlockPoolPointerAndFlags: NativeUInt;
  end;

  // Small block layout:
  // At offset -SizeOf(Pointer) = Flags + address of the small block pool.
  // At offset BlockSize - SizeOf(Pointer) = Flags + address of the small block
  // pool for the next small block.

  {------------------------Medium block structures------------------------}

  // The medium block pool from which medium blocks are drawn.
  // Size = 16 bytes for 32-bit and 32 bytes for 64-bit.
  PMediumBlockPoolHeader = ^TMediumBlockPoolHeader;
  TMediumBlockPoolHeader = packed record
    // Points to the previous and next medium block pools. This circular linked
    // list is used to track memory leaks on program shutdown.
    PreviousMediumBlockPoolHeader: PMediumBlockPoolHeader;
    NextMediumBlockPoolHeader: PMediumBlockPoolHeader;

    // Padding
    Reserved1: NativeUInt;

    // The block size and flags of the first medium block in the block pool
    FirstMediumBlockSizeAndFlags: NativeUInt;
  end;

  // Medium block layout:
  // Offset: -2 * SizeOf(Pointer) = Previous Block Size (only if the previous block is free)
  // Offset: -SizeOf(Pointer) = This block size and flags
  // Offset: 0 = User data / Previous Free Block (if this block is free)
  // Offset: SizeOf(Pointer) = Next Free Block (if this block is free)
  // Offset: BlockSize - 2*SizeOf(Pointer) = Size of this block (if this block is free)
  // Offset: BlockSize - SizeOf(Pointer) = Size of the next block and flags

  // A medium block that is unused
  PMediumFreeBlock = ^TMediumFreeBlock;
  TMediumFreeBlock = packed record
    PreviousFreeBlock: PMediumFreeBlock;
    NextFreeBlock: PMediumFreeBlock;
  end;

  {-------------------------Large block structures------------------------}

  // Large block header record (Size = 20 for 32-bit, 40 for 64-bit)
  PLargeBlockHeader = ^TLargeBlockHeader;
  TLargeBlockHeader = packed record
    // Points to the previous and next large blocks. This circular linked
    // list is used to track memory leaks on program shutdown.
    PreviousLargeBlockHeader: PLargeBlockHeader;
    NextLargeBlockHeader: PLargeBlockHeader;

    // ThreadPool where the memory allocated
    ThreadPool: Pointer;

    // The user allocated size of the Large block
    UserAllocatedSize: NativeUInt;

    // The size of this block plus the flags
    BlockSizeAndFlags: NativeUInt;
  end;

const
  // The size of the block header in front of small and medium blocks
  CBlockHeaderSize = SizeOf(Pointer);

  // The size of a small block pool header
  CSmallBlockPoolHeaderSize = SizeOf(TSmallBlockPoolHeader);

  // The size of a medium block pool header
  CMediumBlockPoolHeaderSize = SizeOf(TMediumBlockPoolHeader);

  // The size of the header in front of large block
  CLargeBlockHeaderSize = SizeOf(TLargeBlockHeader);

  CMaximumSmallBlockUserSize = CMaximumSmallBlockSize - CBlockHeaderSize;
  CMaximumMediumBlockUserSize = CMaximumMediumBlockSize - CBlockHeaderSize;

type
  PThreadPool = ^TThreadPool;  
  TThreadPool = record
    {-----------------------Small block management--------------------------}

    // The small block types. Sizes include the leading block header. Sizes are
    // picked to limit maximum wastage to about 10% or 256 bytes (whichever is less) where possible.
    SmallBlockTypes: TSmallBlockTypes;

    // Size to small block type translation table
    AllocSize2SmallBlockTypeIndX4: array[0..(CMaximumSmallBlockSize - 1) div CSmallBlockGranularity] of Byte;

    {-----------------------Medium block management-------------------------}

    // Are medium blocks locked?
    MediumBlocksLocked: Byte;

    // A dummy medium block pool header: Maintains a circular list of all medium
    // block pools to enable memory leak detection on program shutdown.
    MediumBlockPoolsCircularList: TMediumBlockPoolHeader;

    // The sequential feed medium block pool.
    LastSequentiallyFedMediumBlock: Pointer;
    MediumSequentialFeedBytesLeft: UInt32;

    // The medium block bins are divided into groups of 32 bins. If a bit
    // is set in this group bitmap, then at least one bin in the group has free blocks.
    MediumBlockBinGroupBitmap: UInt32;

    // The medium block bins: total of 32 * 32 = 1024 bins of a certain minimum size.
    MediumBlockBinBitmaps: array[0..CMediumBlockBinGroupCount - 1] of UInt32;

    // The medium block bins. There are 1024 LIFO circular linked lists each
    // holding blocks of a specified minimum size. The sizes vary in size from
    // MinimumMediumBlockSize to MaximumMediumBlockSize. The bins are treated as
    // type TMediumFreeBlock to avoid pointer checks.
    MediumBlockBins: array[0..CMediumBlockBinCount - 1] of TMediumFreeBlock;

{$ifdef F4mCacheThreadOSAlloc}
    MediumBlockPoolCacheds: PLinkNode;
    MediumBlockPoolCachedsCount: Byte;
{$endif}    

    {-------------------------Large block management------------------------}

    // Are large blocks locked?
    LargeBlocksLocked: Byte;

    // A dummy large block header: Maintains a list of all allocated large blocks
    // to enable memory leak detection on program shutdown.
    LargeBlocksCircularList: TLargeBlockHeader;

    {-------------------------Thread related management------------------------}

    // Index of this pool used with medium block header to identify the memory pool
    Index: NativeUInt;  // High 8 bits

    // Number of threads being used in this pool
    UsedCount: NativeUInt;

{$ifdef F4mTrackThreadLife}
    // Thread Id that being used in this pool
    ThreadIds: array[0..CMaximumThreadPerPool - 1] of TThreadId;
{$endif}
  end;

var
  // Memory thread pool and locked
  ThreadPoolsLocked: Byte;
  ThreadPools: array[0..CMaximumThreadPool - 1] of TThreadPool;

{$ifdef F4mCacheThreadOSAlloc}
  MediumBlockPoolCachedsLocked: Byte;
  MediumBlockPoolCacheds: PLinkNode;
  MediumBlockPoolCachedsCount: Byte;
{$endif}

  // Indicate if shutdown is in process if not zero
  Shutdown: Byte;

implementation

end.
