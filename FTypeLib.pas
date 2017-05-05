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

unit FTypeLib;

interface

{$include FOption.inc}

const
  ResultOK = 0;
  ResultError = -1;

  NumSmallBlockTypes = {$ifdef F4mAlign16Bytes}46{$else}55{$endif};

type
{$if CompilerVersion <= 15} // Delphi 7
  Int16 = Smallint;
  UInt16 = Word;

  Int32 = Integer;
  UInt32 = Cardinal;

  NativeInt = Integer;
  PNativeInt = PInteger;

  NativeUInt = Cardinal;
  PNativeUInt = PCardinal;

  PMemoryManagerEx = System.PMemoryManager;
  TMemoryManagerEx = System.TMemoryManager;
{$ifend}

  TSharedMemory = (smOK, smAlreadySet, smAlreadyShared, smAlreadyUsed, smFailed);

  // Memory map
  TChunkStatus = (csUnallocated, csAllocated, csReserved, csSysAllocated, csSysReserved);
  
  TMemoryMap = array[0..65535] of TChunkStatus;

  TSmallBlockTypeState = packed record
    // The internal size of the block type
    InternalBlockSize: UInt32;

    // Useable block size: The number of non-reserved bytes inside the block.
    UseableBlockSize: UInt32;

    // The number of allocated blocks
    AllocatedBlockCount: NativeUInt;
    
    // The total address space reserved for this block type (both allocated and free blocks)
    ReservedAddressSpace: NativeUInt;
  end;
  
  TSmallBlockTypeStates = array[0..NumSmallBlockTypes - 1] of TSmallBlockTypeState;

  TMemoryManagerState = packed record
    // Small block type states
    SmallBlockTypeStates: TSmallBlockTypeStates;

    // Medium block stats
    AllocatedMediumBlockCount: UInt32;
    TotalAllocatedMediumBlockSize: NativeUInt;
    ReservedMediumBlockAddressSpace: NativeUInt;

    // Large block stats
    AllocatedLargeBlockCount: UInt32;
    TotalAllocatedLargeBlockSize: NativeUInt;
    ReservedLargeBlockAddressSpace: NativeUInt;
  end;

  TStatisticCalls = packed record
    ThreadYieldCount: NativeUInt;
    OSAllocCount: NativeUInt;
    OSAllocFailCount: NativeUInt;
    OSFreeCount: NativeUInt;
  end;

implementation

end.
