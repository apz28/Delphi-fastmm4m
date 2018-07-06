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

unit FTrackLeak;

interface

{$include FOption.inc}

uses
  FTypeLib, FType, FUtil, FVirtual; // System

const
  CExpectedMemoryLeaksListSizeAlloc = 64 * 1024;
  CExpectedMemoryLeaksListSize = CExpectedMemoryLeaksListSizeAlloc div SizeOf(Pointer);
{$if SizeOf(Pointer) = 4}
  CExpectedMemoryLeaksListSizePrim = 16381;
{$else}
  CExpectedMemoryLeaksListSizePrim = 8191;
{$ifend}
  
type
  // Leaked class type
  PLeakedClass = ^TLeakedClass;
  TLeakedClass = packed record
    ClassPointer: TClass;
    NumLeaks: UInt32;
  end;

  PLeakedClasses = ^TLeakedClasses;
  TLeakedClasses = array[0..255] of TLeakedClass;

  // Leak statistics for a small block type
  TSmallBlockLeaks = array[0..CNumSmallBlockTypes - 1] of TLeakedClasses;

  // A leaked medium or large block
  TMediumAndLargeBlockLeaks = array[0..4095] of NativeUInt;

var
  ExpectedMemoryLeaksFileName: array[0..1023] of AnsiChar;

function IsExpectedMemoryLeak(P: Pointer): Boolean;
function RegisterExpectedMemoryLeak(P: Pointer): Boolean;
function UnregisterExpectedMemoryLeak(P: Pointer): Boolean;
procedure ScanForMemoryLeaks;

implementation

uses
  FMemory;

const
  {-------------------Memory leak messages (may be localized)-------------}
  
  // Leak checking messages
  CLeakMessageHeader: PAnsiChar = 'An unexpected memory leak has occurred. ';
  CSmallLeakDetail: PAnsiChar = 'The unexpected small block leaks are:'#13#10;
  CLargeLeakDetail: PAnsiChar = 'The sizes of unexpected leaked medium and large blocks are: ';
  CBytesMessage: PAnsiChar = ' bytes: ';
  CUnknownClassNameMsg: PAnsiChar = 'Unknown';

  CAnsiStringBlockMessage: PAnsiChar = 'AnsiString';
  CUnicodeStringBlockMessage: PAnsiChar = 'UnicodeString';

type
  // Have to redeclare StrRec here, because it is not in the interface section of system.pas
  PStrRec = ^StrRec;
  StrRec = packed record
{$if CompilerVersion >= 20} // Delphi 2009
    codePage: Word;
    elemSize: Word;
{$ifend}
    refCnt: Longint;
    length: Longint;
  end;

var
  ExpectedMemoryLeaksLocked: Byte;

  // The expected leaks buffer
  ExpectedMemoryLeaks: PPointerArray;

  // The number of entries used in the expected leaks buffer
  ExpectedMemoryLeaksCount: Int32;
  
  // Keep track of maximum collision count to avoid full buffer scan
  ExpectedMemoryLeaksScanCollisions: Int32;

// Appends the name of the class to the destination buffer and returns the new destination position
function AppendClassNameToBuffer(AClass: TClass; ADestination: PAnsiChar): PAnsiChar;
var
  LPClassName: PShortString;
begin
  // Get a pointer to the class name
  if AClass <> nil then
  begin
    LPClassName := PShortString(PPointer(NativeInt(AClass) + vmtClassName)^);

    // Append the class name
    Result := AppendToBuffer(@LPClassName^[1], Length(LPClassName^), ADestination);
  end
  else
    Result := AppendToBuffer(CUnknownClassNameMsg, Length(CUnknownClassNameMsg), ADestination);
end;

procedure FinalizeTrackLeak;
begin
  // Free the expected memory leaks list
  if ExpectedMemoryLeaks <> nil then
  begin
    OSFree(ExpectedMemoryLeaks);
    ExpectedMemoryLeaks := nil;
  end;

  ExpectedMemoryLeaksCount := 0;
  ExpectedMemoryLeaksScanCollisions := 0;
end;

procedure InitializeTrackLeak;
const
  FileName: PAnsiChar = 'MemoryLeaks.log';
var
  Ptr: PAnsiChar;
begin
  Ptr := AppendToBufferModuleFileName(@ExpectedMemoryLeaksFileName[0]);
  if Ptr <> nil then
  begin
    Ptr := AppendToBuffer(FileName, Length(FileName), Ptr);
    AppendToBuffer(#0, Ptr);

    FileDelete(@ExpectedMemoryLeaksFileName[0]);
  end;

  // Allocate the list if it does not exist
  if ExpectedMemoryLeaks = nil then
    ExpectedMemoryLeaks := OSAlloc(CExpectedMemoryLeaksListSizeAlloc);
end;

// Locks the expected leaks. Returns false if the list could not be allocated.
function LockExpectedMemoryLeaksList: Boolean;
begin
  // Lock the expected leaks list
  LockAcquire(@ExpectedMemoryLeaksLocked);

  Result := ExpectedMemoryLeaks <> nil;
end;

function IsExpectedMemoryLeakIndex(P: Pointer; var I: Int32): Boolean;
var
  C: Int32;
begin
  Result := False;
  if (ExpectedMemoryLeaks <> nil) and (ExpectedMemoryLeaksCount > 0) then
  begin
    C := ExpectedMemoryLeaksScanCollisions;
    I := NativeUInt(P) mod CExpectedMemoryLeaksListSizePrim;
    while C > 0 do
    begin
      if ExpectedMemoryLeaks[I] = P then
      begin
        Result := True;
        Exit;
      end;

      Inc(I);
      if I >= CExpectedMemoryLeaksListSize then
        I := 0;

      Dec(C);
    end;
  end;
end;

function IsExpectedMemoryLeak(P: Pointer): Boolean;
var
  I: Int32;
begin
  if LockExpectedMemoryLeaksList and (P <> nil) then
    Result := IsExpectedMemoryLeakIndex(P, I)
  else
    Result := True;

  //LockRelease(@ExpectedMemoryLeaksLocked);
  ExpectedMemoryLeaksLocked := 0;
end;

function RegisterExpectedMemoryLeak(P: Pointer): Boolean;
var
  I, C: Int32;
begin
  if LockExpectedMemoryLeaksList and (P <> nil) and (ExpectedMemoryLeaksCount < CExpectedMemoryLeaksListSize) then
  begin
    C := 1;
    I := NativeUInt(P) mod CExpectedMemoryLeaksListSizePrim;
    while ExpectedMemoryLeaks[I] <> nil do
    begin
      Inc(C);
      Inc(I);
      if I >= CExpectedMemoryLeaksListSize then
        I := 0;
    end;

    ExpectedMemoryLeaks[I] := P;
    Inc(ExpectedMemoryLeaksCount);
    if C > ExpectedMemoryLeaksScanCollisions then
      ExpectedMemoryLeaksScanCollisions := C;

    Result := True;
  end
  else
    Result := False;

  //LockRelease(@ExpectedMemoryLeaksLocked);
  ExpectedMemoryLeaksLocked := 0;
end;

function UnregisterExpectedMemoryLeak(P: Pointer): Boolean;
var
  I: Int32;
begin
  if LockExpectedMemoryLeaksList and (P <> nil) and IsExpectedMemoryLeakIndex(P, I) then
  begin
    ExpectedMemoryLeaks[I] := nil;
    Dec(ExpectedMemoryLeaksCount);
    Result := True;
  end
  else
    Result := False;

  //LockRelease(@ExpectedMemoryLeaksLocked);
  ExpectedMemoryLeaksLocked :=0;
end;

// Checks for memory leaks on shutdown
procedure ScanForMemoryLeaks;
var
  LLeakMessage: array[0..32767] of AnsiChar;
  LMsgPtr: PAnsiChar;
  LNumMediumAndLargeLeaks, LIndPool: Int32;
  LExpectedLeaksOnly: Boolean;

  // The leaked classes for small blocks
  LSmallBlockLeaks: TSmallBlockLeaks;
  LMediumAndLargeBlockLeaks: TMediumAndLargeBlockLeaks;
  LPLargeBlock: PLargeBlockHeader;
  LPMediumBlock: Pointer;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LBlockSize, LBlockSizeAndFlags: NativeUInt;
  LBlockTypeInd, LClassInd, LPreviousBlockSize, LThisBlockSize, LBlockInd: UInt32;
  LSmallLeakHeaderAdded, LBlockSizeHeaderAdded: Boolean;

  // Checks the small block pool for leaks.
  procedure CheckSmallBlockPoolForLeaksPool(APool: PThreadPool; APSmallBlockPool: PSmallBlockPoolHeader);
  var
    LLeakedClass: TClass;
    LPAnsiStr: PAnsiChar;
    LPUniStr: PWideChar;
    LCurPtr, LEndPtr: Pointer;
    LPLeakedClasses: PLeakedClasses;
    LCharInd, LClassIndex, LStringLength, LElemSize: Int32;
    LBlockTypeIndex: UInt32;
    LPossibleString: Boolean;
  begin
    // Get the block type index
    LBlockTypeIndex := (NativeUInt(APSmallBlockPool.BlockType) - NativeUInt(@APool.SmallBlockTypes[0]))
      div SizeOf(TSmallBlockType);
    LPLeakedClasses := @LSmallBlockLeaks[LBlockTypeIndex];
    
    // Get the first and last pointer for the pool
    GetFirstAndLastSmallBlockInPool(APSmallBlockPool, LCurPtr, LEndPtr);

    // Step through all blocks
    while NativeUInt(LCurPtr) <= NativeUInt(LEndPtr) do
    begin
      // Is this block an unexpected leak?
      if ((PNativeUInt(NativeUInt(LCurPtr) - CBlockHeaderSize)^ and CIsFreeBlockFlag) = 0)
        and (not IsExpectedMemoryLeak(LCurPtr)) then
      begin
        LExpectedLeaksOnly := False;

        // Default to an unknown block
        LClassIndex := 0;

        // Get the class contained by the block
        LLeakedClass := GetObjectClass(LCurPtr);

        // Not a class? -> is it perhaps a string?
        if LLeakedClass = nil then
        begin
          // Reference count < 256
          if PStrRec(LCurPtr).refCnt < 256 then
          begin
            // Get the string length and element size
            LStringLength := PStrRec(LCurPtr).length;
{$if CompilerVersion >= 20} // Delphi 2009
            LElemSize := PStrRec(LCurPtr).elemSize;
{$else}
            LElemSize := 1;
{$ifend}

            // Valid element size?
            if (LElemSize = 1) or (LElemSize = 2) then
            begin
              // Does the string fit?
              if (LStringLength > 0)
                and ((APSmallBlockPool.BlockType.BlockSize - CBlockHeaderSize - SizeOf(StrRec)) div LElemSize > LStringLength) then
              begin
                // It is possibly a string
                LPossibleString := True;

                // Check for no characters < #32. If there are, then it is probably not a string.
                if LElemSize = 1 then
                begin
                  // Check that all characters are >= #32
                  LPAnsiStr := PAnsiChar(NativeUInt(LCurPtr) + SizeOf(StrRec));
                  for LCharInd := 1 to LStringLength do
                  begin
                    LPossibleString := LPossibleString and (LPAnsiStr^ >= #32);
                    Inc(LPAnsiStr);
                  end;

                  // Must have a trailing #0
                  if LPossibleString and (LPAnsiStr^ = #0) then
                    LClassIndex := 1;
                end
                else
                begin
                  // Check that all characters are >= #32
                  LPUniStr := PWideChar(NativeUInt(LCurPtr) + SizeOf(StrRec));
                  for LCharInd := 1 to LStringLength do
                  begin
                    LPossibleString := LPossibleString and (LPUniStr^ >= #32);
                    Inc(LPUniStr);
                  end;

                  // Must have a trailing #0
                  if LPossibleString and (LPUniStr^ = #0) then
                    LClassIndex := 2;
                end;
              end;
            end;
          end;
        end
        else
        begin
          // Class index 3 and up are actual classes
          LClassIndex := 3;
          while LClassIndex <= High(TLeakedClasses) do
          begin
            if (LPLeakedClasses[LClassIndex].ClassPointer = LLeakedClass)
              or (LPLeakedClasses[LClassIndex].ClassPointer = nil) then
            begin
              Break;
            end;
            Inc(LClassIndex);
          end;
          if LClassIndex <= High(TLeakedClasses) then
            LPLeakedClasses[LClassIndex].ClassPointer := LLeakedClass
          else
            LClassIndex := 0;
        end;

        // Add to the number of leaks for the class
        Inc(LPLeakedClasses[LClassIndex].NumLeaks);
      end;

      // Next block
      Inc(NativeUInt(LCurPtr), APSmallBlockPool.BlockType.BlockSize);
    end;
  end;

  procedure ScanForMemoryLeaksPool(APool: PThreadPool);
  begin
    // Step through all the medium block pools
    LPMediumBlockPoolHeader := APool.MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    while LPMediumBlockPoolHeader <> @APool.MediumBlockPoolsCircularList do
    begin
      LPMediumBlock := GetFirstMediumBlockInPool(APool, LPMediumBlockPoolHeader);
      while LPMediumBlock <> nil do
      begin
        LBlockSizeAndFlags := PNativeUInt(NativeUInt(LPMediumBlock) - CBlockHeaderSize)^;
        // Is the block in use?
        if (LBlockSizeAndFlags and CIsFreeBlockFlag) = 0 then
        begin
          if (LBlockSizeAndFlags and CIsSmallBlockPoolInUseFlag) <> 0 then
          begin
            // Get all the leaks for the small block pool
            CheckSmallBlockPoolForLeaksPool(APool, LPMediumBlock);
          end
          else
          begin
            if LNumMediumAndLargeLeaks < Length(LMediumAndLargeBlockLeaks) then
            begin
              // Is it an expected leak?
              if not IsExpectedMemoryLeak(LPMediumBlock) then
              begin
                LExpectedLeaksOnly := False;

                // Add the leak to the list
                LBlockSize := (LBlockSizeAndFlags and CExtractMediumSizeMask) - CBlockHeaderSize;
                LMediumAndLargeBlockLeaks[LNumMediumAndLargeLeaks] := LBlockSize;
                Inc(LNumMediumAndLargeLeaks);
              end;
            end;
          end;
        end;

        // Next medium block
        LPMediumBlock := NextMediumBlock(LPMediumBlock);
      end;

      // Get the next medium block pool
      LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
    end;

    // Get all leaked large blocks
    LPLargeBlock := APool.LargeBlocksCircularList.NextLargeBlockHeader;
    while (LPLargeBlock <> @APool.LargeBlocksCircularList)
      and (LNumMediumAndLargeLeaks < Length(LMediumAndLargeBlockLeaks)) do
    begin
      // Is it an expected leak?
      if not IsExpectedMemoryLeak(Pointer(NativeUInt(LPLargeBlock) + CLargeBlockHeaderSize)) then
      begin
        // Add the leak
        LExpectedLeaksOnly := False;
        LBlockSize := (LPLargeBlock.BlockSizeAndFlags and CExtractLargeSizeMask)
          - CBlockHeaderSize - CLargeBlockHeaderSize;
        LMediumAndLargeBlockLeaks[LNumMediumAndLargeLeaks] := LBlockSize;
        Inc(LNumMediumAndLargeLeaks);
      end;

      // Get the next large block
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;
  end;

begin
  // Clear the leak arrays
  FillChar(LSmallBlockLeaks, SizeOf(LSmallBlockLeaks), 0);
  FillChar(LMediumAndLargeBlockLeaks, SizeOf(LMediumAndLargeBlockLeaks), 0);

  LMsgPtr := @LLeakMessage[0];

  // Step through all the medium block pools
  LNumMediumAndLargeLeaks := 0;

  // No unexpected leaks so far
  LExpectedLeaksOnly := True;

  for LIndPool := 0 to High(ThreadPools) do
    ScanForMemoryLeaksPool(@ThreadPools[LIndPool]);

  // Display the leak message if required
  if not LExpectedLeaksOnly then
  begin
    // Small leak header has not been added
    LSmallLeakHeaderAdded := False;
    LPreviousBlockSize := 0;

    // Set up the leak message header so long
    LMsgPtr := AppendToBuffer(CLeakMessageHeader, Length(CLeakMessageHeader), LMsgPtr);

    // Step through all the small block types
    for LBlockTypeInd := 0 to (CNumSmallBlockTypes - 1) do
    begin
      LThisBlockSize := CSmallBlockSizes[LBlockTypeInd].BlockSize - CBlockHeaderSize;
      LBlockSizeHeaderAdded := False;

      // Any leaks?
      for LClassInd := High(LSmallBlockLeaks[LBlockTypeInd]) downto 0 do
      begin
        // Is there still space in the message buffer? Reserve space for the message footer.
        if LMsgPtr > @LLeakMessage[High(LLeakMessage) - 2048] then
          Break;

        // Check the count
        if LSmallBlockLeaks[LBlockTypeInd][LClassInd].NumLeaks > 0 then
        begin
          // Need to add the header?
          if not LSmallLeakHeaderAdded then
          begin
            LMsgPtr := AppendToBuffer(CSmallLeakDetail, Length(CSmallLeakDetail), LMsgPtr);
            LSmallLeakHeaderAdded := True;
          end;

          // Need to add the size header?
          if not LBlockSizeHeaderAdded then
          begin
            LMsgPtr := AppendToBufferLn(LMsgPtr);
            LMsgPtr := AppendToBuffer(LPreviousBlockSize + 1, LMsgPtr);
            LMsgPtr := AppendToBuffer(' ', LMsgPtr);
            LMsgPtr := AppendToBuffer('-', LMsgPtr);
            LMsgPtr := AppendToBuffer(' ', LMsgPtr);
            LMsgPtr := AppendToBuffer(LThisBlockSize, LMsgPtr);
            LMsgPtr := AppendToBuffer(CBytesMessage, Length(CBytesMessage), LMsgPtr);
            LBlockSizeHeaderAdded := True;
          end
          else
          begin
            LMsgPtr := AppendToBuffer(',', LMsgPtr);
            LMsgPtr := AppendToBuffer(' ', LMsgPtr);
          end;

          // Show the count
          case LClassInd of
            // Unknown
            0:
            begin
              LMsgPtr := AppendToBuffer(CUnknownClassNameMsg, Length(CUnknownClassNameMsg), LMsgPtr);
            end;
            // AnsiString
            1:
            begin
              LMsgPtr := AppendToBuffer(CAnsiStringBlockMessage, Length(CAnsiStringBlockMessage), LMsgPtr);
            end;
            // UnicodeString
            2:
            begin
              LMsgPtr := AppendToBuffer(CUnicodeStringBlockMessage, Length(CUnicodeStringBlockMessage), LMsgPtr);
            end;
            // Classes
            else
            begin
              LMsgPtr := AppendClassNameToBuffer(LSmallBlockLeaks[LBlockTypeInd][LClassInd].ClassPointer, LMsgPtr);
            end;
          end;

          // Add the count
          LMsgPtr := AppendToBuffer(' ', LMsgPtr);
          LMsgPtr := AppendToBuffer(LSmallBlockLeaks[LBlockTypeInd][LClassInd].NumLeaks, LMsgPtr);
        end;
      end;

      // If the minimum block alignment is 16 bytes, then disregard 8-byte
      // aligned sizes in the calculation of the starting size for the next block
      // type. Note: Under 32-bit it is possible for there to be 8-byte aligned
      // leaks if the minimum alignment was set to 16 bytes after the leaked
      // blocks were allocated.
      if LBlockSizeHeaderAdded
        or (CMinimumBlockAlignment = 8)
        or (((LThisBlockSize + CBlockHeaderSize) and 15) = 0) then
      begin
        LPreviousBlockSize := LThisBlockSize;
      end;
    end;

    // Add the medium/large block leak message
    if LNumMediumAndLargeLeaks > 0 then
    begin
      // Any non-small leaks?
      if LSmallLeakHeaderAdded then
      begin
        LMsgPtr := AppendToBufferLn(LMsgPtr);
        LMsgPtr := AppendToBufferLn(LMsgPtr);
      end;

      // Add the medium/large block leak message
      LMsgPtr := AppendToBuffer(CLargeLeakDetail, Length(CLargeLeakDetail), LMsgPtr);

      // List all the blocks
      for LBlockInd := 0 to (LNumMediumAndLargeLeaks - 1) do
      begin
        if LBlockInd <> 0 then
        begin
          LMsgPtr := AppendToBuffer(',', LMsgPtr);
          LMsgPtr := AppendToBuffer(' ', LMsgPtr);
        end;
        LMsgPtr := AppendToBuffer(LMediumAndLargeBlockLeaks[LBlockInd], LMsgPtr);

        // Is there still space in the message buffer? Reserve space for the message footer.
        if LMsgPtr > @LLeakMessage[High(LLeakMessage) - 2048] then
          Break;
      end;
    end;

    FileWriteText(@ExpectedMemoryLeaksFileName[0], LMsgPtr, LMsgPtr - @LLeakMessage[0]);
  end;
end;

initialization
  InitializeTrackLeak;

finalization
  FinalizeTrackLeak;

end.
