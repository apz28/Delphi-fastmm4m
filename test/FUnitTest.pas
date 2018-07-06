{
License:
 This work is copyright Professional Software Development / An Pham. It
 is released under a dual license, and you may choose to use it under either the
 Mozilla Public License 1.1 (MPL 1.1, available from
 http://www.mozilla.org/MPL/MPL-1.1.html)
 or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
 http://www.opensource.org/licenses/lgpl-license.php).


Completed Change Log:
 Please see FChange.log file
}

unit FUnitTest;

interface

{$if defined(CompilerVersion) = False}
  {$define CompilerVersion = 15} // Delphi 7
{$ifend}

uses
  Windows, SysUtils, Math, Classes, SyncObjs; // System

const
  CTestThreadCount = 8;
  CTestThreadInterval = 1000 * 30;
  CTestMaxPointers = 50000;

type
{$if CompilerVersion <= 15} // Delphi 7
  UInt64 = Int64;

  NativeInt = Integer;
  PNativeInt = PInteger;

  NativeUInt = Cardinal;
  PNativeUInt = PCardinal;

  PMemoryManagerEx = System.PMemoryManager;
  TMemoryManagerEx = System.TMemoryManager;
{$ifend}

  TRandomGen = class(TObject)
  private
    FSeed: array [0..1] of UInt64;
  public
    constructor Create(ASeed: UInt64);
    function Next: UInt64; overload;
    function Next(const AMax: Cardinal): Cardinal; overload;
    procedure Randomize;
    class function RotL(const x: UInt64; const k: Byte): UInt64;
    procedure SetSeed(ASeed: UInt64);
    class function SplitMix(var x: UInt64): UInt64;
  end;

  TThreadIDList = class(TStringList)
  private
    FLock: TCriticalSection;
  public
    Slots: array[0..1023] of Integer; // Best for slot count is 251
    ColliCount: Integer;
    DiffIDCount: Integer;
    DupIDCount: Integer;
    MaxColliCount: Integer;
    SlotSize: Cardinal;
    ThreadCounter: Integer;
    constructor Create(const ASize: Cardinal);
    destructor Destroy; override;
    function Check(const AThreadID: THandle): Integer;
  end;

  TThreadID = class(TThread)
  private
    FList: TThreadIDList;
  protected
    procedure Execute; override;
  public
    constructor Create(AList: TThreadIDList);
    destructor Destroy; override;
  end;

  TTestTerminated = function: Boolean of object;

  PTestPointer = ^TTestPointer;
  TTestPointer = record
    Ptr: PByte;
    Size: NativeInt;
    C: Byte;
    K: Byte;
  end;

  TTestCounter = record
    EMsg: string;
    GetMemCount: NativeUInt;
    GetMemCountMax: NativeUInt;
    GetMemFailedCount: NativeUInt;
    GetMemSize: Int64;
    GetMemSizeMax: Int64;
    GetMemTick: Int64;
    ReallocMemCount: NativeUInt;
    ReallocMemCountMax: NativeUInt;
    ReallocMemFailedCount: NativeUInt;
    ReallocMemSize: Int64;
    ReallocMemSizeMax: Int64;
    ReallocMemTick: Int64;
    FreeMemCount: NativeUInt;
    FreeMemTick: Int64;
  end;

  PTestMemory = ^TTestMemory;
  TTestMemory = record
    Ptrs: array of TTestPointer;
    Ptrc: Integer;
    Counter: TTestCounter;
    Manager: TMemoryManagerEx;
    RandomGen: TRandomGen;
    Terminated: TTestTerminated;
    KSeed: Word;
    Validated: Boolean;
  end;

  TTestInfo = record
    Name: string; // In - required
    Manager: TMemoryManagerEx; // In - required
    ThreadDone: TNotifyEvent;
    Index: Cardinal;
  end;

  TTestMonitorThread = class;

  TTestThread = class(TThread)
  private
    FMonitorThread: TTestMonitorThread;
    FInfo: TTestInfo;
    FRandomGen: TRandomGen;
    FTestMemory: TTestMemory;
    function GetCounter: TTestCounter;
    function IsTerminated: Boolean;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(const AMonitorThread: TTestMonitorThread; const AInfo: TTestInfo);
    destructor Destroy; override;
    property Counter: TTestCounter read GetCounter;
    property Info: TTestInfo read FInfo;
  end;

  TTestMonitorThread = class(TThread)
  private
    FThreads: array[0..CTestThreadCount - 1] of TTestThread;
    FInfo: TTestInfo;
  protected
    FUMThreadDone: Integer;
    procedure Execute; override;
  public
    constructor Create(const AInfo: TTestInfo);
  end;

const
  CStarterRandomSeed: UInt64 = 2463534242;

  CEmptyTestCounter: TTestCounter = (
    EMsg: '';
    GetMemCount: 0;
    GetMemCountMax: 0;
    GetMemFailedCount: 0;
    GetMemSize: 0;
    GetMemSizeMax: 0;
    GetMemTick: 0;
    ReallocMemCount: 0;
    ReallocMemCountMax: 0;
    ReallocMemFailedCount: 0;
    ReallocMemSize: 0;
    ReallocMemSizeMax: 0;
    ReallocMemTick: 0;
    FreeMemCount: 0;
    FreeMemTick: 0;
  );

procedure GetOptimumMemoryPoolSize(const AInfo: TStrings);

procedure FinaTestMemory(var Info: TTestMemory);
procedure InitTestMemory(out Info: TTestMemory);

procedure AddCounter(var Destination: TTestCounter; const ASource: TTestCounter);

function TestSeed(var Info: TTestMemory): Byte;
function TestValidated(P: PTestPointer; ASize: NativeInt; out At: NativeUInt; out V: Byte): Boolean;
procedure TestFreeMem(var Info: TTestMemory);
procedure TestGetMem(var Info: TTestMemory; const AMaxSize, AMaxCount, AStaticSize: NativeInt);
procedure TestReallocMem(var Info: TTestMemory; const AMaxSize, AMaxCount: NativeInt);

var
  RefreshApplication: procedure of object;
  MemoryBug: Boolean;

implementation

procedure GetOptimumMemoryPoolSize(const AInfo: TStrings);
  procedure DoPoolSize(const ASize: Cardinal);
  var
    List: TThreadIDList;
    I, MaxThreads: Integer;
  begin
    MaxThreads := Min(ASize * 2, 400);
    List := TThreadIDList.Create(ASize);
    for I := 1 to 100000 do
    begin
      TThreadID.Create(List);
      if List.ThreadCounter > MaxThreads then
        Sleep(2);
    end;
    while List.ThreadCounter > 0 do
      Sleep(100);
    AInfo.Add(Format('Size: %d, DiffID: %d, DupID: %d, ColliCount: %d, ColliMaxCount: %d', [
      ASize, List.DiffIDCount, List.DupIDCount, List.ColliCount, List.MaxColliCount]));
    List.Free;
  end;

const
  // Prim numbers
  CPoolSizes: array[0..51] of Integer = (
    5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
    41, 43, 47, 53, 59, 61, 67, 71, 73, 79,
    83, 89, 97, 101, 103, 107, 109, 113, 127, 131,
    137, 139, 149, 151, 157, 163, 167, 173, 179, 181,
    191, 193, 197, 199, 211, 223, 227, 229, 233, 239,
    241, 251
  );

var
  I: Integer;
begin
  AInfo.Add('Best Size one is with smallest number of ColliCount & ColliMaxCount');
  for I := 0 to High(CPoolSizes) do
  begin
    if Assigned(RefreshApplication) then
      RefreshApplication();
      
    DoPoolSize(CPoolSizes[I]);
  end;
end;

procedure FinaTestMemory(var Info: TTestMemory);
begin
  SetLength(Info.Ptrs, 0);
  FillChar(Info, SizeOf(Info), 0);
end;

procedure InitTestMemory(out Info: TTestMemory);
begin
  FillChar(Info, SizeOf(Info), 0);

  SetLength(Info.Ptrs, CTestMaxPointers);
  FillChar(Info.Ptrs[0], CTestMaxPointers * SizeOf(TTestPointer), 0);
end;

procedure AddCounter(var Destination: TTestCounter; const ASource: TTestCounter);
begin
  Inc(Destination.GetMemCount, ASource.GetMemCount);
  Inc(Destination.GetMemFailedCount, ASource.GetMemFailedCount);
  Inc(Destination.GetMemSize, ASource.GetMemSize);
  Inc(Destination.GetMemTick, ASource.GetMemTick);
  Inc(Destination.ReallocMemCount, ASource.ReallocMemCount);
  Inc(Destination.ReallocMemFailedCount, ASource.ReallocMemFailedCount);
  Inc(Destination.ReallocMemSize, ASource.ReallocMemSize);
  Inc(Destination.ReallocMemTick, ASource.ReallocMemTick);
  Inc(Destination.FreeMemCount, ASource.FreeMemCount);
  Inc(Destination.FreeMemTick, ASource.FreeMemTick);

  Destination.GetMemCountMax := Max(Destination.GetMemCountMax, ASource.GetMemCountMax);
  Destination.GetMemSizeMax := Max(Destination.GetMemSizeMax, ASource.GetMemSizeMax);

  Destination.ReallocMemCountMax := Max(Destination.ReallocMemCountMax, ASource.ReallocMemCountMax);
  Destination.ReallocMemSizeMax := Max(Destination.ReallocMemSizeMax, ASource.ReallocMemSizeMax);
end;

function TestSeed(var Info: TTestMemory): Byte;
begin
  Info.KSeed := (Info.KSeed + 1) mod 256;
  if Info.KSeed = 0 then
    Info.KSeed := 1;
  Result := Info.KSeed;
end;

function TestValidated(P: PTestPointer; ASize: NativeInt; out At: NativeUInt; out V: Byte): Boolean;
var
  Ptr: PByte;
  I: NativeInt;
begin
  At := NativeUInt(-1);
  V := $FF;

  Ptr := P.Ptr;
  for I := 0 to (ASize - 1) do
  begin
    if Ptr^ <> P.C then
    begin
      V := Ptr^;
      At := I;
      Result := False;
      Exit;
    end;
    Inc(Ptr);
  end;
  Result := True;
end;

var
  FreeMemCount: Integer;

procedure TestFreeMem(var Info: TTestMemory);
var
  P: TTestPointer;
  T, At, FreeCount: NativeUInt;
  R: Integer;
  V: Byte;
begin
  Assert(Info.Ptrc >= 0);

  FreeCount := 0;

  T := GetTickCount;
  while (Info.Ptrc > 0) and (not MemoryBug) do
  begin
    InterlockedIncrement(FreeMemCount);

    Dec(Info.Ptrc);
    P := Info.Ptrs[Info.Ptrc];
    with Info.Ptrs[Info.Ptrc] do
    begin
      Ptr := nil;
      Size := 0;
      K := 11;
    end;

    if P.Ptr = nil then
      raise Exception.CreateFmt('TestFreeMem - Invalid pointer at: %u, size: %u, k: %u', [
        Info.Ptrc, P.Size, P.K]);

    if P.Size = 0 then
      raise Exception.CreateFmt('TestFreeMem - Invalid size at: %u, size: %u, k: %u', [
        Info.Ptrc, P.Size, P.K]);

    if Info.Validated then
    begin
      if not TestValidated(@P, P.Size, At, V) then
      begin
        MemoryBug := True;
        raise Exception.CreateFmt('TestFreeMem - Invalid value at: %u, size: %u, value: %u/%u, k: %u', [
          At, P.Size, V, P.C, P.K]);
      end;

      FillChar(P.Ptr^, P.Size, 0);
    end;

    R := Info.Manager.FreeMem(P.Ptr);
    if R <> 0 then
    begin
      MemoryBug := True;
      raise Exception.CreateFmt('TestFreeMem - r=%d, k=%d, size=%d', [R, P.K, P.Size]);
    end;
    Inc(FreeCount);
  end;
  T := GetTickCount - T;
  
  Inc(Info.Counter.FreeMemTick, T);
  Inc(Info.Counter.FreeMemCount, FreeCount);

  if not MemoryBug then
  begin
    Assert(Info.Ptrc >= 0);
    if Length(Info.Ptrs) = 0 then
      raise Exception.CreateFmt('TestFreeMem: Length(Info.Ptrs) is zero [%d]', [Info.Ptrc]);
  end;
end;

procedure TestGetMem(var Info: TTestMemory; const AMaxSize, AMaxCount, AStaticSize: NativeInt);
var
  P: TTestPointer;
  AllocSize: Int64;
  T, AllocCount: NativeUInt;
  I, SCnt: Integer;
begin
  SCnt := 0;
  AllocCount := 0;
  AllocSize := 0;
  FillChar(P, SizeOf(TTestPointer), 0);

  T := GetTickCount;
  for I := 1 to AMaxCount do
  begin
    if Info.Terminated or MemoryBug then
      Break;

    P.K := 10;
    if AStaticSize <= 0 then
    begin
      if SCnt = 0 then
      begin
        SCnt := Info.RandomGen.Next(7);
        P.Size := Info.RandomGen.Next(AMaxSize);
        //if P.Size <= MaximumSmallBlockUserSize then
        //  Inc(P.Size, MaximumSmallBlockUserSize);
      end;

      Dec(SCnt);
    end
    else
      P.Size := AStaticSize;
    P.Ptr := Info.Manager.GetMem(P.Size);

    if P.Ptr = nil then
    begin
      Inc(Info.Counter.GetMemFailedCount);
      Break;
    end;

    if P.Size <= 0 then
      raise Exception.CreateFmt('TestGetMem - Invalid new size at: %u, size: %u, k: %u', [
        I, P.Size, P.K]);

    if Info.Ptrc >= Length(Info.Ptrs) then
      raise Exception.CreateFmt('TestGetMem index overflow: %d / %d', [Info.Ptrc, Length(Info.Ptrs)]);

    if Info.Validated then
    begin
      P.C := TestSeed(Info);
      FillChar(P.Ptr^, P.Size, P.C);
    end;
    
    Info.Ptrs[Info.Ptrc] := P;
    Inc(Info.Ptrc);
    Inc(AllocSize, P.Size);
    Inc(AllocCount);
  end;
  T := GetTickCount - T;
  
  Inc(Info.Counter.GetMemTick, T);
  Inc(Info.Counter.GetMemCount, AllocCount);
  Inc(Info.Counter.GetMemSize, AllocSize);
  Info.Counter.GetMemCountMax := Max(Info.Counter.GetMemCountMax, AllocCount);
  Info.Counter.GetMemSizeMax := Max(Info.Counter.GetMemSizeMax, AllocSize);

  if not MemoryBug then
  begin
    Assert(Info.Ptrc > 0);
    if Length(Info.Ptrs) = 0 then
      raise Exception.CreateFmt('TestGetMem: Length(Info.Ptrs) is zero [%d]', [Info.Ptrc]);
  end;
end;

procedure TestReallocMem(var Info: TTestMemory; const AMaxSize, AMaxCount: NativeInt);
var
  P: TTestPointer;
  AllocSize: Int64;
  T, At, AllocCount: NativeUInt;
  VSize: NativeInt;
  I, Count, K, SSiz1, SCnt1, SSiz2, SCnt2: Integer;
  V: Byte;
begin
  Assert(Info.Ptrc >= 0);

  Count := Min(AMaxCount, Info.Ptrc);
  if Count <= 0 then
    Exit;

  AllocCount := 0;
  AllocSize := 0;
  SCnt1 := 0;
  SSiz1 := 0;
  SCnt2 := 0;
  SSiz2 := 0;
  K := -1;

  T := GetTickCount;
  for I := 0 to (Count - 1) do
  begin
    if Info.Terminated or MemoryBug then
      Break;

    K := (K + 1) mod 3;
    P := Info.Ptrs[I];

    if P.Ptr = nil then
      raise Exception.CreateFmt('TestReallocMem - Invalid pointer at: %u, size: %u, k: %u', [
        I, P.Size, P.K]);

    if P.Size = 0 then
      raise Exception.CreateFmt('TestReallocMem - Invalid size at: %u, size: %u, k: %u', [
        I, P.Size, P.K]);

    if K = 0 then
    begin
(*
      P.Ptr := Info.Manager.ReallocMem(P.Ptr, P.Size);

      if P.Ptr <> Info.Ptrs[I].Ptr then
        raise Exception.CreateFmt('ReallocMem not return same pointer for same size at: %u, size: %u', [I, P.Size]);
*)
    end
    else if K = 1 then
    begin
      if SCnt1 = 0 then
      begin
        SCnt1 := Info.RandomGen.Next(7);
        SSiz1 := Info.RandomGen.Next(Max(P.Size div 2, 1));
      end;
      P.Size := P.Size - Min(SSiz1, P.Size - Max(P.Size div 2, 1));
      Dec(SCnt1);
    end
    else
    begin
      if SCnt2 = 0 then
      begin
        SCnt2 := Info.RandomGen.Next(7);
        SSiz2 := Info.RandomGen.Next(Max(P.Size div 2, 1));
      end;
      P.Size := P.Size + SSiz2;
      Dec(SCnt2);
    end;

    if (P.Size > 0) and (P.Size <> Info.Ptrs[I].Size) then
    begin
      P.Ptr := Info.Manager.ReallocMem(P.Ptr, P.Size);

      if P.Ptr = nil then
      begin
        Inc(Info.Counter.ReallocMemFailedCount);
        Break;
      end;

      VSize := Min(P.Size, Info.Ptrs[I].Size);
      if (VSize <> 0) and Info.Validated and (not TestValidated(@P, VSize, At, V)) then
      begin
        MemoryBug := True;
        raise Exception.CreateFmt('TestReallocMem - Invalid value at: %u, size: %u/%u, value: %u/%u, k: %u', [
          At, VSize, Info.Ptrs[I].Size, V, P.C, P.K]);
      end;

      if P.Size = 0 then
        raise Exception.CreateFmt('TestReallocMem - Invalid new size at: %u, size: %u, k: %u', [
          I, P.Size, P.K]);

      if Info.Validated and ((P.Ptr <> Info.Ptrs[I].Ptr) or (P.Size <> Info.Ptrs[I].Size)) then
      begin
        P.C := TestSeed(Info);
        FillChar(P.Ptr^, P.Size, P.C);
      end;
      
      P.K := K;
      Info.Ptrs[I] := P;
      Inc(AllocSize, P.Size);
      Inc(AllocCount);
    end;
  end;
  T := GetTickCount - T;
  
  Inc(Info.Counter.ReallocMemTick, T);
  Inc(Info.Counter.ReallocMemSize, AllocSize);
  Inc(Info.Counter.ReallocMemCount, AllocCount);
  Info.Counter.ReallocMemCountMax := Max(Info.Counter.ReallocMemCountMax, AllocCount);
  Info.Counter.ReallocMemSizeMax := Max(Info.Counter.ReallocMemSizeMax, AllocSize);

  if not MemoryBug then
  begin
    Assert(Info.Ptrc > 0);
    if Length(Info.Ptrs) = 0 then
      raise Exception.CreateFmt('TestReallocMem: Length(Info.Ptrs) is zero [%d]', [Info.Ptrc]);
  end;
end;


{ TThreadIDList }

function TThreadIDList.Check(const AThreadID: THandle): Integer;
var
  S: string;
  NewThreadID: THandle;
  Slot: Integer;
  Colli: Boolean;
begin
  S := IntToStr(AThreadID);

  NewThreadID := AThreadID;
  while (NewThreadID and $FF) = 0 do
    NewThreadID := NewThreadID shr 8;

  Slot := NewThreadID mod SlotSize;
  Colli := Windows.InterlockedIncrement(Slots[Slot]) > 1;

  FLock.Enter;

  if AThreadID <> NewThreadID then
    Inc(DiffIDCount);

  if Colli then
  begin
    Inc(ColliCount);
    if MaxColliCount < Slots[Slot] then
      MaxColliCount := Slots[Slot];
  end;
      
  if IndexOf(S) >= 0 then
    Inc(DupIDCount)
  else
    Add(S);

  FLock.Leave;

  Result := Slot;
end;

constructor TThreadIDList.Create(const ASize: Cardinal);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  SlotSize := ASize;
  Capacity := 1000000;
  Duplicates := dupError;
  Sorted := True;
end;

destructor TThreadIDList.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

{ TThreadID }

constructor TThreadID.Create(AList: TThreadIDList);
begin
  Windows.InterlockedIncrement(AList.ThreadCounter);
  FList := AList;
  inherited Create(False);
end;

destructor TThreadID.Destroy;
begin
  inherited Destroy;
  Windows.InterlockedDecrement(FList.ThreadCounter);
end;

procedure TThreadID.Execute;
var
  Slot: Integer;
begin
  FreeOnTerminate := True;
  Slot := FList.Check(ThreadID);
  Sleep(1);
  Windows.InterlockedDecrement(FList.Slots[Slot]);
end;

{ TTestThread }

constructor TTestThread.Create(const AMonitorThread: TTestMonitorThread; const AInfo: TTestInfo);
begin
  FMonitorThread := AMonitorThread;
  FInfo := AInfo;
  FRandomGen := TRandomGen.Create(CStarterRandomSeed + (AInfo.Index * 799)); // Consistent random number
  inherited Create(False);
end;

destructor TTestThread.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FRandomGen);
end;

procedure TTestThread.DoTerminate;
begin
  if (FatalException <> nil) and (FatalException is Exception) then
    FTestMemory.Counter.EMsg := Exception(FatalException).Message;

  if Assigned(FInfo.ThreadDone) then
    FInfo.ThreadDone(Self);

  InterlockedIncrement(FMonitorThread.FUMThreadDone);

  FinaTestMemory(FTestMemory);

  inherited DoTerminate;
end;

procedure TTestThread.Execute;
begin
  FreeOnTerminate := True;
  InitTestMemory(FTestMemory);
  FTestMemory.Manager := FInfo.Manager;
  FTestMemory.RandomGen := FRandomGen;
  FTestMemory.Terminated := IsTerminated;
  //FTestMemory.Validated := True;

  while (not Terminated) and (not MemoryBug) do
  begin
    Assert(FTestMemory.Ptrc = 0);

    TestGetMem(FTestMemory, 64000, FRandomGen.Next(5000), 0);

    if (not Terminated) and (FRandomGen.Next(3) mod 2 = 0) and (FTestMemory.Ptrc <> 0) then
      TestReallocMem(FTestMemory, 50000, FRandomGen.Next(FTestMemory.Ptrc));

    if (not Terminated) and (FRandomGen.Next(3) mod 2 = 0) then
      TestGetMem(FTestMemory, 64000, FRandomGen.Next(2000), 0);

    if (not Terminated) and (FRandomGen.Next(3) mod 2 = 0) and (FTestMemory.Ptrc <> 0) then
      TestReallocMem(FTestMemory, 80000, FRandomGen.Next(FTestMemory.Ptrc));

    TestFreeMem(FTestMemory);
  end;
end;

function TTestThread.GetCounter: TTestCounter;
begin
  Result := FTestMemory.Counter;
end;

function TTestThread.IsTerminated: Boolean;
begin
  Result := Terminated;
end;

{ TTestMonitorThread }

constructor TTestMonitorThread.Create(const AInfo: TTestInfo);
begin
  FInfo := AInfo;
  inherited Create(False);
end;

procedure TTestMonitorThread.Execute;
var
  LInfo: TTestInfo;
  I: Integer;
begin
  FreeOnTerminate := True;
  MemoryBug := False;
  
  LInfo := FInfo;
  for I := 0 to (CTestThreadCount - 1) do
  begin
    LInfo.Index := I;
    FThreads[I] := TTestThread.Create(Self, LInfo);
  end;

  Sleep(CTestThreadInterval);

  for I := 0 to (CTestThreadCount - 1) do
    FThreads[I].Terminate;

  while FUMThreadDone <> CTestThreadCount do
    Sleep(50);
end;

{ TRandomGen }

constructor TRandomGen.Create(ASeed: UInt64);
begin
  if ASeed = 0 then
    Randomize
  else
    SetSeed(ASeed);
end;

function TRandomGen.Next: UInt64;
var
  s0, s1: UInt64;
begin
  s0 := FSeed[0];
  s1 := FSeed[1];
  Result := s0 + s1;

  if Result < 0 then
    Result := Abs(Result);

  s1 := s1 xor s0;
  FSeed[0] := RotL(s0, 55) xor s1 xor (s1 shl 14);
  FSeed[1] := RotL(s1, 36);
end;

function TRandomGen.Next(const AMax: Cardinal): Cardinal;
begin
  Assert(AMax <> 0);
  
  Result := (Next mod AMax) + 1;
end;

procedure TRandomGen.Randomize;
var
  Counter: Int64;
begin
  //SetSeed(RDTSC);
  if QueryPerformanceCounter(Counter) then
    SetSeed(Counter)
  else
    SetSeed(GetTickCount);
end;

class function TRandomGen.RotL(const x: UInt64; const k: Byte): UInt64;
begin
  Result := (x shl k) or (x shr (64 - k));
end;

procedure TRandomGen.SetSeed(ASeed: UInt64);
begin
  FSeed[0] := SplitMix(ASeed);
  FSeed[1] := SplitMix(ASeed);
end;

class function TRandomGen.SplitMix(var x: UInt64): UInt64;
var
  z: UInt64;
begin
  Inc(x, UInt64($9E3779B97F4A7C15));
	z := (x xor (x shr 30)) * UInt64($BF58476D1CE4E5B9);
	z := (z xor (z shr 27)) * UInt64($94D049BB133111EB);
	Result := z xor (z shr 31);
end;

end.
