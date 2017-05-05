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
  Windows, SysUtils, Math, Classes;

const
  CTestThreadCount = 8;
  CTestThreadInterval = 1000 * 60;
  CTestMaxPointers = 50000;

type
{$if CompilerVersion <= 15} // Delphi 7
  NativeInt = Integer;
  PNativeInt = PInteger;

  NativeUInt = Cardinal;
  PNativeUInt = PCardinal;

  PMemoryManagerEx = System.PMemoryManager;
  TMemoryManagerEx = System.TMemoryManager;
{$ifend}

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
    GetMemc: NativeUInt;
    GetMemf: NativeUInt;
    GetMems: Int64;
    GetMemt: Int64;
    ReallocMemc: NativeUInt;
    ReallocMemf: NativeUInt;
    ReallocMems: Int64;
    ReallocMemt: Int64;
    FreeMemt: Int64;
  end;

  PTestMemory = ^TTestMemory;
  TTestMemory = record
    Ptrs: array of TTestPointer;
    Ptrc: Integer;
    Counter: TTestCounter;
    Manager: TMemoryManagerEx;
    RandomSeed: Cardinal;
    Terminated: TTestTerminated;
    KSeed: Word;
    Validated: Boolean;
  end;

  TTestInfo = record
    Name: string; // In - required
    Manager: TMemoryManagerEx; // In - required
    ThreadDone: TNotifyEvent;
    Index: Integer;
  end;

  TTestMonitorThread = class;

  TTestThread = class(TThread)
  private
    FMonitorThread: TTestMonitorThread;
    FInfo: TTestInfo;
    FTestMemory: TTestMemory;
    FRandomSeed: Cardinal;
    function IsTerminated: Boolean;
    function GetCounter: TTestCounter;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(const AMonitorThread: TTestMonitorThread; const AInfo: TTestInfo);
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
  CStarterRandomSeed = 2463534242;

  CEmptyTestCounter: TTestCounter = (
    EMsg: '';
    GetMemc: 0;
    GetMemf: 0;
    GetMems: 0;
    GetMemt: 0;
    ReallocMemc: 0;
    ReallocMemf: 0;
    ReallocMems: 0;
    ReallocMemt: 0;
    FreeMemt: 0;
  );

function GetRandom(var RandSeed: Cardinal; ARange: Cardinal): Cardinal;
procedure FinaTestMemory(var Info: TTestMemory);
procedure InitTestMemory(out Info: TTestMemory);
function TestSeed(var Info: TTestMemory): Byte;
function TestValidated(P: PTestPointer; ASize: NativeInt; out At: NativeUInt; out V: Byte): Boolean;
procedure TestFreeMem(var Info: TTestMemory);
procedure TestGetMem(var Info: TTestMemory; const AMaxSize, AMaxCount, AStaticSize: NativeInt);
procedure TestReallocMem(var Info: TTestMemory; const AMaxSize, AMaxCount: NativeInt);

implementation

var
  MemoryBug: Boolean;

function GetRandom(var RandSeed: Cardinal; ARange: Cardinal): Cardinal;
var
  Temp: Cardinal;
begin
  if ARange = 0 then
    ARange := 1;

  Temp := RandSeed xor (RandSeed shl 13);
  Temp := Temp xor (Temp shr 17);
  RandSeed := Temp xor (Temp shl 15);
  if RandSeed = 0 then
    RandSeed := 1;

  Result := RandSeed mod ARange;
  if Result = 0 then
    Result := 1;
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
  P: PTestPointer;
  T, At: NativeUInt;
  R: Integer;
  V: Byte;
begin
  Assert(Info.Ptrc >= 0);
  
  T := GetTickCount;
  while (Info.Ptrc > 0) and (not MemoryBug) do
  begin
    InterlockedIncrement(FreeMemCount);

    Dec(Info.Ptrc);
    P := @Info.Ptrs[Info.Ptrc];

    if P.Ptr = nil then
      raise Exception.CreateFmt('TestFreeMem - Invalid pointer at: %u, size: %u, k: %u', [
        Info.Ptrc, P.Size, P.K]);

    if P.Size = 0 then
      raise Exception.CreateFmt('TestFreeMem - Invalid size at: %u, size: %u, k: %u', [
        Info.Ptrc, P.Size, P.K]);

    if Info.Validated then
    begin
      if not TestValidated(P, P.Size, At, V) then
      begin
        MemoryBug := True;
        raise Exception.CreateFmt('TestFreeMem - Invalid value at: %u, size: %u, value: %u/%u, k: %u', [
          At, P.Size, V, P.C, P.K]);
      end;

      FillChar(P.Ptr^, P.Size, 0);
    end;

    R := Info.Manager.FreeMem(P.Ptr);
    if R = 0 then
    begin
      P.K := 11;
      P.Ptr := nil;
      P.Size := 0;
    end
    else
    begin
      MemoryBug := True;
      raise Exception.CreateFmt('TestFreeMem - r=%d, k=%d, size=%d', [R, P.K, P.Size]);
    end;
  end;
  T := GetTickCount - T;
  Inc(Info.Counter.FreeMemt, T);

  Assert(Info.Ptrc >= 0);
  if Length(Info.Ptrs) = 0 then
    raise Exception.CreateFmt('TestFreeMem: Length(Info.Ptrs) is zero [%d]', [Info.Ptrc]);
end;

procedure TestGetMem(var Info: TTestMemory; const AMaxSize, AMaxCount, AStaticSize: NativeInt);
var
  P: TTestPointer;
  T: NativeUInt;
  I, SCnt: Integer;
begin
  Assert(Info.Ptrc = 0);

  SCnt := 0;
  T := GetTickCount;
  for I := 1 to AMaxCount do
  begin
    if Info.Terminated or MemoryBug then
      Break;

    Inc(Info.Counter.GetMemc);
    P.K := 10;
    if AStaticSize <= 0 then
    begin
      if SCnt = 0 then
      begin
        SCnt := GetRandom(Info.RandomSeed, 7);
        P.Size := GetRandom(Info.RandomSeed, AMaxSize);
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
      Inc(Info.Counter.GetMemf);
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
    Inc(Info.Counter.GetMems, P.Size);
  end;
  T := GetTickCount - T;
  Inc(Info.Counter.GetMemt, T);

  Assert(Info.Ptrc > 0);
  if Length(Info.Ptrs) = 0 then
    raise Exception.CreateFmt('TestGetMem: Length(Info.Ptrs) is zero [%d]', [Info.Ptrc]);
end;

procedure TestReallocMem(var Info: TTestMemory; const AMaxSize, AMaxCount: NativeInt);
var
  P: TTestPointer;
  T, At: NativeUInt;
  VSize: NativeInt;
  I, Count, K, SSiz1, SCnt1, SSiz2, SCnt2: Integer;
  V: Byte;
begin
  Assert(Info.Ptrc >= 0);

  Count := Min(AMaxCount, Info.Ptrc);
  if Count <= 0 then
    Exit;

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

    Inc(Info.Counter.ReallocMemc);
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
        SCnt1 := GetRandom(Info.RandomSeed, 7);
        SSiz1 := GetRandom(Info.RandomSeed, P.Size div 2);
      end;
      P.Size := P.Size - Min(SSiz1, P.Size - (P.Size div 2));
      Dec(SCnt1);
    end
    else
    begin
      if SCnt2 = 0 then
      begin
        SCnt2 := GetRandom(Info.RandomSeed, 7);
        SSiz2 := GetRandom(Info.RandomSeed, P.Size div 2);
      end;
      P.Size := P.Size + SSiz2;
      Dec(SCnt2);
    end;

    if (P.Size > 0) and (P.Size <> Info.Ptrs[I].Size) then
    begin
      P.Ptr := Info.Manager.ReallocMem(P.Ptr, P.Size);

      if P.Ptr = nil then
      begin
        Inc(Info.Counter.ReallocMemf);
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
      Inc(Info.Counter.ReallocMems, P.Size);
    end;
  end;
  T := GetTickCount - T;
  Inc(Info.Counter.ReallocMemt, T);

  Assert(Info.Ptrc > 0);
  if Length(Info.Ptrs) = 0 then
    raise Exception.CreateFmt('TestReallocMem: Length(Info.Ptrs) is zero [%d]', [Info.Ptrc]);
end;

{ TTestThread }

constructor TTestThread.Create(const AMonitorThread: TTestMonitorThread; const AInfo: TTestInfo);
var
  I: Integer;
begin
  FMonitorThread := AMonitorThread;
  FInfo := AInfo;
  FRandomSeed := CStarterRandomSeed; // Consistent random number
  for I := 1 to AInfo.Index do
    GetRandom(FRandomSeed, 1);
  inherited Create(False);
  FreeOnTerminate := True;
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
  InitTestMemory(FTestMemory);
  FTestMemory.Manager := FInfo.Manager;
  FTestMemory.RandomSeed := FRandomSeed;
  FTestMemory.Terminated := IsTerminated;
  //FTestMemory.Validated := True;

  while (not Terminated) and (not MemoryBug) do
  begin
    Assert(FTestMemory.Ptrc = 0);

    TestGetMem(FTestMemory, 49999, 40000, 0);

    if not Terminated then
      TestReallocMem(FTestMemory, 59999, (FTestMemory.Ptrc div 3) * 2);

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
  FreeOnTerminate := True;
end;

procedure TTestMonitorThread.Execute;
var
  LInfo: TTestInfo;
  I: Integer;
begin
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

  MemoryBug := False;
end;

end.
