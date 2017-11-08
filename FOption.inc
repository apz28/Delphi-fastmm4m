{$if defined(CompilerVersion) = False}
  {$define CompilerVersion = 15} // Delphi 7
{$ifend}

{$if SizeOf(Pointer) = 8}
  {$define F4mAlign16Bytes}
{$ifend}

{.$define F4mTestManager}
{.$define F4mDebugManager}
{.$define F4mTestThreadPool}

// Include the memory leak tracking and reporting code. Without this define set
// the memory leak registration functions will do nothing and the memory leak
// report will not be saved on shutdown.
{.$define F4mIncludeMemoryLeakTrackingCode}

// Cache requested OS blocks for medium blocks
{$define F4mCacheThreadOSAlloc}

// Should tracking thread tied to memory pool - require to be built as DLL
{.$define F4mTrackThreadLife}

// Increase requested size when doing reallocation
{.$define F4mReallocUpsize}

// Keep track of certain API calls
{.$define F4mStatisticInfo}

{$define F4mInstallMemoryManager}
{$define F4mShareMemoryManager}
{$define F4mUseSharedMemoryManager}

// Enable to use the pascal code path
{.$define F4mUsePascalCode}

// Use custom fixed size move routines for some small blocks
{$define F4mUseCustomFixedSizeMoveRoutines}

// Use custom variable size move routines for larger blocks
{$define F4mUseCustomVariableSizeMoveRoutines}

{$ifdef F4mTestManager}
  {$undef F4mInstallMemoryManager}
{$elseif F4mDebugManager}
  {$undef F4mInstallMemoryManager}
{$ifend}

{$ifndef F4mInstallMemoryManager}
  {$undef F4mShareMemoryManager}
  {$undef F4mUseSharedMemoryManager}
{$endif}

// Off compiler directives
{$BOOLEVAL OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$STACKFRAMES OFF}
{$WRITEABLECONST OFF}

// On compiler directives
{$ALIGN ON}
{$EXTENDEDSYNTAX ON}
{$TYPEDADDRESS ON}

{$ifndef F4mDebugManager}
  {$ASSERTIONS OFF}
  {$DEBUGINFO OFF}
  
  {$OPTIMIZATION ON}
{$endif}

{$MINENUMSIZE 1}

{$if CompilerVersion >= 20} // Delphi 2009
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ifend}

