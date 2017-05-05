{$if defined(CompilerVersion) = False}
{$define CompilerVersion = 15} // Delphi 7
{$ifend}

{.$define F4mTestManager}
{.$define F4mDebugManager}
{.$define F4mTestThreadPool}

{$if SizeOf(Pointer) = 8}
{$define F4mAlign16Bytes}
{$ifend}

// Include the memory leak tracking and reporting code. Without this define set
// the memory leak registration functions will do nothing and the memory leak
// report will not be saved on shutdown.
{.$define F4mIncludeMemoryLeakTrackingCode}

// Keep a instance of requested OS block for medium block
{.$define F4mCacheThreadOSAlloc}

// Increase requested size when doing reallocation
{.$define F4mReallocUpsize}

// Keep track of certain API calls
{.$define F4mStatisticInfo}

// Enable to use the pascal code path
{.$define F4mUsePascalCode}

// Use custom fixed size move routines for some small blocks
{$define F4mUseCustomFixedSizeMoveRoutines}

// Use custom variable size move routines for larger blocks
{$define F4mUseCustomVariableSizeMoveRoutines}

{$BOOLEVAL OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$STACKFRAMES OFF}
{$WRITEABLECONST OFF}

{$EXTENDEDSYNTAX ON}
{$TYPEDADDRESS ON}

{$ifndef F4mDebugManager}
{$OPTIMIZATION ON}
{$endif}

{$MINENUMSIZE 1}

{$if CompilerVersion >= 20} // Delphi 2009
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ifend}

