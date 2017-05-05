# FastMM4m
A fast memory manager for Delphi (Enhance FastMM4 for multi-threads - MultiCore CPUs)

Description: A fast replacement memory manager for Embarcadero Delphi applications that scales well under multi-threaded usage, is not prone to memory fragmentation, and supports shared memory.
Orginal Implementation Homepage: https://github.com/pleriche/FastMM4
Advantages:
Avoid multi-threads locking issue from original implementation. Good for web usage and program with a lot of threads
Supports Delphi 4 (or later).
Usage: Delphi: Build FastMM4m.dpr to have FastMM4m.dll; place FMemoryLib.pas unit as the very first unit under the "uses" section in your project's .dpr file, include the search path to FastMM4m source directory and deploy FastMM4m.dll as part of the application. When sharing memory between an application and a DLL (e.g. when passing a long string or dynamic array to a DLL function), both the main application and the DLL must be compiled using this memory manager (with the required conditional defines set and function calls FShareMemoryManager and FAttemptToUseSharedMemoryManager). There are some conditional defines (inside FOption.inc) that may be used to tweak the memory manager. 
