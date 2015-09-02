unit uFpcMem;

{$mode objfpc}{$H+}
interface

function SysGetmem(Size: ptrint): Pointer;
function SysFreemem(p: pointer): ptrint;
function SysFreememSize(p: pointer; Size: ptrint): ptrint;
function SysAllocMem(size: ptrint): Pointer;
function SysMemSize(p: pointer): ptrint;
function SysGetHeapStatus: THeapStatus;
function SysGetFPCHeapStatus: TFPCHeapStatus;

implementation

function SysGetmem(Size: ptrint): Pointer;
begin
  Result := System.SysGetmem(Size);
end;

function SysFreemem(p: pointer): ptrint;
begin
  Result := System.SysFreemem(p);
end;

function SysFreememSize(p: pointer; Size: ptrint): ptrint;
begin
  Result := System.SysFreememSize(p, Size);
end;

function SysAllocMem(size: ptrint): Pointer;
begin
  Result := System.SysAllocMem(Size);
end;

function SysReAllocMem(var p: pointer; size: ptrint): Pointer;
begin
  Result := System.SysReallocMem(p, Size);
end;

function SysMemSize(p: pointer): ptrint;
begin
  Result := System.SysMemSize(p);
end;


function SysGetHeapStatus: THeapStatus;
begin
  Result := System.SysGetHeapStatus;
end;


function SysGetFPCHeapStatus: TFPCHeapStatus;
begin
  Result := System.SysGetFPCHeapStatus;
end;

end.

