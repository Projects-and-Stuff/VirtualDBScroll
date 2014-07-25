{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PASVirtualDBScroll;

interface

uses
  PASVirtualDBScrollMemo, PASEmbeddedMemo, PASEmbeddedScrollBar, 
  PASFormatEditor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PASVirtualDBScrollMemo', @PASVirtualDBScrollMemo.Register);
end;

initialization
  RegisterPackage('PASVirtualDBScroll', @Register);
end.
