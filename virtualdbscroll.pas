{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit VirtualDBScroll;

interface

uses
  VirtualDBScrollMemo, EmbeddedMemo, EmbeddedScrollBar, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VirtualDBScrollMemo', @VirtualDBScrollMemo.Register);
end;

initialization
  RegisterPackage('VirtualDBScroll', @Register);
end.
