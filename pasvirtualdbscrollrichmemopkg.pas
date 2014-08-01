{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PASVirtualDBScrollRichMemoPkg;

interface

uses
  PASEmbeddedRichMemo, PASVirtualDBScrollRichMemo, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PASVirtualDBScrollRichMemo', 
    @PASVirtualDBScrollRichMemo.Register);
end;

initialization
  RegisterPackage('PASVirtualDBScrollRichMemoPkg', @Register);
end.
