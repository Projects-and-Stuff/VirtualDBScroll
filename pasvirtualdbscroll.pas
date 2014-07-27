{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PASVirtualDBScroll;

interface

uses
  PASVirtualDBScrollMemo, PASEmbeddedMemo, PASEmbeddedScrollBar, 
  PASFormatEditor, PASVirtualDBScrollBase, PASEmbeddedRichMemo, 
  PASEmbeddedSynEdit, PASVirtualDBScrollRichMemo, PASVirtualDBScrollSynEdit, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PASVirtualDBScrollMemo', @PASVirtualDBScrollMemo.Register);
  RegisterUnit('PASVirtualDBScrollBase', @PASVirtualDBScrollBase.Register);
  RegisterUnit('PASVirtualDBScrollRichMemo', @PASVirtualDBScrollRichMemo.Register);
  RegisterUnit('PASVirtualDBScrollSynEdit', @PASVirtualDBScrollSynEdit.Register
    );
end;

initialization
  RegisterPackage('PASVirtualDBScroll', @Register);
end.
