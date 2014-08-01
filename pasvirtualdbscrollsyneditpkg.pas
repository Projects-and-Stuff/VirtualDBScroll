{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PASVirtualDBScrollSynEditPkg;

interface

uses
  PASEmbeddedSynEdit, PASVirtualDBScrollSynEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PASVirtualDBScrollSynEdit', @PASVirtualDBScrollSynEdit.Register
    );
end;

initialization
  RegisterPackage('PASVirtualDBScrollSynEditPkg', @Register);
end.
