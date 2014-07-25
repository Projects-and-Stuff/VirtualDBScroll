unit PASFormatEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  PropEdits;

type

  { TformPASFormatEditor }

  TformPASFormatEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    memoFormat: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

  { TPASFormatEditor }

  TPASFormatEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

var
  formPASFormatEditor: TformPASFormatEditor;

implementation

{$R *.lfm}

{ TPASFormatEditor }

procedure TPASFormatEditor.Edit;
var
  formPASFormatEditor: TformPASFormatEditor;
begin
  formPASFormatEditor := TformPASFormatEditor.Create(nil);

  try
    with formPASFormatEditor do
    begin

      Caption := 'Format Display';

      // Place form at mouse position
      Left := Mouse.CursorPos.x;
      Top := Mouse.CursorPos.y;

      // Set memo text to current property value
      memoFormat.Text := GetStrValue;

      ShowModal;
      if ModalResult = mrOk then
      begin
        // If user selects OK, set the property to the memo text value
        SetStrValue(memoFormat.Text);
      end;

    end;
  finally
    formPASFormatEditor.Free;
  end;
end;

function TPASFormatEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paMultiSelect];
end;

end.
