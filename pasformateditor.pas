unit PASFormatEditor;

{$mode objfpc}{$H+}
{$DEFINE DoLog}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls {$ifdef DoLog}, LazLogger{$endif},
  PropEdits;

type

  { TformPASFormatEditor }

  TformPASFormatEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    memoFormat: TMemo;
    procedure btnOKClick(Sender: TObject);
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

{ TformPASFormatEditor }

procedure TformPASFormatEditor.btnOKClick(Sender: TObject);
begin


  // Test that the statement is correctly formatted
  // If not, set ModalResult := mrNone;


end;

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
      Top := Mouse.CursorPos.y - (Height div 2);

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

