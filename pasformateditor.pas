unit PASFormatEditor;

{$mode objfpc}{$H+}
{$DEFINE dbgDBScroll}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls {$ifdef dbgDBScroll}, LazLogger{$endif},
  PropEdits, strutils;

type

  { TformPASFormatEditor }

  TformPASFormatEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    memoFormat: TMemo;
    procedure btnOKClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TformPASFormatEditor.Button1Click(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to memoFormat.Lines.Count - 1 do
  begin
    memoFormat.Lines[i] := AnsiReplaceText(memoFormat.Lines[i], #32, #$c2#$b7);
    memoFormat.Lines[i] := AnsiReplaceText(memoFormat.Lines[i], #9, '  >');
  end;
end;

procedure TformPASFormatEditor.Button2Click(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to memoFormat.Lines.Count - 1 do
  begin
    memoFormat.Lines[i] := AnsiReplaceText(memoFormat.Lines[i], #$c2#$b7, #32);
    memoFormat.Lines[i] := AnsiReplaceText(memoFormat.Lines[i], '  >', #9);
  end;
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
      memoFormat.Lines := TStrings(GetObjectValue);

      ShowModal;
      if ModalResult = mrOk then
      begin
        // If user selects OK, set the property to the memo text value
        SetPtrValue(memoFormat.Lines);
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

