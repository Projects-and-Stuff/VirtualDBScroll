unit PASFormatEditor;

{
VirtualDBScroll Package

The MIT License (MIT)

Copyright (c) 2014 Jack D Linke

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
}

{$mode objfpc}{$H+}
{$DEFINE dbgDBScroll}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls {$ifdef dbgDBScroll}, LazLogger{$endif},
  PropEdits, strutils, IDEIntf, GraphPropEdits, typinfo, ComCtrls, db, DBPropEdits;

type

  { TformPASFormatEditor }

  TformPASFormatEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    cmbFieldType: TComboBox;
    cmbFieldNames: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    memoFormat: TMemo;
    procedure btnOKClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
    procedure GetValues(Proc: TGetStrProc); override;
    procedure FillValues(const Values: TStringList);
  end;

procedure LoadDataSourceFieldss(DataSource: TDataSource; List: TStrings);

var
  formPASFormatEditor: TformPASFormatEditor;

implementation

uses
  PASVirtualDBScrollBase, PASVirtualDBScrollUtilities;

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

procedure TformPASFormatEditor.Button3Click(Sender: TObject);
var
  CurrentPos : TPoint;
  CurrentLine : String;
begin
  CurrentPos := memoFormat.CaretPos;
  if (Pos(' ', cmbFieldNames.Text) <> 0) or (Pos(#9, cmbFieldNames.Text) <> 0) then
  begin
    ShowMessage('No spaces or tabs are allowed in the Field Name');
  end
  else
  begin
    CurrentLine := memoFormat.Lines[CurrentPos.y];
    Insert('[' + cmbFieldNames.Text + ':' + cmbFieldType.Text + ']', CurrentLine, CurrentPos.x);
    memoFormat.Lines[CurrentPos.y] := CurrentLine;
  end

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
      cmbFieldNames.Items := FFieldValues;

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
  Result := [paDialog, paMultiSelect, paValueList, paFullWidthName]; // paValueList OR paReadOnly causes access violation
end;

procedure TPASFormatEditor.GetValues(Proc: TGetStrProc);
var
  i : Integer;
begin

  try
    FFieldValues.Clear;
    FillValues(FFieldValues);
  finally

  end;

  //inherited GetValues(Proc);
end;

// procedure LoadDataSourceFields is not exposed in Lazarus 1.2.4, so it's copied
// here and renamed with an extra 's'.
// This procedure is technically licensed under LCL. In future stable versions of
// Lazarus I can go back to simply linking, and keep my code fully MIT Licensed
procedure LoadDataSourceFieldss(DataSource: TDataSource; List: TStrings);
var
  DataSet: TDataSet;
  i: Integer;
begin
  if Assigned(DataSource) then
  begin
    DataSet := DataSource.DataSet;
    if Assigned(DataSet) then
    begin
      if DataSet.Fields.Count > 0 then
      begin
        DataSet.GetFieldNames(List);
      end
      else
      begin
        for i := 0 to DataSet.FieldDefs.Count - 1 do
        begin
          List.Add(DataSet.FieldDefs[i].Name);
        end;
      end;
    end;
  end;
end;

procedure TPASFormatEditor.FillValues(const Values: TStringList);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetComponent(0), 'DataSource') as TDataSource;

  LoadDataSourceFieldss(DataSource, Values);
end;

end.

