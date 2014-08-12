unit PASEmbeddedSynEdit;

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
  Classes, SysUtils, Controls, StdCtrls, IDEIntf, PropEdits, typinfo, SynEdit {$ifdef dbgDBScroll}, LazLogger{$endif};

type

  { TPASEmbeddedSynEdit }

  TPASEmbeddedSynEdit = class (TSynEdit)
  private
    function GetAlign: TAlign;
    function GetReadOnly: Boolean;
    { Private declarations }
    function GetScrollBars: TScrollStyle;
    procedure SetAlign(AValue: TAlign);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    //property Align : TAlign read GetAlign write SetAlign default alNone;
    property Color;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property HideSelection;
    property Hint;
    property Lines;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True; // Must remain set to True
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssAutoHorizontal; // The only allowed values are ssNone, ssHorizontal, and ssAutoHorizontal
    property ShowHint;
    property WantTabs;

  end;

  TPASEmbeddedSynEditPropertyEditor = class(TEnumPropertyEditor)
    public
      procedure GetValues(Proc: TGetStrProc); override;
    end;

implementation



{ TPASEmbeddedMemoPropertyEditor }

procedure TPASEmbeddedSynEditPropertyEditor.GetValues(Proc: TGetStrProc);
type
  TRestricted = 1..3;
  TRestrictedNames = array[TRestricted] of shortstring;
const
  RestrictedStyleNames: TRestrictedNames =
                          ('ssNone', 'ssHorizontal', 'ssAutoHorizontal');
var
  i: TRestricted;
begin
  for i := Low(TRestricted) to High(TRestricted) do
    Proc(RestrictedStyleNames[i]);
end;

{ TEmbeddedMemo }

function TPASEmbeddedSynEdit.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

function TPASEmbeddedSynEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

function TPASEmbeddedSynEdit.GetScrollBars: TScrollStyle;
begin
  Result := inherited ScrollBars;
end;

procedure TPASEmbeddedSynEdit.SetAlign(AValue: TAlign);
begin
  inherited Align := alNone;
end;

procedure TPASEmbeddedSynEdit.SetReadOnly(AValue: Boolean);
begin
  inherited ReadOnly := True;
end;

procedure TPASEmbeddedSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if Value in [ssNone, ssHorizontal] then
  begin
    inherited ScrollBars := Value;
  end
  else
  begin
    inherited ScrollBars := ssAutoHorizontal;
  end;
end;

constructor TPASEmbeddedSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set the initial values for these properties
  Align := alClient;
  ReadOnly := True;
  Gutter.Visible := False;
  ScrollBars := ssAutoHorizontal;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TScrollStyle), TPASEmbeddedSynEdit, 'ScrollBars', TPASEmbeddedSynEditPropertyEditor);

end.

