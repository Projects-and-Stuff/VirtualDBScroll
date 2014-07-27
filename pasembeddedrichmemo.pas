unit PASEmbeddedRichMemo;

{
VirtualDBScroll Package
Copyright 2014 Jack D Linke

This package is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This package is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this package. If not, see <http://www.gnu.org/licenses/>.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, IDEIntf, PropEdits, typinfo, RichMemo;

type

  { TEmbeddedMemo }

  { TPASEmbeddedRichMemo }

  TPASEmbeddedRichMemo = class (TRichMemo)
  private
    function GetReadOnly: Boolean;
    { Private declarations }
    function GetScrollBars: TScrollStyle;
    function GetWordWrap: Boolean;
    procedure SetReadOnly(AValue: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetWordWrap(AValue: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property Align;
  published
    property Alignment;
    property CharCase;
    property Color;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property HideSelection;
    property Hint;
    property Lines;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False; // Must remain set to false
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssHorizontal; // The only allowed values are ssNone, ssHorizontal, and ssAutoHorizontal
    property ShowHint;
    property WantReturns;
    property WantTabs;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False; // Must remain set to false
  end;

  TPASEmbeddedRichMemoPropertyEditor = class(TEnumPropertyEditor)
    public
      procedure GetValues(Proc: TGetStrProc); override;
    end;

implementation



{ TPASEmbeddedMemoPropertyEditor }

procedure TPASEmbeddedRichMemoPropertyEditor.GetValues(Proc: TGetStrProc);
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

function TPASEmbeddedRichMemo.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

function TPASEmbeddedRichMemo.GetScrollBars: TScrollStyle;
begin
  Result := inherited ScrollBars;
end;

function TPASEmbeddedRichMemo.GetWordWrap: Boolean;
begin
  Result := inherited WordWrap;
end;

procedure TPASEmbeddedRichMemo.SetReadOnly(AValue: Boolean);
begin
  inherited ReadOnly := False;
end;

procedure TPASEmbeddedRichMemo.SetScrollBars(const Value: TScrollStyle);
begin
  if Value in [ssNone, ssAutoHorizontal] then
  begin
    inherited ScrollBars := Value;
  end
  else
  begin
    inherited ScrollBars := ssHorizontal;
  end;
end;

procedure TPASEmbeddedRichMemo.SetWordWrap(AValue: Boolean);
begin
  inherited WordWrap := False;
end;

constructor TPASEmbeddedRichMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Permanently set the values for these private properties
  Align := alClient;
  WordWrap := False;
  ReadOnly := True;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TScrollStyle), TPASEmbeddedRichMemo, 'ScrollBars', TPASEmbeddedRichMemoPropertyEditor);

end.

