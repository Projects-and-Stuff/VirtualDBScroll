unit PASEmbeddedSynEdit;

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
  Classes, SysUtils, Controls, StdCtrls, IDEIntf, PropEdits, typinfo, SynEdit;

type

  { TEmbeddedMemo }

  { TPASEmbeddedSynEdit }

  TPASEmbeddedSynEdit = class (TSynEdit)
  private
    function GetReadOnly: Boolean;
    { Private declarations }
    function GetScrollBars: TScrollStyle;
    procedure SetReadOnly(AValue: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property Align;
    //property WordWrap;
  published
    //property Alignment;
    //property CharCase;
    property Color;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property HideSelection;
    property Hint;
    property Lines;
    //property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False; // Must remain set to false
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssHorizontal; // The only allowed values are ssNone, ssHorizontal, and ssAutoHorizontal
    property ShowHint;
    //property WantReturns;
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

function TPASEmbeddedSynEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

function TPASEmbeddedSynEdit.GetScrollBars: TScrollStyle;
begin
  Result := inherited ScrollBars;
end;

procedure TPASEmbeddedSynEdit.SetReadOnly(AValue: Boolean);
begin
  inherited ReadOnly := False;
end;

procedure TPASEmbeddedSynEdit.SetScrollBars(const Value: TScrollStyle);
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

constructor TPASEmbeddedSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Permanently set the values for these private properties
  Align := alClient;
  //WordWrap := False;
  ReadOnly := True;
  Gutter.Visible := False;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TScrollStyle), TPASEmbeddedSynEdit, 'ScrollBars', TPASEmbeddedSynEditPropertyEditor);

end.

