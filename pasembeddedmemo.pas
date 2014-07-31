unit PASEmbeddedMemo;

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
  Classes, SysUtils, Controls, StdCtrls, IDEIntf, PropEdits, typinfo;

type

  { TPASEmbeddedMemo }

  TPASEmbeddedMemo = class (TCustomMemo)
  private
    { Private declarations }
    function GetAlign: TAlign;
    function GetReadOnly: Boolean;
    function GetScrollBars: TScrollStyle;
    function GetWordWrap: Boolean;
    procedure SetAlign(AValue: TAlign);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetWordWrap(AValue: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    //property Align : TAlign read GetAlign write SetAlign default alNone;
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
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True; // Must remain set to True
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssHorizontal; // The only allowed values are ssNone, ssHorizontal, and ssAutoHorizontal
    property ShowHint;
    property WantReturns;
    property WantTabs;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False; // Must remain set to false
  end;

  TPASEmbeddedMemoScrollPropertyEditor = class(TEnumPropertyEditor)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;


implementation


{ TPASEmbeddedMemoPropertyEditor }

procedure TPASEmbeddedMemoScrollPropertyEditor.GetValues(Proc: TGetStrProc);
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

function TPASEmbeddedMemo.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

function TPASEmbeddedMemo.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

function TPASEmbeddedMemo.GetScrollBars: TScrollStyle;
begin
  Result := inherited ScrollBars;
end;

function TPASEmbeddedMemo.GetWordWrap: Boolean;
begin
  Result := inherited WordWrap;
end;

procedure TPASEmbeddedMemo.SetAlign(AValue: TAlign);
begin
  inherited Align := alNone;
end;

procedure TPASEmbeddedMemo.SetReadOnly(AValue: Boolean);
begin
  inherited ReadOnly := True;
end;

procedure TPASEmbeddedMemo.SetScrollBars(const Value: TScrollStyle);
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

procedure TPASEmbeddedMemo.SetWordWrap(AValue: Boolean);
begin
  inherited WordWrap := False;
end;

constructor TPASEmbeddedMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set the initial values for these properties
  Align := alClient;
  WordWrap := False;
  ReadOnly := True;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TScrollStyle), TPASEmbeddedMemo, 'ScrollBars', TPASEmbeddedMemoScrollPropertyEditor);

end.

