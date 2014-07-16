unit EmbeddedScrollBar;

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

  { TEmbeddedMemo }

  TEmbeddedScrollBar = class (TCustomScrollBar)
  private
    { Private declarations }

  protected
    { Protected declarations }

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    property Kind;
    property LargeChange;
    property Max;
    property Min;
    property PageSize;
    property Position;
    property SmallChange;

  published
    property Cursor;
    property Hint;
    property Name;
    property ShowHint;
    property Width;

  end;

implementation


{ TEmbeddedScrollBar }


constructor TEmbeddedScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Permanently set the values for these private properties

end;

initialization
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacingDefault), TEmbeddedScrollBar, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TEmbeddedScrollBar, 'FTop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TEmbeddedScrollBar, 'Left', THiddenPropertyEditor);


end.

