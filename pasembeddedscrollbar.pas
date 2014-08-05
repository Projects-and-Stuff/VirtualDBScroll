unit PASEmbeddedScrollBar;

{
PASVirtualDBScroll Package
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
{$DEFINE DoLog}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, IDEIntf, PropEdits, typinfo {$ifdef DoLog}, LazLogger{$endif};

type

  { TEmbeddedScrollBar }

  TPASEmbeddedScrollBar = class (TCustomScrollBar)
  private
    { Private declarations }

  protected
    { Protected declarations }

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    //property Kind;
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
    property Visible;

  end;

implementation


{ TEmbeddedScrollBar }


constructor TPASEmbeddedScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Permanently set the values for these private properties




end;

initialization
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacingDefault), TPASEmbeddedScrollBar, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TPASEmbeddedScrollBar, 'FTop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TPASEmbeddedScrollBar, 'Left', THiddenPropertyEditor);


end.

