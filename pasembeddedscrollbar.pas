unit PASEmbeddedScrollBar;

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
  Classes, SysUtils, Controls, StdCtrls, IDEIntf, PropEdits, typinfo {$ifdef dbgDBScroll}, LazLogger{$endif};

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

