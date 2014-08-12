unit PASVirtualDBScrollUtilities;

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

interface

uses
  Classes, SysUtils, strutils;

implementation

// Converts [Name:Type] to FieldByName('Name').AsVariant
// Run a string through this function until the result is 0 (no change made to the string)
function ConvertFields(var i : Integer; S : String): String;
var
  startPos, colonPos, endPos : Integer;
  S2, FType : String;
begin

  // Look for the first set of
  startPos := Pos('[', S);
  colonPos := PosEx(':', S, startPos);
  endPos := PosEx(']', S, colonPos);

  //ShowMessage(IntToStr(startPos) + ' ' + IntToStr(colonPos) + ' ' + IntToStr(endPos));
  if (startPos > 0) and (colonPos > startPos) and (endPos > colonPos) then
  begin

    // Extract the FieldType
    case Copy(S, colonPos + 1, endPos - colonPos - 1) of
      'String'   : FType := 'AsString';
      'Integer'  : FType := 'AsInteger';
      'DateTime' : FType := 'AsDateTime';
      'Boolean'  : FType := 'AsBoolean';
      'Float'    : FType := 'AsFloat';
      'Variant'  : FType := 'AsVariant';
    else
      FType := 'AsVariant';
    end;

    S2 := 'FieldByName(' + QuotedStr(Copy(S, startPos + 1, colonPos - startPos - 1)) + ').' + FType;
    i := 1;
    result := StuffString(S, startPos, endPos - startPos + 1, S2);
  end
  else
  begin
    i := 0;
    result := S;
  end;

end;

end.

