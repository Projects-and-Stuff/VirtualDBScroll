unit PASVirtualDBScrollUtilities;

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

