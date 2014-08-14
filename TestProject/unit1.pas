unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn, FileUtil, SynCompletion,
  SynHighlighterAny, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PASVirtualDBScrollMemo, strutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    DataSource1: TDataSource;
    Memo1: TMemo;
    PASVirtualDBScrollMemo1: TPASVirtualDBScrollMemo;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function ConvertFields(var i : Integer; S : String): String;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

  {SQLite3Connection1.Close;
  SQLite3Connection1.DatabaseName := './DB_5000.db3';
  SQLite3Connection1.Open;
  SQLTransaction1.Active := True;

  with SQLQuery1 do
  begin

    //Close;

    SQL.Text := 'Select * FROM Entries';


    //Prepare;

    Open;
    ApplyUpdates;
    Close;

    Eventually, once the DataSet (in this case, the SQLQuery) is opened, the
    data will be automatically displayed in the memo, formatted based upon the
    'Format' property.



  end;

  SQLTransaction1.Commit;
  SQLTransaction1.StartTransaction;
  SQLTransaction1.Active := False;
  SQLite3Connection1.Connected := False;
  SQLite3Connection1.Close;}

  SQLite3Connection1.Connected := False;
  SQLite3Connection1.DatabaseName := './DB_5000.db3';
  //SQLQuery1.Close;
  SQLQuery1.Active := False;
  SQLQuery1.SQL.Text := 'Select * FROM Entries';
  SQLQuery1.Prepare;
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery1.Active := True;
  SQLQuery1.ApplyUpdates;




end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SQLite3Connection1.Connected := False;
  SQLite3Connection1.DatabaseName := './DB_1000.db3';
  //SQLQuery1.Close;
  SQLQuery1.Active := False;
  SQLQuery1.SQL.Text := 'Select * FROM Entries';
  SQLQuery1.Prepare;
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery1.Active := True;
  SQLQuery1.ApplyUpdates;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i : Integer;
begin
  //ShowMessage(ExtractDelimited(1, Memo1.Lines[0], Brackets));

  i := 1;
  while i = 1 do
  begin
    //ShowMessage(IntToStr(i));
    Memo1.Text := ConvertFields(i, Memo1.Text);
    //ShowMessage(IntToStr(i));
  end;


end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.SetFocus;
end;

// Run a string through this function until the result is 0 (no change made to the string)
function TForm1.ConvertFields(var i : Integer; S : String): String;
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

    S2 := '+ FieldByName(' + QuotedStr(Copy(S, startPos + 1, colonPos - startPos - 1)) + ').' + FType;
    i := 1;
    result := StuffString(S, startPos, endPos - startPos + 1, S2);
  end
  else
  begin
    i := 0;
    result := S;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;


procedure TForm1.FormShow(Sender: TObject);
begin

end;


end.

