unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, PASVirtualDBScrollMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DataSource1: TDataSource;
    PASVirtualDBScrollMemo1: TPASVirtualDBScrollMemo;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
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

  SQLite3Connection1.Close;
  SQLite3Connection1.DatabaseName := './DB_5000.db3';
  SQLite3Connection1.Open;
  SQLTransaction1.Active := True;

  with SQLQuery1 do
  begin
    Close;

    SQL.Text := 'Select * FROM Entries';

    Prepare;

    Open;

    Last;

    First;
  end;

  SQLTransaction1.Commit;
  SQLTransaction1.StartTransaction;
  SQLTransaction1.Active := False;
  SQLite3Connection1.Connected := False;
  SQLite3Connection1.Close;

end;

end.

