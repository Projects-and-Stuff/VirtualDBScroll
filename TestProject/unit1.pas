unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn, FileUtil, SynCompletion,
  SynHighlighterAny, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PASVirtualDBScrollMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    PASVirtualDBScrollMemo1: TPASVirtualDBScrollMemo;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin

end;


procedure TForm1.FormShow(Sender: TObject);
begin

end;


end.

