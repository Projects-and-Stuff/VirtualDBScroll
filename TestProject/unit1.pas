unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn, FileUtil, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, PASVirtualDBScrollMemo,
  PASVirtualDBScrollRichMemo, PASVirtualDBScrollSynEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DataSource1: TDataSource;
    PASVirtualDBScrollMemo1: TPASVirtualDBScrollMemo;
    PASVirtualDBScrollSynEdit1: TPASVirtualDBScrollSynEdit;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure DataSource1StateChange(Sender: TObject);
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

  SQLite3Connection1.DatabaseName := './DB_5000.db3';
  SQLQuery1.Active := False;
  SQLQuery1.SQL.Text := 'Select * FROM Entries';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery1.Active := True;
  SQLQuery1.ApplyUpdates;



end;

procedure TForm1.DataSource1StateChange(Sender: TObject);
begin


  {
  if DataSource1.DataSet.State = dsInactive then
  begin
    ShowMessage('DataSet dsInactive');
  end
  else if DataSource1.DataSet.State = dsBrowse then
  begin
    ShowMessage('DataSet dsBrowse');
  end
  else if DataSource1.DataSet.State = dsOpening then
  begin
    ShowMessage('DataSet dsOpening');
  end;

  if DataSource1.State = dsInactive then
  begin
    ShowMessage('dsInactive');
  end
  else if DataSource1.State = dsBrowse then
  begin
    ShowMessage('dsBrowse');
  end
  else if DataSource1.State = dsOpening then
  begin
    ShowMessage('dsOpening');
  end;
  }

  //ShowMessage('DataSource1StateChange');
end;

procedure TForm1.FormShow(Sender: TObject);
begin

end;


end.

