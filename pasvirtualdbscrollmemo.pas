unit PASVirtualDBScrollMemo;

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

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, PASEmbeddedMemo, PASEmbeddedScrollBar, db, DBGrids, PropEdits,
  PASFormatEditor;

type

  { TPASVirtualDBScrollMemo }

  TPASVirtualDBScrollMemo = class(TCustomPanel)

  private
    { Private declarations }
    FMemo : TPASEmbeddedMemo;
    FScrollBar : TPASEmbeddedScrollBar;
    FDataLink : TComponentDataLink;

    FRecordCount : Integer;                       // Total number of records in the DataSet
    FRecordChunkSize : Integer;                   // The maximum number of records per record chunk
    FRecordChunkCount : Integer;                  // Number of record chunks in the DataSet
    FRecordChunkLineCounts : Array of Integer;    // Keeps track of the number of lines displayed per chunk
    FCurrentRecordChunk : Integer;                // Tracks which chunk is currently at the center of display
    FLineResolution : Integer;                    // The number of lines positions on the scrollbar allocated to each record
    FVisibleLines : Integer;                      // How many lines are visible in EmbeddedMemo

    FFormat : String;                             // The format for displaying content

    FError : String;                              //

    function GetFormat : String;
    procedure SetFormat(Value : String);

    function GetChunkSize: Integer;
    function GetDataSource: TDataSource;
    procedure SetChunkSize(const Value: Integer);
    procedure SetDataSource(Value: TDataSource);

    function GetRecordCount: Integer; // Also sets FLineResolution


    // Event Handlers
    procedure DataLinkOnRecordChanged(Field : TField);
    procedure DataLinkOnDataSetChanged(ADataSet : TDataSet);
    procedure DataLinkOnDataSetOpen(ADataSet : TDataSet);
    procedure DataLinkOnDataSetClose(ADataSet : TDataSet);
    procedure DataLinkOnNewDataSet(ADataSet : TDataSet);
    procedure DataLinkOnInvalidDataset(ADataSet : TDataSet);
    procedure DataLinkOnInvalidDataSource(ADataSet : TDataSet);
    procedure DataLinkOnDataSetScrolled(ADataSet : TDataSet; Distance : Integer);
    procedure DataLinkOnLayoutChanged(ADataSet : TDataSet);
    procedure DataLinkOnEditingChanged(ADataSet : TDataSet);
    procedure DataLinkOnUpdateData(ADataSet : TDataSet);
    procedure EMemoOnKeyPress(Sender: TObject; var Key: char);
    procedure EScrollBarOnChange(Sender: TObject);
    procedure EScrollBarOnKeyPress(Sender: TObject; var Key: char);
    procedure EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property EMemo : TPASEmbeddedMemo read FMemo;
    property EScrollBar : TPASEmbeddedScrollBar read FScrollBar;

    property RecordChunkSize : Integer read GetChunkSize write SetChunkSize default 50; // Used to set the number of records per chunk. Allowable range is 1 to 500
    property DataLink : TComponentDataLink read FDataLink write FDataLink;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
    property LineResolution : Integer read FLineResolution;

    property Format : String read GetFormat write SetFormat;

    property Align;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Enabled;
    property Height;
    property Left;
    property Name;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property UseDockManager;
    property Visible;
    property Width;

  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Additional', [TPASVirtualDBScrollMemo]);

  RegisterPropertyEditor(TypeInfo(String), TPASVirtualDBScrollMemo, 'Format', TPASFormatEditor);
end;

function TPASVirtualDBScrollMemo.GetFormat: String;
begin
  Result := FFormat;
end;

procedure TPASVirtualDBScrollMemo.SetFormat(Value: String);
begin
  FFormat := Value;
end;

function TPASVirtualDBScrollMemo.GetChunkSize: Integer;
begin
  Result := FRecordChunkSize;
end;

function TPASVirtualDBScrollMemo.GetDataSource: TDataSource;
begin
  result := FDataLink.DataSource;
end;

procedure TPASVirtualDBScrollMemo.SetChunkSize(const Value: Integer);
begin
  if Value < 1 then
  begin
    FRecordChunkSize := 1;
  end
  else if Value > 500 then
  begin
    FRecordChunkSize := 500;
  end
  else
  begin
    FRecordChunkSize := Value;
  end;
end;

procedure TPASVirtualDBScrollMemo.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TPASVirtualDBScrollMemo.GetRecordCount: Integer;
begin
  // Need to move to the last record in the dataset in order to get an accurate count
  FDataLink.DataSet.Last;


  if FDataLink.DataSet.RecordCount < 1 then
  begin

  end
  else if (FDataLink.DataSet.RecordCount > 0) and (FDataLink.DataSet.RecordCount < 268436) then
  begin
    FLineResolution := 8000;
  end
  else if (FDataLink.DataSet.RecordCount > 268435) and (FDataLink.DataSet.RecordCount < 536871) then
  begin
    FLineResolution := 4000;
  end
  else if (FDataLink.DataSet.RecordCount > 536870) and (FDataLink.DataSet.RecordCount < 1073742) then
  begin
    FLineResolution := 2000;
  end
  else if (FDataLink.DataSet.RecordCount > 1073741) and (FDataLink.DataSet.RecordCount < 2147484) then
  begin
    FLineResolution := 1000;
  end
  else if (FDataLink.DataSet.RecordCount > 2147483) and (FDataLink.DataSet.RecordCount < 4294967) then
  begin
    FLineResolution := 500;
  end
  else if (FDataLink.DataSet.RecordCount > 4294966) and (FDataLink.DataSet.RecordCount < 8589933) then
  begin
    FLineResolution := 250;
  end;

  // If RecordCount is between 0 and 268,435 FLineResolution = 8000
  // If RecordCount is between 268,436 and 536,870 FLineResolution = 4000
  // If RecordCount is between 536,871 and 1,073,741 FLineResolution = 2000
  // If RecordCount is between 1,073,742 and 2,147,483 FLineResolution = 1000
  // If RecordCount is between 2,147,484 and 4,294,966 we can reduce FLineResolution to 500
  // If RecordCount is between 4,294,967 and 8,589,932 we can reduce FLineResolution to 250

  result := FDataLink.DataSet.RecordCount;
end;

procedure TPASVirtualDBScrollMemo.DataLinkOnRecordChanged(Field: TField);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnDataSetChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnDataSetOpen(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnDataSetClose(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnNewDataSet(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnInvalidDataset(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnInvalidDataSource(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnDataSetScrolled(ADataSet: TDataSet;
  Distance: Integer);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnLayoutChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnEditingChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnUpdateData(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.EMemoOnKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TPASVirtualDBScrollMemo.EScrollBarOnChange(Sender: TObject);
begin

end;

procedure TPASVirtualDBScrollMemo.EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

end;

procedure TPASVirtualDBScrollMemo.EScrollBarOnKeyPress(Sender: TObject; var Key: char);
begin

end;

constructor TPASVirtualDBScrollMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default width and height
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
  end;

  Height := 50;
  Caption := '';

  // Initialize the Embedded Memo
  FMemo := TPASEmbeddedMemo.Create(Self); // Add the embedded memo
  FMemo.Parent := self;         // Show the memo in the panel
  FMemo.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FMemo.Name := 'EMemo';
  FMemo.ControlStyle := FMemo.ControlStyle - [csNoDesignSelectable]; // Make sure it can not be selected/deleted within the IDE
  FMemo.Lines.Clear; // Should I allow the user to set some default text?
  FMemo.OnKeyPress := @EMemoOnKeyPress;

  // Initialize the Embedded ScrollBar
  FScrollBar := TPASEmbeddedScrollBar.Create(Self); // Add the embedded memo
  FScrollBar.Parent := self;         // Show the memo in the panel
  FScrollBar.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FScrollBar.Name := 'EScrollBar';
  FScrollBar.ControlStyle := FScrollBar.ControlStyle - [csNoDesignSelectable]; // Make sure it can not be selected/deleted within the IDE
  FScrollBar.Width := 15;
  FScrollBar.Align := alRight;
  FScrollBar.Kind := sbVertical;
  FScrollBar.OnChange := @EScrollBarOnChange;
  FScrollBar.OnScroll := @EScrollBarOnScroll;
  FScrollBar.OnKeyPress := @EScrollBarOnKeyPress;

  // Initialize the Dataset
  FDataLink := TComponentDataLink.Create;
  FDataLink.OnRecordChanged := @DataLinkOnRecordChanged;
  FDataLink.OnDatasetChanged := @DataLinkOnDataSetChanged;
  FDataLink.OnDataSetOpen := @DataLinkOnDataSetOpen;
  FDataLink.OnDataSetClose := @DataLinkOnDataSetClose;
  FDataLink.OnNewDataSet := @DataLinkOnNewDataSet;
  FDataLink.OnInvalidDataSet := @DataLinkOnInvalidDataset;
  FDataLink.OnInvalidDataSource := @DataLinkOnInvalidDataSource;
  FDataLink.OnDataSetScrolled := @DataLinkOnDataSetScrolled;
  FDataLink.OnLayoutChanged := @DataLinkOnLayoutChanged;
  FDataLink.OnEditingChanged := @DataLinkOnEditingChanged;
  FDataLink.OnUpdateData := @DataLinkOnUpdateData;
  FDataLink.VisualControl := True;


  // Initially set the chunk size to 50
  FRecordChunkSize := 50;

  // Inititally allow 1000 positions on EmbeddedScrollBar per record in the dataset (but this may need to be adjusted when we check the RecordCount)
  FLineResolution := 1000;

  // Set Format property default value (blank)
  FFormat := '';




end;

destructor TPASVirtualDBScrollMemo.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  FMemo.Free;
  FMemo := nil;

  FScrollBar.Free;
  FScrollBar := nil;

  inherited Destroy;
end;

initialization
  {$I TPASVirtualDBScrollMemo.lrs}

end.
