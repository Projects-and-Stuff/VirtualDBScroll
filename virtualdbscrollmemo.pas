unit VirtualDBScrollMemo;

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EmbeddedMemo, EmbeddedScrollBar, db, DBGrids;

type



  { TVirtualDBScrollMemo }

  TVirtualDBScrollMemo = class(TCustomPanel)

  private
    { Private declarations }
    FEMemo : TEmbeddedMemo;
    FEScrollBar : TEmbeddedScrollBar;
    FDataLink : TComponentDataLink;

    FRecordCount : Integer;                       // Total number of records in the DataSet
    FRecordChunkSize : Integer;                   // The maximum number of records per record chunk
    FRecordChunkCount : Integer;                  // Number of record chunks in the DataSet
    FRecordChunkLineCounts : Array of Integer;    // Keeps track of the number of lines displayed per chunk
    FCurrentRecordChunk : Integer;                // Tracks which chunk is currently at the center of display
    FLineResolution : Integer;                    // The number of lines positions on the scrollbar allocated to each record
    FVisibleLines : Integer;                      // How many lines are visible in EmbeddedMemo

    function GetChunkSize: Integer;
    function GetDataSource: TDataSource;
    procedure SetChunkSize(const Value: Integer);
    procedure SetDataSource(AValue: TDataSource);

    function GetRecordCount: Integer;



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
    property EMemo : TEmbeddedMemo read FEMemo;
    property EScrollBar : TEmbeddedScrollBar read FEScrollBar;

    property RecordChunkSize : Integer read GetChunkSize write SetChunkSize default 50; // Used to set the number of records per chunk. Allowable range is 1 to 500
    property DataLink : TComponentDataLink read FDataLink write FDataLink;
    property DataSource : TDataSource read GetDataSource write SetDataSource;

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
  RegisterComponents('Additional', [TVirtualDBScrollMemo]);
end;

function TVirtualDBScrollMemo.GetChunkSize: Integer;
begin
  Result := FRecordChunkSize;
end;

function TVirtualDBScrollMemo.GetDataSource: TDataSource;
begin
  result := FDataLink.DataSource;
end;

procedure TVirtualDBScrollMemo.SetChunkSize(const Value: Integer);
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

procedure TVirtualDBScrollMemo.SetDataSource(AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
end;

function TVirtualDBScrollMemo.GetRecordCount: Integer;
begin
  result := FDataLink.DataSet.RecordCount;


  // TODO: We need to verify that the RecordCount is less than 2,147,483
  // If it's between 2,147,483 and 4,294,966 we can reduce FLineResolution to 500 (vice 1000)
  // If it's between 4,294,966 and 8,589,932 we can reduce FLineResolution to 250
end;

procedure TVirtualDBScrollMemo.DataLinkOnRecordChanged(Field: TField);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnDataSetChanged(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnDataSetOpen(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnDataSetClose(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnNewDataSet(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnInvalidDataset(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnInvalidDataSource(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnDataSetScrolled(ADataSet: TDataSet;
  Distance: Integer);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnLayoutChanged(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnEditingChanged(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.DataLinkOnUpdateData(ADataSet: TDataSet);
begin

end;

procedure TVirtualDBScrollMemo.EMemoOnKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TVirtualDBScrollMemo.EScrollBarOnChange(Sender: TObject);
begin

end;

procedure TVirtualDBScrollMemo.EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

end;

procedure TVirtualDBScrollMemo.EScrollBarOnKeyPress(Sender: TObject; var Key: char);
begin

end;

constructor TVirtualDBScrollMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default width and height
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
  end;

  Caption := '';

  // Add the Embedded Components
  FEMemo := TEmbeddedMemo.Create(Self); // Add the embedded memo
  FEMemo.Parent := self;         // Show the memo in the panel
  FEMemo.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FEMemo.Name := 'EmbeddedMemo';
  FEMemo.ScrollBars := ssHorizontal; //[ssNone, ssHorizontal, ssAutoHorizontal];
  FEMemo.Lines.Clear;
  FEMemo.OnKeyPress := @EMemoOnKeyPress;


  FEScrollBar := TEmbeddedScrollBar.Create(Self); // Add the embedded memo
  FEScrollBar.Parent := self;         // Show the memo in the panel
  FEScrollBar.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FEScrollBar.Width := 15;
  FEScrollBar.Align := alRight;
  FEScrollBar.Name := 'EmbeddedScrollBar';
  FEScrollBar.Kind := sbVertical;
  FEScrollBar.OnChange := @EScrollBarOnChange;
  FEScrollBar.OnScroll := @EScrollBarOnScroll;
  FEScrollBar.OnKeyPress := @EScrollBarOnKeyPress;



  // Make sure the embedded components can not be selected/deleted within the IDE
  FEMemo.ControlStyle := FEMemo.ControlStyle - [csNoDesignSelectable];
  FEScrollBar.ControlStyle := FEScrollBar.ControlStyle - [csNoDesignSelectable];

  // Initially set the chunk size to 50
  FRecordChunkSize := 50;

  // Inititally allow 1000 positions on EmbeddedScrollBar per record in the dataset (but this may need to be adjusted when we check the RecordCount)
  FLineResolution := 1000;


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






end;

destructor TVirtualDBScrollMemo.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

initialization
  {$I TVirtualDBScrollMemo.lrs}

end.
