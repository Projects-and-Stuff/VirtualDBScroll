unit PASVirtualDBScrollBase;

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, PASEmbeddedMemo, PASEmbeddedScrollBar, db, DBGrids, PropEdits,
  PASFormatEditor, PASEmbeddedPanel {$ifdef DoLog}, LazLogger{$endif};

type

  { TOperationMode }

  TOperationMode = (Standard, SmallDataSet, Disconnected);
  // Standard :      The default behavior for scrolling through large DataSets
  // SmallDataSet:   Can be selected when scrolling through a small DataSet (smoother operation)
  // Disconnected :  Allows the programmer to specify the content of the memo (disconnecting from DataSet content)
  //                 Hides the Embedded Scrollbar, sets Memo Scrollbar to ssAutoBoth


  { TPASVirtualDBScrollBase }

  TPASVirtualDBScrollBase = class(TCustomPanel)

  private
    { Private declarations }
    FScrollBar : TPASEmbeddedScrollBar;
    FPopupInfo : TPASEmbeddedPanel;
    FDataLink : TComponentDataLink;

    FRecordCount : Integer;                       // Total number of records in the DataSet
    FRecordSliceSize : Integer;                   // The maximum number of records per record Slice
    FRecordSliceCount : Integer;                  // Number of record Slices in the DataSet
    FRecordSliceLineCounts : Array of Integer;    // Keeps track of the number of lines displayed per Slice
    FCurrentRecordSlice : Integer;                // Tracks which Slice is currently at the center of display
    FLineResolution : Integer;                    // The number of lines positions on the scrollbar allocated to each record
    FVisibleLines : Integer;                      // How many lines are visible in EmbeddedMemo

    FFormat : String;                             // The format for displaying content

    FError : String;                              //

    FOperationMode : TOperationMode;

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
    procedure EScrollBarOnChange(Sender : TObject);
    procedure EScrollBarOnKeyPress(Sender : TObject; var Key : char);
    procedure EScrollBarOnScroll(Sender : TObject; ScrollCode : TScrollCode;
      var ScrollPos : Integer);

    procedure SetOperationMode(AValue: TOperationMode);

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

    function GetFormat : String;
    procedure SetFormat(Value : String);

    function GetSliceSize : Integer;
    function GetDataSource : TDataSource;
    procedure SetSliceSize(const Value : Integer);
    procedure SetDataSource(Value : TDataSource);

    procedure GetRecordCount;
    procedure CalculateLineResolution;
    procedure CalculateScrollBarMax;
    procedure CalculateRecordSliceCount; // Actually calculates both FRecordSliceCount and the length of array FRecordSliceLineCounts

    property EScrollBar : TPASEmbeddedScrollBar read FScrollBar;
    property EPopupInfo : TPASEmbeddedPanel read FPopupInfo;

    property RecordSliceSize : Integer read GetSliceSize write SetSliceSize default 50; // Used to set the number of records per Slice. Allowable range is 1 to 500
    property DataLink : TComponentDataLink read FDataLink write FDataLink;
    property DataSource : TDataSource read GetDataSource write SetDataSource; // Used to access the DataLink. The DataLink property of the DataSource must be set for this component to operate
    property LineResolution : Integer read FLineResolution; // The number of positions on the scrollbar allocated per record. This property is automatically calculated based upon the number of Records in the DataSet
    property RecordCount : Integer read FRecordCount; // The number of records in the dataset. For instance, if the DataSet is an SQLQuery, this value is the number of records returned from a query. This property is automatically calculated when the dataset is opened.



    {The Format property allows the programmer to determine how records will be displayed within the component}
    property Format : String read GetFormat write SetFormat;
    property OperationMode: TOperationMode read FOperationMode write SetOperationMode default Standard;

    property Align;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Caption;
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


  { TPASOperationModePropertyEditor }

  TPASOperationModePropertyEditor = class(TEnumPropertyEditor)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TOperationMode), TPASVirtualDBScrollBase, 'OperationMode', TEnumPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TPASVirtualDBScrollBase, 'Format', TPASFormatEditor);
end;

{ TPASOperationModePropertyEditor }

procedure TPASOperationModePropertyEditor.GetValues(Proc: TGetStrProc);
type
  TRestricted = 1..3;
  TRestrictedNames = array[TRestricted] of ShortString;
const
  RestrictedStyleNames: TRestrictedNames =
                          ('Standard', 'SmallDataSet', 'Disconnected');
var
  i: TRestricted;
begin
  for i := Low(TRestricted) to High(TRestricted) do
    Proc(RestrictedStyleNames[i]);
end;

function TPASVirtualDBScrollBase.GetFormat: String;
begin
  Result := FFormat;
end;

procedure TPASVirtualDBScrollBase.SetFormat(Value: String);
begin
  FFormat := Value;
end;

function TPASVirtualDBScrollBase.GetSliceSize: Integer;
begin
  Result := FRecordSliceSize;
end;

function TPASVirtualDBScrollBase.GetDataSource: TDataSource;
begin
  result := FDataLink.DataSource;
end;

procedure TPASVirtualDBScrollBase.SetSliceSize(const Value: Integer);
begin
  if Value < 1 then
  begin
    FRecordSliceSize := 1;
  end
  else if Value > 500 then
  begin
    FRecordSliceSize := 500;
  end
  else
  begin
    FRecordSliceSize := Value;
  end;
end;

procedure TPASVirtualDBScrollBase.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TPASVirtualDBScrollBase.GetRecordCount;
begin

  DataLink.DataSet.Last; // Need to move to the last record in the dataset in order to get an accurate count
  FRecordCount := DataLink.DataSet.RecordCount;
  DataLink.DataSet.First; // Move back to the front

end;

procedure TPASVirtualDBScrollBase.CalculateLineResolution;
begin
  {
  Based on the number of records in the DataSet, we set the resolution
  per record. If there are less than 8,389 records, for instance, we
  allot up to 256,000 positions on the scrollbar per record. In this case,
  as long as there are less than 256,000 lines in the record, we can scroll
  smoothly through every line. There should only be a problem when there
  are millions of records which are very long (several hundred lines each).
  }
  if RecordCount < 1 then
  begin
    FLineResolution := 0;
  end
  else if (RecordCount > 0) and (RecordCount < 8389) then
  begin
    FLineResolution := 256000;
  end
  else if (RecordCount > 8388) and (RecordCount < 16778) then
  begin
    FLineResolution := 128000;
  end
  else if (RecordCount > 16777) and (RecordCount < 33555) then
  begin
    FLineResolution := 64000;
  end
  else if (RecordCount > 33554) and (RecordCount < 67109) then
  begin
    FLineResolution := 32000;
  end
  else if (RecordCount > 67108) and (RecordCount < 134218) then
  begin
    FLineResolution := 16000;
  end
  else if (RecordCount > 134217) and (RecordCount < 268436) then
  begin
    FLineResolution := 8000;
  end
  else if (RecordCount > 268435) and (RecordCount < 536871) then
  begin
    FLineResolution := 4000;
  end
  else if (RecordCount > 536870) and (RecordCount < 1073742) then
  begin
    FLineResolution := 2000;
  end
  else if (RecordCount > 1073741) and (RecordCount < 2147484) then
  begin
    FLineResolution := 1000;
  end
  else if (RecordCount > 2147483) and (RecordCount < 4294968) then
  begin
    FLineResolution := 500;
  end
  else if (RecordCount > 4294967) and (RecordCount < 8589934) then
  begin
    FLineResolution := 250;
  end;
end;

procedure TPASVirtualDBScrollBase.CalculateScrollBarMax;
begin
  EScrollBar.Max := RecordCount * LineResolution;
end;

// Count the number of Slices required based on RecordCount and RecordSliceSize
procedure TPASVirtualDBScrollBase.CalculateRecordSliceCount;
begin
  // If the record count divides into the Slice size, getting the Slice count is easy
  if FRecordCount mod FRecordSliceSize = 0 then
  begin
    FRecordSliceCount := FRecordCount div FRecordSliceSize;
    SetLength(FRecordSliceLineCounts, FRecordCount div FRecordSliceSize);
  end
  // Otherwise we need to add one more Slice
  else
  begin
    FRecordSliceCount := (FRecordCount div FRecordSliceSize) + 1;
    SetLength(FRecordSliceLineCounts, (FRecordCount div FRecordSliceSize) + 1);
  end;
end;

procedure TPASVirtualDBScrollBase.DataLinkOnRecordChanged(Field: TField);
begin

end;

procedure TPASVirtualDBScrollBase.DataLinkOnDataSetChanged(ADataSet: TDataSet);
begin
  //ShowMessage('Changed');
  {if ADataSet.State = dsBrowse then
  begin
    ShowMessage('dsBrowse');
  end
  else if ADataSet.State = dsInactive then
  begin
    ShowMessage('dsInactive');
  end;}

  // Every time we move positions within the DataSet
  // ShowMessage('OnDataSetChanged');
end;

procedure TPASVirtualDBScrollBase.DataLinkOnDataSetOpen(ADataSet: TDataSet);
begin
  // Probably the most useful DataSet event
  // Every time this fires we need to get the record count and perform our calculations

  {
  //////////////////////
   BEFORE ANYTHING ELSE - We need to clear any old values/set back to default
  //////////////////////
  }


  GetRecordCount;
  CalculateLineResolution;
  CalculateScrollBarMax;

end;

procedure TPASVirtualDBScrollBase.DataLinkOnDataSetClose(ADataSet: TDataSet);
begin
  // May be useful for clean-up
  //ShowMessage('OnDataSetClose');
end;

procedure TPASVirtualDBScrollBase.DataLinkOnNewDataSet(ADataSet: TDataSet);
begin
  // Not particularly useful for my purposes
  // ShowMessage('OnNewDataSet');
end;

procedure TPASVirtualDBScrollBase.DataLinkOnInvalidDataset(ADataSet: TDataSet);
begin
  // Return an error
end;

procedure TPASVirtualDBScrollBase.DataLinkOnInvalidDataSource(ADataSet: TDataSet);
begin
  // Return an error
end;

procedure TPASVirtualDBScrollBase.DataLinkOnDataSetScrolled(ADataSet: TDataSet;
  Distance: Integer);
begin
  ShowMessage('OnDataSetScrolled');
end;

procedure TPASVirtualDBScrollBase.DataLinkOnLayoutChanged(ADataSet: TDataSet);
begin
  ShowMessage('OnDataSetLayoutChanged');
end;

procedure TPASVirtualDBScrollBase.DataLinkOnEditingChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollBase.DataLinkOnUpdateData(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollBase.EScrollBarOnChange(Sender: TObject);
begin

end;

procedure TPASVirtualDBScrollBase.EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

end;

procedure TPASVirtualDBScrollBase.SetOperationMode(AValue: TOperationMode);
begin
  if AValue in [SmallDataSet, Disconnected] then
  begin
    FOperationMode := AValue;
  end
  else
  begin
    FOperationMode := Standard;
  end;
end;

procedure TPASVirtualDBScrollBase.EScrollBarOnKeyPress(Sender: TObject; var Key: char);
begin

end;

constructor TPASVirtualDBScrollBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default component size and values
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
  end;
  Height := 50;
  Caption := '';



  // Initialize the Embedded ScrollBar
  FScrollBar := TPASEmbeddedScrollBar.Create(Self); // Add the embedded memo
  FScrollBar.Parent := Self;         // Show the memo in the panel
  FScrollBar.SetSubComponent(True);  // Tell the IDE to store the modified properties
  FScrollBar.Name := 'EScrollBar';
  FScrollBar.ControlStyle := FScrollBar.ControlStyle - [csNoDesignSelectable]; // Make sure it can not be selected/deleted within the IDE
  FScrollBar.Width := 15;
  FScrollBar.Align := alRight;
  FScrollBar.Kind := sbVertical;
  FScrollBar.OnChange := @EScrollBarOnChange;
  FScrollBar.OnScroll := @EScrollBarOnScroll;
  FScrollBar.OnKeyPress := @EScrollBarOnKeyPress;


  // Initialize the Embedded Label
  FPopupInfo := TPASEmbeddedPanel.Create(Self);
  FPopupInfo.Parent := Self;
  FPopupInfo.SetSubComponent(True);
  FPopupInfo.Name := 'EPopupInfo';
  FPopupInfo.ControlStyle := FPopupInfo.ControlStyle - [csNoDesignSelectable];
  FPopupInfo.IsDisplayed := True;
  FPopupInfo.Width := 50;
  FPopupInfo.Height := 24;
  FPopupInfo.Caption := '0';
  FPopupInfo.Left := 0; //Mouse.CursorPos.x;
  FPopupInfo.Top := 0; //Mouse.CursorPos.y;
  FPopupInfo.BringToFront;


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


  // Initially set the Slice size to 50
  FRecordSliceSize := 50;


  // Set Format property default value (blank)
  FFormat := '';



end;

destructor TPASVirtualDBScrollBase.Destroy;
begin
  {$ifdef DoLog}
    DebugLn('TPASVirtualDBScrollBase.Destroy');
  {$endif}

  FDataLink.Free;
  FDataLink := Nil;

  FPopupInfo.Free;
  FPopupInfo := Nil;

  FScrollBar.Free;
  FScrollBar := Nil;

  inherited Destroy;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TOperationMode), TPASVirtualDBScrollBase, 'OperationMode', TPASOperationModePropertyEditor);




end.
