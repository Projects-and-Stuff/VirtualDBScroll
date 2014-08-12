unit PASVirtualDBScrollBase;

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, PASEmbeddedMemo, PASEmbeddedScrollBar, db, PropEdits,
  PASFormatEditor, PASEmbeddedPanel, PASDataLink
  {$ifdef dbgDBScroll}, LazLogger{$endif}, PASVirtualDBScrollUtilities;

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
    FDataLink : TPASDataLink;

    FCurrentDBName : String;                      // Stores the current DB Name. This way we can determine if the database has changed

    FCountingRecords : Boolean;                   // While performing a RecordCount, this will be set to True

    FFormat : TStrings;                             // The format for displaying content

    FError : String;                              //

    FOperationMode : TOperationMode;

    // Event Handlers
    procedure OnRecordChanged(Field : TField);
    procedure OnDataSetChanged(ADataSet : TDataSet);
    procedure OnDataSetOpen(ADataSet : TDataSet);
    procedure OnDataSetOpening(ADataSet : TDataSet);
    procedure OnDataSetClose(ADataSet : TDataSet);
    procedure OnNewDataSet(ADataSet : TDataSet);
    procedure OnInvalidDataset(ADataSet : TDataSet);
    procedure OnInvalidDataSource(ADataSet : TDataSet);
    procedure OnDataSetScrolled(ADataSet : TDataSet; Distance : Integer);
    procedure OnLayoutChanged(ADataSet : TDataSet);
    procedure OnEditingChanged(ADataSet : TDataSet);
    procedure OnUpdateData(ADataSet : TDataSet);
    procedure OnDataSetBrowse(ADataSet : TDataSet);
    procedure OnActiveChanged(ADataSet : TDataSet);
    procedure EScrollBarOnChange(Sender : TObject);
    procedure EScrollBarOnScroll(Sender : TObject; ScrollCode : TScrollCode;
      var ScrollPos : Integer);

    procedure InitializeWithData;

    procedure SetFormat(const AValue: TStrings);
    procedure DoFormatChange(Sender: TObject);

  protected
    { Protected declarations }
    FRecordSliceSize : Integer;                   // The maximum number of records per record Slice
    FLineResolution : Integer;                    // The number of lines positions on the scrollbar allocated to each record
    FRecordCount : Integer;                       // Total number of records in the DataSet
    FRecordSliceCount : Integer;                  // Number of record Slices in the DataSet
    FRecordSliceLineCounts : Array of Integer;    // Keeps track of the number of lines displayed per Slice
    FCurrentRecordSlice : Integer;                // Tracks which Slice is currently at the center of display
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    { Published declarations }

    function GetSliceSize : Integer;
    function GetDataSource : TDataSource;
    procedure SetSliceSize(const Value : Integer);
    procedure SetDataSource(Value : TDataSource);

    procedure SetOperationMode(AValue: TOperationMode);

    procedure GetRecordCount;
    procedure CalculateLineResolution;
    procedure CalculateScrollBarMax;
    procedure CalculateRecordSliceCount; // Actually calculates both FRecordSliceCount and the length of array FRecordSliceLineCounts

    property EScrollBar : TPASEmbeddedScrollBar read FScrollBar;
    property EPopupInfo : TPASEmbeddedPanel read FPopupInfo;

    property RecordSliceSize : Integer read GetSliceSize write SetSliceSize default 50; // Used to set the number of records per Slice. Allowable range is 1 to 500
    property DataLink : TPASDataLink read FDataLink write FDataLink;
    property DataSource : TDataSource read GetDataSource write SetDataSource; // Used to access the DataLink. The DataLink property of the DataSource must be set for this component to operate
    property LineResolution : Integer read FLineResolution; // The number of positions on the scrollbar allocated per record. This property is automatically calculated based upon the number of Records in the DataSet
    property RecordCount : Integer read FRecordCount; // The number of records in the dataset. For instance, if the DataSet is an SQLQuery, this value is the number of records returned from a query. This property is automatically calculated when the dataset is opened.



    {The Format property allows the programmer to determine how records will be displayed within the component}
    property Format : TStrings read FFormat write SetFormat;
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
  RegisterPropertyEditor(TypeInfo(TStrings), TPASVirtualDBScrollBase, 'Format', TPASFormatEditor);
  RegisterPropertyEditor(TypeInfo(TOperationMode), TPASVirtualDBScrollBase, 'OperationMode', TPASOperationModePropertyEditor);
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
  {$ifdef dbgDBScroll} DebugLnEnter(ClassName,'(inherited).GetRecordCount INIT'); {$endif}

  // Set this to true to that we're not trying to perform work on the dataset for every OnDataSetChange this creates
  FCountingRecords := True;

  DataLink.DataSet.Last; // Need to move to the last record in the dataset in order to get an accurate count
  FRecordCount := DataLink.DataSet.RecordCount;
  DataLink.DataSet.First; // Move back to the front

  // All done
  FCountingRecords := False;
  {$ifdef dbgDBScroll} DebugLnExit(ClassName,'(inherited).GetRecordCount DONE RecordCount=', IntToStr(FRecordCount)); {$endif}
end;

procedure TPASVirtualDBScrollBase.CalculateLineResolution;
begin
  {$ifdef dbgDBScroll} DebugLnEnter(ClassName,'(inherited).CalculateLineResolution INIT'); {$endif}
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
    FLineResolution := 1;
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
  {$ifdef dbgDBScroll} DebugLnExit(ClassName,'(inherited).CalculateLineResolution DONE RecordCount=',IntToStr(RecordCount), ' FLineResolution=',IntToStr(FLineResolution)); {$endif}
end;

procedure TPASVirtualDBScrollBase.CalculateScrollBarMax;
begin
  {$ifdef dbgDBScroll} DebugLnEnter(ClassName,'(inherited).CalculateScrollBarMax INIT'); {$endif}
  EScrollBar.Max := RecordCount * LineResolution;
  {$ifdef dbgDBScroll} DebugLnExit(ClassName,'(inherited).CalculateScrollBarMax DONE EScrollBar.Max=',IntToStr(EScrollBar.Max)); {$endif}
end;

// Count the number of Slices required based on RecordCount and RecordSliceSize
procedure TPASVirtualDBScrollBase.CalculateRecordSliceCount;
begin
  {$ifdef dbgDBScroll} DebugLnEnter(ClassName,'(inherited).CalculateRecordSliceCount INIT'); {$endif}
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
  {$ifdef dbgDBScroll} DebugLnExit(ClassName,'(inherited).CalculateRecordSliceCount DONE Length(FRecordSliceLineCounts)=',IntToStr(Length(FRecordSliceLineCounts))); {$endif}
end;

procedure TPASVirtualDBScrollBase.OnRecordChanged(Field: TField);
begin
  ShowMessage('OnRecordChanged');
end;

procedure TPASVirtualDBScrollBase.OnDataSetChanged(ADataSet: TDataSet);
begin
  //ShowMessage('OnDataSetChanged');
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

  DataLink.DataSet.FieldCount;
end;

procedure TPASVirtualDBScrollBase.OnDataSetOpen(ADataSet: TDataSet);
begin
  // Called every time the DataSet is Opened (Except when a *New* DataSet is Opened)
  // Every time this fires we need to get the record count and perform our calculations

  ShowMessage('OnDataSetOpen');

  InitializeWithData;

end;

procedure TPASVirtualDBScrollBase.OnDataSetOpening(ADataSet: TDataSet);
begin
  ShowMessage('OnDataSetOpening!');
end;

procedure TPASVirtualDBScrollBase.OnDataSetClose(ADataSet: TDataSet);
var
  i : Integer;
begin
  // Clear all old values/set back to default
  FCountingRecords := False;
  FLineResolution := 1;
  FRecordCount := 0;
  FRecordSliceCount := 0;
  FCurrentRecordSlice := 0;
  FCurrentDBName := '';

  for i := Low(FRecordSliceLineCounts) to High(FRecordSliceLineCounts) do
  begin
    FRecordSliceLineCounts[i] := 0;
  end;
  SetLength(FRecordSliceLineCounts, 0);

  ShowMessage('OnDataSetClose');

end;

procedure TPASVirtualDBScrollBase.OnNewDataSet(ADataSet: TDataSet);
begin
  // Called only when a *New* DataSet is Opened
  // Every time this fires we need to get the record count and perform our calculations

  ShowMessage('OnNewDataSet');

  InitializeWithData;

end;

procedure TPASVirtualDBScrollBase.OnInvalidDataset(ADataSet: TDataSet);
begin
  // Return an error
end;

procedure TPASVirtualDBScrollBase.OnInvalidDataSource(ADataSet: TDataSet);
begin
  // Return an error
end;

procedure TPASVirtualDBScrollBase.OnDataSetScrolled(ADataSet: TDataSet;
  Distance: Integer);
begin
  ShowMessage('OnDataSetScrolled');
end;

procedure TPASVirtualDBScrollBase.OnLayoutChanged(ADataSet: TDataSet);
begin
  ShowMessage('OnDataSetLayoutChanged');
end;

procedure TPASVirtualDBScrollBase.OnEditingChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollBase.OnUpdateData(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollBase.OnDataSetBrowse(ADataSet: TDataSet);
begin
  if not FCountingRecords then
  begin
    ShowMessage('OnDataSetBrowse');
  end;
end;

procedure TPASVirtualDBScrollBase.OnActiveChanged(ADataSet: TDataSet);
begin


  if ADataSet.Active then
  begin
    ShowMessage('OnActiveChanged');



  end;

end;

procedure TPASVirtualDBScrollBase.EScrollBarOnChange(Sender: TObject);
begin

end;

procedure TPASVirtualDBScrollBase.EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

end;

procedure TPASVirtualDBScrollBase.InitializeWithData;
begin
  {$ifdef dbgDBScroll} DebugLn(ClassName,'(inherited).InitializeWithData'); {$endif}
  GetRecordCount;
  CalculateLineResolution;
  CalculateScrollBarMax;
end;

procedure TPASVirtualDBScrollBase.SetFormat(const AValue: TStrings);
begin
  FFormat.Assign(AValue);
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

constructor TPASVirtualDBScrollBase.Create(AOwner: TComponent);
begin
  {$ifdef dbgDBScroll}
  DebugLnEnter('>> ',ClassName,'(inherited).Create INIT');
  {$endif}

  inherited Create(AOwner);



  // Set default component size and values
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
  end;
  Height := 50;
  Caption := '';

  FCurrentDBName := '';

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
  FDataLink := TPASDataLink.Create;
  FDataLink.OnRecordChanged := @OnRecordChanged;
  FDataLink.OnDatasetChanged := @OnDataSetChanged;
  FDataLink.OnDataSetOpen := @OnDataSetOpen;
  FDataLink.OnDataSetOpening := @OnDataSetOpening;
  FDataLink.OnDataSetClose := @OnDataSetClose;
  FDataLink.OnNewDataSet := @OnNewDataSet;
  FDataLink.OnInvalidDataSet := @OnInvalidDataset;
  FDataLink.OnInvalidDataSource := @OnInvalidDataSource;
  FDataLink.OnDataSetScrolled := @OnDataSetScrolled;
  FDataLink.OnLayoutChanged := @OnLayoutChanged;
  FDataLink.OnEditingChanged := @OnEditingChanged;
  FDataLink.OnUpdateData := @OnUpdateData;
  FDataLink.OnDataSetBrowse := @OnDataSetBrowse;
  FDataLink.OnActiveChanged := @OnActiveChanged;
  FDataLink.VisualControl := True;

  // Initialize the Format property
  FFormat := TStringList.Create;
  TStringList(FFormat).OnChange := @DoFormatChange;

  // Only true when we're in the midst of performing a RecordCount
  FCountingRecords := False;

  // Initially set the Slice size to 50
  FRecordSliceSize := 50;


  // Set Format property default value (blank)
  //FFormat := '';


  {$ifdef dbgDBScroll}
    DebugLnExit(ClassName,'(inherited).Create DONE');
  {$endif}

end;

destructor TPASVirtualDBScrollBase.Destroy;
begin
  {$ifdef dbgDBScroll}
    DebugLn('<< ',ClassName,'(inherited).Destroy');
  {$endif}

  FDataLink.Free;
  FDataLink := Nil;

  FPopupInfo.Free;
  FPopupInfo := Nil;

  FScrollBar.Free;
  FScrollBar := Nil;

  FFormat.Free;
  FFormat := Nil;

  inherited Destroy;

end;

procedure TPASVirtualDBScrollBase.Paint;
begin
  inherited Paint;

  //inherited Canvas.Rectangle(0,0,self.Width,self.Height);

end;

procedure TPASVirtualDBScrollBase.DoFormatChange(Sender: TObject);
begin
  Changed;
end;


end.
