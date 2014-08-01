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
  PASFormatEditor, PASVirtualDBScrollBase, windows, types;

type

  { TPASVirtualDBScrollMemo }

  TPASVirtualDBScrollMemo = class(TPASVirtualDBScrollBase)

  private
    { Private declarations }
    FMemo : TPASEmbeddedMemo;

    FRecordSliceCount : Integer;                  // Number of record Slices in the DataSet
    FRecordSliceLineCounts : Array of Integer;    // Keeps track of the number of lines displayed per Slice
    FCurrentRecordSlice : Integer;                // Tracks which Slice is currently at the center of display
    FVisibleLines : Integer;                      // How many lines are visible in EmbeddedMemo


    FError : String;                              //


    function GetVisibleLineCount : Integer; // Approximates the visible line count of EMemo

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
    procedure EMemoOnResize(Sender: TObject);
    procedure EScrollBarOnChange(Sender: TObject);
    procedure EScrollBarOnKeyPress(Sender: TObject; var Key: char);
    procedure EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    procedure MoveToLine(LineNo: Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }


    property EMemo : TPASEmbeddedMemo read FMemo;
    //property EMemo; // Inherited from TPASVirtualDBScrollBase
    property EScrollBar; // Inherited from TPASVirtualDBScrollBase

    property RecordSliceSize;
    // property RecordSliceSize : Integer read GetSliceSize write SetSliceSize default 50; // Used to set the number of records per Slice. Allowable range is 1 to 500

    property DataLink;
    // property DataLink : TComponentDataLink read FDataLink write FDataLink;

    property DataSource;
    // property DataSource : TDataSource read GetDataSource write SetDataSource;

    property LineResolution;
    // property LineResolution : Integer read FLineResolution;

    property RecordCount;
    // property RecordCount : Integer read FRecordCount;

    property Format;
    // property Format : String read GetFormat write SetFormat;


    //property Align;
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

    procedure OnResize(Sender: TObject);
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Additional', [TPASVirtualDBScrollMemo]);

  RegisterPropertyEditor(TypeInfo(String), TPASVirtualDBScrollMemo, 'Format', TPASFormatEditor);
end;

function TPASVirtualDBScrollMemo.GetVisibleLineCount : Integer;
begin
  try
    Result := FMemo.Height div FMemo.Font.GetTextHeight('I');
  except
    Result := FMemo.Height div 12;
  end;
end;

procedure TPASVirtualDBScrollMemo.DataLinkOnRecordChanged(Field: TField);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnDataSetChanged(ADataSet: TDataSet);
begin
  // Every time we move positions within the DataSet
  // ShowMessage('OnDataSetChanged');
end;

procedure TPASVirtualDBScrollMemo.DataLinkOnDataSetOpen(ADataSet: TDataSet);
begin
  // Probably the most useful DataSet event
  // Every time this fires we need to get the record count and perform our calculations
  // ShowMessage('OnDataSetOpen');
  /////////////GetRecordCount;
  //ShowMessage(IntToStr(FRecordCount));
end;

procedure TPASVirtualDBScrollMemo.DataLinkOnDataSetClose(ADataSet: TDataSet);
begin
  // May be useful for clean-up
  // ShowMessage('OnDataSetClose');
end;

procedure TPASVirtualDBScrollMemo.DataLinkOnNewDataSet(ADataSet: TDataSet);
begin
  // Not particularly useful for my purposes
  // ShowMessage('OnNewDataSet');
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
  ShowMessage('OnDataSetScrolled');
end;

procedure TPASVirtualDBScrollMemo.DataLinkOnLayoutChanged(ADataSet: TDataSet);
begin
  ShowMessage('OnDataSetLayoutChanged');
end;

procedure TPASVirtualDBScrollMemo.DataLinkOnEditingChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.DataLinkOnUpdateData(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.EMemoOnKeyPress(Sender: TObject; var Key: char);
begin
  ShowMessage('KeyPress: ' + Key); // Works
end;

procedure TPASVirtualDBScrollMemo.EScrollBarOnChange(Sender: TObject);
begin

end;

procedure TPASVirtualDBScrollMemo.EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  //ShowMessage('');
end;

procedure TPASVirtualDBScrollMemo.MoveToLine(LineNo: Integer);
begin
  EMemo.CaretPos := Point(0, LineNo);
  EMemo.SetFocus;
end;

procedure TPASVirtualDBScrollMemo.EMemoOnResize(Sender: TObject);
begin
  FVisibleLines := GetVisibleLineCount;
  //ShowMessage(IntToStr(FVisibleLines));
end;

procedure TPASVirtualDBScrollMemo.EScrollBarOnKeyPress(Sender: TObject; var Key: char);
begin

end;

constructor TPASVirtualDBScrollMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default component size and values
  Height := 50;
  Caption := '';
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
  end;


  // Initialize the Embedded Memo
  FMemo := TPASEmbeddedMemo.Create(Self); // Add the embedded memo
  FMemo.Parent := self;         // Show the memo in the panel
  FMemo.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FMemo.Name := 'EMemo';
  FMemo.ControlStyle := FMemo.ControlStyle - [csNoDesignSelectable]; // Make sure it can not be selected/deleted within the IDE
  FMemo.Lines.Clear; // Should I allow the user to set some default text?
  FMemo.Align := alClient;

  // Set up Memo Events
  FMemo.OnKeyPress := @EMemoOnKeyPress;
  FMemo.OnResize := @EMemoOnResize;

  // Set up Scrollbar Events
  EScrollBar.OnChange := @EScrollBarOnChange;
  EScrollBar.OnKeyPress := @EScrollBarOnKeyPress;
  EScrollBar.OnScroll := @EScrollBarOnScroll;


  FVisibleLines := GetVisibleLineCount;

end;

destructor TPASVirtualDBScrollMemo.Destroy;
begin
  FMemo.Free;
  FMemo := nil;

  inherited Destroy;
end;

procedure TPASVirtualDBScrollMemo.OnResize(Sender: TObject);
begin
  //ShowMessage('');
  //FMemo.Width := Self.Width - EScrollBar.Width;
end;

initialization
  {$I TPASVirtualDBScrollMemo.lrs}

end.
