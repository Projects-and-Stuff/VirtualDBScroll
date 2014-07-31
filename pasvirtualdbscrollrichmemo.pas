unit PASVirtualDBScrollRichMemo;

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
  ExtCtrls, PASEmbeddedRichMemo, PASEmbeddedScrollBar, db, DBGrids, PropEdits,
  PASFormatEditor, PASVirtualDBScrollBase, windows;

type

  { TPASVirtualDBScrollRichMemo }

  TPASVirtualDBScrollRichMemo = class(TPASVirtualDBScrollBase)

  private
    { Private declarations }
    FRichMemo : TPASEmbeddedRichMemo;

    FRecordSliceCount : Integer;                  // Number of record Slices in the DataSet
    FRecordSliceLineCounts : Array of Integer;    // Keeps track of the number of lines displayed per Slice
    FCurrentRecordSlice : Integer;                // Tracks which Slice is currently at the center of display
    FVisibleLines : Integer;                      // How many lines are visible in EmbeddedRichMemo


    FError : String;                              //

    function GetVisibleLineCount : Integer; // Approximates the visible line count of ERichMemo


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
    procedure ERichMemoOnKeyPress(Sender: TObject; var Key: char);
    procedure ERichMemoOnResize(Sender: TObject);
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


    property ERichMemo : TPASEmbeddedRichMemo read FRichMemo;
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

  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Additional', [TPASVirtualDBScrollRichMemo]);

  RegisterPropertyEditor(TypeInfo(String), TPASVirtualDBScrollRichMemo, 'Format', TPASFormatEditor);
end;

function TPASVirtualDBScrollRichMemo.GetVisibleLineCount : Integer;
begin
  try
    Result := FRichMemo.Height div FRichMemo.Font.GetTextHeight('I');
  except
    Result := FRichMemo.Height div 12;
  end;
end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnRecordChanged(Field: TField);
begin

end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnDataSetChanged(ADataSet: TDataSet);
begin
  // Every time we move positions within the DataSet
  // ShowMessage('OnDataSetChanged');
end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnDataSetOpen(ADataSet: TDataSet);
begin
  // Probably the most useful DataSet event
  // Every time this fires we need to get the record count and perform our calculations
  // ShowMessage('OnDataSetOpen');
  /////////////GetRecordCount;
  //ShowMessage(IntToStr(FRecordCount));
end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnDataSetClose(ADataSet: TDataSet);
begin
  // May be useful for clean-up
  // ShowMessage('OnDataSetClose');
end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnNewDataSet(ADataSet: TDataSet);
begin
  // Not particularly useful for my purposes
  // ShowMessage('OnNewDataSet');
end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnInvalidDataset(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnInvalidDataSource(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnDataSetScrolled(ADataSet: TDataSet;
  Distance: Integer);
begin
  ShowMessage('OnDataSetScrolled');
end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnLayoutChanged(ADataSet: TDataSet);
begin
  ShowMessage('OnDataSetLayoutChanged');
end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnEditingChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollRichMemo.DataLinkOnUpdateData(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollRichMemo.ERichMemoOnKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TPASVirtualDBScrollRichMemo.ERichMemoOnResize(Sender: TObject);
begin

end;

procedure TPASVirtualDBScrollRichMemo.EScrollBarOnChange(Sender: TObject);
begin

end;

procedure TPASVirtualDBScrollRichMemo.EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  ShowMessage('');
end;

procedure TPASVirtualDBScrollRichMemo.EScrollBarOnKeyPress(Sender: TObject; var Key: char);
begin

end;

constructor TPASVirtualDBScrollRichMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default component size and values
  Height := 50;
  Caption := '';
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
  end;


  // Initialize the Embedded RichMemo
  FRichMemo := TPASEmbeddedRichMemo.Create(Self); // Add the embedded RichMemo
  FRichMemo.Parent := self;         // Show the RichMemo in the panel
  FRichMemo.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FRichMemo.Name := 'ERichMemo';
  FRichMemo.ControlStyle := FRichMemo.ControlStyle - [csNoDesignSelectable]; // Make sure it can not be selected/deleted within the IDE
  FRichMemo.Lines.Clear; // Should I allow the user to set some default text?
  FRichMemo.Align := alClient;

  // Set up RichMemo Events
  FRichMemo.OnKeyPress := @ERichMemoOnKeyPress;
  FRichMemo.OnResize := @ERichMemoOnResize;

  // Set up Scrollbar Events
  EScrollBar.OnChange := @EScrollBarOnChange;
  EScrollBar.OnKeyPress := @EScrollBarOnKeyPress;
  EScrollBar.OnScroll := @EScrollBarOnScroll;

  FVisibleLines := GetVisibleLineCount;

end;

destructor TPASVirtualDBScrollRichMemo.Destroy;
begin

  FRichMemo.Free;
  FRichMemo := nil;

  inherited Destroy;
end;

initialization
  {$I TPASVirtualDBScrollRichMemo.lrs}

end.
