unit PASVirtualDBScrollMemo;

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
  PASVirtualDBScrollBase, windows, types {$ifdef dbgDBScroll}, LazLogger{$endif};

type

  { TPASVirtualDBScrollMemo }

  TPASVirtualDBScrollMemo = class(TPASVirtualDBScrollBase)
  private
    { Private declarations }
    FMemo : TPASEmbeddedMemo;

    FVisibleLines : Integer;                      // How many lines are visible in EmbeddedMemo

    FError : String;                              //


    // Event Handlers
    procedure OnRecordChanged(Field : TField);
    procedure OnDataSetChanged(ADataSet : TDataSet);
    procedure OnDataSetOpen(ADataSet : TDataSet);
    procedure OnDataSetClose(ADataSet : TDataSet);
    procedure OnNewDataSet(ADataSet : TDataSet);
    procedure OnInvalidDataset(ADataSet : TDataSet);
    procedure OnInvalidDataSource(ADataSet : TDataSet);
    procedure OnDataSetScrolled(ADataSet : TDataSet; Distance : Integer);
    procedure OnLayoutChanged(ADataSet : TDataSet);
    procedure OnEditingChanged(ADataSet : TDataSet);
    procedure OnUpdateData(ADataSet : TDataSet);
    procedure EMemoOnKeyPress(Sender: TObject; var Key: char);
    procedure EMemoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EMemoOnResize(Sender: TObject);
    procedure EScrollBarOnChange(Sender: TObject);
    procedure EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    procedure MoveToLine(LineNo: Integer);
    function GetVisibleLineCount : Integer; // Approximates the visible line count of EMemo
    procedure CalcSmallStep(var ASmallStep : Integer);
    procedure CalcStepsPerLine(var AStepsPerLine : Integer);
    procedure CalcCurrentSlice(var ATempCurrentSlice : Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property EMemo : TPASEmbeddedMemo read FMemo;
    property EScrollBar; // Inherited from TPASVirtualDBScrollBase
    property EPopupInfo; // Inherited from TPASVirtualDBScrollBase


    property OperationMode;

    property RecordSliceSize;

    property DataLink;

    property DataSource;

    property LineResolution;

    property RecordCount;

    property Format;

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
end;

function TPASVirtualDBScrollMemo.GetVisibleLineCount : Integer;
begin
  try
    Result := FMemo.Height div FMemo.Font.GetTextHeight('I');
    {$ifdef dbgDBScroll} DebugLn(ClassName,'.GetVisibleLineCount Count=',IntToStr(Result)); {$endif}
  except
    Result := FMemo.Height div 12;
    {$ifdef dbgDBScroll} DebugLn(ClassName,'.GetVisibleLineCount (Exception Occurred) Count=',IntToStr(Result)); {$endif}
  end;
end;

procedure TPASVirtualDBScrollMemo.CalcSmallStep(var ASmallStep: Integer);
begin
  {$ifdef dbgDBScroll} DebugLnEnter(Classname,'.CalcSmallStep INIT FRecordSliceCount=',IntToStr(FRecordSliceCount)); {$endif}
  try
    ASmallStep := (EScrollBar.Max div FRecordSliceCount) div EMemo.Lines.Count;
  except
    ASmallStep := 1
  end;
  {$ifdef dbgDBScroll} DebugLnExit(Classname,'.CalcSmallStep DONE ASmallStep=',IntToStr(ASmallStep)); {$endif}
end;

procedure TPASVirtualDBScrollMemo.CalcStepsPerLine(var AStepsPerLine: Integer);
begin
  {$ifdef dbgDBScroll} DebugLnEnter(Classname,'.CalcStepsPerLine INIT'); {$endif}
  try
    AStepsPerLine := EScrollBar.Max div (FRecordSliceCount * EMemo.Lines.Count);
  except
    AStepsPerLine := 1
  end;
  {$ifdef dbgDBScroll} DebugLnExit(Classname,'.CalcStepsPerLine DONE AStepsPerLine=',IntToStr(AStepsPerLine)); {$endif}
end;

procedure TPASVirtualDBScrollMemo.CalcCurrentSlice(var ATempCurrentSlice: Integer);
begin
  {$ifdef dbgDBScroll} DebugLnEnter(Classname,'.CalcCurrentSlice INIT FLineResolution=',IntToStr(FLineResolution),' FRecordSliceSize=',IntToStr(FRecordSliceSize)); {$endif}
  try
    ATempCurrentSlice := (((EScrollBar.Position div FLineResolution) div FRecordSliceSize) + 1);
  except
    ATempCurrentSlice := 1;
  end;
  {$ifdef dbgDBScroll} DebugLnExit(Classname,'.CalcCurrentSlice DONE ATempCurrentSlice=',IntToStr(ATempCurrentSlice)); {$endif}
end;

procedure TPASVirtualDBScrollMemo.OnRecordChanged(Field: TField);
begin

end;

procedure TPASVirtualDBScrollMemo.OnDataSetChanged(ADataSet: TDataSet);
begin
  // Every time we move positions within the DataSet
  // ShowMessage('OnDataSetChanged');
end;

procedure TPASVirtualDBScrollMemo.OnDataSetOpen(ADataSet: TDataSet);
begin
  // Probably the most useful DataSet event
  // Every time this fires we need to get the record count and perform our calculations
  // ShowMessage('OnDataSetOpen');
  /////////////GetRecordCount;
  //ShowMessage(IntToStr(FRecordCount));
end;

procedure TPASVirtualDBScrollMemo.OnDataSetClose(ADataSet: TDataSet);
begin
  // May be useful for clean-up
  // ShowMessage('OnDataSetClose');
end;

procedure TPASVirtualDBScrollMemo.OnNewDataSet(ADataSet: TDataSet);
begin
  // Not particularly useful for my purposes
  // ShowMessage('OnNewDataSet');
end;

procedure TPASVirtualDBScrollMemo.OnInvalidDataset(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.OnInvalidDataSource(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.OnDataSetScrolled(ADataSet: TDataSet;
  Distance: Integer);
begin
  ShowMessage('OnDataSetScrolled');
end;

procedure TPASVirtualDBScrollMemo.OnLayoutChanged(ADataSet: TDataSet);
begin
  ShowMessage('OnDataSetLayoutChanged');
end;

procedure TPASVirtualDBScrollMemo.OnEditingChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.OnUpdateData(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollMemo.EMemoOnKeyPress(Sender: TObject; var Key: char);
begin

end;

{ Used to allow the end-user to navigate the DataSet using the directional keys }
procedure TPASVirtualDBScrollMemo.EMemoOnKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin

  end
  else if Key = VK_UP then
  begin

  end
  else if Key = VK_PRIOR then // Page Up
  begin

  end
  else if Key = VK_NEXT then // Page Down
  begin

  end
  else if Key = VK_HOME then
  begin

  end
  else if Key = VK_END then
  begin

  end;
end;

procedure TPASVirtualDBScrollMemo.EScrollBarOnChange(Sender: TObject);
begin

end;


procedure TPASVirtualDBScrollMemo.EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  SmallStep : Integer; // How many positions we move on the scrollbar for each arrow press on the scrollbar
  StepsPerLine : Integer; // How many positions on the scrollbar moves the caret one line down in the memo
  LinesCurrentSliceSet : Integer; // Lines in the current set of three slices
  TempCurrentSlice : Integer; // The center slice of the 3 current slices
  ScrollingDone : Boolean; // Whether we've finished scrolling or not
begin
  ScrollingDone := False;

  CalcSmallStep(SmallStep);
  CalcStepsPerLine(StepsPerLine);
  CalcCurrentSlice(TempCurrentSlice);

  if EPopupInfo.IsDisplayed then
  begin
    EPopupInfo.Visible := True;
    EPopupInfo.Left := EScrollBar.Left - EPopupInfo.Width;
    EPopupInfo.Top := Mouse.CursorPos.y - Parent.Top - Self.Top - (EPopupInfo.Height);
    // ToDo: Improve EPopupInfo.Top placement
    EPopupInfo.Caption := IntToStr(EScrollBar.Position); // Eventually this should specify which record we're on
    EPopupInfo.BringToFront;
    EMemo.Text := 'x: ' + (IntToStr(Mouse.CursorPos.x) + ' - ' + IntToStr(Parent.Left) + ' = ' + IntToStr(Mouse.CursorPos.x - Parent.Left)) +
    ' and y: ' + (IntToStr(Mouse.CursorPos.y) + ' - ' + IntToStr(Parent.Top) + ' = ' + IntToStr(Mouse.CursorPos.y - Parent.Top));
  end;

  case ScrollCode of
    scLineUp : ScrollPos := ScrollPos - (SmallStep - 1); //
    scLineDown : ScrollPos := ScrollPos + SmallStep - 1; //
    scPageUp : ScrollPos := ScrollPos - (SmallStep * FVisibleLines); //
    scPageDown : ScrollPos := ScrollPos + (SmallStep * FVisibleLines); //
    scPosition : ; //
    scTrack : ; //
    scTop : ScrollingDone := True; //
    scBottom : ScrollingDone := True; //
    scEndScroll : begin
                    EPopupInfo.Visible := False; // Turn off EPopupInfo
                    ScrollingDone := True;
                  end;
  end;


end;

procedure TPASVirtualDBScrollMemo.MoveToLine(LineNo: Integer);
begin
  {$ifdef dbgDBScroll} DebugLn(Classname,'.MoveToLine LineNo=',IntToStr(LineNo)); {$endif}
  EMemo.CaretPos := Point(0, LineNo);
  EMemo.SetFocus;
end;

procedure TPASVirtualDBScrollMemo.EMemoOnResize(Sender: TObject);
begin
  {$ifdef dbgDBScroll} DebugLn(Classname,'.EMemoOnResize'); {$endif}
  FVisibleLines := GetVisibleLineCount;
end;

constructor TPASVirtualDBScrollMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {$ifdef dbgDBScroll} DebugLnEnter(Classname,'.Create INIT'); {$endif}

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
  FMemo.SetSubComponent(True);  // Tell the IDE to store the modified properties
  FMemo.Name := 'EMemo';
  FMemo.ControlStyle := FMemo.ControlStyle - [csNoDesignSelectable]; // Make sure it can not be selected/deleted within the IDE
  FMemo.Lines.Clear; // Should I allow the user to set some default text?
  FMemo.Align := alClient;

  // Set up Memo Events
  FMemo.OnKeyPress := @EMemoOnKeyPress;
  FMemo.OnKeyDown := @EMemoOnKeyDown;
  FMemo.OnResize := @EMemoOnResize;

  // Set up Scrollbar Events
  EScrollBar.OnChange := @EScrollBarOnChange;
  EScrollBar.OnScroll := @EScrollBarOnScroll;

  {$ifdef dbgDBScroll} DebugLnExit(Classname,'.Create DONE'); {$endif}
end;

destructor TPASVirtualDBScrollMemo.Destroy;
begin
  {$ifdef dbgDBScroll} DebugLn(Classname,'.Destroy'); {$endif}

  FMemo.Free;
  FMemo := nil;

  inherited Destroy;
end;

initialization
  {$I TPASVirtualDBScrollMemo.lrs}

end.
