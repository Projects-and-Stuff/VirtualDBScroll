unit PASVirtualDBScrollSynEdit;

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
  ExtCtrls, PASEmbeddedSynEdit, PASEmbeddedScrollBar, db, PropEdits,
  PASVirtualDBScrollBase, SynEdit, SynCompletion, windows, types {$ifdef dbgDBScroll}, LazLogger{$endif};

type

  { TPASVirtualDBScrollSynEdit }

  TPASVirtualDBScrollSynEdit = class(TPASVirtualDBScrollBase)
  private
    { Private declarations }
    FSynEdit : TPASEmbeddedSynEdit;
    FCompletion : TSynCompletion;

    FVisibleLines : Integer;                      // How many lines are visible in EmbeddedSynEdit

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
    procedure ESynEditOnKeyPress(Sender: TObject; var Key: char);
    procedure ESynEditOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ESynEditOnResize(Sender: TObject);
    procedure EScrollBarOnChange(Sender: TObject);
    procedure EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    procedure MoveToLine(LineNo: Integer);
    function GetVisibleLineCount : Integer; // Approximates the visible line count of ESynEdit
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
    property Completion : TSynCompletion read FCompletion write FCompletion;
    property ESynEdit : TPASEmbeddedSynEdit read FSynEdit;
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
  RegisterComponents('Additional', [TPASVirtualDBScrollSynEdit]);
end;

function TPASVirtualDBScrollSynEdit.GetVisibleLineCount : Integer;
begin
  try
    Result := FSynEdit.Height div FSynEdit.Font.GetTextHeight('I');
    {$ifdef dbgDBScroll} DebugLn(ClassName,'.GetVisibleLineCount Count=',IntToStr(Result)); {$endif}
  except
    Result := FSynEdit.Height div 12;
    {$ifdef dbgDBScroll} DebugLn(ClassName,'.GetVisibleLineCount (Exception Occurred) Count=',IntToStr(Result)); {$endif}
  end;
end;

procedure TPASVirtualDBScrollSynEdit.CalcSmallStep(var ASmallStep: Integer);
begin
  {$ifdef dbgDBScroll} DebugLnEnter(Classname,'.CalcSmallStep INIT FRecordSliceCount=',IntToStr(FRecordSliceCount)); {$endif}
  try
    ASmallStep := (EScrollBar.Max div FRecordSliceCount) div ESynEdit.Lines.Count;
  except
    ASmallStep := 1
  end;
  {$ifdef dbgDBScroll} DebugLnExit(Classname,'.CalcSmallStep DONE ASmallStep=',IntToStr(ASmallStep)); {$endif}
end;

procedure TPASVirtualDBScrollSynEdit.CalcStepsPerLine(var AStepsPerLine: Integer
  );
begin
  {$ifdef dbgDBScroll} DebugLnEnter(Classname,'.CalcStepsPerLine INIT'); {$endif}
  try
    AStepsPerLine := EScrollBar.Max div (FRecordSliceCount * ESynEdit.Lines.Count);
  except
    AStepsPerLine := 1
  end;
  {$ifdef dbgDBScroll} DebugLnExit(Classname,'.CalcStepsPerLine DONE AStepsPerLine=',IntToStr(AStepsPerLine)); {$endif}
end;

procedure TPASVirtualDBScrollSynEdit.CalcCurrentSlice(var ATempCurrentSlice: Integer
  );
begin
  {$ifdef dbgDBScroll} DebugLnEnter(Classname,'.CalcCurrentSlice INIT FLineResolution=',IntToStr(FLineResolution),' FRecordSliceSize=',IntToStr(FRecordSliceSize)); {$endif}
  try
    ATempCurrentSlice := (((EScrollBar.Position div FLineResolution) div FRecordSliceSize) + 1);
  except
    ATempCurrentSlice := 1;
  end;
  {$ifdef dbgDBScroll} DebugLnExit(Classname,'.CalcCurrentSlice DONE ATempCurrentSlice=',IntToStr(ATempCurrentSlice)); {$endif}
end;

procedure TPASVirtualDBScrollSynEdit.OnRecordChanged(Field: TField);
begin

end;

procedure TPASVirtualDBScrollSynEdit.OnDataSetChanged(ADataSet: TDataSet);
begin
  // Every time we move positions within the DataSet
  // ShowMessage('OnDataSetChanged');
end;

procedure TPASVirtualDBScrollSynEdit.OnDataSetOpen(ADataSet: TDataSet);
begin
  // Probably the most useful DataSet event
  // Every time this fires we need to get the record count and perform our calculations
  // ShowMessage('OnDataSetOpen');
  /////////////GetRecordCount;
  //ShowMessage(IntToStr(FRecordCount));
end;

procedure TPASVirtualDBScrollSynEdit.OnDataSetClose(ADataSet: TDataSet);
begin
  // May be useful for clean-up
  // ShowMessage('OnDataSetClose');
end;

procedure TPASVirtualDBScrollSynEdit.OnNewDataSet(ADataSet: TDataSet);
begin
  // Not particularly useful for my purposes
  // ShowMessage('OnNewDataSet');
end;

procedure TPASVirtualDBScrollSynEdit.OnInvalidDataset(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollSynEdit.OnInvalidDataSource(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollSynEdit.OnDataSetScrolled(ADataSet: TDataSet;
  Distance: Integer);
begin
  ShowMessage('OnDataSetScrolled');
end;

procedure TPASVirtualDBScrollSynEdit.OnLayoutChanged(ADataSet: TDataSet);
begin
  ShowMessage('OnDataSetLayoutChanged');
end;

procedure TPASVirtualDBScrollSynEdit.OnEditingChanged(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollSynEdit.OnUpdateData(ADataSet: TDataSet);
begin

end;

procedure TPASVirtualDBScrollSynEdit.ESynEditOnKeyPress(Sender: TObject; var Key: char);
begin

end;

{ Used to allow the end-user to navigate the DataSet using the directional keys }
procedure TPASVirtualDBScrollSynEdit.ESynEditOnKeyDown(Sender: TObject;
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

procedure TPASVirtualDBScrollSynEdit.ESynEditOnResize(Sender: TObject);
begin
  {$ifdef dbgDBScroll} DebugLn(Classname,'.ESynEditOnResize'); {$endif}
  FVisibleLines := GetVisibleLineCount;
end;

procedure TPASVirtualDBScrollSynEdit.EScrollBarOnChange(Sender: TObject);
begin

end;

procedure TPASVirtualDBScrollSynEdit.EScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
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
    EPopupInfo.Top := Mouse.CursorPos.y - Parent.Top - Self.Top - EPopupInfo.Height;
    // ToDo: Improve EPopupInfo.Top placement
    EPopupInfo.Caption := IntToStr(EScrollBar.Position); // Eventually this should specify which record we're on
    EPopupInfo.BringToFront;
    ESynEdit.Text := 'x: ' + (IntToStr(Mouse.CursorPos.x) + ' - ' + IntToStr(Parent.Left) + ' = ' + IntToStr(Mouse.CursorPos.x - Parent.Left)) +
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

procedure TPASVirtualDBScrollSynEdit.MoveToLine(LineNo: Integer);
begin
  {$ifdef dbgDBScroll} DebugLn(Classname,'.MoveToLine LineNo=',IntToStr(LineNo)); {$endif}
  ESynEdit.CaretXY := Point(0, LineNo);
  ESynEdit.SetFocus;
end;

constructor TPASVirtualDBScrollSynEdit.Create(AOwner: TComponent);
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


  // Initialize the Embedded SynEdit
  FSynEdit := TPASEmbeddedSynEdit.Create(Self); // Add the embedded SynEdit
  FSynEdit.Parent := self;         // Show the SynEdit in the panel
  FSynEdit.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FSynEdit.Name := 'ESynEdit';
  FSynEdit.ControlStyle := FSynEdit.ControlStyle - [csNoDesignSelectable]; // Make sure it can not be selected/deleted within the IDE
  FSynEdit.Lines.Clear; // Should I allow the user to set some default text
  FSynEdit.Align := alClient;

  // Set up SynEdit Events
  FSynEdit.OnKeyPress := @ESynEditOnKeyPress;
  FSynEdit.OnKeyDown := @ESynEditOnKeyDown;
  FSynEdit.OnResize := @ESynEditOnResize;

  // Set up Scrollbar Events
  EScrollBar.OnChange := @EScrollBarOnChange;
  EScrollBar.OnScroll := @EScrollBarOnScroll;

  {$ifdef dbgDBScroll} DebugLnExit(Classname,'.Create DONE'); {$endif}
end;

destructor TPASVirtualDBScrollSynEdit.Destroy;
begin
  {$ifdef dbgDBScroll} DebugLn(Classname,'.Destroy'); {$endif}

  FSynEdit.Free;
  FSynEdit := nil;

  inherited Destroy;
end;

initialization
  {$I TPASVirtualDBScrollSynEdit.lrs}

end.
