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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EmbeddedMemo, EmbeddedScrollBar, db;

type



  TVirtualDBScrollMemo = class(TCustomPanel)

  private
    { Private declarations }
    FEmbeddedMemo : TEmbeddedMemo;
    FEmbeddedScrollBar : TEmbeddedScrollBar;
    FRecordChunkSize : Integer;
    FDataSet : TDataSet;

    function GetChunkSize: Integer;
    procedure SetChunkSize(const Value: Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
  published
    { Published declarations }
    property EmbeddedMemo : TEmbeddedMemo read FEmbeddedMemo;
    property EmbeddedScrollBar : TEmbeddedScrollBar read FEmbeddedScrollBar;

    property RecordChunkSize : Integer read GetChunkSize write SetChunkSize default 50; // Used to set the number of records per chunk. Allowable range is 1 to 500
    property DataSet : TDataSet read FDataSet write FDataSet;

    property Align;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Color; // Do I need this?
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
  RegisterComponents('Additional',[TVirtualDBScrollMemo]);

end;

function TVirtualDBScrollMemo.GetChunkSize: Integer;
begin
  Result := FRecordChunkSize;
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
  FEmbeddedMemo := TEmbeddedMemo.Create(Self); // Add the embedded memo
  FEmbeddedMemo.Parent := self;         // Show the memo in the panel
  FEmbeddedMemo.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FEmbeddedMemo.Name := 'EmbeddedMemo';
  FEmbeddedMemo.ScrollBars := ssHorizontal; //[ssNone, ssHorizontal, ssAutoHorizontal];
  FEmbeddedMemo.Lines.Clear;


  FEmbeddedScrollBar := TEmbeddedScrollBar.Create(Self); // Add the embedded memo
  FEmbeddedScrollBar.Parent := self;         // Show the memo in the panel
  FEmbeddedScrollBar.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FEmbeddedScrollBar.Width := 15;
  FEmbeddedScrollBar.Align := alRight;
  FEmbeddedScrollBar.Name := 'EmbeddedScrollBar';
  FEmbeddedScrollBar.Kind := sbVertical;



  // Make sure the embedded components can not be selected/deleted within the IDE
  FEmbeddedMemo.ControlStyle := FEmbeddedMemo.ControlStyle - [csNoDesignSelectable];
  FEmbeddedScrollBar.ControlStyle := FEmbeddedScrollBar.ControlStyle - [csNoDesignSelectable];

  FRecordChunkSize := 50;



end;

initialization
  {$I TVirtualDBScrollMemo.lrs}

end.
