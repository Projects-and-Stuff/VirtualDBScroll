unit PASDataLink;

{$mode objfpc}{$H+}
{$DEFINE dbgDBScroll}

interface

uses
  Classes, SysUtils, db, DBGrids, LazLogger;

type

{ TPASDataLink }

TPASDataLink = class(TDataLink)
  private
    FDataSet : TDataSet;
    FDataSetName : string;
    FModified : Boolean;
    FOnActiveChanged : TDatasetNotifyEvent;
    FOnDatasetChanged : TDatasetNotifyEvent;
    FOnDataSetClose : TDataSetNotifyEvent;
    FOnDataSetOpen : TDataSetNotifyEvent;
    FOnDataSetScrolled : TDataSetScrolledEvent;
    FOnEditingChanged : TDataSetNotifyEvent;
    FOnInvalidDataSet : TDataSetNotifyEvent;
    FOnInvalidDataSource : TDataSetNotifyEvent;
    FOnLayoutChanged : TDataSetNotifyEvent;
    FOnNewDataSet : TDataSetNotifyEvent;
    FOnRecordChanged : TFieldNotifyEvent;
    FOnUpdateData : TDataSetNotifyEvent;
    FOnDataSetInactive : TDataSetNotifyEvent;
    FOnDataSetBrowse : TDataSetNotifyEvent;
    FOnDataSetEdit : TDataSetNotifyEvent;
    FOnDataSetInsert : TDataSetNotifyEvent;
    FOnDataSetSetKey : TDataSetNotifyEvent;
    FOnDataSetCalcField : TDataSetNotifyEvent;
    FOnDataSetFilter : TDataSetNotifyEvent;
    FOnDataSetNewValue : TDataSetNotifyEvent;
    FOnDataSetOldValue : TDataSetNotifyEvent;
    FOnDataSetCurValue : TDataSetNotifyEvent;
    FOnDataSetBlockRead : TDataSetNotifyEvent;
    FOnDataSetInternalCalc : TDataSetNotifyEvent;
    FOnDataSetOpening : TDataSetNotifyEvent;

    function GetDataSetName : string;
    function GetFields(Index : Integer) : TField;
    procedure SetDataSetName(const AValue : string);
  protected
    procedure RecordChanged(Field : TField); override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetScrolled(Distance : Integer); override;
    procedure FocusControl(Field : TFieldRef); override;

    procedure CheckBrowseMode; override;
    procedure EditingChanged; override;
    procedure UpdateData; override;
    function  MoveBy(Distance : Integer) : Integer; override;
    property  Modified : Boolean read FModified write FModified;
  public
    property OnRecordChanged : TFieldNotifyEvent read FOnRecordChanged write FOnRecordChanged;
    property OnDataSetInactive : TDataSetNotifyEvent read FOnDataSetInactive write FOnDataSetInactive;
    property OnDataSetBrowse : TDataSetNotifyEvent read FOnDataSetBrowse write FOnDataSetBrowse;
    property OnDataSetEdit : TDataSetNotifyEvent read FOnDataSetEdit write FOnDataSetEdit;
    property OnDataSetInsert : TDataSetNotifyEvent read FOnDataSetInsert write FOnDataSetInsert;
    property OnDataSetSetKey : TDataSetNotifyEvent read FOnDataSetSetKey write FOnDataSetSetKey;
    property OnDataSetCalcField : TDataSetNotifyEvent read FOnDataSetCalcField write FOnDataSetCalcField;
    property OnDataSetFilter : TDataSetNotifyEvent read FOnDataSetFilter write FOnDataSetFilter;
    property OnDataSetNewValue : TDataSetNotifyEvent read FOnDataSetNewValue write FOnDataSetNewValue;
    property OnDataSetOldValue : TDataSetNotifyEvent read FOnDataSetOldValue write FOnDataSetOldValue;
    property OnDataSetCurValue : TDataSetNotifyEvent read FOnDataSetCurValue write FOnDataSetCurValue;
    property OnDataSetBlockRead : TDataSetNotifyEvent read FOnDataSetBlockRead write FOnDataSetBlockRead;
    property OnDataSetInternalCalc : TDataSetNotifyEvent read FOnDataSetInternalCalc write FOnDataSetInternalCalc;
    property OnDataSetOpening : TDataSetNotifyEvent read FOnDataSetOpening write FOnDataSetOpening;
    property OnDataSetChanged : TDatasetNotifyEvent read FOnDatasetChanged write FOnDataSetChanged;
    property OnActiveChanged : TDatasetNotifyEvent read FOnActiveChanged write FOnActiveChanged;
    property OnNewDataSet : TDataSetNotifyEvent read FOnNewDataSet write FOnNewDataSet;
    property OnDataSetOpen : TDataSetNotifyEvent read FOnDataSetOpen write FOnDataSetOpen;
    property OnInvalidDataSet : TDataSetNotifyEvent read FOnInvalidDataSet write FOnInvalidDataSet;
    property OnInvalidDataSource : TDataSetNotifyEvent read FOnInvalidDataSource write FOnInvalidDataSource;
    property OnLayoutChanged : TDataSetNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnDataSetClose : TDataSetNotifyEvent read FOnDataSetClose write FOnDataSetClose;
    property OnDataSetScrolled : TDataSetScrolledEvent read FOnDataSetScrolled write FOnDataSetScrolled;
    property OnEditingChanged : TDataSetNotifyEvent read FOnEditingChanged write FOnEditingChanged;
    property OnUpdateData : TDataSetNotifyEvent read FOnUpdateData write FOnUpdateData;
    property DataSetName : string read GetDataSetName write SetDataSetName;
    property Fields[Index : Integer] : TField read GetFields;
    property VisualControl;
  end;

implementation

{ TPASDataLink }

function TPASDataLink.GetDataSetName: string;
begin
  Result := FDataSetName;
  if DataSet <> nil then
  begin
    Result := DataSet.Name;
  end;
end;

function TPASDataLink.GetFields(Index: Integer): TField;
begin
  if (index >= 0) and (index < DataSet.FieldCount) then
  begin
    result := DataSet.Fields[index];
  end;
end;

procedure TPASDataLink.SetDataSetName(const AValue: string);
begin
  if FDataSetName <> AValue then
  begin
    FDataSetName := AValue;
  end;
end;

procedure TPASDataLink.RecordChanged(Field: TField);
begin
  {$ifdef dbgDBScroll} DebugLn(ClassName,'.RecordChanged'); {$endif}
  if Assigned(OnRecordChanged) then
    OnRecordChanged(Field);
end;

procedure TPASDataLink.DataSetChanged;
begin
  {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, FirstRecord=', DbgS(FirstRecord)); {$Endif}
  if Assigned(OnDataSetChanged) then
    OnDataSetChanged(DataSet);

  case DataSet.State of
    dsInactive     : begin
                       if Assigned(OnDataSetInactive) then OnDataSetInactive(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsInactive'); {$Endif}
                     end;

    dsBrowse       : begin
                       if Assigned(OnDataSetBrowse) then OnDataSetBrowse(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsBrowse'); {$Endif}
                     end;

    dsEdit         : begin
                       if Assigned(OnDataSetEdit) then OnDataSetEdit(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsEdit'); {$Endif}
                     end;

    dsInsert       : begin
                       if Assigned(OnDataSetInsert) then OnDataSetInsert(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsInsert'); {$Endif}
                     end;

    dsSetKey       : begin
                       if Assigned(OnDataSetSetKey) then OnDataSetSetKey(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsSetKey'); {$Endif}
                     end;

    dsCalcFields   : begin
                       if Assigned(OnDataSetCalcField) then OnDataSetCalcField(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsCalcFields'); {$Endif}
                     end;

    dsFilter       : begin
                       if Assigned(OnDataSetFilter) then OnDataSetFilter(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsFilter'); {$Endif}
                     end;

    dsNewValue     : begin
                       if Assigned(OnDataSetNewValue) then OnDataSetNewValue(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsNewValue'); {$Endif}
                     end;

    dsOldValue     : begin
                       if Assigned(OnDataSetOldValue) then OnDataSetOldValue(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsOldValue'); {$Endif}
                     end;

    dsCurValue     : begin
                       if Assigned(OnDataSetCurValue) then OnDataSetCurValue(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsCurValue'); {$Endif}
                     end;

    dsBlockRead    : begin
                       if Assigned(OnDataSetBlockRead) then OnDataSetBlockRead(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsBlockRead'); {$Endif}
                     end;

    dsInternalCalc : begin
                       if Assigned(OnDataSetInternalCalc) then OnDataSetInternalCalc(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsInternalCalc'); {$Endif}
                     end;

    dsOpening      : begin
                       if Assigned(OnDataSetOpening) then OnDataSetOpening(DataSet);
                       {$ifdef dbgDBScroll} DebugLn(ClassName,'.DataSetChanged, DataSet.State = dsOpening'); {$Endif}
                     end;
  end;

end;

procedure TPASDataLink.ActiveChanged;
begin
  {$ifdef dbgDBScroll} DebugLnEnter(ClassName, '.ActiveChanged INIT'); {$endif}
  if Active then
  begin
    if Assigned(OnActiveChanged) then OnActiveChanged(DataSet);
    FDataSet := DataSet;
    if DataSetName <> FDataSetName then
    begin
      FDataSetName := DataSetName;
      if Assigned(FOnNewDataSet) then
      begin
        FOnNewDataSet(DataSet);
      end;
    end else
      if Assigned(FOnDataSetOpen) then
      begin
        FOnDataSetOpen(DataSet);
      end;
  end else
  begin
    if Assigned(OnActiveChanged) then OnActiveChanged(DataSet);
    BufferCount := 0;
    if (DataSource = nil) then
    begin
      if Assigned(FOnInvalidDataSource) then
      begin
        FOnInvalidDataSource(FDataSet);
      end;
      FDataSet := nil;
      FDataSetName := '[???]';
    end else
    begin
      if (DataSet=nil) or (csDestroying in DataSet.ComponentState) then
      begin
        if Assigned(FOnInvalidDataSet) then
        begin
          FOnInvalidDataSet(FDataSet);
        end;
        FDataSet := nil;
        FDataSetName := '[???]';
      end else
      begin
        if Assigned(FOnDataSetClose) then
        begin
          FOnDataSetClose(DataSet);
          {$ifdef dbgDBScroll} DebugLn(ClassName, '.ActiveChanged OnDataSetClose Called'); {$endif}
        end;
        if DataSet <> nil then
        begin
          FDataSetName := DataSetName;
        end;
      end;
    end;
  end;
  {$ifdef dbgDBScroll} DebugLnExit(ClassName, '.ActiveChanged DONE'); {$endif}
end;

procedure TPASDataLink.LayoutChanged;
begin
  {$ifdef dbgDBScroll} DebugLnEnter(ClassName, '.LayoutChanged INIT'); {$Endif}
  if Assigned(OnLayoutChanged) then
  begin
    OnLayoutChanged(DataSet);
  end;
  {$ifdef dbgDBScroll} DebugLnExit(ClassName, '.LayoutChanged DONE'); {$Endif}
end;

procedure TPASDataLink.DataSetScrolled(Distance: Integer);
begin
  {$ifdef dbgDBScroll} DebugLn(ClassName, '.DataSetScrolled Distance=',IntToStr(Distance)); {$endif}
  if Assigned(OnDataSetScrolled) then
    OnDataSetScrolled(DataSet, Distance);
end;

procedure TPASDataLink.FocusControl(Field: TFieldRef);
begin
  {$ifdef dbgDBScroll} DebugLn(ClassName, '.FocusControl'); {$endif}
end;

procedure TPASDataLink.CheckBrowseMode;
begin
  {$ifdef dbgDBScroll} DebugLn(ClassName, '.CheckBrowseMode'); {$endif}
  inherited CheckBrowseMode;
end;

procedure TPASDataLink.EditingChanged;
begin
  {$ifdef dbgDBScroll} DebugLn(ClassName, '.EditingChanged'); {$endif}
  if Assigned(OnEditingChanged) then
    OnEditingChanged(DataSet);
end;

procedure TPASDataLink.UpdateData;
begin
  {$ifdef dbgDBScroll} DebugLn(ClassName, '.UpdateData'); {$endif}
  if Assigned(OnUpdatedata) then
    OnUpdateData(DataSet);
end;

function TPASDataLink.MoveBy(Distance: Integer): Integer;
begin
  {$ifdef dbgDBScroll} DebugLnEnter(ClassName, '.MoveBy INIT Distance=', IntToStr(Distance)); {$endif}
  Result := inherited MoveBy(Distance);
  {$ifdef dbgDBScroll} DebugLnExit(ClassName, '.MoveBy DONE'); {$endif}
end;

end.

