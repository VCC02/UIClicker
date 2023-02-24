{
    Copyright (C) 2022 VCC
    creation date: Dec 2019
    initial release date: 13 Sep 2022

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit ClickerWinInterpForm;

{$mode ObjFPC}{$H+}

interface


uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ColorBox, Menus, Buttons, VirtualTrees, IniFiles,
  ClickerUtils, Types;

type
  TColoredHandle = record
    Handle: THandle;
    Color: TColor;
  end;

  PHighlightedCompRec = ^THighlightedCompRec;
  THighlightedCompRec = record
    CompRec: TCompRec;
    LocalX, LocalY: Integer;  //relative to the screenshot image
    AssignedColor: TColor;
  end;


  TLastFindControlGeneratedAction = (lfcNone, lfcClick, lfcCachePos);

  TColoredHandleArr = array of TColoredHandle;

  //TRectArr = array of TRect;
  TExtendDir = (edLeft, edTop, edRight, edBottom);
  TExtendDirs = set of TExtendDir;

  TColorArr = array of TColor;
  THandleArr = array of THandle;

  { TfrmClickerWinInterp }

  TfrmClickerWinInterp = class(TForm)
    btnStartRec: TButton;
    btnStopRec: TButton;
    btnExport: TButton;
    btnSaveTree: TButton;
    btnLoadTree: TButton;
    chkHighlightSelectedComponent: TCheckBox;
    chkMinimizeWhileRecording: TCheckBox;
    colboxHighlightingLabels: TColorBox;
    imgSpinner: TImage;
    imglstSpinner: TImageList;
    imgScannedWindow: TImage;
    imgHandleColors: TImage;
    imgScreenshot: TImage;
    imgAvgScreenshotAndGreenComp: TImage;
    imgAvgScreenshotAndAssignedComp: TImage;
    imgLiveScreenshot: TImage;
    imgSpinnerDiff: TImage;
    lblHighlightingLabels: TLabel;
    lbeStep: TLabeledEdit;
    lblGauge: TLabel;
    memCompInfo: TMemo;
    MenuItemRecordWithMouseSwipe: TMenuItem;
    Separator1: TMenuItem;
    MenuItem_RecordFromRemote: TMenuItem;
    MenuItemCopyFindControlAndCachePositionActionsToClipBoard: TMenuItem;
    MenuItem_SaveSelectedComponentToFile: TMenuItem;
    MenuItem_CopySelectedComponentToClipboard: TMenuItem;
    MenuItem_SaveSelectionToFile: TMenuItem;
    MenuItem_CopySelectionToClipboard: TMenuItem;
    MenuItemCopyFindControlAndClickActionsToClipBoard: TMenuItem;
    MenuItemCopyFindControlActionsToClipBoard: TMenuItem;
    pnlMouseCoordsOnScreenshot: TPanel;
    pnlDrag: TPanel;
    pmComponents: TPopupMenu;
    pmScreenshot: TPopupMenu;
    pmExtraRecording: TPopupMenu;
    prbRecording: TProgressBar;
    rdgrpLayers: TRadioGroup;
    scrboxScannedComponents: TScrollBox;
    spdbtnExtraRecording: TSpeedButton;
    tmrSpinner: TTimer;
    vstComponents: TVirtualStringTree;
    procedure btnExportClick(Sender: TObject);
    procedure btnLoadTreeClick(Sender: TObject);
    procedure btnSaveTreeClick(Sender: TObject);
    procedure btnStartRecClick(Sender: TObject);
    procedure btnStopRecClick(Sender: TObject);
    procedure chkHighlightSelectedComponentChange(Sender: TObject);
    procedure colboxHighlightingLabelsSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgLiveScreenshotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScannedWindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScannedWindowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MenuItemCopyFindControlActionsToClipBoardClick(Sender: TObject);
    procedure MenuItemCopyFindControlAndCachePositionActionsToClipBoardClick(
      Sender: TObject);
    procedure MenuItemCopyFindControlAndClickActionsToClipBoardClick(
      Sender: TObject);
    procedure MenuItemRecordWithMouseSwipeClick(Sender: TObject);
    procedure MenuItem_CopySelectedComponentToClipboardClick(Sender: TObject);
    procedure MenuItem_CopySelectionToClipboardClick(Sender: TObject);
    procedure MenuItem_RecordFromRemoteClick(Sender: TObject);
    procedure MenuItem_SaveSelectedComponentToFileClick(Sender: TObject);
    procedure MenuItem_SaveSelectionToFileClick(Sender: TObject);
    procedure pnlDragMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlDragMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlDragMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgrpLayersClick(Sender: TObject);
    procedure scrboxScannedComponentsMouseWheel(Sender: TObject;
      Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure spdbtnExtraRecordingClick(Sender: TObject);
    procedure tmrSpinnerTimer(Sender: TObject);
    procedure vstComponentsClick(Sender: TObject);
    procedure vstComponentsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstComponentsLoadNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure vstComponentsLoadTree(Sender: TBaseVirtualTree; Stream: TStream);
    procedure vstComponentsSaveNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure vstComponentsSaveTree(Sender: TBaseVirtualTree; Stream: TStream);
  private
    FDragging: Boolean;

    FInterprettedHandle: THandle;     //used by the dragging panel
    FDoneRec: Boolean;
    clrs: array [0..300] of TColor;
    clrs2: array [0..300] of TColor;
    FColoredHandles: TColoredHandleArr;
    FOldSelectedNode: PVirtualNode;

    FSelectedComponentLeftLimitLabel: TLabel;
    FSelectedComponentTopLimitLabel: TLabel;
    FSelectedComponentRightLimitLabel: TLabel;
    FSelectedComponentBottomLimitLabel: TLabel;

    FTransparent_SelectedComponentLeftLimitLabel: TLabel;
    FTransparent_SelectedComponentTopLimitLabel: TLabel;
    FTransparent_SelectedComponentRightLimitLabel: TLabel;
    FTransparent_SelectedComponentBottomLimitLabel: TLabel;

    FSelectionHold: Boolean;
    FMouseDownGlobalPos: TPoint;
    FMouseDownSelPos: TPoint;

    FSelectedComponentText: string;
    FSelectedComponentClassName: string;

    FOnGetConnectionAddress: TOnGetConnectionAddress;
    FOnGetSelectedCompFromRemoteWin: TOnGetSelectedCompFromRemoteWin;

    procedure FTransparent_LeftMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_LeftMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_LeftMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparent_RightMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_RightMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_RightMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparent_TopMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_TopMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_TopMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FTransparent_BottomMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_BottomMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FTransparent_BottomMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetHighlightingLinesColor: TColor;
    procedure SetHighlightingLinesColor(Value: TColor);

    function GetSelectedLayer: TColor;
    procedure SetSelectedLayer(Value: TColor);

    function GetHighlightSelectedComponent: Boolean;
    procedure SetHighlightSelectedComponent(Value: Boolean);

    function DoOnGetConnectionAddress: string;
    function DoOnGetSelectedCompFromRemoteWin: THandle;

    procedure CreateRemainingComponents;
    procedure AdjustHighlightingLabelsToScreenshot;
    procedure HighlightComponent(Node: PVirtualNode);
    procedure CopyFindControlActionsToClipBoard(AIncludeAction: TLastFindControlGeneratedAction);

    procedure GetWindowInfo;
    procedure BuildColors;
    procedure GenerateCompImagesfromTreeContent;
    function GetParentNodeByRectangle(AComp: THighlightedCompRec): PVirtualNode;
    procedure AddTreeComponent(AComp: THighlightedCompRec);
    procedure SelectTreeNodeByHandle(HW: THandle);
    procedure SelectTreeNodeByImgPoint(X, Y: Integer);

    function GetIndexOfColoredHandleByHandle(HW: THandle): Integer;
    function GetIndexOfColoredHandleByColor(AColor: TColor): Integer;
    procedure AddColoredHandle(HW: THandle; AColor: TColor);
    function AddComponentToList(HW: THandle; ALocalX, ALocalY: Integer): TColor;
    function AddSubComponentToList(HW: THandle; ARect: TCompRec; ALocalX, ALocalY: Integer): TColor;
    procedure PrepareLayers(ARect: TRect);
    procedure UpdateLayersVisibility;
    procedure SetHighlightingLabelsColor;
    procedure CropImageFromScreenshot(ComponentNode: PVirtualNode; CroppedBMP: TBitmap; ComponentOnly: Boolean = True);

    procedure GenerateContent_AvgScreenshotAndGreenComp(Node: PVirtualNode);
    procedure GenerateContent_AvgScreenshotAndGenComp;

    procedure RectsToTree(var ADiffRects: TCompRecArr; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr);
    procedure LoadImages(ABasePath: string);
    procedure SaveImages(ABasePath: string);

    property HighlightingLinesColor: TColor read GetHighlightingLinesColor write SetHighlightingLinesColor;
    property SelectedLayer: Integer read GetSelectedLayer write SetSelectedLayer;
    property HighlightSelectedComponent: Boolean read GetHighlightSelectedComponent write SetHighlightSelectedComponent;
  public
    procedure LoadSettings(AIni: TMemIniFile);
    procedure SaveSettings(AIni: TMemIniFile);

    procedure GetTreeContent(AStream: TMemoryStream);
    procedure RecordComponent(AInterprettedHandle: THandle; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr);
    procedure GetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);

    property SelectedComponentText: string read FSelectedComponentText;
    property SelectedComponentClassName: string read FSelectedComponentClassName;

    property OnGetConnectionAddress: TOnGetConnectionAddress read FOnGetConnectionAddress write FOnGetConnectionAddress;
    property OnGetSelectedCompFromRemoteWin: TOnGetSelectedCompFromRemoteWin read FOnGetSelectedCompFromRemoteWin write FOnGetSelectedCompFromRemoteWin;
  end;

var
  frmClickerWinInterp: TfrmClickerWinInterp;

implementation

{$R *.frm}


uses
  BitmapProcessing, ClickerTemplates, Clipbrd, ClickerActionsClient, imgList;


{ TfrmClickerWinInterp }


procedure TfrmClickerWinInterp.LoadSettings(AIni: TMemIniFile);
begin
  Left := AIni.ReadInteger('WinInterpWindow', 'Left', Left);
  Top := AIni.ReadInteger('WinInterpWindow', 'Top', Top);
  Width := AIni.ReadInteger('WinInterpWindow', 'Width', Width);
  Height := AIni.ReadInteger('WinInterpWindow', 'Height', Height);

  HighlightingLinesColor := AIni.ReadInteger('WinInterpWindow', 'HighlightingLinesColor', CLabel_Orange);
  SelectedLayer := AIni.ReadInteger('WinInterpWindow', 'SelectedLayer', 1);
  HighlightSelectedComponent := AIni.ReadBool('WinInterpWindow', 'HighlightSelectedComponent', True);
end;


procedure TfrmClickerWinInterp.SaveSettings(AIni: TMemIniFile);
begin
  AIni.WriteInteger('WinInterpWindow', 'Left', Left);
  AIni.WriteInteger('WinInterpWindow', 'Top', Top);
  AIni.WriteInteger('WinInterpWindow', 'Width', Width);
  AIni.WriteInteger('WinInterpWindow', 'Height', Height);

  AIni.WriteInteger('WinInterpWindow', 'HighlightingLinesColor', frmClickerWinInterp.HighlightingLinesColor);
  AIni.WriteInteger('WinInterpWindow', 'SelectedLayer', frmClickerWinInterp.SelectedLayer);
  AIni.WriteBool('WinInterpWindow', 'HighlightSelectedComponent', frmClickerWinInterp.HighlightSelectedComponent);
end;


function TfrmClickerWinInterp.GetHighlightingLinesColor: TColor;
begin
  Result := colboxHighlightingLabels.Selected;
end;


procedure TfrmClickerWinInterp.SetHighlightingLinesColor(Value: TColor);
begin
  colboxHighlightingLabels.Selected := Value;
  SetHighlightingLabelsColor;
end;


function TfrmClickerWinInterp.GetSelectedLayer: TColor;
begin
  Result := rdgrpLayers.ItemIndex;
end;


procedure TfrmClickerWinInterp.SetSelectedLayer(Value: TColor);
begin
  rdgrpLayers.ItemIndex := Value;
end;


function TfrmClickerWinInterp.GetHighlightSelectedComponent: Boolean;
begin
  Result := chkHighlightSelectedComponent.Checked;
end;


procedure TfrmClickerWinInterp.SetHighlightSelectedComponent(Value: Boolean);
begin
  chkHighlightSelectedComponent.Checked := Value;
end;


function TfrmClickerWinInterp.DoOnGetConnectionAddress: string;
begin
  if not Assigned(FOnGetConnectionAddress) then
  begin
    Result := '';
    Exit;
  end;

  Result := FOnGetConnectionAddress();
end;


function TfrmClickerWinInterp.DoOnGetSelectedCompFromRemoteWin: THandle;
begin
  if not Assigned(FOnGetSelectedCompFromRemoteWin) then
  begin
    Result := 0;
    Exit;
  end;

  Result := FOnGetSelectedCompFromRemoteWin();
end;


procedure TfrmClickerWinInterp.BuildColors;
var
  r, g, b: Byte;
  step: array [0..110] of TColor;
  i: Integer;
begin {Build the colors}
  for i := 1 to 110 do
    step[i] := 0;

  for i := 0 to 51 do
  begin
    step[i shl 1] := 229 - i shl 2;
    step[i shl 1 + 1] := 27 + i shl 2;
  end;

  i := 0;

  r := 0; //0
  g := 0; //34
  b := 0; //67
  repeat
    inc(i);

    if (i >= 1) and (i <= 100) then
    begin
      r := i mod 100;
      g := (i + 35) mod 100;
      b := (i + 67) mod 100;
    end;

    if (i >= 101) and (i <= 200) then
    begin
      g := i mod 100;
      r := (i + 35) mod 100;
      b := (i + 67) mod 100;
    end;

    if (i >= 201) and (i <= 300) then
    begin
      b := i mod 100;
      g := (i + 35) mod 100;
      r := (i + 67) mod 100;
    end;

    clrs[i] := step[b] shl 16 + step[g] shl 8 + step[r];
  until i = 300;

  for i := 1 to 300 do
    clrs2[i] := clrs[(i * 9) mod 299];
end;


procedure TfrmClickerWinInterp.vstComponentsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  NodeData: PHighlightedCompRec;
begin
  NodeData := vstComponents.GetNodeData(Node);
  if NodeData = nil then
  begin
    CellText := 'Data bug';
    Exit;
  end;

  case Column of
    0: CellText := IntToStr(NodeData^.CompRec.Handle);
    1: CellText := NodeData^.CompRec.ClassName;
    2: CellText := NodeData^.CompRec.Text;
    3: CellText := '0x' + IntToHex(NodeData^.AssignedColor, 6);
    4: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Left);
    5: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Top);
    6: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Right);
    7: CellText := IntToStr(NodeData^.CompRec.ComponentRectangle.Bottom);
    8: CellText := IntToStr(NodeData^.LocalX);
    9: CellText := IntToStr(NodeData^.LocalY);
  end;
end;


procedure TfrmClickerWinInterp.vstComponentsLoadNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  NodeData: PHighlightedCompRec;
  DataStr: string;
  Len: LongInt;
begin
  NodeData := vstComponents.GetNodeData(Node);

  Stream.Read(NodeData^.CompRec.Handle, 4);

  Stream.Read(Len, 4);
  SetLength(DataStr, Len);
  Stream.Read(DataStr[1], Len);
  NodeData^.CompRec.Text := DataStr;

  Stream.Read(Len, 4);
  SetLength(DataStr, Len);
  Stream.Read(DataStr[1], Len);
  NodeData^.CompRec.ClassName := DataStr;

  Stream.Read(NodeData^.CompRec.ComponentRectangle.Left, 4);
  Stream.Read(NodeData^.CompRec.ComponentRectangle.Top, 4);
  Stream.Read(NodeData^.CompRec.ComponentRectangle.Right, 4);
  Stream.Read(NodeData^.CompRec.ComponentRectangle.Bottom, 4);

  Stream.Read(NodeData^.LocalX, 4);
  Stream.Read(NodeData^.LocalY, 4);
  Stream.Read(NodeData^.AssignedColor, 4);
end;


procedure TfrmClickerWinInterp.vstComponentsLoadTree(Sender: TBaseVirtualTree;
  Stream: TStream);
begin
  //
end;

procedure TfrmClickerWinInterp.vstComponentsSaveNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  NodeData: PHighlightedCompRec;
  DataStr: string;
  Len: LongInt;
begin
  NodeData := vstComponents.GetNodeData(Node);

  Stream.Write(NodeData^.CompRec.Handle, 4);

  DataStr := NodeData^.CompRec.Text;
  Len := Length(DataStr);
  Stream.Write(Len, 4);
  Stream.Write(DataStr[1], Len);

  DataStr := NodeData^.CompRec.ClassName;
  Len := Length(DataStr);
  Stream.Write(Len, 4);
  Stream.Write(DataStr[1], Len);

  Stream.Write(NodeData^.CompRec.ComponentRectangle.Left, 4);
  Stream.Write(NodeData^.CompRec.ComponentRectangle.Top, 4);
  Stream.Write(NodeData^.CompRec.ComponentRectangle.Right, 4);
  Stream.Write(NodeData^.CompRec.ComponentRectangle.Bottom, 4);

  Stream.Write(NodeData^.LocalX, 4);
  Stream.Write(NodeData^.LocalY, 4);
  Stream.Write(NodeData^.AssignedColor, 4);
end;


procedure TfrmClickerWinInterp.vstComponentsSaveTree(Sender: TBaseVirtualTree;
  Stream: TStream);
begin
  //MessageBox(Handle, 'TreeSave', PChar(Application.Title), MB_ICONERROR);
end;


procedure TfrmClickerWinInterp.pnlDragMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then
    Exit;

  FDragging := True;

  imgLiveScreenshot.Canvas.Pen.Color := clWhite;
  imgLiveScreenshot.Canvas.Brush.Color := clWhite;
  imgLiveScreenshot.Canvas.Rectangle(0, 0, imgLiveScreenshot.Width - 1, imgLiveScreenshot.Height - 1);
  imgLiveScreenshot.Show;
end;


procedure TfrmClickerWinInterp.pnlDragMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  Comp: TCompRec;
begin
  if FDragging then
  begin
    GetCursorPos(tp);
    Comp := GetWindowClassRec(tp);

    memCompInfo.Clear;
    memCompInfo.Lines.Add('Handle=' + IntToStr(Comp.Handle));
    memCompInfo.Lines.Add('Class=' + Comp.ClassName);
    memCompInfo.Lines.Add('Text=' + Comp.Text);

    if pnlDrag.Color <> clLime then
      pnlDrag.Color := clLime;

    imgLiveScreenshot.Width := Comp.ComponentRectangle.Width;
    imgLiveScreenshot.Height := Comp.ComponentRectangle.Height;
    imgLiveScreenshot.Picture.Bitmap.Width := imgLiveScreenshot.Width;
    imgLiveScreenshot.Picture.Bitmap.Height := imgLiveScreenshot.Height;

    scrboxScannedComponents.HorzScrollBar.Position := 0;
    scrboxScannedComponents.VertScrollBar.Position := 0;
    ScreenShot(Comp.Handle, imgLiveScreenshot.Picture.Bitmap, 0, 0, Comp.ComponentRectangle.Width, Comp.ComponentRectangle.Height);
  end;
end;


procedure TfrmClickerWinInterp.pnlDragMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  Comp: TCompRec;
begin
  if FDragging then
  begin
    FDragging := False;

    GetCursorPos(tp);
    Comp := GetWindowClassRec(tp);
    FInterprettedHandle := Comp.Handle;
    GetWindowInfo;
    pnlDrag.Color := clYellow;

    if vstComponents.RootNodeCount > 0 then
      imgLiveScreenshot.Hide;  //hidden when recordong
  end;
end;


procedure TfrmClickerWinInterp.CreateRemainingComponents;
begin
  FSelectedComponentLeftLimitLabel := TLabel.Create(Self);
  FSelectedComponentTopLimitLabel := TLabel.Create(Self);
  FSelectedComponentRightLimitLabel := TLabel.Create(Self);
  FSelectedComponentBottomLimitLabel := TLabel.Create(Self);

  FSelectedComponentLeftLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentTopLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentRightLimitLabel.Parent := scrboxScannedComponents;
  FSelectedComponentBottomLimitLabel.Parent := scrboxScannedComponents;

  FSelectedComponentLeftLimitLabel.AutoSize := False;
  FSelectedComponentTopLimitLabel.AutoSize := False;
  FSelectedComponentRightLimitLabel.AutoSize := False;
  FSelectedComponentBottomLimitLabel.AutoSize := False;

  FSelectedComponentLeftLimitLabel.Caption := '';
  FSelectedComponentTopLimitLabel.Caption := '';
  FSelectedComponentRightLimitLabel.Caption := '';
  FSelectedComponentBottomLimitLabel.Caption := '';

  FSelectedComponentLeftLimitLabel.Color := CLabel_Orange;
  FSelectedComponentTopLimitLabel.Color := CLabel_Orange;
  FSelectedComponentRightLimitLabel.Color := CLabel_Orange;
  FSelectedComponentBottomLimitLabel.Color := CLabel_Orange;

  FSelectedComponentLeftLimitLabel.Width := 1;
  FSelectedComponentTopLimitLabel.Height := 1;
  FSelectedComponentRightLimitLabel.Width := 1;
  FSelectedComponentBottomLimitLabel.Height := 1;

  FSelectedComponentLeftLimitLabel.Transparent := False;
  FSelectedComponentTopLimitLabel.Transparent := False;
  FSelectedComponentRightLimitLabel.Transparent := False;
  FSelectedComponentBottomLimitLabel.Transparent := False;


  FTransparent_SelectedComponentLeftLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedComponentTopLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedComponentRightLimitLabel := TLabel.Create(Self);
  FTransparent_SelectedComponentBottomLimitLabel := TLabel.Create(Self);

  FTransparent_SelectedComponentLeftLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedComponentTopLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedComponentRightLimitLabel.Parent := scrboxScannedComponents;
  FTransparent_SelectedComponentBottomLimitLabel.Parent := scrboxScannedComponents;

  FTransparent_SelectedComponentLeftLimitLabel.AutoSize := False;
  FTransparent_SelectedComponentTopLimitLabel.AutoSize := False;
  FTransparent_SelectedComponentRightLimitLabel.AutoSize := False;
  FTransparent_SelectedComponentBottomLimitLabel.AutoSize := False;

  FTransparent_SelectedComponentLeftLimitLabel.Caption := '';
  FTransparent_SelectedComponentTopLimitLabel.Caption := '';
  FTransparent_SelectedComponentRightLimitLabel.Caption := '';
  FTransparent_SelectedComponentBottomLimitLabel.Caption := '';

  FTransparent_SelectedComponentLeftLimitLabel.Color := clDefault;
  FTransparent_SelectedComponentTopLimitLabel.Color := clDefault;
  FTransparent_SelectedComponentRightLimitLabel.Color := clDefault;
  FTransparent_SelectedComponentBottomLimitLabel.Color := clDefault;

  FTransparent_SelectedComponentLeftLimitLabel.Width := 7;
  FTransparent_SelectedComponentTopLimitLabel.Height := 7;
  FTransparent_SelectedComponentRightLimitLabel.Width := 7;
  FTransparent_SelectedComponentBottomLimitLabel.Height := 7;

  FTransparent_SelectedComponentLeftLimitLabel.Transparent := True;
  FTransparent_SelectedComponentTopLimitLabel.Transparent := True;
  FTransparent_SelectedComponentRightLimitLabel.Transparent := True;
  FTransparent_SelectedComponentBottomLimitLabel.Transparent := True;

  FTransparent_SelectedComponentLeftLimitLabel.Cursor := crSizeWE;
  FTransparent_SelectedComponentTopLimitLabel.Cursor := crSizeNS;
  FTransparent_SelectedComponentRightLimitLabel.Cursor := crSizeWE;
  FTransparent_SelectedComponentBottomLimitLabel.Cursor := crSizeNS;

  FTransparent_SelectedComponentLeftLimitLabel.OnMouseDown := @FTransparent_LeftMouseDown;
  FTransparent_SelectedComponentLeftLimitLabel.OnMouseMove := @FTransparent_LeftMouseMove;
  FTransparent_SelectedComponentLeftLimitLabel.OnMouseUp := @FTransparent_LeftMouseUp;

  FTransparent_SelectedComponentRightLimitLabel.OnMouseDown := @FTransparent_RightMouseDown;
  FTransparent_SelectedComponentRightLimitLabel.OnMouseMove := @FTransparent_RightMouseMove;
  FTransparent_SelectedComponentRightLimitLabel.OnMouseUp := @FTransparent_RightMouseUp;

  FTransparent_SelectedComponentTopLimitLabel.OnMouseDown := @FTransparent_TopMouseDown;
  FTransparent_SelectedComponentTopLimitLabel.OnMouseMove := @FTransparent_TopMouseMove;
  FTransparent_SelectedComponentTopLimitLabel.OnMouseUp := @FTransparent_TopMouseUp;

  FTransparent_SelectedComponentBottomLimitLabel.OnMouseDown := @FTransparent_BottomMouseDown;
  FTransparent_SelectedComponentBottomLimitLabel.OnMouseMove := @FTransparent_BottomMouseMove;
  FTransparent_SelectedComponentBottomLimitLabel.OnMouseUp := @FTransparent_BottomMouseUp;
end;


procedure TfrmClickerWinInterp.FormCreate(Sender: TObject);
begin
  FDragging := False;
  FDoneRec := False;
  BuildColors;
  vstComponents.NodeDataSize := SizeOf(THighlightedCompRec);
  FOldSelectedNode := nil;

  FInterprettedHandle := 0;
  FOnGetConnectionAddress := nil;
  FOnGetSelectedCompFromRemoteWin := nil;

  CreateRemainingComponents;
  colboxHighlightingLabels.AddItem('clOrange', TObject({%H-}Pointer(CLabel_Orange)));
  colboxHighlightingLabels.Selected := CLabel_Orange;

  imgLiveScreenshot.Left := 0;
  imgLiveScreenshot.Top := 0;
  FSelectionHold := False;

  FSelectedComponentText := 'no selected component';
  FSelectedComponentClassName := 'no selected component';

  imgScannedWindow.Left := 0;
  imgScannedWindow.Top := 0;
  imgScreenshot.Left := 0;
  imgScreenshot.Top := 0;
  imgAvgScreenshotAndGreenComp.Left := 0;
  imgAvgScreenshotAndGreenComp.Top := 0;
  imgAvgScreenshotAndAssignedComp.Left := 0;
  imgAvgScreenshotAndAssignedComp.Top := 0;
  imgLiveScreenshot.Left := 0;
  imgLiveScreenshot.Top := 0;
  imgHandleColors.Left := 0;
  imgHandleColors.Top := 0;

  //some default values
  imgScannedWindow.Width := 1920;
  imgScannedWindow.Height := 1080;
  imgScreenshot.Width := 1920;
  imgScreenshot.Height := 1080;
  imgAvgScreenshotAndGreenComp.Width := 1920;
  imgAvgScreenshotAndGreenComp.Height := 1080;
  imgAvgScreenshotAndAssignedComp.Width := 1920;
  imgAvgScreenshotAndAssignedComp.Height := 1080;
  imgLiveScreenshot.Width := 1920;
  imgLiveScreenshot.Height := 1080;
  imgHandleColors.Width := 1920;
  imgHandleColors.Height := 1080;
end;


procedure TfrmClickerWinInterp.imgLiveScreenshotMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if vstComponents.RootNodeCount > 0 then
    imgLiveScreenshot.Hide;
end;


procedure TfrmClickerWinInterp.AdjustHighlightingLabelsToScreenshot;
begin
  FSelectedComponentLeftLimitLabel.Top := 0;
  FSelectedComponentLeftLimitLabel.Height := imgScannedWindow.Height;

  FSelectedComponentTopLimitLabel.Left := 0;
  FSelectedComponentTopLimitLabel.Width := imgScannedWindow.Width;

  FSelectedComponentRightLimitLabel.Top := 0;
  FSelectedComponentRightLimitLabel.Height := imgScannedWindow.Height;

  FSelectedComponentBottomLimitLabel.Left := 0;
  FSelectedComponentBottomLimitLabel.Width := imgScannedWindow.Width;

  FTransparent_SelectedComponentLeftLimitLabel.Top := 0;
  FTransparent_SelectedComponentLeftLimitLabel.Height := imgScannedWindow.Height;

  FTransparent_SelectedComponentTopLimitLabel.Left := 0;
  FTransparent_SelectedComponentTopLimitLabel.Width := imgScannedWindow.Width;

  FTransparent_SelectedComponentRightLimitLabel.Top := 0;
  FTransparent_SelectedComponentRightLimitLabel.Height := imgScannedWindow.Height;

  FTransparent_SelectedComponentBottomLimitLabel.Left := 0;
  FTransparent_SelectedComponentBottomLimitLabel.Width := imgScannedWindow.Width;
end;


procedure TfrmClickerWinInterp.HighlightComponent(Node: PVirtualNode);
var
  NodeData: PHighlightedCompRec;
begin
  if Node = nil then
    Exit;

  NodeData := vstComponents.GetNodeData(Node);

  FSelectedComponentLeftLimitLabel.Left := NodeData^.LocalX;
  FSelectedComponentTopLimitLabel.Top := NodeData^.LocalY;
  FSelectedComponentRightLimitLabel.Left := NodeData^.LocalX + NodeData^.CompRec.ComponentRectangle.Width;
  FSelectedComponentBottomLimitLabel.Top := NodeData^.LocalY + NodeData^.CompRec.ComponentRectangle.Height;

  FTransparent_SelectedComponentLeftLimitLabel.Left := FSelectedComponentLeftLimitLabel.Left - 3;
  FTransparent_SelectedComponentTopLimitLabel.Top := FSelectedComponentTopLimitLabel.Top - 3;
  FTransparent_SelectedComponentRightLimitLabel.Left := FSelectedComponentRightLimitLabel.Left - 3;
  FTransparent_SelectedComponentBottomLimitLabel.Top := FSelectedComponentBottomLimitLabel.Top - 3;

  FSelectedComponentText := NodeData^.CompRec.Text;
  FSelectedComponentClassName := NodeData^.CompRec.ClassName;
end;


procedure TfrmClickerWinInterp.SelectTreeNodeByHandle(HW: THandle);
var
  Node: PVirtualNode;
  NodeData: PHighlightedCompRec;
begin
  Node := vstComponents.GetFirst;

  if Node = nil then
    Exit;

  repeat
    NodeData := vstComponents.GetNodeData(Node);

    if NodeData^.CompRec.Handle = HW then
    begin
      vstComponents.Selected[Node] := True;
      vstComponents.FocusedNode := Node;
      vstComponents.ScrollIntoView(Node, True);
      Exit;
    end;

    Node := vstComponents.GetNext(Node);
  until Node = nil;
end;


procedure TfrmClickerWinInterp.SelectTreeNodeByImgPoint(X, Y: Integer);
var
  HandleIndex: Integer;
begin
  if Length(FColoredHandles) = 0 then
    Exit;

  HandleIndex := GetIndexOfColoredHandleByHandle(imgHandleColors.Canvas.Pixels[X, Y]);

  if HandleIndex > -1 then
    SelectTreeNodeByHandle(FColoredHandles[HandleIndex].Handle);
end;


procedure TfrmClickerWinInterp.imgScannedWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  if ssLeft in Shift then
  begin
    SelectTreeNodeByImgPoint(X, Y);

    Node := vstComponents.GetFirstSelected;
    GenerateContent_AvgScreenshotAndGreenComp(Node);
    HighlightComponent(Node);
  end;
end;


procedure TfrmClickerWinInterp.imgScannedWindowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  pnlMouseCoordsOnScreenshot.Caption := IntToStr(X) + ' : ' + IntToStr(Y);

  if ssLeft in Shift then
  begin
    SelectTreeNodeByImgPoint(X, Y);

    if FOldSelectedNode <> vstComponents.GetFirstSelected then
    begin
      FOldSelectedNode := vstComponents.GetFirstSelected;
      GenerateContent_AvgScreenshotAndGreenComp(FOldSelectedNode);
      HighlightComponent(FOldSelectedNode);
    end;
  end;
end;


procedure SetActionToFindControlByComponentData(ACompData: PHighlightedCompRec; ASearchWholeScreen: Boolean; var AAction: TClkActionRec);
begin
  AAction.ActionOptions.Action := acFindControl;
  AAction.ActionOptions.ActionEnabled := True;
  AAction.ActionOptions.ActionTimeout := 1000;
  AAction.ActionOptions.ActionCondition := '';
  AAction.ActionOptions.ActionName := 'Find ' + ACompData^.CompRec.Text + ' (' + ACompData^.CompRec.ClassName + ')';

  AAction.FindControlOptions.AllowToFail := False;
  AAction.FindControlOptions.MatchCriteria.SearchForControlMode := sfcmGenGrid;
  AAction.FindControlOptions.MatchCriteria.WillMatchClassName := True;
  AAction.FindControlOptions.MatchCriteria.WillMatchText := True;
  AAction.FindControlOptions.MatchCriteria.WillMatchBitmapText := False;
  AAction.FindControlOptions.MatchCriteria.WillMatchBitmapFiles := False;

  AAction.FindControlOptions.MatchText := ACompData^.CompRec.Text;
  AAction.FindControlOptions.MatchClassName := ACompData^.CompRec.ClassName;
  AAction.FindControlOptions.MatchTextSeparator := '';
  AAction.FindControlOptions.MatchClassNameSeparator := '';

  AAction.FindControlOptions.UseWholeScreen := ASearchWholeScreen;
  AAction.FindControlOptions.InitialRectangle.Left := '$Control_Left$';
  AAction.FindControlOptions.InitialRectangle.Top := '$Control_Top$';
  AAction.FindControlOptions.InitialRectangle.Right := '$Control_Right$';
  AAction.FindControlOptions.InitialRectangle.Bottom := '$Control_Bottom$';

  if ASearchWholeScreen then
  begin
    AAction.FindControlOptions.InitialRectangle.LeftOffset := '0';
    AAction.FindControlOptions.InitialRectangle.TopOffset := '0';
    AAction.FindControlOptions.InitialRectangle.RightOffset := '0';
    AAction.FindControlOptions.InitialRectangle.BottomOffset := '0';
  end
  else
  begin
    AAction.FindControlOptions.InitialRectangle.LeftOffset := '-10';
    AAction.FindControlOptions.InitialRectangle.TopOffset := '-10';
    AAction.FindControlOptions.InitialRectangle.RightOffset := '10';
    AAction.FindControlOptions.InitialRectangle.BottomOffset := '10';
  end;

  AAction.FindControlOptions.ColorError := '0';
  AAction.FindControlOptions.AllowedColorErrorCount := '0';
  AAction.FindControlOptions.WaitForControlToGoAway := False;
end;


procedure SetActionToClick(ACompData: PHighlightedCompRec; var AAction: TClkActionRec);
begin
  AAction.ActionOptions.Action := acClick;
  AAction.ActionOptions.ActionEnabled := True;
  AAction.ActionOptions.ActionTimeout := 0;
  AAction.ActionOptions.ActionCondition := '';
  AAction.ActionOptions.ActionName := 'Click on ' + ACompData^.CompRec.Text + ' (' + ACompData^.CompRec.ClassName + ')';

  AAction.ClickOptions.XClickPointReference := xrefLeft;
  AAction.ClickOptions.YClickPointReference := yrefTop;
  AAction.ClickOptions.XClickPointVar := '$Control_Left$';
  AAction.ClickOptions.YClickPointVar := '$Control_Top$';
  AAction.ClickOptions.XOffset := '3';
  AAction.ClickOptions.YOffset := '3';
  AAction.ClickOptions.MouseButton := mbLeft;
  AAction.ClickOptions.ClickWithCtrl := False;
  AAction.ClickOptions.ClickWithAlt := False;
  AAction.ClickOptions.ClickWithShift := False;
  AAction.ClickOptions.ClickWithDoubleClick := False;
  AAction.ClickOptions.Count := 1;
  AAction.ClickOptions.LeaveMouse := False;
  AAction.ClickOptions.MoveWithoutClick := False;
  AAction.ClickOptions.ClickType := CClickType_Click;
  AAction.ClickOptions.XClickPointReferenceDest := xrefLeft;
  AAction.ClickOptions.YClickPointReferenceDest := yrefTop;
  AAction.ClickOptions.XClickPointVarDest := '$Control_Left$';;
  AAction.ClickOptions.YClickPointVarDest := '$Control_Top$';
  AAction.ClickOptions.XOffsetDest := '0';
  AAction.ClickOptions.YOffsetDest := '0';
end;


procedure SetActionToCachePosition(ACompData: PHighlightedCompRec; var AAction: TClkActionRec);
begin
  AAction.ActionOptions.Action := acSetVar;
  AAction.ActionOptions.ActionEnabled := True;
  AAction.ActionOptions.ActionTimeout := 0;
  AAction.ActionOptions.ActionCondition := '';
  AAction.ActionOptions.ActionName := 'Cache pos ' + ACompData^.CompRec.Text + ' (' + ACompData^.CompRec.ClassName + ')';

  AAction.SetVarOptions.ListOfVarNames := '$MyComp_Left$' + #13#10 + '$MyComp_Top$' + #13#10 + '$MyComp_Right$' + #13#10 + '$MyComp_Bottom$' + #13#10;
  AAction.SetVarOptions.ListOfVarValues := '$Control_Left$' + #13#10 + '$Control_Top$' + #13#10 + '$Control_Right$' + #13#10 + '$Control_Bottom$' + #13#10;
  AAction.SetVarOptions.ListOfVarEvalBefore := '1' + #13#10 + '1' + #13#10 + '1' + #13#10 + '1' + #13#10;
end;


procedure TfrmClickerWinInterp.CopyFindControlActionsToClipBoard(AIncludeAction: TLastFindControlGeneratedAction);
var
  Node, OldNode: PVirtualNode;
  ComponentChain: array of PHighlightedCompRec;
  ActionChain: TClkActionsRecArr;
  i, n: Integer;
  GeneratedActions: TStringList;
  FirstAction: Boolean;
begin
  Node := vstComponents.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select a component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  vstComponents.RootNode^.Parent := nil;
  SetLength(ComponentChain, 0);
  repeat
    n := Length(ComponentChain);
    SetLength(ComponentChain, n + 1);
    ComponentChain[n] := vstComponents.GetNodeData(Node);

    OldNode := Node;
    Node := Node^.Parent;
  until (Node = nil) or (vstComponents.GetNodeLevel(OldNode) = 0);

  SetLength(ActionChain, Length(ComponentChain));

  for i := 0 to Length(ComponentChain) - 1 do
  begin
    FirstAction := i = Length(ComponentChain) - 1;
    SetActionToFindControlByComponentData(ComponentChain[i], FirstAction, ActionChain[Length(ComponentChain) - i - 1]);
  end;

  case AIncludeAction of
    lfcNone:
    begin
    end;

    lfcClick:
    begin
      SetLength(ActionChain, Length(ActionChain) + 1);
      SetActionToClick(ComponentChain[0], ActionChain[Length(ActionChain) - 1]);
    end;

    lfcCachePos:
    begin
      SetLength(ActionChain, Length(ActionChain) + 1);
      SetActionToCachePosition(ComponentChain[0], ActionChain[Length(ActionChain) - 1]);
    end;
  end;

  GeneratedActions := TStringList.Create;
  try
    SaveTemplateWithCustomActionsToStringList_V2(GeneratedActions, ActionChain, '');
    Clipboard.AsText := GeneratedActions.Text;
  finally
    GeneratedActions.Free;
  end;
end;


procedure TfrmClickerWinInterp.MenuItemCopyFindControlActionsToClipBoardClick(
  Sender: TObject);
begin
  CopyFindControlActionsToClipBoard(lfcNone);
end;


procedure TfrmClickerWinInterp.MenuItemCopyFindControlAndClickActionsToClipBoardClick
  (Sender: TObject);
begin
  CopyFindControlActionsToClipBoard(lfcClick);
end;


//Returns "found" if the searchpoint is 1px outside a rectangle
function GetIndexOfRect(var ADiffRects: TCompRecArr; ASearchPointX, ASearchPointY: Integer; out ExtDirs: TExtendDirs): Integer;
var
  i: Integer;
begin
  Result := -1;
  ExtDirs := [];

  for i := 0 to Length(ADiffRects) - 1 do
    if (ASearchPointX >= ADiffRects[i].ComponentRectangle.Left - 1) and (ASearchPointX <= ADiffRects[i].ComponentRectangle.Right + 1) and
       (ASearchPointY >= ADiffRects[i].ComponentRectangle.Top - 1) and (ASearchPointY <= ADiffRects[i].ComponentRectangle.Bottom + 1) then
    begin
      Result := i;

      if ASearchPointX = ADiffRects[i].ComponentRectangle.Left - 1 then
        ExtDirs := ExtDirs + [edLeft];

      if ASearchPointX = ADiffRects[i].ComponentRectangle.Right + 1 then
        ExtDirs := ExtDirs + [edRight];

      if ASearchPointY = ADiffRects[i].ComponentRectangle.Top - 1 then
        ExtDirs := ExtDirs + [edTop];

      if ASearchPointY = ADiffRects[i].ComponentRectangle.Bottom + 1 then
        ExtDirs := ExtDirs + [edBottom];

      Exit;
    end;
end;


procedure TfrmClickerWinInterp.RectsToTree(var ADiffRects: TCompRecArr; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr);
var
  AllocatedColor: TColor;
  i: Integer;
  YLine: Integer;
begin
  for i := 0 to Length(ADiffRects) - 1 do
  begin
    ADiffRects[i].Handle := i + 200;
    AllocatedColor := AddSubComponentToList(ADiffRects[i].Handle, ADiffRects[i], ADiffRects[i].MouseXOffset, ADiffRects[i].MouseYOffset);

    YLine := ADiffRects[i].MouseYOffset * ADiffRects[i].ComponentRectangle.Width;

    ImgMatrix[YLine + ADiffRects[i].MouseXOffset] := AllocatedColor;
    ImgHWMatrix[YLine + ADiffRects[i].MouseXOffset] := ADiffRects[i].Handle;
  end;
end;


procedure TfrmClickerWinInterp.MenuItemRecordWithMouseSwipeClick(Sender: TObject);
var
  InitBmp, CurrentBmp: TBitmap;
  rct: TRect;
  x, y, w, h: Integer;
  CurrentX, CurrentY, YLine, Step: Integer;
  DiffRects: TCompRecArr;
  IndexOfRect, n: Integer;
  ExtDirs: TExtendDirs;
  AppTitle: string;
  ImgMatrix: array of TColor;
  ImgHWMatrix: array of THandle;
  tk, Duration: QWord;
  EstimatedDuration: Double;
begin
  if FInterprettedHandle = 0 then
  begin
    pnlDrag.Color := clRed;
    MessageBox(Handle, 'Please set the target control (or window) before recording it.', PChar(Caption), MB_ICONINFORMATION);
    pnlDrag.Color := clYellow;
    Exit;
  end;

  FDoneRec := False;
  if GetWindowRect(FInterprettedHandle, rct) = False then
    Exit;

  EstimatedDuration := (rct.Width * rct.Height * 4.11) / 60000;   //2.6 for a small window   - also depends on CPU and GPU speed
  if MessageBox(Handle,
                PChar('Recording with mouse swipe, will take a very long time, while keeping busy at least a CPU core. The scanning can be stopped with the Esc key.' + #13#10 +
                      'Control size: ' + IntToStr(rct.Width) + ' x ' + IntToStr(rct.Height) + #13#10 +
                      'Estimated recording time: ' + FloatToStr(EstimatedDuration) + ' min' + #13#10 +
                      'Continue?'),
                PChar(Caption),
                MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  SetLength(ImgMatrix, 0);
  SetLength(ImgHWMatrix, 0);

  RecordComponent(FInterprettedHandle, ImgMatrix, ImgHWMatrix);

  if Length(ImgMatrix) = 0 then
    Exit;

  if FDoneRec then
    Exit;

  memCompInfo.Lines.Add('Estimated duration: ' + FloatToStr(EstimatedDuration) + ' min.');
  tk := GetTickCount64;

  w := rct.Width; //rct.Width and rct.Height are functions, so better use some local vars
  h := rct.Height;

  memCompInfo.Lines.Add('Width: ' + IntToStr(w));
  memCompInfo.Lines.Add('Height: ' + IntToStr(h));

  prbRecording.Max := w * h;

  if prbRecording.Max = 0 then
    prbRecording.Max := 1;

  if chkMinimizeWhileRecording.Checked then
    if FInterprettedHandle <> Handle then
    begin
      //WindowState := wsMinimized;
      Application.Minimize;
    end;

  imgScannedWindow.Canvas.Lock;
  imgHandleColors.Canvas.Lock;
  vstComponents.BeginUpdate;
  btnStartRec.Enabled := False;
  spdbtnExtraRecording.Enabled := False;
  AppTitle := Application.Title;
  try
    SetCursorPos(Screen.Width, Screen.Height);
    InitBmp := TBitmap.Create;
    CurrentBmp := TBitmap.Create;
    try
      ScreenShot(FInterprettedHandle, InitBmp, 0, 0, w, h);

      Step := 1; //StrToIntDef(lbeStep.Text, 1);
      y := 0;
      repeat
        Inc(y, Step);

        x := 0;
        repeat
          Inc(x, Step);

          CurrentX := x + rct.Left;
          CurrentY := y + rct.Top;

          SetCursorPos(CurrentX, CurrentY);
          ScreenShot(FInterprettedHandle, CurrentBmp, 0, 0, w, h);

          if not BitmapsAreEqual(InitBmp, CurrentBmp, w, h) then
          begin
            imgSpinnerDiff.Visible := True;

            IndexOfRect := GetIndexOfRect(DiffRects, CurrentX, CurrentY, ExtDirs);
            if IndexOfRect = -1 then
            begin
              n := Length(DiffRects);
              SetLength(DiffRects, n + 1);

              DiffRects[n].ComponentRectangle.Left := CurrentX;
              DiffRects[n].ComponentRectangle.Top := CurrentY;
              DiffRects[n].ComponentRectangle.Width := 1;
              DiffRects[n].ComponentRectangle.Height := 1;

              DiffRects[n].MouseXOffset := x;
              DiffRects[n].MouseYOffset := y;
            end
            else
            begin  //extend the current rectangle
              if edLeft in ExtDirs then
                DiffRects[IndexOfRect].ComponentRectangle.Left := DiffRects[IndexOfRect].ComponentRectangle.Left - 1;

              if edTop in ExtDirs then
                DiffRects[IndexOfRect].ComponentRectangle.Top := DiffRects[IndexOfRect].ComponentRectangle.Top - 1;

              if edRight in ExtDirs then
                DiffRects[IndexOfRect].ComponentRectangle.Right := DiffRects[IndexOfRect].ComponentRectangle.Right + 1;

              if edBottom in ExtDirs then
                DiffRects[IndexOfRect].ComponentRectangle.Bottom := DiffRects[IndexOfRect].ComponentRectangle.Bottom + 1;
            end;
          end
          else
            imgSpinnerDiff.Visible := False;

          Application.ProcessMessages;
          if GetAsyncKeyState(VK_ESCAPE) < 0 then
            Exit;

          if x and $FF = $FF then
          begin
            prbRecording.Position := YLine + x;
            lblGauge.Caption := IntToStr(prbRecording.Position * 100 div Length(ImgMatrix)) + ' %';
            Application.Title := lblGauge.Caption;
          end;

          if FDoneRec then
            Break;
        until x >= w - 1;

      until y >= h - 1;
    finally
      InitBmp.Free;
      CurrentBmp.Free;
    end;

    RectsToTree(DiffRects, ImgMatrix, ImgHWMatrix);
    memCompInfo.Lines.Add('Found ' + IntToStr(Length(DiffRects)) + ' subcontrol(s).');
  finally
    Application.Title := AppTitle;

    for y := 0 to h - 1 do
    begin
      YLine := y * w;

      for x := 0 to w - 1 do
      begin
        imgScannedWindow.Canvas.Pixels[x, y] := ImgMatrix[YLine + x];
        imgHandleColors.Canvas.Pixels[x, y] := ImgHWMatrix[YLine + x];
      end;
    end;

    SetLength(ImgMatrix, 0);
    SetLength(ImgHWMatrix, 0);

    imgScannedWindow.Canvas.Unlock;
    imgHandleColors.Canvas.Unlock;
    vstComponents.EndUpdate;
    btnStartRec.Enabled := True;
    spdbtnExtraRecording.Enabled := True;

    SetLength(DiffRects, 0);
    imgSpinnerDiff.Visible := False;

    if chkMinimizeWhileRecording.Checked then
      if FInterprettedHandle <> Handle then
      begin
        //WindowState := wsNormal;
        Application.Restore;
      end;
  end;

  Duration := GetTickCount64 - tk;
  memCompInfo.Lines.Add('Recording with mouse swipe duration: ' + IntToStr(Duration) + 'ms   ~= ' + FloatToStr(Duration / 60000) + ' min.');

  vstComponents.Repaint;
  lblGauge.Caption := '100%';
  prbRecording.Position := 0;
end;


procedure TfrmClickerWinInterp.MenuItemCopyFindControlAndCachePositionActionsToClipBoardClick
  (Sender: TObject);
begin
  CopyFindControlActionsToClipBoard(lfcCachePos);
end;


procedure TfrmClickerWinInterp.CropImageFromScreenshot(ComponentNode: PVirtualNode; CroppedBMP: TBitmap; ComponentOnly: Boolean = True);
var
  CompData: PHighlightedCompRec;
  X, Y: Integer;
begin
  if ComponentOnly then
  begin
    CompData := vstComponents.GetNodeData(ComponentNode);
    if CompData = nil then
    begin
      MessageBox(Handle, 'Can''t get component info.', PChar(Caption), MB_ICONERROR);
      Exit;
    end;

    CroppedBMP.Width := CompData^.CompRec.ComponentRectangle.Width;
    CroppedBMP.Height := CompData^.CompRec.ComponentRectangle.Height;
    X := CompData^.LocalX;
    Y := CompData^.LocalY;
  end
  else
  begin
    CroppedBMP.Width := FSelectedComponentRightLimitLabel.Left - FSelectedComponentLeftLimitLabel.Left;
    CroppedBMP.Height := FSelectedComponentBottomLimitLabel.Top - FSelectedComponentTopLimitLabel.Top;
    X := FSelectedComponentLeftLimitLabel.Left;
    Y := FSelectedComponentTopLimitLabel.Top;
  end;

  CroppedBMP.PixelFormat := pf24bit;
  CroppedBMP.Canvas.Pen.Color := clWhite;
  CroppedBMP.Canvas.Brush.Color := clWhite;
  CroppedBMP.Canvas.Rectangle(0, 0, CroppedBMP.Width - 1, CroppedBMP.Height - 1);

  BitBlt(CroppedBMP.Canvas.Handle, 0, 0, CroppedBMP.Width, CroppedBMP.Height, imgScreenshot.Picture.Bitmap.Canvas.Handle, X, Y, SRCCOPY);
end;


procedure TfrmClickerWinInterp.MenuItem_CopySelectionToClipboardClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  CroppedBMP: TBitmap;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  CroppedBMP := TBitmap.Create;
  try
    CropImageFromScreenshot(Node, CroppedBMP, False);
    Clipboard.Assign(CroppedBMP);
  finally
    CroppedBMP.Free;
  end;
end;


procedure TfrmClickerWinInterp.MenuItem_RecordFromRemoteClick(Sender: TObject);
var
  TreeStream: TMemoryStream;
  ServerAddress: string;
  CompHandleToBeRecorded: THandle;
begin
  TreeStream := TMemoryStream.Create;
  try
    ServerAddress := DoOnGetConnectionAddress;
    CompHandleToBeRecorded := DoOnGetSelectedCompFromRemoteWin;

    if CompHandleToBeRecorded = 0 then
    begin
      MessageBox(Handle, PChar('No selected component. Please open the remote screen tool, refresh the screenshot there, then click on a component from screenshot.'), PChar(Application.Title), MB_ICONINFORMATION);
      Exit;
    end;

    btnStartRec.Enabled := False;
    spdbtnExtraRecording.Enabled := False;
    btnStopRec.Enabled := False;
    tmrSpinner.Enabled := True;
    imgSpinner.Show;
    try
      RecordComponentOnServer(ServerAddress, CompHandleToBeRecorded, TreeStream);

      WipeImage(imgScreenshot, imgScannedWindow.Width, imgScannedWindow.Height);
      GetCurrentlyRecordedScreenShotImageFromServer(ServerAddress, imgScreenshot.Picture.Bitmap);

      try
        TreeStream.Position := 0;
        vstComponents.LoadFromStream(TreeStream);
        GenerateCompImagesfromTreeContent;
      except
        on E: Exception do
          MessageBox(Handle, PChar(E.Message + #13#10 + 'Make sure the server is available.'), PChar(Application.Title), MB_ICONERROR);
      end;
    finally
      btnStartRec.Enabled := True;
      spdbtnExtraRecording.Enabled := True;
      btnStopRec.Enabled := True;
      tmrSpinner.Enabled := False;
      imgSpinner.Hide;
    end;
  finally
    TreeStream.Free;
  end;
end;


procedure TfrmClickerWinInterp.MenuItem_SaveSelectionToFileClick(Sender: TObject);
var
  Node: PVirtualNode;
  CroppedBMP: TBitmap;
  SaveDialog: TSaveDialog;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';

    if not SaveDialog.Execute then
      Exit;

    if UpperCase(ExtractFileExt(SaveDialog.FileName)) <> '.BMP' then
      SaveDialog.FileName := SaveDialog.FileName + '.bmp';

    CroppedBMP := TBitmap.Create;
    try
      CropImageFromScreenshot(Node, CroppedBMP, False);
      CroppedBMP.SaveToFile(SaveDialog.FileName);
    finally
      CroppedBMP.Free;
    end;
  finally
    SaveDialog.Free;
  end;
end;


procedure TfrmClickerWinInterp.MenuItem_CopySelectedComponentToClipboardClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  CroppedBMP: TBitmap;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  CroppedBMP := TBitmap.Create;
  try
    CropImageFromScreenshot(Node, CroppedBMP);
    Clipboard.Assign(CroppedBMP);
  finally
    CroppedBMP.Free;
  end;
end;


procedure TfrmClickerWinInterp.MenuItem_SaveSelectedComponentToFileClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  CroppedBMP: TBitmap;
  SaveDialog: TSaveDialog;
begin
  Node := vstComponents.GetFirstSelected;

  if Node = nil then
  begin
    MessageBox(Handle, 'Please select at least one component.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';

    if not SaveDialog.Execute then
      Exit;

    if UpperCase(ExtractFileExt(SaveDialog.FileName)) <> '.BMP' then
      SaveDialog.FileName := SaveDialog.FileName + '.bmp';

    CroppedBMP := TBitmap.Create;
    try
      CropImageFromScreenshot(Node, CroppedBMP);
      CroppedBMP.SaveToFile(SaveDialog.FileName);
    finally
      CroppedBMP.Free;
    end;
  finally
    SaveDialog.Free;
  end;
end;


function TfrmClickerWinInterp.GetParentNodeByRectangle(AComp: THighlightedCompRec): PVirtualNode;
var
  ParentData: PHighlightedCompRec;
  Node: PVirtualNode;
begin
  Result := nil;
  Node := vstComponents.GetFirst;

  if Node = nil then
  begin
    Result := Node;
    Exit;
  end
  else
  begin
    repeat
      ParentData := vstComponents.GetNodeData(Node);

      if (AComp.CompRec.ComponentRectangle.Left >= ParentData^.CompRec.ComponentRectangle.Left) and
         (AComp.CompRec.ComponentRectangle.Right <= ParentData^.CompRec.ComponentRectangle.Right) and
         (AComp.CompRec.ComponentRectangle.Top >= ParentData^.CompRec.ComponentRectangle.Top) and
         (AComp.CompRec.ComponentRectangle.Bottom <= ParentData^.CompRec.ComponentRectangle.Bottom) and
         (AComp.CompRec.ComponentRectangle <> ParentData^.CompRec.ComponentRectangle) then
      begin
        Result := Node;
        //Exit;  //do not exit, because there might be another parent, smaller in size
      end;

      Node := vstComponents.GetNext(Node);
    until Node = nil;
  end;
end;


procedure TfrmClickerWinInterp.AddTreeComponent(AComp: THighlightedCompRec);
var
  NewData: PHighlightedCompRec;
  Node, ParentNode: PVirtualNode;
begin
  ParentNode := GetParentNodeByRectangle(AComp);

  if ParentNode = nil then
  begin
    try
      Node := vstComponents.InsertNode(vstComponents.RootNode, amAddChildLast)
    except  //There is some internal race condition in VirtualTreeView, which might trigger.
      vstComponents.Clear;
      Application.ProcessMessages;
      Sleep(100);

      Node := vstComponents.InsertNode(vstComponents.RootNode, amAddChildLast); //maybe it works this time
    end;
  end
  else
  begin
    Node := vstComponents.InsertNode(ParentNode, amAddChildLast);
    vstComponents.FullExpand(ParentNode);
  end;

  NewData := vstComponents.GetNodeData(Node);
  if NewData = nil then
  begin
    memCompInfo.Lines.Add('Can''t get node data.');
    Exit;
  end;

  NewData^ := AComp;
end;


function TfrmClickerWinInterp.GetIndexOfColoredHandleByHandle(HW: THandle): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FColoredHandles) - 1 do
    if FColoredHandles[i].Handle = HW then
    begin
      Result := i;
      Exit;
    end;
end;


function TfrmClickerWinInterp.GetIndexOfColoredHandleByColor(AColor: TColor): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FColoredHandles) - 1 do
    if FColoredHandles[i].Color = AColor then
    begin
      Result := i;
      Exit;
    end;
end;


procedure TfrmClickerWinInterp.AddColoredHandle(HW: THandle; AColor: TColor);
var
  n: Integer;
begin
  n := Length(FColoredHandles);
  SetLength(FColoredHandles, n + 1);
  FColoredHandles[n].Handle := HW;
  FColoredHandles[n].Color := AColor;
end;


function TfrmClickerWinInterp.AddComponentToList(HW: THandle; ALocalX, ALocalY: Integer): TColor;
var
  HighlightedCompRec: THighlightedCompRec;
  HandleIndex: Integer;
  Found: Boolean;
begin
  HandleIndex := GetIndexOfColoredHandleByHandle(HW);
  Found := HandleIndex > -1;

  if not Found then
  begin
    HandleIndex := Length(FColoredHandles); //without -1

    Result := clrs2[(HandleIndex + 1) mod Length(clrs2)];  //colors are reused, leading to selection bugs if selection relies on color @ [x, y]
    AddColoredHandle(HW, Result);

    HighlightedCompRec.CompRec := GetWindowClassRec(HW);
    HighlightedCompRec.CompRec.IsSubControl := False;
    HighlightedCompRec.AssignedColor := Result;
    HighlightedCompRec.LocalX := ALocalX;
    HighlightedCompRec.LocalY := ALocalY;

    try
      AddTreeComponent(HighlightedCompRec);
    except
      //there is a bug in VirtualTreeView, which causes a bad AV when inserting a node.
      //See "if (vsExpanded in Destination.Parent.States) and IsEffectivelyVisible(Node) then"  in VirtualTrees.pas.  Destination.Parent is nil.
      //The tree becomes empty (by calling Clear), but the RootNode is not properly initialized.
    end;
  end
  else
    Result := clrs2[(HandleIndex + 1) mod Length(clrs2)];
end;


function TfrmClickerWinInterp.AddSubComponentToList(HW: THandle; ARect: TCompRec; ALocalX, ALocalY: Integer): TColor;
var
  HighlightedCompRec: THighlightedCompRec;
  HandleIndex: Integer;
  Found: Boolean;
begin
  HandleIndex := GetIndexOfColoredHandleByHandle(HW);
  Found := HandleIndex > -1;

  if not Found then
  begin
    HandleIndex := Length(FColoredHandles); //without -1

    Result := clrs2[(HandleIndex + 1) mod Length(clrs2)];  //colors are reused, leading to selection bugs if selection relies on color @ [x, y]
    AddColoredHandle(HW, Result);

    HighlightedCompRec.CompRec := ARect;
    HighlightedCompRec.CompRec.IsSubControl := True;

    HighlightedCompRec.AssignedColor := Result;
    HighlightedCompRec.LocalX := ALocalX;
    HighlightedCompRec.LocalY := ALocalY;

    try
      AddTreeComponent(HighlightedCompRec);
    except
      //there is a bug in VirtualTreeView, which causes a bad AV when inserting a node.
      //See "if (vsExpanded in Destination.Parent.States) and IsEffectivelyVisible(Node) then"  in VirtualTrees.pas.  Destination.Parent is nil.
      //The tree becomes empty (by calling Clear), but the RootNode is not properly initialized.
    end;
  end
  else
    Result := clrs2[(HandleIndex + 1) mod Length(clrs2)];
end;


procedure TfrmClickerWinInterp.PrepareLayers(ARect: TRect);
begin
  imgScannedWindow.Picture.Clear;
  imgScannedWindow.Left := 0;
  imgScannedWindow.Top := 0;
  imgScannedWindow.Width := ARect.Right - ARect.Left;
  imgScannedWindow.Height := ARect.Bottom - ARect.Top;
  imgScannedWindow.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgScannedWindow.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgScannedWindow.Canvas.Pen.Color := clBlack;
  imgScannedWindow.Canvas.Brush.Color := clBlack;
  imgScannedWindow.Canvas.Rectangle(0, 0, imgScannedWindow.Width - 1, imgScannedWindow.Height - 1);
  imgScannedWindow.Repaint;

  imgScreenshot.Picture.Clear;
  imgScreenshot.Left := 0;
  imgScreenshot.Top := 0;
  imgScreenshot.Width := imgScannedWindow.Width;
  imgScreenshot.Height := imgScannedWindow.Height;
  imgScreenshot.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgScreenshot.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgScreenshot.Canvas.Pen.Color := clBlack;
  imgScreenshot.Canvas.Brush.Color := clBlack;
  imgScreenshot.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);

  imgAvgScreenshotAndGreenComp.Picture.Clear;
  imgAvgScreenshotAndGreenComp.Left := 0;
  imgAvgScreenshotAndGreenComp.Top := 0;
  imgAvgScreenshotAndGreenComp.Width := imgScannedWindow.Width;
  imgAvgScreenshotAndGreenComp.Height := imgScannedWindow.Height;
  imgAvgScreenshotAndGreenComp.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgAvgScreenshotAndGreenComp.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgAvgScreenshotAndGreenComp.Canvas.Pen.Color := clBlack;
  imgAvgScreenshotAndGreenComp.Canvas.Brush.Color := clBlack;
  imgAvgScreenshotAndGreenComp.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);

  imgAvgScreenshotAndAssignedComp.Picture.Clear;
  imgAvgScreenshotAndAssignedComp.Left := 0;
  imgAvgScreenshotAndAssignedComp.Top := 0;
  imgAvgScreenshotAndAssignedComp.Width := imgScannedWindow.Width;
  imgAvgScreenshotAndAssignedComp.Height := imgScannedWindow.Height;
  imgAvgScreenshotAndAssignedComp.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgAvgScreenshotAndAssignedComp.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgAvgScreenshotAndAssignedComp.Canvas.Pen.Color := clBlack;
  imgAvgScreenshotAndAssignedComp.Canvas.Brush.Color := clBlack;
  imgAvgScreenshotAndAssignedComp.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);

  imgHandleColors.Picture.Clear;
  imgHandleColors.Left := 0;
  imgHandleColors.Top := 0;
  imgHandleColors.Width := imgScannedWindow.Width;
  imgHandleColors.Height := imgScannedWindow.Height;
  imgHandleColors.Picture.Bitmap.Width := imgScannedWindow.Width;
  imgHandleColors.Picture.Bitmap.Height := imgScannedWindow.Height;
  imgHandleColors.Canvas.Pen.Color := clBlack;
  imgHandleColors.Canvas.Brush.Color := clBlack;
  imgHandleColors.Canvas.Rectangle(0, 0, imgScreenshot.Width - 1, imgScreenshot.Height - 1);
end;


procedure TfrmClickerWinInterp.GenerateContent_AvgScreenshotAndGreenComp(Node: PVirtualNode);
var
  {$IFDEF UNIX}
    i, j: Integer;
    Color1: TColor;
  {$ENDIF}
  NodeData: PHighlightedCompRec;
  LocX, LocY: Integer;
  CompWidth, CompHeight: Integer;
begin
  if Node = nil then
    Exit;

  NodeData := vstComponents.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  LocX := NodeData^.LocalX;
  LocY := NodeData^.LocalY;
  CompWidth := NodeData^.CompRec.ComponentRectangle.Width;
  CompHeight := NodeData^.CompRec.ComponentRectangle.Height;

  //reset with screenshot
  imgAvgScreenshotAndGreenComp.Canvas.Draw(0, 0, imgScreenshot.Picture.Bitmap);

  //highlight with green

  {$IFDEF UNIX}  //using the slower method, because of ScanLine
    for i := LocY to CompHeight + LocY - 1 do
      for j := LocX to CompWidth + LocX - 1 do
      begin
        Color1 := imgAvgScreenshotAndGreenComp.Canvas.Pixels[j, i];
        imgAvgScreenshotAndGreenComp.Canvas.Pixels[j, i] := AvgTwoTrueColors(Color1, clLime);
      end;
  {$ELSE}
    AvgBitmapWithColor(imgScreenshot.Picture.Bitmap, imgAvgScreenshotAndGreenComp.Picture.Bitmap, clLime, LocX, LocY, CompWidth, CompHeight);
  {$ENDIF}
end;


procedure TfrmClickerWinInterp.GenerateContent_AvgScreenshotAndGenComp;
{$IFDEF UNIX}
  var
    i, j: Integer;
    Color1, Color2: TColor;
{$ENDIF}
begin
  {$IFDEF UNIX}
  for i := 0 to imgScannedWindow.Height - 1 do
    for j := 0 to imgScannedWindow.Width - 1 do
    begin
      Color1 := imgScannedWindow.Canvas.Pixels[j, i];
      Color2 := imgScreenshot.Canvas.Pixels[j, i];
      imgAvgScreenshotAndAssignedComp.Canvas.Pixels[j, i] := AvgTwoTrueColors(Color1, Color2);
    end;
  {$ELSE}
    AvgBitmapWithBitmap(imgScreenshot.Picture.Bitmap, imgScannedWindow.Picture.Bitmap, imgAvgScreenshotAndAssignedComp.Picture.Bitmap, 0, 0, imgScreenshot.Width, imgScreenshot.Height);
  {$ENDIF}
end;


procedure TfrmClickerWinInterp.RecordComponent(AInterprettedHandle: THandle; var ImgMatrix: TColorArr; var ImgHWMatrix: THandleArr);
var
  x, y, Step, YLine: Integer;
  tp: TPoint;
  rct: TRect;
  HW, InitialHW: THandle;
  AllocatedColor: TColor;
  RectWidth, RectHeight: Integer;
  tk: QWord;
  AppTitle: string;
begin
  if chkMinimizeWhileRecording.Checked then
    if AInterprettedHandle <> Handle then
    begin
      //WindowState := wsMinimized;
      Application.Minimize;
    end;

  tk := GetTickCount64;

  FDoneRec := False;
  if GetWindowRect(AInterprettedHandle, rct) = False then
    Exit;

  vstComponents.BeginUpdate;
  try
    SetLength(FColoredHandles, 0);
    vstComponents.Clear;
  finally
    vstComponents.EndUpdate;
  end;

  PrepareLayers(Rct);

  RectWidth := rct.Width;  //rct.Width and rct.Height are functions, so better use some local vars
  RectHeight := rct.Height;

  memCompInfo.Lines.Add('Width: ' + IntToStr(RectWidth));
  memCompInfo.Lines.Add('Height: ' + IntToStr(RectHeight));

  tp.X := rct.Left;
  tp.Y := rct.Top;
  InitialHW := WindowFromPoint(tp);

  prbRecording.Max := RectWidth * RectHeight;

  if prbRecording.Max = 0 then
    prbRecording.Max := 1;

  Step := StrToIntDef(lbeStep.Text, 1);

  if Step = 1 then
  begin
    SetLength(ImgMatrix, prbRecording.Max);
    SetLength(ImgHWMatrix, prbRecording.Max);
  end;

  imgScannedWindow.Canvas.Lock;
  imgHandleColors.Canvas.Lock;
  vstComponents.BeginUpdate;
  btnStartRec.Enabled := False;
  spdbtnExtraRecording.Enabled := False;
  AppTitle := Application.Title;
  try
    y := -1;
    repeat
      Inc(y, Step);
      if y and $F = $F then
        Application.ProcessMessages;

      YLine := y * RectWidth;
      tp.Y := y + rct.Top;
      x := -1;
      repeat
        Inc(x, Step);
        tp.X := x + rct.Left;
        HW := WindowFromPoint(tp);

        AllocatedColor := AddComponentToList(HW, x, y);

        if Step = 1 then
        begin
          ImgMatrix[YLine + x] := AllocatedColor;
          ImgHWMatrix[YLine + x] := HW;
        end
        else
        begin
          imgScannedWindow.Canvas.Pen.Color := AllocatedColor;
          imgScannedWindow.Canvas.Brush.Color := imgScannedWindow.Canvas.Pen.Color;
          imgScannedWindow.Canvas.Rectangle(x, y, x + Step, y + Step);

          imgHandleColors.Canvas.Pen.Color := HW;
          imgHandleColors.Canvas.Brush.Color := imgHandleColors.Canvas.Pen.Color;
          imgHandleColors.Canvas.Rectangle(x, y, x + Step, y + Step);
        end;

        if x and $FF = $FF then
        begin
          prbRecording.Position := YLine + x;
          lblGauge.Caption := IntToStr(prbRecording.Position * 100 div Length(ImgMatrix)) + ' %';
          Application.Title := lblGauge.Caption;
        end;
      until x >= RectWidth - 1;

      if FDoneRec then
        Break;
    until y >= RectHeight - 1;
  finally
    Application.Title := AppTitle;

    if Step = 1 then
    begin
      for y := 0 to RectHeight - 1 do
      begin
        YLine := y * RectWidth;

        for x := 0 to RectWidth - 1 do
        begin
          imgScannedWindow.Canvas.Pixels[x, y] := ImgMatrix[YLine + x];
          imgHandleColors.Canvas.Pixels[x, y] := ImgHWMatrix[YLine + x];
        end;
      end;
    end;

    imgScannedWindow.Canvas.Unlock;
    imgHandleColors.Canvas.Unlock;
    vstComponents.EndUpdate;
    btnStartRec.Enabled := True;
    spdbtnExtraRecording.Enabled := True;
  end;

  if chkMinimizeWhileRecording.Checked then
    if AInterprettedHandle <> Handle then
    begin
      //WindowState := wsNormal;
      Application.Restore;
    end;

  memCompInfo.Lines.Add('Done recording in ' + IntToStr(GetTickCount64 - tk) + 'ms.');

  ScreenShot(InitialHW, imgScreenshot.Picture.Bitmap, 0, 0, RectWidth, RectHeight);
  GenerateContent_AvgScreenshotAndGenComp;
  AdjustHighlightingLabelsToScreenshot;
  UpdateLayersVisibility;

  //MessageBox(Handle, 'Done recording', PChar(Caption), MB_ICONINFORMATION);  //sometimes, this messagebox pops up under a Stay-On-Top window
  lblGauge.Caption := '100%';

  prbRecording.Position := 0;
  vstComponents.Repaint;
  imgLiveScreenshot.Hide;
end;


procedure TfrmClickerWinInterp.GetCurrentlyRecordedScreenShotImage(ABmp: TBitmap);
var
  Msg: string;
begin
  if imgScreenshot.Picture.Bitmap = nil then
  begin
    Msg := 'No component is recorded yet.';
    ABmp.Width := ABmp.Canvas.TextWidth(Msg) + 10;
    ABmp.Height := 20;
    ABmp.Canvas.Pen.Color := clWhite;
    ABmp.Canvas.Brush.Color := clWhite;
    ABmp.Canvas.Font.Color := clRed;

    ABmp.Canvas.Rectangle(0, 0, ABmp.Width - 1, ABmp.Height - 1);
    ABmp.Canvas.TextOut(5, 6, Msg);
  end;

  ABmp.Assign(imgScreenshot.Picture.Bitmap);
end;


procedure TfrmClickerWinInterp.btnStartRecClick(Sender: TObject);
var
  ImgMatrix: TColorArr;
  ImgHWMatrix: THandleArr;
begin
  if FInterprettedHandle = 0 then
  begin
    pnlDrag.Color := clRed;
    MessageBox(Handle, 'Please set the target control (or window) before recording it.', PChar(Caption), MB_ICONINFORMATION);
    pnlDrag.Color := clYellow;
    Exit;
  end;

  try
    RecordComponent(FInterprettedHandle, ImgMatrix, ImgHWMatrix);
  finally
    SetLength(ImgMatrix, 0);
    SetLength(ImgHWMatrix, 0);
  end;
end;


procedure TfrmClickerWinInterp.LoadImages(ABasePath: string);
var
  APng: TPNGImage;
  ExtLen: Integer;
begin
  ExtLen := Length(ExtractFileExt(ABasePath));
  Delete(ABasePath, Length(ABasePath) - ExtLen + 1, ExtLen);

  CreateDirWithSubDirs(ABasePath);
  if ABasePath > '' then
    if ABasePath[Length(ABasePath)] <> PathDelim then
      ABasePath := ABasePath + PathDelim;

  if FileExists(ABasePath + 'ScannedWindow.png') then
  begin
    APng := TPNGImage.Create;
    try
      APng.LoadFromFile(ABasePath + 'ScannedWindow.png');
      imgScannedWindow.Picture.Bitmap.LoadFromDevice(APng.Canvas.Handle);
    finally
      APng.Free;
    end;
  end;

  if FileExists(ABasePath + 'Screenshot.png') then
  begin
    APng := TPNGImage.Create;
    try
      APng.LoadFromFile(ABasePath + 'Screenshot.png');
      imgScreenshot.Picture.Bitmap.LoadFromDevice(APng.Canvas.Handle);
    finally
      APng.Free;
    end;
  end;

  if FileExists(ABasePath + 'HandleColors.png') then
  begin
    APng := TPNGImage.Create;
    try
      APng.LoadFromFile(ABasePath + 'HandleColors.png');
      imgHandleColors.Picture.Bitmap.LoadFromDevice(APng.Canvas.Handle);
    finally
      APng.Free;
    end;
  end;
end;


procedure TfrmClickerWinInterp.SaveImages(ABasePath: string);
var
  APng: TPNGImage;
  ExtLen: Integer;
begin
  ExtLen := Length(ExtractFileExt(ABasePath));
  Delete(ABasePath, Length(ABasePath) - ExtLen + 1, ExtLen);

  CreateDirWithSubDirs(ABasePath);
  if ABasePath > '' then
    if ABasePath[Length(ABasePath)] <> PathDelim then
      ABasePath := ABasePath + PathDelim;

  APng := TPNGImage.Create;
  try
    APng.LoadFromDevice(imgScannedWindow.Canvas.Handle);
    APng.SaveToFile(ABasePath + 'ScannedWindow.png');
  finally
    APng.Free;
  end;

  APng := TPNGImage.Create;
  try
    APng.LoadFromDevice(imgScreenshot.Canvas.Handle);
    APng.SaveToFile(ABasePath + 'Screenshot.png');
  finally
    APng.Free;
  end;

  APng := TPNGImage.Create;
  try
    APng.LoadFromDevice(imgHandleColors.Canvas.Handle);
    APng.SaveToFile(ABasePath + 'HandleColors.png');
  finally
    APng.Free;
  end;
end;


procedure TfrmClickerWinInterp.btnExportClick(Sender: TObject);
  function MakeBlanks(ACount: Integer): string;
  begin
    SetLength(Result, ACount);
    FillChar(Result[1], ACount, ' ');
  end;

var
  Content: TStringList;
  TempSaveDialog: TSaveDialog;
  Node: PVirtualNode;
  NodeData: PHighlightedCompRec;
  Blanks: string;
begin
  TempSaveDialog := TSaveDialog.Create(Self);
  try
    TempSaveDialog.Filter := 'Yml files (*.yml)|*.yml|All files (*.*)|*.*';

    if not TempSaveDialog.Execute then
      Exit;

    if ExtractFileExt(TempSaveDialog.FileName) = '' then
      TempSaveDialog.FileName := TempSaveDialog.FileName + '.yml';

    Content := TStringList.Create;
    try
      Node := vstComponents.GetFirst;
      try
        if Node = nil then
          Exit;

        repeat
          NodeData := vstComponents.GetNodeData(Node);
          Blanks := MakeBlanks(vstComponents.GetNodeLevel(Node) shl 2);

          if NodeData = nil then
            Content.Add(Blanks + 'Data_bug')
          else
          begin
            Content.Add(Blanks + 'Handle: ' + IntToStr(NodeData^.CompRec.Handle));

            if NodeData^.CompRec.IsSubControl then
              Content.Add(Blanks + '    Type: ' + 'Subcontrol')
            else
              Content.Add(Blanks + '    Type: ' + 'Control');

            Content.Add(Blanks + '    Text: "' + NodeData^.CompRec.Text + '"');
            Content.Add(Blanks + '    Class: "' + NodeData^.CompRec.ClassName + '"');
            Content.Add(Blanks + '    Left: ' + IntToStr(NodeData^.CompRec.ComponentRectangle.Left));
            Content.Add(Blanks + '    Top: ' + IntToStr(NodeData^.CompRec.ComponentRectangle.Top));
            Content.Add(Blanks + '    Right: ' + IntToStr(NodeData^.CompRec.ComponentRectangle.Right));
            Content.Add(Blanks + '    Bottom: ' + IntToStr(NodeData^.CompRec.ComponentRectangle.Bottom));
            Content.Add(Blanks + '    LocalX: ' + IntToStr(NodeData^.LocalX));
            Content.Add(Blanks + '    LocalY: ' + IntToStr(NodeData^.LocalY));
          end;

          Node := vstComponents.GetNext(Node);
        until Node = nil;
      finally
        Content.SaveToFile(TempSaveDialog.FileName);
      end;
    finally
      Content.Free;
    end;

    SaveImages(TempSaveDialog.FileName);
  finally
    TempSaveDialog.Free;
  end;
end;


procedure TfrmClickerWinInterp.btnLoadTreeClick(Sender: TObject);
var
  TempOpenDialog: TOpenDialog;
begin
  TempOpenDialog := TOpenDialog.Create(nil);
  try
    TempOpenDialog.Filter := 'Tree files (*.tree)|*.tree|All files (*.*)|*.*';
    if not TempOpenDialog.Execute then
      Exit;

    try
      vstComponents.LoadFromFile(TempOpenDialog.FileName);
    except
      on E: Exception do
        MessageBox(Handle, PChar(E.Message), PChar(Application.Title), MB_ICONERROR);
    end;

    GenerateCompImagesfromTreeContent;
    LoadImages(TempOpenDialog.FileName);
  finally
    TempOpenDialog.Free;
  end;
end;


procedure TfrmClickerWinInterp.btnSaveTreeClick(Sender: TObject);
var
  TempSaveDialog: TSaveDialog;
begin
  TempSaveDialog := TSaveDialog.Create(nil);
  try
    TempSaveDialog.Filter := 'Tree files (*.tree)|*.tree|All files (*.*)|*.*';
    if not TempSaveDialog.Execute then
      Exit;

    if ExtractFileExt(TempSaveDialog.FileName) = '' then
      TempSaveDialog.FileName := TempSaveDialog.FileName + '.tree';

    vstComponents.SaveToFile(TempSaveDialog.FileName);
    SaveImages(TempSaveDialog.FileName);
  finally
    TempSaveDialog.Free;
  end;
end;


procedure TfrmClickerWinInterp.GenerateCompImagesfromTreeContent;
var
  Node: PVirtualNode;
  NodeData: PHighlightedCompRec;
begin
  Node := vstComponents.GetFirst;  //first component defines the image size
  if Node = nil then
    Exit;

  NodeData := vstComponents.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  vstComponents.BeginUpdate;
  try
    SetLength(FColoredHandles, 0);
  finally
    vstComponents.EndUpdate;
  end;

  imgScannedWindow.Width := NodeData^.CompRec.ComponentRectangle.Width;
  imgScannedWindow.Height := NodeData^.CompRec.ComponentRectangle.Height;

  WipeImage(imgScannedWindow, imgScannedWindow.Width, imgScannedWindow.Height);
  //WipeImage(imgScreenshot, imgScannedWindow.Width, imgScannedWindow.Height);     //expected to be already loaded with a real screenshot
  WipeImage(imgAvgScreenshotAndGreenComp, imgScannedWindow.Width, imgScannedWindow.Height);
  WipeImage(imgAvgScreenshotAndAssignedComp, imgScannedWindow.Width, imgScannedWindow.Height);
  WipeImage(imgLiveScreenshot, imgScannedWindow.Width, imgScannedWindow.Height);
  WipeImage(imgHandleColors, imgScannedWindow.Width, imgScannedWindow.Height);

  imgScannedWindow.Canvas.Lock;
  imgHandleColors.Canvas.Lock;
  try
    repeat
      NodeData := vstComponents.GetNodeData(Node);

      if NodeData <> nil then
      begin
        imgScannedWindow.Canvas.Pen.Color := NodeData^.AssignedColor;
        imgScannedWindow.Canvas.Brush.Color := NodeData^.AssignedColor;
        imgScannedWindow.Canvas.Rectangle(NodeData^.LocalX,             //LocalX and LocalY are relative to the screenshot image (a.k.a. first component)
                                          NodeData^.LocalY,
                                          NodeData^.LocalX + NodeData^.CompRec.ComponentRectangle.Width - 1,
                                          NodeData^.LocalY + NodeData^.CompRec.ComponentRectangle.Height - 1);

        imgHandleColors.Canvas.Pen.Color := NodeData^.CompRec.Handle;
        imgHandleColors.Canvas.Brush.Color := NodeData^.CompRec.Handle;
        imgHandleColors.Canvas.Rectangle(NodeData^.LocalX,
                                         NodeData^.LocalY,
                                         NodeData^.LocalX + NodeData^.CompRec.ComponentRectangle.Width - 1,
                                         NodeData^.LocalY + NodeData^.CompRec.ComponentRectangle.Height - 1);

        AddColoredHandle(NodeData^.CompRec.Handle, NodeData^.AssignedColor);
      end
      else
        AddColoredHandle(0, 0); //some dummy values

      Node := vstComponents.GetNext(Node);
    until Node = nil;

    GenerateContent_AvgScreenshotAndGenComp;   //requires images to have some screenshots, already loaded
    AdjustHighlightingLabelsToScreenshot;
    UpdateLayersVisibility;
  finally
    imgScannedWindow.Canvas.Unlock;
    imgHandleColors.Canvas.Unlock;
  end;
end;


procedure TfrmClickerWinInterp.GetTreeContent(AStream: TMemoryStream);
begin
  vstComponents.SaveToStream(AStream);
end;


procedure TfrmClickerWinInterp.btnStopRecClick(Sender: TObject);
begin
  FDoneRec := True;
end;


procedure TfrmClickerWinInterp.chkHighlightSelectedComponentChange(Sender: TObject);
begin
  FSelectedComponentLeftLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FSelectedComponentTopLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FSelectedComponentRightLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FSelectedComponentBottomLimitLabel.Visible := chkHighlightSelectedComponent.Checked;

  FTransparent_SelectedComponentLeftLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FTransparent_SelectedComponentTopLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FTransparent_SelectedComponentRightLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
  FTransparent_SelectedComponentBottomLimitLabel.Visible := chkHighlightSelectedComponent.Checked;
end;


procedure TfrmClickerWinInterp.SetHighlightingLabelsColor;
begin
  FSelectedComponentLeftLimitLabel.Color := colboxHighlightingLabels.Selected;
  FSelectedComponentTopLimitLabel.Color := colboxHighlightingLabels.Selected;
  FSelectedComponentRightLimitLabel.Color := colboxHighlightingLabels.Selected;
  FSelectedComponentBottomLimitLabel.Color := colboxHighlightingLabels.Selected;
end;


procedure TfrmClickerWinInterp.colboxHighlightingLabelsSelect(Sender: TObject);
begin
  SetHighlightingLabelsColor;
end;


procedure TfrmClickerWinInterp.GetWindowInfo;
var
  InterprettedRectangle: TRect;
begin
  InterprettedRectangle.Top := 0;
  InterprettedRectangle.Bottom := 0;
  InterprettedRectangle.Left := 0;
  InterprettedRectangle.Right := 0;

  if GetWindowRect(FInterprettedHandle, InterprettedRectangle) = False then
    Exit;

  memCompInfo.Lines.Add('Left: ' + IntToStr(InterprettedRectangle.Left) + '   ' +
                        'Top: ' + IntToStr(InterprettedRectangle.Top) + '   ' +
                        'Right: ' + IntToStr(InterprettedRectangle.Right) + '   ' +
                        'Bottom: ' + IntToStr(InterprettedRectangle.Bottom));
end;


procedure TfrmClickerWinInterp.UpdateLayersVisibility;
var
  Node: PVirtualNode;
begin
  imgScreenshot.Visible := False;
  imgScannedWindow.Visible := False;
  imgAvgScreenshotAndGreenComp.Visible := False;
  imgAvgScreenshotAndAssignedComp.Visible := False;

  Node := vstComponents.GetFirstSelected;
  if Node = nil then
    Node := vstComponents.GetFirst;

  case rdgrpLayers.ItemIndex of
    0: imgScreenshot.Visible := True;

    1: imgScannedWindow.Visible := True;

    2:
    begin
      imgAvgScreenshotAndGreenComp.Visible := True;
      GenerateContent_AvgScreenshotAndGreenComp(Node);
    end;

    3: imgAvgScreenshotAndAssignedComp.Visible := True;
    else
  end;

  HighlightComponent(Node);
end;


procedure TfrmClickerWinInterp.rdgrpLayersClick(Sender: TObject);
begin
  UpdateLayersVisibility;
end;


procedure TfrmClickerWinInterp.scrboxScannedComponentsMouseWheel(
  Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Factor: Integer;
begin
  if ssCtrl in Shift then
    Factor := 1
  else
    Factor := 3;

  if ssShift in Shift then
    scrboxScannedComponents.HorzScrollBar.Position := scrboxScannedComponents.HorzScrollBar.Position - WheelDelta div Factor
  else
    scrboxScannedComponents.VertScrollBar.Position := scrboxScannedComponents.VertScrollBar.Position - WheelDelta div Factor;

  Handled := True;
end;


procedure TfrmClickerWinInterp.spdbtnExtraRecordingClick(Sender: TObject);
var
  tp: TPoint;
begin
  GetCursorPos(tp);
  pmExtraRecording.PopUp(tp.X, tp.Y);
end;


procedure TfrmClickerWinInterp.tmrSpinnerTimer(Sender: TObject);
begin
  //imgSpinner.Picture.PNG;
  imglstSpinner.Draw(imgSpinner.Canvas, 0, 0, tmrSpinner.Tag and $7, dsNormal, itImage);
  tmrSpinner.Tag := tmrSpinner.Tag + 1;
end;


procedure TfrmClickerWinInterp.vstComponentsClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstComponents.GetFirstSelected;

  GenerateContent_AvgScreenshotAndGreenComp(Node);
  HighlightComponent(Node);
end;


procedure TfrmClickerWinInterp.FTransparent_LeftMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := FTransparent_SelectedComponentLeftLimitLabel.Left; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrmClickerWinInterp.FTransparent_LeftMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
    //if NewLeft <> CurrentLabel.Left then
    //  Modified := True;

    CurrentLabel.Left := Max(0, Min(FTransparent_SelectedComponentRightLimitLabel.Left - 8, NewLeft));
    FSelectedComponentLeftLimitLabel.Left := CurrentLabel.Left + 3;
  end;
end;


procedure TfrmClickerWinInterp.FTransparent_LeftMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrmClickerWinInterp.FTransparent_RightMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.X := FTransparent_SelectedComponentRightLimitLabel.Left; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrmClickerWinInterp.FTransparent_RightMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewLeft: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewLeft := FMouseDownSelPos.X + tp.X - FMouseDownGlobalPos.X;
    //if NewLeft <> CurrentLabel.Left then
    //  Modified := True;

    CurrentLabel.Left := Max(FTransparent_SelectedComponentLeftLimitLabel.Left + 8, Min(imgLiveScreenshot.Width - 2, NewLeft));
    FSelectedComponentRightLimitLabel.Left := CurrentLabel.Left + 3;
  end;
end;


procedure TfrmClickerWinInterp.FTransparent_RightMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrmClickerWinInterp.FTransparent_TopMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.Y := FTransparent_SelectedComponentTopLimitLabel.Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrmClickerWinInterp.FTransparent_TopMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    //if NewTop <> CurrentLabel.Top then
    //  Modified := True;

    CurrentLabel.Top := Max(0, Min(FTransparent_SelectedComponentBottomLimitLabel.Top - 8, NewTop));
    FSelectedComponentTopLimitLabel.Top := CurrentLabel.Top + 3;
  end;
end;


procedure TfrmClickerWinInterp.FTransparent_TopMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrmClickerWinInterp.FTransparent_BottomMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    Exit;

  GetCursorPos(FMouseDownGlobalPos);

  if not FSelectionHold then
  begin
    FMouseDownSelPos.Y := FTransparent_SelectedComponentBottomLimitLabel.Top; //component coordinates on the window
    FSelectionHold := True;
  end;
end;


procedure TfrmClickerWinInterp.FTransparent_BottomMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
  CurrentLabel: TLabel;
  tp: TPoint;
begin
  if not FSelectionHold then
    Exit;

  GetCursorPos(tp);
  if Sender is TLabel then
  begin
    CurrentLabel := Sender as TLabel;
    NewTop := FMouseDownSelPos.Y + tp.Y - FMouseDownGlobalPos.Y;
    //if NewTop <> CurrentLabel.Top then
    //  Modified := True;

    CurrentLabel.Top := Max(FTransparent_SelectedComponentTopLimitLabel.Top + 8, Min(imgLiveScreenshot.Height - 2, NewTop));
    FSelectedComponentBottomLimitLabel.Top := CurrentLabel.Top + 3;
  end;
end;


procedure TfrmClickerWinInterp.FTransparent_BottomMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;

end.

