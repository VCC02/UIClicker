{
    Copyright (C) 2023 VCC
    creation date: 22 Nov 2023
    initial release date: 23 Nov 2023

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


unit ClickerFontFinderSettingsForm;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls,
  ExtCtrls, Menus, VirtualTrees, ClickerUtils;

type

  { TfrmClickerFontFinderSettings }

  TfrmClickerFontFinderSettings = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkShowAllFonts: TCheckBox;
    grpPreview: TGroupBox;
    imgPreviewByBmp: TImage;
    lbeSearch: TLabeledEdit;
    lblMinSize: TLabel;
    lblMaxSize: TLabel;
    lblFontNames: TLabel;
    lblPreviewSize: TLabel;
    lblPreviewText: TLabel;
    MenuItem_ExportListOfSelectedFonts: TMenuItem;
    MenuItem_ImportListOfFonts_MergeWithExisting: TMenuItem;
    MenuItem_ImportListOfFonts_ReplaceExisting: TMenuItem;
    MenuItem_ImportListOfFonts: TMenuItem;
    N4: TMenuItem;
    MenuItem_InvertCheckedState: TMenuItem;
    MenuItem_InvertSelection: TMenuItem;
    N3: TMenuItem;
    MenuItem_SelectAllCheckedByClearingSelection: TMenuItem;
    MenuItem_SelectAllCheckedByPreservingSelection: TMenuItem;
    N2: TMenuItem;
    MenuItem_UnCheckAllSelected: TMenuItem;
    MenuItem_CheckAllSelected: TMenuItem;
    N1: TMenuItem;
    MenuItem_UnCheckAll: TMenuItem;
    MenuItem_CheckAll: TMenuItem;
    pmSelection: TPopupMenu;
    spnedtMinSize: TSpinEdit;
    spnedtMaxSize: TSpinEdit;
    spnedtPreviewSize: TSpinEdit;
    tmrStartup: TTimer;
    tmrChecked: TTimer;
    vstFonts: TVirtualStringTree;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkShowAllFontsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbeSearchChange(Sender: TObject);
    procedure MenuItem_CheckAllClick(Sender: TObject);
    procedure MenuItem_CheckAllSelectedClick(Sender: TObject);
    procedure MenuItem_ExportListOfSelectedFontsClick(Sender: TObject);
    procedure MenuItem_ImportListOfFonts_MergeWithExistingClick(Sender: TObject);
    procedure MenuItem_ImportListOfFonts_ReplaceExistingClick(Sender: TObject);
    procedure MenuItem_InvertCheckedStateClick(Sender: TObject);
    procedure MenuItem_InvertSelectionClick(Sender: TObject);
    procedure MenuItem_SelectAllCheckedByClearingSelectionClick(Sender: TObject
      );
    procedure MenuItem_SelectAllCheckedByPreservingSelectionClick(Sender: TObject);
    procedure MenuItem_UnCheckAllClick(Sender: TObject);
    procedure MenuItem_UnCheckAllSelectedClick(Sender: TObject);
    procedure spnedtPreviewSizeChange(Sender: TObject);
    procedure tmrCheckedTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure vstFontsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstFontsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFontsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure vstFontsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstFontsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstFontsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure vstFontsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    FListOfUsedFonts: TStringList;  //only the used fonts
    FLatestCheckedNode: PVirtualNode;
    FListOfHistogramDiffs: TStringList;
    FPreviewBitmap: TBitmap;

    procedure SetVSTCheckStates;
    procedure SetCheckedStateToAll(AState, ASelectedOnly: Boolean);
    procedure SelectAllChecked(AClearSelection: Boolean);
    procedure InvertSelection;
    procedure InvertCheckedState;
    procedure RefreshDisplayFontSettings;
    procedure HandleNodeChecked;
    procedure RebuildListOfUsedFonts;
    procedure ImportListOfFonts(ClearExisting: Boolean);
    procedure ComputeHistogramDiffs;

    procedure GetHistogramFromPreviewBitmap(APreviewBitmap: TBitmap; var APreviewHist, APreviewHistColorCounts: TIntArr);
    function GetHistogramDiffByFontName(APreviewBitmap: TBitmap; AFontName: string; var APreviewHist, APreviewHistColorCounts: TIntArr): string;
  public

  end;


function EditFontFinderSettings(var AFontFinderSettings: TFontFinderSettings; APreviewFont: TFont; APreviewBitmap: TBitmap; AFGColor, ABGColor: TColor; APreviewText: string): Boolean;


implementation

{$R *.frm}


uses
  Math, BitmapProcessing, BitmapConv, Types;


function EditFontFinderSettings(var AFontFinderSettings: TFontFinderSettings; APreviewFont: TFont; APreviewBitmap: TBitmap; AFGColor, ABGColor: TColor; APreviewText: string): Boolean;
var
  frmClickerFontFinderSettings: TfrmClickerFontFinderSettings;
  i: Integer;
begin
  Result := False;
  Application.CreateForm(TfrmClickerFontFinderSettings, frmClickerFontFinderSettings);

  frmClickerFontFinderSettings.Left := AFontFinderSettings.WinRect.Left;
  frmClickerFontFinderSettings.Top := AFontFinderSettings.WinRect.Top;
  frmClickerFontFinderSettings.Width := AFontFinderSettings.WinRect.Width;
  frmClickerFontFinderSettings.Height := AFontFinderSettings.WinRect.Height;

  for i := 0 to Min(frmClickerFontFinderSettings.vstFonts.Header.Columns.Count, Length(AFontFinderSettings.ColWidths)) - 1 do
    frmClickerFontFinderSettings.vstFonts.Header.Columns.Items[i].Width := AFontFinderSettings.ColWidths[i];

  frmClickerFontFinderSettings.FListOfUsedFonts.Text := AFontFinderSettings.ListOfUsedFonts;
  frmClickerFontFinderSettings.spnedtMinSize.Value := AFontFinderSettings.MinFontSize;
  frmClickerFontFinderSettings.spnedtMaxSize.Value := AFontFinderSettings.MaxFontSize;
  frmClickerFontFinderSettings.chkShowAllFonts.Checked := AFontFinderSettings.ShowAllFonts;

  frmClickerFontFinderSettings.lblPreviewText.Caption := APreviewText;

  //if frmClickerFontFinderSettings.FListOfUsedFonts.Count > 0 then
  //  frmClickerFontFinderSettings.lblPreviewText.Font.Name := frmClickerFontFinderSettings.FListOfUsedFonts.Strings[0]; //not sure what was the purpose of this

  frmClickerFontFinderSettings.lblPreviewText.Font.Name := APreviewFont.Name;
  frmClickerFontFinderSettings.lblPreviewText.Font.Size := Max(8, APreviewFont.Size);
  frmClickerFontFinderSettings.lblPreviewText.Font.Color := AFGColor; //APreviewFont.Color;
  frmClickerFontFinderSettings.lblPreviewText.Font.Quality := APreviewFont.Quality;
  frmClickerFontFinderSettings.lblPreviewText.Font.Style := APreviewFont.Style;
  frmClickerFontFinderSettings.lblPreviewText.Color := ABGColor;

  frmClickerFontFinderSettings.spnedtPreviewSize.Value := frmClickerFontFinderSettings.lblPreviewText.Font.Size;

  //frmClickerFontFinderSettings.vstFonts.Hint := 'Initial fonts: ' + #13#10#13#10 + AFontFinderSettings.ListOfUsedFonts;
  frmClickerFontFinderSettings.SetVSTCheckStates;
  frmClickerFontFinderSettings.FPreviewBitmap := APreviewBitmap;
  frmClickerFontFinderSettings.imgPreviewByBmp.Picture.Assign(APreviewBitmap);

  frmClickerFontFinderSettings.ShowModal;

  AFontFinderSettings.WinRect.Left := frmClickerFontFinderSettings.Left;
  AFontFinderSettings.WinRect.Top := frmClickerFontFinderSettings.Top;
  AFontFinderSettings.WinRect.Width := frmClickerFontFinderSettings.Width;
  AFontFinderSettings.WinRect.Height := frmClickerFontFinderSettings.Height;

  SetLength(AFontFinderSettings.ColWidths, frmClickerFontFinderSettings.vstFonts.Header.Columns.Count);
  for i := 0 to Length(AFontFinderSettings.ColWidths) - 1 do
    AFontFinderSettings.ColWidths[i] := frmClickerFontFinderSettings.vstFonts.Header.Columns.Items[i].Width;

  if frmClickerFontFinderSettings.Tag = 1 then
  begin
    Result := True;
    AFontFinderSettings.ListOfUsedFonts := frmClickerFontFinderSettings.FListOfUsedFonts.Text;
    AFontFinderSettings.MinFontSize := frmClickerFontFinderSettings.spnedtMinSize.Value;
    AFontFinderSettings.MaxFontSize := frmClickerFontFinderSettings.spnedtMaxSize.Value;
    AFontFinderSettings.ShowAllFonts := frmClickerFontFinderSettings.chkShowAllFonts.Checked;
  end;
end;


{ TfrmClickerFontFinderSettings }

procedure TfrmClickerFontFinderSettings.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  vstFonts.RootNodeCount := Screen.Fonts.Count;
  FListOfUsedFonts := TStringList.Create;
  FListOfHistogramDiffs := TStringList.Create;
  FLatestCheckedNode := nil;

  for i := 0 to Screen.Fonts.Count - 1 do
    FListOfHistogramDiffs.Add('...');

  tmrStartup.Enabled := True;
end;


procedure TfrmClickerFontFinderSettings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FListOfUsedFonts);
  FreeAndNil(FListOfHistogramDiffs);
end;


procedure TfrmClickerFontFinderSettings.lbeSearchChange(Sender: TObject);
begin
  RefreshDisplayFontSettings;
end;


procedure TfrmClickerFontFinderSettings.MenuItem_CheckAllClick(Sender: TObject);
begin
  SetCheckedStateToAll(True, False);
end;


procedure TfrmClickerFontFinderSettings.MenuItem_CheckAllSelectedClick(
  Sender: TObject);
begin
  SetCheckedStateToAll(True, True);
end;


const
  CTextFilesFilter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*';


procedure TfrmClickerFontFinderSettings.ImportListOfFonts(ClearExisting: Boolean);
var
  Node: PVirtualNode;
  ListOfSelected: TStringList;
  OpenDialog: TOpenDialog;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  ListOfSelected := TStringList.Create;
  try
    OpenDialog := TOpenDialog.Create(Self);
    try
      OpenDialog.Filter := CTextFilesFilter;
      if not OpenDialog.Execute then
        Exit;

      ListOfSelected.LoadFromFile(OpenDialog.FileName);
    finally
      OpenDialog.Free;
    end;

    if ClearExisting then
      vstFonts.ClearChecked;

    repeat
      if ListOfSelected.IndexOf(Screen.Fonts.Strings[Node^.Index]) > -1 then
        Node^.CheckState := csCheckedNormal;

      Node := Node^.NextSibling;
    until Node = nil;
  finally
    ListOfSelected.Free;
  end;

  RebuildListOfUsedFonts;
end;


procedure TfrmClickerFontFinderSettings.MenuItem_ImportListOfFonts_ReplaceExistingClick
  (Sender: TObject);
begin
  ImportListOfFonts(True);
end;


procedure TfrmClickerFontFinderSettings.MenuItem_ImportListOfFonts_MergeWithExistingClick
  (Sender: TObject);
begin
  ImportListOfFonts(False);
end;


procedure TfrmClickerFontFinderSettings.MenuItem_ExportListOfSelectedFontsClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  ListOfSelected: TStringList;
  SaveDialog: TSaveDialog;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  ListOfSelected := TStringList.Create;
  try
    repeat
      if Node^.CheckState = csCheckedNormal then
        ListOfSelected.Add(Screen.Fonts.Strings[Node^.Index]);

      Node := Node^.NextSibling;
    until Node = nil;

    SaveDialog := TSaveDialog.Create(Self);
    try
      SaveDialog.Filter := CTextFilesFilter;

      if SaveDialog.Execute then
      begin
        if ExtractFileExt(SaveDialog.FileName) = '' then
          SaveDialog.FileName := SaveDialog.FileName + '.txt';

        ListOfSelected.SaveToFile(SaveDialog.FileName);
      end;
    finally
      SaveDialog.Free;
    end;
  finally
    ListOfSelected.Free;
  end;
end;


procedure TfrmClickerFontFinderSettings.MenuItem_InvertCheckedStateClick(
  Sender: TObject);
begin
  InvertCheckedState;
end;


procedure TfrmClickerFontFinderSettings.MenuItem_InvertSelectionClick(
  Sender: TObject);
begin
  InvertSelection;
end;


procedure TfrmClickerFontFinderSettings.MenuItem_SelectAllCheckedByClearingSelectionClick
  (Sender: TObject);
begin
  SelectAllChecked(True);
end;


procedure TfrmClickerFontFinderSettings.MenuItem_SelectAllCheckedByPreservingSelectionClick(
  Sender: TObject);
begin
  SelectAllChecked(False);
end;


procedure TfrmClickerFontFinderSettings.MenuItem_UnCheckAllClick(Sender: TObject);
begin
  SetCheckedStateToAll(False, False);
end;


procedure TfrmClickerFontFinderSettings.MenuItem_UnCheckAllSelectedClick(
  Sender: TObject);
begin
  SetCheckedStateToAll(False, True);
end;


procedure TfrmClickerFontFinderSettings.spnedtPreviewSizeChange(Sender: TObject);
begin
  lblPreviewText.Font.Size := spnedtPreviewSize.Value;
end;


procedure TfrmClickerFontFinderSettings.GetHistogramFromPreviewBitmap(APreviewBitmap: TBitmap; var APreviewHist, APreviewHistColorCounts: TIntArr);
begin
  //WipeBitmap(APreviewBitmap, lblPreviewText.Width, lblPreviewText.Height);

  //APreviewBitmap.Canvas.Font.Name := lblPreviewText.Font.Name;
  //APreviewBitmap.Canvas.Font.Size := lblPreviewText.Font.Size;
  //APreviewBitmap.Canvas.Font.Color := lblPreviewText.Font.Color;
  //APreviewBitmap.Canvas.Font.Quality := lblPreviewText.Font.Quality;
  //APreviewBitmap.Canvas.Font.Style := lblPreviewText.Font.Style;
  //APreviewBitmap.Canvas.Brush.Color := lblPreviewText.Color;
  //
  //APreviewBitmap.Canvas.TextOut(0, 0, lblPreviewText.Caption);

  GetHistogram(APreviewBitmap, APreviewHist, APreviewHistColorCounts);
end;


function TfrmClickerFontFinderSettings.GetHistogramDiffByFontName(APreviewBitmap: TBitmap; AFontName: string; var APreviewHist, APreviewHistColorCounts: TIntArr): string;
var
  TempBitmap: TBitmap;
  TextSize: TSize;
  TempHist, TempHistColorCounts: TIntArr;
  HistDiff: Double;
begin
  TempBitmap := TBitmap.Create;
  try
    TempBitmap.Canvas.Font.Name := AFontName;
    TempBitmap.Canvas.Font.Size := lblPreviewText.Font.Size;
    TempBitmap.Canvas.Font.Color := lblPreviewText.Font.Color;
    TempBitmap.Canvas.Font.Quality := lblPreviewText.Font.Quality;
    TempBitmap.Canvas.Font.Style := lblPreviewText.Font.Style;
    TempBitmap.Canvas.Brush.Color := lblPreviewText.Color;
    TextSize := TempBitmap.Canvas.TextExtent(lblPreviewText.Caption);

    WipeBitmap(TempBitmap, TextSize.Width, TextSize.Height);
    TempBitmap.Canvas.TextOut(0, 0, lblPreviewText.Caption);
    SetLength(TempHist, 0);
    SetLength(TempHistColorCounts, 0);
    GetHistogram(TempBitmap, TempHist, TempHistColorCounts);

    FillInMissingHistogramPointsToBeCompared(APreviewHist, APreviewHistColorCounts, TempHist, TempHistColorCounts);
    HistDiff := CompareHistograms(APreviewHist, APreviewHistColorCounts, TempHist, TempHistColorCounts);
    Result := FloatToStrF(HistDiff, ffNumber, 15, 3);
  finally
    TempBitmap.Free;
  end;
end;


procedure TfrmClickerFontFinderSettings.ComputeHistogramDiffs;
var
  i: Integer;
  //PreviewBitmap: TBitmap;
  PreviewHist, PreviewHistColorCounts: TIntArr;
begin
  //PreviewBitmap := TBitmap.Create;
  try
    GetHistogramFromPreviewBitmap({PreviewBitmap} FPreviewBitmap, PreviewHist, PreviewHistColorCounts);

    for i := 0 to Screen.Fonts.Count - 1 do
      if FListOfUsedFonts.IndexOf(Screen.Fonts[i]) <> -1 then
      begin
        FListOfHistogramDiffs.Strings[i] := GetHistogramDiffByFontName({PreviewBitmap} FPreviewBitmap, Screen.Fonts[i], PreviewHist, PreviewHistColorCounts);
        Application.ProcessMessages;
      end
      else
        FListOfHistogramDiffs.Strings[i] := '-';
  finally
    //PreviewBitmap.Free;
  end;
end;


procedure TfrmClickerFontFinderSettings.tmrCheckedTimer(Sender: TObject);
begin
  tmrChecked.Enabled := False;
  HandleNodeChecked;
end;


procedure TfrmClickerFontFinderSettings.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;

  ComputeHistogramDiffs;
  RefreshDisplayFontSettings;
end;


procedure TfrmClickerFontFinderSettings.vstFontsBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if Column in [1, 2] then
  begin
    TargetCanvas.Pen.Color := clSilver;
    TargetCanvas.Brush.Color := lblPreviewText.Color;
    TargetCanvas.Rectangle(CellRect);
  end;
end;


procedure TfrmClickerFontFinderSettings.RefreshDisplayFontSettings;
var
  ShowAll, VisibleFromSearch: Boolean;
  Node: PVirtualNode;
  UpperCaseSearchedText: string;
begin
  ShowAll := chkShowAllFonts.Checked;
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  VisibleFromSearch := lbeSearch.Text = '';
  UpperCaseSearchedText := UpperCase(lbeSearch.Text);
  repeat
    vstFonts.IsVisible[Node] := (ShowAll or (Node^.CheckState = csCheckedNormal)) and
                                (VisibleFromSearch or (Pos(UpperCaseSearchedText, UpperCase(Screen.Fonts.Strings[Node^.Index])) > 0));
    Node := Node^.NextSibling;
  until Node = nil;

  vstFonts.Repaint;
end;


procedure TfrmClickerFontFinderSettings.chkShowAllFontsChange(Sender: TObject);
begin
  RefreshDisplayFontSettings;
end;


procedure TfrmClickerFontFinderSettings.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TfrmClickerFontFinderSettings.btnCancelClick(Sender: TObject);
begin
  Tag := 0;
  Close;
end;


procedure TfrmClickerFontFinderSettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;


procedure TfrmClickerFontFinderSettings.HandleNodeChecked;
var
  FontIsUsed: Boolean;
  FontIndexInUsedList: Integer;
  PreviewHist, PreviewHistColorCounts: TIntArr;
begin
  if FLatestCheckedNode = nil then
    Exit;

  FontIndexInUsedList := FListOfUsedFonts.IndexOf(Screen.Fonts.Strings[FLatestCheckedNode^.Index]);
  FontIsUsed := FontIndexInUsedList > -1;

  if FontIsUsed then
  begin
    if FLatestCheckedNode^.CheckState = csUncheckedNormal then //remove from list
      FListOfUsedFonts.Delete(FontIndexInUsedList);
  end
  else
    if FLatestCheckedNode^.CheckState = csCheckedNormal then  //add to list
    begin
      FListOfUsedFonts.Add(Screen.Fonts.Strings[FLatestCheckedNode^.Index]);

      GetHistogramFromPreviewBitmap({PreviewBitmap} FPreviewBitmap, PreviewHist, PreviewHistColorCounts);
      FListOfHistogramDiffs.Strings[FLatestCheckedNode^.Index] := GetHistogramDiffByFontName({PreviewBitmap} FPreviewBitmap, Screen.Fonts[FLatestCheckedNode^.Index], PreviewHist, PreviewHistColorCounts);
      vstFonts.InvalidateNode(FLatestCheckedNode);
    end;
end;


procedure TfrmClickerFontFinderSettings.RebuildListOfUsedFonts;
var
  Node: PVirtualNode;
  //PreviewBitmap: TBitmap;
  PreviewHist, PreviewHistColorCounts: TIntArr;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  GetHistogramFromPreviewBitmap({PreviewBitmap} FPreviewBitmap, PreviewHist, PreviewHistColorCounts);

  //PreviewBitmap := TBitmap.Create;
  try
    FListOfUsedFonts.Clear;
    repeat
      if Node^.CheckState = csCheckedNormal then  //add to list
      begin
        FListOfUsedFonts.Add(Screen.Fonts.Strings[Node^.Index]);

        if FListOfUsedFonts.IndexOf(Screen.Fonts[Node^.Index]) <> -1 then
          FListOfHistogramDiffs.Strings[Node^.Index] := GetHistogramDiffByFontName({PreviewBitmap} FPreviewBitmap, Screen.Fonts[Node^.Index], PreviewHist, PreviewHistColorCounts)
        else
          FListOfHistogramDiffs.Strings[Node^.Index] := '-';
      end;

      Node := Node^.NextSibling;
    until Node = nil;
  finally
    //PreviewBitmap.Free;
  end;

  vstFonts.Repaint;
end;


procedure TfrmClickerFontFinderSettings.vstFontsChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FLatestCheckedNode := Node;
  tmrChecked.Enabled := True;
end;


procedure TfrmClickerFontFinderSettings.vstFontsChecking(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
  var Allowed: Boolean);
begin
  Allowed := True;
end;


procedure TfrmClickerFontFinderSettings.vstFontsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
begin
  try
    case Column of
      0: CellText := Screen.Fonts.Strings[Node^.Index];
      1: CellText := lblPreviewText.Caption;
      2: CellText := 'The quick brown fox jumps over the lazy dog. (0123456789)';
      3: CellText := FListOfHistogramDiffs.Strings[Node^.Index];
    end;
  except
    CellText := 'bug'; //in case the list changes on the fly
  end;
end;


procedure TfrmClickerFontFinderSettings.SetVSTCheckStates;
var
  Node: PVirtualNode;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  repeat
    Node^.CheckType := ctCheckBox;   //this is also executed on vstFontsInitNode, but only for nodes which come into view

    try
      if FListOfUsedFonts.IndexOf(Screen.Fonts.Strings[Node^.Index]) > -1 then
        vstFonts.CheckState[Node] := csCheckedNormal
      else
        vstFonts.CheckState[Node] := csUncheckedNormal;
    except
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrmClickerFontFinderSettings.SetCheckedStateToAll(AState, ASelectedOnly: Boolean);
const
  CBoolToChecked: array[Boolean] of TCheckState = (csUncheckedNormal, csCheckedNormal);
var
  Node: PVirtualNode;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  repeat
    if not ASelectedOnly or (ASelectedOnly and vstFonts.Selected[Node]) then  //this should be simplified
      vstFonts.CheckState[Node] := CBoolToChecked[AState];

    Node := Node^.NextSibling;
  until Node = nil;

  RebuildListOfUsedFonts;;
end;


procedure TfrmClickerFontFinderSettings.SelectAllChecked(AClearSelection: Boolean);
var
  Node: PVirtualNode;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  if AClearSelection then
    vstFonts.ClearSelection;

  repeat
    if vstFonts.CheckState[Node] = csCheckedNormal then
      vstFonts.Selected[Node] := True;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrmClickerFontFinderSettings.InvertSelection;
var
  Node: PVirtualNode;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  repeat
    vstFonts.Selected[Node] := not vstFonts.Selected[Node];

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrmClickerFontFinderSettings.InvertCheckedState;
var
  Node: PVirtualNode;
begin
  Node := vstFonts.GetFirst;
  if Node = nil then
    Exit;

  repeat
    if vstFonts.CheckState[Node] = csCheckedNormal then
      vstFonts.CheckState[Node] := csUncheckedNormal
    else
      if vstFonts.CheckState[Node] = csUncheckedNormal then
        vstFonts.CheckState[Node] := csCheckedNormal;

    Node := Node^.NextSibling;
  until Node = nil;

  RebuildListOfUsedFonts;;
end;


procedure TfrmClickerFontFinderSettings.vstFontsInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
end;


procedure TfrmClickerFontFinderSettings.vstFontsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('A')) and (ssctrl in Shift) then
    vstFonts.SelectAll(True);
end;


procedure TfrmClickerFontFinderSettings.vstFontsPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  try
    case Column of
      0:
      begin
        TargetCanvas.Font.Name := 'default';
        TargetCanvas.Font.Size := 0;
      end;

      1, 2:
      begin
        TargetCanvas.Font.Name := Screen.Fonts.Strings[Node^.Index];
        TargetCanvas.Font.Size := lblPreviewText.Font.Size;
        TargetCanvas.Font.Style := lblPreviewText.Font.Style;
        TargetCanvas.Font.Color := lblPreviewText.Font.Color;
        TargetCanvas.Font.Quality := lblPreviewText.Font.Quality;
      end;
    end;
  except
     //in case the list changes on the fly
  end;
end;

end.

