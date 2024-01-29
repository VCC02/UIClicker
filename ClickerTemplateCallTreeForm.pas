{
    Copyright (C) 2023 VCC
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


unit ClickerTemplateCallTreeForm;

{$H+}
{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, VirtualTrees, ClickerUtils, IniFiles, ClickerIniFiles;

type
  TTemplateFile = record
    FilePath: string;
    FileName: string; //cached extracted name
    IconPath: string;
    ClkActions: TClkActionsRecArr;
    Loaded: Boolean;
    Highlighted: Boolean;
    ItemVisible: Boolean;  //used on searching for files
    IconIndex: Integer;
  end;

  PTemplateFile = ^TTemplateFile;

  TTemplateFileArr = array of PTemplateFile;


  TTmplDataRec = record
    Template: PTemplateFile;
  end;

  PTmplDataRec = ^TTmplDataRec;

  TOnGetFullTemplatesDir = function: string of object;


  { TfrmClickerTemplateCallTree }

  TfrmClickerTemplateCallTree = class(TForm)
    btnBrowse: TButton;
    btnGenerate: TButton;
    chkFullPathComparison: TCheckBox;
    chkDisplayFullPaths: TCheckBox;
    cmbSearchMode: TComboBox;
    edtSearch: TEdit;
    imglstTemplateIcons: TImageList;
    lblSearchMode: TLabel;
    lblSelectedPath: TLabel;
    lblCallTree: TLabel;
    lblInfoTemplates: TLabel;
    memTemplates: TMemo;
    MenuItem_Export: TMenuItem;
    N1: TMenuItem;
    MenuItem_CopySelectedFilePathToClipboard: TMenuItem;
    MenuItem_CopySelectedFileNameToClipboard: TMenuItem;
    pmTree: TPopupMenu;
    tmrSearch: TTimer;
    vstCallTree: TVirtualStringTree;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure chkDisplayFullPathsChange(Sender: TObject);
    procedure cmbSearchModeChange(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem_CopySelectedFileNameToClipboardClick(Sender: TObject);
    procedure MenuItem_CopySelectedFilePathToClipboardClick(Sender: TObject);
    procedure MenuItem_ExportClick(Sender: TObject);
    procedure tmrSearchTimer(Sender: TObject);
    procedure vstCallTreeBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstCallTreeClick(Sender: TObject);
    procedure vstCallTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstCallTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FTemplateFiles: TTemplateFileArr;
    FOnSetOpenDialogMultiSelect: TOnSetOpenDialogMultiSelect;
    FOnFileExists: TOnFileExists;
    FOnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate;
    FOnOpenDialogExecute: TOnOpenDialogExecute;
    FOnGetOpenDialogFileName: TOnGetOpenDialogFileName;
    FOnGetFullTemplatesDir: TOnGetFullTemplatesDir;
    FOnLoadBitmap: TOnLoadBitmap;

    procedure DoOnSetOpenDialogMultiSelect;
    function DoOnFileExists(const AFileName: string): Boolean;
    function DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
    function DoOnOpenDialogExecute(AFilter: string): Boolean;
    function DoOnGetOpenDialogFileName: string;
    function DoOnGetFullTemplatesDir: string;
    function DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;

    procedure ResetItemVisibleFlagOnAllFiles;
    procedure MarkAllParentNodesAsVisible(ACurrentNode: PVirtualNode);

    procedure ClearContent;
    procedure LoadTemplate(AFileName: string; var ACustomClkActions: TClkActionsRecArr; var ATemplateIconPath: string);
    procedure LoadAllTemplates;
    procedure AddTopLevelNodes;
    function GetTemplateIndexByPath(ASearchedPath: string; AFullPathCmp: Boolean): Integer;
    procedure DeleteTopLevelNodeByPath(AFilePath: string);
    function TemplatePathFoundInParentNodes(Node: PVirtualNode; ASearchedPath: string): Boolean;
    procedure InsertCalledTemplates(Node: PVirtualNode; AFullPathCmp: Boolean; AListOfTopLevelNodesToDelete: TStringList);
    procedure GenerateCallTree;
    procedure LoadTemplateIcons;
    function GetCallPath(Node: PVirtualNode): string;
  public
    procedure LoadSettings(AIni: TMemIniFile);
    procedure SaveSettings(AIni: TMemIniFile);

    property OnSetOpenDialogMultiSelect: TOnSetOpenDialogMultiSelect write FOnSetOpenDialogMultiSelect;
    property OnFileExists: TOnFileExists write FOnFileExists;
    property OnTClkIniReadonlyFileCreate: TOnTClkIniReadonlyFileCreate write FOnTClkIniReadonlyFileCreate;
    property OnOpenDialogExecute: TOnOpenDialogExecute write FOnOpenDialogExecute;
    property OnGetOpenDialogFileName: TOnGetOpenDialogFileName write FOnGetOpenDialogFileName;
    property OnGetFullTemplatesDir: TOnGetFullTemplatesDir write FOnGetFullTemplatesDir;
    property OnLoadBitmap: TOnLoadBitmap write FOnLoadBitmap;
  end;

var
  frmClickerTemplateCallTree: TfrmClickerTemplateCallTree;

implementation

{$R *.frm}

uses
  ClickerTemplates, Clipbrd;


{ TfrmClickerTemplateCallTree }


procedure TfrmClickerTemplateCallTree.LoadSettings(AIni: TMemIniFile);
begin
  Left := AIni.ReadInteger('CallTreeWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
  Top := AIni.ReadInteger('CallTreeWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
  Width := AIni.ReadInteger('CallTreeWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
  Height := AIni.ReadInteger('CallTreeWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));
end;


procedure TfrmClickerTemplateCallTree.SaveSettings(AIni: TMemIniFile);
begin
  AIni.WriteInteger('CallTreeWindow', 'Left', Min(Left, Screen.DesktopWidth - 60));
  AIni.WriteInteger('CallTreeWindow', 'Top', Min(Top, Screen.DesktopHeight - 60));
  AIni.WriteInteger('CallTreeWindow', 'Width', Min(Width, Screen.DesktopWidth - 40));
  AIni.WriteInteger('CallTreeWindow', 'Height', Min(Height, Screen.DesktopHeight - 40));
end;


procedure TfrmClickerTemplateCallTree.DoOnSetOpenDialogMultiSelect;
begin
  if not Assigned(FOnSetOpenDialogMultiSelect) then
    raise Exception.Create('OnSetOpenDialogMultiSelect is not assigned.')
  else
    FOnSetOpenDialogMultiSelect;
end;


function TfrmClickerTemplateCallTree.DoOnFileExists(const AFileName: string): Boolean;
begin
  if not Assigned(FOnFileExists) then
    raise Exception.Create('OnFileExists is not assigned.')
  else
    Result := FOnFileExists(AFileName);
end;


function TfrmClickerTemplateCallTree.DoOnTClkIniReadonlyFileCreate(AFileName: string): TClkIniReadonlyFile;
begin
  if not Assigned(FOnTClkIniReadonlyFileCreate) then
    raise Exception.Create('OnTClkIniReadonlyFileCreate is not assigned.')
  else
    Result := FOnTClkIniReadonlyFileCreate(AFileName);
end;


function TfrmClickerTemplateCallTree.DoOnOpenDialogExecute(AFilter: string): Boolean;
begin
  if not Assigned(FOnOpenDialogExecute) then
    raise Exception.Create('OnOpenDialogExecute is not assigned.')
  else
    Result := FOnOpenDialogExecute(AFilter);
end;


function TfrmClickerTemplateCallTree.DoOnGetOpenDialogFileName: string;
begin
  if not Assigned(FOnGetOpenDialogFileName) then
    raise Exception.Create('OnGetOpenDialogFileName is not assigned.')
  else
    Result := FOnGetOpenDialogFileName;
end;


function TfrmClickerTemplateCallTree.DoOnGetFullTemplatesDir: string;
begin
  if not Assigned(FOnGetFullTemplatesDir) then
    raise Exception.Create('OnGetFullTemplatesDir is not assigned.')
  else
    Result := FOnGetFullTemplatesDir;
end;


function TfrmClickerTemplateCallTree.DoOnLoadBitmap(ABitmap: TBitmap; AFileName: string): Boolean;
begin
  if not Assigned(FOnLoadBitmap) then
    raise Exception.Create('OnLoadBitmap is not assigned.')
  else
    Result := FOnLoadBitmap(ABitmap, AFileName);
end;


procedure TfrmClickerTemplateCallTree.ClearContent;
var
  i, j: Integer;
begin
  for i := 0 to Length(FTemplateFiles) - 1 do
    if FTemplateFiles[i] <> nil then
    begin
      for j := 0 to Length(FTemplateFiles[i]^.ClkActions) - 1 do
        SetLength(FTemplateFiles[i]^.ClkActions[j].FindControlOptions.MatchBitmapText, 0);

      SetLength(FTemplateFiles[i]^.ClkActions, 0);
      Dispose(FTemplateFiles[i]);
      FTemplateFiles[i] := nil;
    end;

  SetLength(FTemplateFiles, 0);
end;


procedure TfrmClickerTemplateCallTree.FormCreate(Sender: TObject);
begin
  FOnSetOpenDialogMultiSelect := nil;
  FOnFileExists := nil;
  FOnTClkIniReadonlyFileCreate := nil;
  FOnOpenDialogExecute := nil;
  FOnGetOpenDialogFileName := nil;
  FOnGetFullTemplatesDir := nil;
  FOnLoadBitmap := nil;

  SetLength(FTemplateFiles, 0);
  vstCallTree.NodeDataSize := SizeOf(TTmplDataRec);
end;


procedure TfrmClickerTemplateCallTree.FormDestroy(Sender: TObject);
begin
  vstCallTree.BeginUpdate;
  try
    vstCallTree.Clear;
    vstCallTree.Repaint;
    ClearContent;
  finally
    vstCallTree.EndUpdate;
  end;
end;


procedure TfrmClickerTemplateCallTree.MenuItem_CopySelectedFileNameToClipboardClick
  (Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PTmplDataRec;
begin
  Node := vstCallTree.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select an item to be copied to clipboard.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  NodeData := vstCallTree.GetNodeData(Node);
  Clipboard.AsText := NodeData^.Template^.FileName;
end;


procedure TfrmClickerTemplateCallTree.MenuItem_CopySelectedFilePathToClipboardClick
  (Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PTmplDataRec;
begin
  Node := vstCallTree.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'Please select an item to be copied to clipboard.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  NodeData := vstCallTree.GetNodeData(Node);
  Clipboard.AsText := NodeData^.Template^.FilePath;
end;


procedure TfrmClickerTemplateCallTree.MenuItem_ExportClick(Sender: TObject);
function MakeBlanks(ACount: Integer): string;
begin
  SetLength(Result, ACount);
  FillChar(Result[1], ACount, ' ');
end;

var
  Content: TStringList;
  TempSaveDialog: TSaveDialog;
  Node: PVirtualNode;
  NodeData: PTmplDataRec;
  Blanks: string;
  i: Integer;
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
      Node := vstCallTree.GetFirst;
      try
        if Node = nil then
          Exit;

        repeat
          NodeData := vstCallTree.GetNodeData(Node);
          Blanks := MakeBlanks(vstCallTree.GetNodeLevel(Node) shl 2);

          if NodeData = nil then
            Content.Add(Blanks + 'Data_bug')
          else
          begin
            Content.Add(Blanks + 'Path: "' + NodeData^.Template^.FileName + '"');

            for i := 0 to Length(NodeData^.Template^.ClkActions) - 1 do
            begin
              if NodeData^.Template^.ClkActions[i].ActionOptions.Action = acFindSubControl then
                Content.Add(Blanks + '    Bmps: "' + FastReplace_ReturnToCSV(NodeData^.Template^.ClkActions[i].FindControlOptions.MatchBitmapFiles) + '"');
            end;
          end;

          Node := vstCallTree.GetNext(Node);
        until Node = nil;
      finally
        Content.SaveToFile(TempSaveDialog.FileName);
      end;
    finally
      Content.Free;
    end;
  finally
    TempSaveDialog.Free;
  end;
end;


procedure TfrmClickerTemplateCallTree.ResetItemVisibleFlagOnAllFiles;
var
  Node: PVirtualNode;
  NodeData: PTmplDataRec;
begin
  Node := vstCallTree.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstCallTree.GetNodeData(Node);
    NodeData^.Template^.ItemVisible := False;

    Node := vstCallTree.GetNext(Node);
  until Node = nil;
end;


procedure TfrmClickerTemplateCallTree.MarkAllParentNodesAsVisible(ACurrentNode: PVirtualNode);
var
  NodeData: PTmplDataRec;
  Level: Integer;
begin
  if ACurrentNode = nil then
    Exit;

  while ACurrentNode^.Parent <> nil do  //unfortunately (for this algorithm), nodes at level 0 do not have a nil parent
  begin
    Level := vstCallTree.GetNodeLevel(ACurrentNode);
    if Level > 0 then
      ACurrentNode := ACurrentNode^.Parent;

    NodeData := vstCallTree.GetNodeData(ACurrentNode);
    NodeData^.Template^.ItemVisible := True;

    if Level = 0 then
      Break;
  end;
end;


procedure TfrmClickerTemplateCallTree.tmrSearchTimer(Sender: TObject);
type
  TSearchMode = (smHideItems, smHighlightItems);
var
  Node: PVirtualNode;
  NodeData: PTmplDataRec;
  Fnm, SearchString: string;
  SearchMode: TSearchMode;
  Visibility: Boolean;
begin
  tmrSearch.Enabled := False;

  //This is not an efficient algorithm, but it should do the job for most projects.
  vstCallTree.BeginUpdate;
  try
    Node := vstCallTree.GetLast;
    if Node = nil then
      Exit;

    ResetItemVisibleFlagOnAllFiles;

    SearchString := UpperCase(edtSearch.Text);
    SearchMode := TSearchMode(cmbSearchMode.ItemIndex);

    repeat
      NodeData := vstCallTree.GetNodeData(Node);

      if chkDisplayFullPaths.Checked then
        Fnm := NodeData^.Template^.FilePath
      else
        Fnm := NodeData^.Template^.FileName;

      Fnm := UpperCase(Fnm);

      Visibility := (SearchMode = smHighlightItems) or (SearchString = '') or (Pos(SearchString, Fnm) > 0);
      vstCallTree.IsVisible[Node] := Visibility or NodeData^.Template^.ItemVisible;
      NodeData^.Template^.Highlighted := (SearchMode = smHighlightItems) and (Pos(SearchString, Fnm) > 0);

      if Visibility then //mark all parent nodes as visible, so they won't be hidden later
        MarkAllParentNodesAsVisible(Node);

      Node := vstCallTree.GetPrevious(Node);
    until Node = nil;
  finally
    vstCallTree.EndUpdate;
    vstCallTree.UpdateScrollBars(True);
  end;
end;


procedure TfrmClickerTemplateCallTree.vstCallTreeBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  NodeData: PTmplDataRec;
begin
  try
    NodeData := vstCallTree.GetNodeData(Node);
    if NodeData^.Template^.Highlighted then
    begin
      TargetCanvas.Pen.Color := clYellow;
      TargetCanvas.Brush.Color := clYellow;
      TargetCanvas.Rectangle(CellRect.Left + 1, CellRect.Top + 1, CellRect.Right - 1, CellRect.Bottom  - 1);
    end;
  except
  end;
end;


procedure TfrmClickerTemplateCallTree.vstCallTreeClick(Sender: TObject);
begin
  lblSelectedPath.Caption := GetCallPath(vstCallTree.GetFirstSelected);
end;


procedure TfrmClickerTemplateCallTree.vstCallTreeGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PTmplDataRec;
begin
  NodeData := vstCallTree.GetNodeData(Node);
  if NodeData <> nil then
    ImageIndex := NodeData^.Template^.IconIndex;
end;


procedure TfrmClickerTemplateCallTree.vstCallTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  NodeData: PTmplDataRec;
begin
  try
    NodeData := vstCallTree.GetNodeData(Node);

    case Column of
      0:
      begin
        if chkDisplayFullPaths.Checked then  //the Checked value can be cached if BeforePaint
          CellText := NodeData^.Template^.FilePath
        else
          CellText := NodeData^.Template^.FileName;
      end;

      1:
        CellText := NodeData^.Template^.IconPath;
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrmClickerTemplateCallTree.LoadTemplate(AFileName: string; var ACustomClkActions: TClkActionsRecArr; var ATemplateIconPath: string);
var
  Ini: TClkIniReadonlyFile;
  FormatVersion: string;
  DummyNotes: string;
begin
  Ini := DoOnTClkIniReadonlyFileCreate(AFileName);
  try
    SetLength(ACustomClkActions, Ini.ReadInteger('Actions', 'Count', 0));
    FormatVersion := Ini.ReadString('Actions', 'Version', '1');
    ATemplateIconPath := '';

    if FormatVersion = '1' then
      LoadTemplateToCustomActions_V1(Ini, ACustomClkActions)
    else
      if FormatVersion = '2' then
        LoadTemplateToCustomActions_V2(Ini, ACustomClkActions, DummyNotes, ATemplateIconPath)
      else
        SetLength(ACustomClkActions, 0);
  finally
    Ini.Free;
  end;
end;


procedure TfrmClickerTemplateCallTree.LoadAllTemplates;
var
  i: Integer;
  Fnm, IconPath: string;
begin
  if Length(FTemplateFiles) > 0 then
    ClearContent; //deallocate all structures, before adding new ones

  SetLength(FTemplateFiles, memTemplates.Lines.Count);
  for i := 0 to memTemplates.Lines.Count - 1 do
  begin
    Fnm := StringReplace(memTemplates.Lines.Strings[i], '"', '', [rfReplaceAll]);
    New(FTemplateFiles[i]);

    FTemplateFiles[i]^.FilePath := Fnm;
    FTemplateFiles[i]^.FileName := ExtractFileName(Fnm);
    FTemplateFiles[i]^.Highlighted := False;

    if DoOnFileExists(Fnm) then
    begin
      LoadTemplate(Fnm, FTemplateFiles[i]^.ClkActions, IconPath);
      FTemplateFiles[i]^.Loaded := Length(FTemplateFiles[i]^.ClkActions) > 0;
      FTemplateFiles[i]^.IconPath := IconPath;
    end
    else
    begin
      FTemplateFiles[i]^.Loaded := False;
      FTemplateFiles[i]^.IconPath := '';
    end;
  end;
end;


procedure TfrmClickerTemplateCallTree.AddTopLevelNodes;
var
  i: Integer;
  NodeData: PTmplDataRec;
  Node: PVirtualNode;
begin
  for i := 0 to Length(FTemplateFiles) - 1 do
  begin
    Node := vstCallTree.InsertNode(vstCallTree.RootNode, amInsertAfter);
    NodeData := vstCallTree.GetNodeData(Node);
    if NodeData <> nil then
      NodeData^.Template := FTemplateFiles[i]; //since FTemplateFiles[i] is a pointer, if the array is reallocated, these pointers can keep their values
  end;
end;


function TfrmClickerTemplateCallTree.GetTemplateIndexByPath(ASearchedPath: string; AFullPathCmp: Boolean): Integer;
var
  i: Integer;
  UpperCaseFilePath: string;
  UpperCaseSearchedPath: string;
begin
  Result := -1;

  UpperCaseSearchedPath := UpperCase(ASearchedPath);
  for i := 0 to Length(FTemplateFiles) - 1 do
  begin
    UpperCaseFilePath := UpperCase(FTemplateFiles[i]^.FilePath);

    if (AFullPathCmp and (UpperCaseSearchedPath = UpperCaseFilePath)) or
       (not AFullPathCmp and (ExtractFileName(UpperCaseSearchedPath) = ExtractFileName(UpperCaseFilePath))) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;


procedure TfrmClickerTemplateCallTree.DeleteTopLevelNodeByPath(AFilePath: string);
var
  Node, NextNode: PVirtualNode;
  NodeData: PTmplDataRec;
begin
  Node := vstCallTree.GetFirst;
  if Node = nil then
    Exit;

  AFilePath := UpperCase(AFilePath);

  repeat  //Iterating top-down, because GetLast may be a node of Level > 0. That would result in deleting other nodes, instead of top-level.
    NodeData := vstCallTree.GetNodeData(Node);
    NextNode := Node^.NextSibling;

    if NodeData <> nil then
      if UpperCase(NodeData^.Template^.FilePath) = AFilePath then
        vstCallTree.DeleteNode(Node, True);

    Node := NextNode;
  until Node = nil;
end;


function TfrmClickerTemplateCallTree.TemplatePathFoundInParentNodes(Node: PVirtualNode; ASearchedPath: string): Boolean;
var
  NodeData: PTmplDataRec;
  RootParent: PVirtualNode;
begin
  Result := False;
  if Node = nil then
    Exit;

  RootParent := vstCallTree.RootNode^.Parent;  //not sure what this points to, but without restoring this, the tree throws AV when cleared
  try
    vstCallTree.RootNode^.Parent := nil; //without this, the "Node := Node^.Parent" line will go past the root node, i.e. outside the tree, resulting in AV

    ASearchedPath := UpperCase(ASearchedPath);
    repeat
      NodeData := vstCallTree.GetNodeData(Node);

      if (NodeData <> nil) and (UpperCase(NodeData^.Template^.FilePath) = ASearchedPath) then
      begin
        Result := True;
        Exit;
      end;

      Node := Node^.Parent;
    until Node = nil;
  finally
    vstCallTree.RootNode^.Parent := RootParent;
  end;
end;


procedure TfrmClickerTemplateCallTree.InsertCalledTemplates(Node: PVirtualNode; AFullPathCmp: Boolean; AListOfTopLevelNodesToDelete: TStringList);
var
  NodeData: PTmplDataRec;
  i: Integer;
  NewNode: PVirtualNode;
  NewNodeData: PTmplDataRec;
  IndexOfCalledTemplate: Integer;
  ListOfCallActions: TStringList; //paths  used to prevent duplicates, in case a template is called multiple times from another template
begin
  NodeData := vstCallTree.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  ListOfCallActions := TStringList.Create;
  try
    for i := 0 to Length(NodeData^.Template^.ClkActions) - 1 do
      if NodeData^.Template^.ClkActions[i].ActionOptions.Action = acCallTemplate then
      begin
        IndexOfCalledTemplate := GetTemplateIndexByPath(NodeData^.Template^.ClkActions[i].CallTemplateOptions.TemplateFileName, AFullPathCmp);

        if (IndexOfCalledTemplate > -1) and
          (ListOfCallActions.IndexOf(UpperCase(FTemplateFiles[IndexOfCalledTemplate]^.FilePath)) = -1) then
        begin
          NewNode := vstCallTree.InsertNode(Node, amAddChildLast);
          NewNodeData := vstCallTree.GetNodeData(NewNode);
          ListOfCallActions.Add(UpperCase(FTemplateFiles[IndexOfCalledTemplate]^.FilePath));

          if NewNodeData <> nil then
          begin
            NewNodeData^.Template := FTemplateFiles[IndexOfCalledTemplate];  //copy the pointer
            if vstCallTree.GetNodeLevel(NewNode) < 30 then  //safety measure, to prevent infinite recursion, in case TemplatePathFoundInParentNodes has bugs
              if not TemplatePathFoundInParentNodes(NewNode^.Parent, FTemplateFiles[IndexOfCalledTemplate]^.FilePath) then
                InsertCalledTemplates(NewNode, AFullPathCmp, AListOfTopLevelNodesToDelete);  //recursion here

            vstCallTree.FullExpand(Node);  //parent
            AListOfTopLevelNodesToDelete.Add(FTemplateFiles[IndexOfCalledTemplate]^.FilePath);   //better use a list of nodes to be deleted, instead of deleting here, while the loop is running in GenerateCallTree
          end;
        end;
      end;
  finally
    ListOfCallActions.Free;
  end;
end;


procedure TfrmClickerTemplateCallTree.GenerateCallTree;
var
  Node: PVirtualNode;
  FullPathCmp: Boolean;
  ListOfTopLevelNodesToDelete: TStringList;
  i: Integer;
begin
  AddTopLevelNodes;

  Node := vstCallTree.GetFirst;
  if Node = nil then
    Exit;

  ListOfTopLevelNodesToDelete := TStringList.Create;
  try
    FullPathCmp := chkFullPathComparison.Checked;
    repeat
      InsertCalledTemplates(Node, FullPathCmp, ListOfTopLevelNodesToDelete);
      Node := vstCallTree.GetNextSibling(Node);
    until Node = nil;

    //bug:  deleting notes causes the tree scrollbars to be hidden (some bad computations)
    for i := 0 to ListOfTopLevelNodesToDelete.Count - 1 do
      DeleteTopLevelNodeByPath(ListOfTopLevelNodesToDelete.Strings[i]);       //bug: if only two templates are in the list, and each calls the other (infinite indirect recursion), than both of them end up deleted, resulting in an empty tree
  finally
    ListOfTopLevelNodesToDelete.Free;
  end;
end;


procedure TfrmClickerTemplateCallTree.LoadTemplateIcons;
var
  Node: PVirtualNode;
  ResolvedPath: string;
  NodeData: PTmplDataRec;
  Bmp: TBitmap;
  FullTemplatesDir: string;
  Idx: Integer;
begin
  imglstTemplateIcons.Clear;

  Node := vstCallTree.GetFirst;
  if Node = nil then
    Exit;

  FullTemplatesDir := DoOnGetFullTemplatesDir;
  Idx := -1;
  repeat
    NodeData := vstCallTree.GetNodeData(Node);
    if NodeData <> nil then
    begin
      ResolvedPath := StringReplace(NodeData^.Template^.IconPath, '$TemplateDir$', FullTemplatesDir, [rfReplaceAll]);
      ResolvedPath := StringReplace(ResolvedPath, '$SelfTemplateDir$', ExtractFileDir(NodeData^.Template^.FilePath), [rfReplaceAll]);
      ResolvedPath := StringReplace(ResolvedPath, '$AppDir$', ExtractFileDir(ParamStr(0)), [rfReplaceAll]);

      Bmp := TBitmap.Create;
      try
        try
          Bmp.Width := 16;
          Bmp.Height := 16;
          Bmp.Canvas.Brush.Color := clWhite;
          Bmp.Canvas.Pen.Color := clLime;
          Bmp.Canvas.Rectangle(0, 0, Bmp.Width, Bmp.Height);

          Inc(Idx);
          NodeData^.Template^.IconIndex := Idx;
          DoOnLoadBitmap(Bmp, ResolvedPath);

          //Bmp.Canvas.Font.Color := clBlack;
          //Bmp.Canvas.TextOut(0, 0, IntToStr(Idx));   //for debugging

          imglstTemplateIcons.AddMasked(Bmp, clFuchsia);
        except
          on E: Exception do
          begin
            Bmp.Canvas.Font.Color := clRed;
            Bmp.Canvas.Brush.Color := clWhite;
            Bmp.Canvas.TextOut(10, 10, 'Error');
          end;
        end;
      finally
        Bmp.Free;
      end;
    end;

    Node := vstCallTree.GetNext(Node);
  until Node = nil;
end;


function TfrmClickerTemplateCallTree.GetCallPath(Node: PVirtualNode): string;
var
  NodeData: PTmplDataRec;
  RootParent: PVirtualNode;
begin
  Result := '';
  if Node = nil then
    Exit;

  RootParent := vstCallTree.RootNode^.Parent;  //not sure what this points to, but without restoring this, the tree throws AV when cleared
  try
    vstCallTree.RootNode^.Parent := nil; //without this, the "Node := Node^.Parent" line will go past the root node, i.e. outside the tree, resulting in AV

    repeat
      NodeData := vstCallTree.GetNodeData(Node);

      if NodeData <> nil then
        Result := NodeData^.Template^.FileName + ' > ' + Result;

      Node := Node^.Parent;
    until Node = nil;
  finally
    vstCallTree.RootNode^.Parent := RootParent;
  end;

  if Length(Result) > 0 then
    Delete(Result, Length(Result) - 2, 3); // ' > '
end;


procedure TfrmClickerTemplateCallTree.btnGenerateClick(Sender: TObject);
var
  Initial: Integer;
begin
  LoadAllTemplates;

  vstCallTree.BeginUpdate;
  try
    vstCallTree.Clear;
  finally
    vstCallTree.EndUpdate;
  end;

  vstCallTree.BeginUpdate;
  Initial := vstCallTree.Height;
  vstCallTree.Height := 3000;
  vstCallTree.UpdateScrollBars(True);
  try
    GenerateCallTree;
  finally
    vstCallTree.EndUpdate;
    vstCallTree.Height := Initial;
  end;

  vstCallTree.UpdateScrollBars(True);   //this may not be needed, since the search timer does the same
  vstCallTree.Repaint;
  lblSelectedPath.Caption := '';
  LoadTemplateIcons;

  tmrSearch.Enabled := True;
end;


procedure TfrmClickerTemplateCallTree.chkDisplayFullPathsChange(Sender: TObject);
begin
  vstCallTree.Repaint;
  tmrSearch.Enabled := True;
end;


procedure TfrmClickerTemplateCallTree.cmbSearchModeChange(Sender: TObject);
begin
  tmrSearch.Enabled := True;
end;


procedure TfrmClickerTemplateCallTree.edtSearchChange(Sender: TObject);
begin
  tmrSearch.Enabled := True;
end;


procedure TfrmClickerTemplateCallTree.btnBrowseClick(Sender: TObject);
begin
  DoOnSetOpenDialogMultiSelect;
  if not DoOnOpenDialogExecute(CTemplateDialogFilter) then
    Exit;

  memTemplates.Lines.Text := DoOnGetOpenDialogFileName;
end;

end.

