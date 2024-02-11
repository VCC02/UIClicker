{
    Copyright (C) 2024 VCC
    creation date: 22 Jan 2024
    initial release date: 23 Jan 2024

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


unit ClickerPluginFrame;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, StdCtrls,
  VirtualTrees, Graphics, ClickerUtils;

type
  TOnPluginDbgStop = procedure of object;
  TOnPluginDbgContinueAll = procedure of object;
  TOnPluginDbgStepOver = procedure of object;
  TOnPluginDbgRequestLineNumber = function(out ALineContent: string): Integer of object;

  TDbgSourceFile = record
    Fnm: string;
    Content: TStringList;  //this field can be removed if/when the vst is replaced by a SynEdit
    DbgLines: TStringList;
  end;

  TDbgSourceFileArr = array of TDbgSourceFile;

  TBreakPointsArr = array of TStringList;  //this does not come from debug symbols file, it is updated by UIClicker when users click on DbgLines arrows

  { TfrClickerPlugin }

  TfrClickerPlugin = class(TFrame)
    imglstPluginDebugging: TImageList;
    imgPlugin: TImage;
    lblMsg: TLabel;
    lblPluginDebugging: TLabel;
    spdbtnContinuePlayingAll: TSpeedButton;
    spdbtnStepOver: TSpeedButton;
    spdbtnScrollToCurrentLine: TSpeedButton;
    spdbtnStopPlaying: TSpeedButton;
    spdbtnGoToPrevDbgPoint: TSpeedButton;
    spdbtnGoToNextDbgPoint: TSpeedButton;
    tmrRequestLineNumber: TTimer;
    vstPluginDebugging: TVirtualStringTree;
    procedure spdbtnContinuePlayingAllClick(Sender: TObject);
    procedure spdbtnGoToNextDbgPointClick(Sender: TObject);
    procedure spdbtnGoToPrevDbgPointClick(Sender: TObject);
    procedure spdbtnScrollToCurrentLineClick(Sender: TObject);
    procedure spdbtnStepOverClick(Sender: TObject);
    procedure spdbtnStopPlayingClick(Sender: TObject);
    procedure tmrRequestLineNumberTimer(Sender: TObject);
    procedure vstPluginDebuggingBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstPluginDebuggingGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: boolean; var ImageIndex: integer);
    procedure vstPluginDebuggingGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstPluginDebuggingMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstPluginDebuggingPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    FSourceFiles: TDbgSourceFileArr;
    FBreakPointsArr: TBreakPointsArr;  //should be kept in sync with FSourceFiles
    FSelectedLine: Integer;
    FLineIntoView: Integer; //similar to FSelectedLine, but set by line navigation buttons
    FSelectedSourceFileIndex: Integer;
    FAllDbgLinesFromSelectedFile: TIntArr;
    FCachedLineContent: string;
    FDbgSymFnm: string;

    FHitTestInfo: THitInfo;

    FOnPluginDbgStop: TOnPluginDbgStop;
    FOnPluginDbgContinueAll: TOnPluginDbgContinueAll;
    FOnPluginDbgStepOver: TOnPluginDbgStepOver;
    FOnPluginDbgRequestLineNumber: TOnPluginDbgRequestLineNumber;

    function GetSelectedLine: string;

    procedure ExtractSelectedSourceFileAndLineFromDbgName(ALineContent: string; var ASelectedLine, ASelectedSourceFileIndex: Integer; var AAllDbgLinesFromSelectedFile: TIntArr);
    procedure UpdateSelectedSourceFileAndLineFromDbgName(ALineContent: string);
    procedure ClearSourceFilesArr;
    procedure ClearBreakPointsArr;

    function GetIndexOfDbgLine(ALineIndex: Integer): Integer;
    function LineIndexIsADebugPoint(AIndex: Integer): Boolean;
    procedure UpdateDebugSymbolsFileWithBreakPoints;

    procedure DoOnPluginDbgStop;
    procedure DoOnPluginDbgContinueAll;
    procedure DoOnPluginDbgStepOver;
    function DoOnPluginDbgRequestLineNumber(ALineContent: string): Integer;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadDebugSymbols(ADbgSymFnm: string);
    procedure SelectLineByContent(ALineContent: string);
    function IsAtBreakPoint(ADebugPoint: string): Boolean;
    procedure DoneDebuging; //clears vst and disables buttons

    procedure EnableRequestLineNumber;
    procedure DisableRequestLineNumber;

    property SelectedLine: string read GetSelectedLine;

    property OnPluginDbgStop: TOnPluginDbgStop write FOnPluginDbgStop;
    property OnPluginDbgContinueAll: TOnPluginDbgContinueAll write FOnPluginDbgContinueAll;
    property OnPluginDbgStepOver: TOnPluginDbgStepOver write FOnPluginDbgStepOver;
    property OnPluginDbgRequestLineNumber: TOnPluginDbgRequestLineNumber write FOnPluginDbgRequestLineNumber;
  end;

implementation

{$R *.frm}


uses
  ClickerExtraUtils, ClickerIniFiles, IniFiles, ClickerActionPlugins;


constructor TfrClickerPlugin.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetLength(FSourceFiles, 0);
  SetLength(FBreakPointsArr, 0);
  SetLength(FAllDbgLinesFromSelectedFile, 0);

  FOnPluginDbgStop := nil;
  FOnPluginDbgContinueAll := nil;
  FOnPluginDbgStepOver := nil;

  FSelectedLine := -1;
  FLineIntoView := -1;
  FSelectedSourceFileIndex := -1;
  FCachedLineContent := '';
  FDbgSymFnm := '';
end;


destructor TfrClickerPlugin.Destroy;
begin
  ClearSourceFilesArr;
  SetLength(FAllDbgLinesFromSelectedFile, 0);

  ClearBreakPointsArr;

  inherited Destroy;
end;


procedure TfrClickerPlugin.ClearSourceFilesArr;
var
  i: Integer;
begin
  for i := 0 to Length(FSourceFiles) - 1 do
  begin
    FSourceFiles[i].Content.Free;
    FSourceFiles[i].DbgLines.Free;

    //Do not free FBreakPointsArr[i] items here!
  end;

  SetLength(FSourceFiles, 0);
  //Do not set length of FBreakPointsArr here!
end;


procedure TfrClickerPlugin.ClearBreakPointsArr;
var
  i: Integer;
begin
  for i := 0 to Length(FBreakPointsArr) - 1 do
    FBreakPointsArr[i].Free;

  SetLength(FBreakPointsArr, 0);
end;


function TfrClickerPlugin.GetSelectedLine: string;
begin
  Result := IntToStr(FSelectedLine) + '=' + FCachedLineContent;
end;


function TfrClickerPlugin.GetIndexOfDbgLine(ALineIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FAllDbgLinesFromSelectedFile) - 1 do
    if FAllDbgLinesFromSelectedFile[i] = ALineIndex then
    begin
      Result := i;
      Exit;
    end;
end;


function TfrClickerPlugin.LineIndexIsADebugPoint(AIndex: Integer): Boolean;
begin
  Result := GetIndexOfDbgLine(AIndex) > -1;
end;


procedure TfrClickerPlugin.vstPluginDebuggingGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  try
    case Column of
      0:
        CellText := IntToStr(Integer(Node^.Index) + 1);

      1:
        if FSelectedSourceFileIndex < Length(FSourceFiles) then
          CellText := FSourceFiles[FSelectedSourceFileIndex].Content.Strings[Node^.Index];
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerPlugin.vstPluginDebuggingMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BreakpointLine: Integer;
  BreakpointLineStr: string;
  IndexOfBreakpointInList: Integer;
begin
  vstPluginDebugging.GetHitTestInfoAt(X, Y, True, FHitTestInfo);
  if FHitTestInfo.HitNode <> nil then
    if (FHitTestInfo.HitColumn = 0) and (X < imglstPluginDebugging.Width + Integer(vstPluginDebugging.Indent) + 12) then
    begin
      BreakpointLine := FHitTestInfo.HitNode.Index;
      BreakpointLineStr := IntToStr(BreakpointLine);

      IndexOfBreakpointInList := FBreakPointsArr[FSelectedSourceFileIndex].IndexOf(BreakpointLineStr);

      if IndexOfBreakpointInList <> -1 then  //breakpoint already set
        FBreakPointsArr[FSelectedSourceFileIndex].Delete(IndexOfBreakpointInList)
      else
        if LineIndexIsADebugPoint(BreakpointLine) then
          FBreakPointsArr[FSelectedSourceFileIndex].Add(BreakpointLineStr);

      vstPluginDebugging.InvalidateNode(FHitTestInfo.HitNode);
      UpdateDebugSymbolsFileWithBreakPoints;
    end;
end;


procedure TfrClickerPlugin.vstPluginDebuggingPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  case Column of
    0:
    begin
      if vstPluginDebugging.Selected[Node] then
      begin
        if vstPluginDebugging.Focused then
          TargetCanvas.Font.Color := clWindow
        else
          TargetCanvas.Font.Color := clWindowText;
      end
      else
        TargetCanvas.Font.Color := clGray;
    end;

    1:
      TargetCanvas.Font.Color := clWindowText;
  end;
end;


procedure TfrClickerPlugin.spdbtnStopPlayingClick(Sender: TObject);
begin
  DoOnPluginDbgStop;
end;


procedure TfrClickerPlugin.tmrRequestLineNumberTimer(Sender: TObject);
var
  RemoteLineNumber: Integer;
  TempLineContent: string;
begin
  RemoteLineNumber := DoOnPluginDbgRequestLineNumber(TempLineContent);

  if (RemoteLineNumber > -1) and (FSelectedLine <> RemoteLineNumber) then
  begin
    try
      FSelectedLine := RemoteLineNumber;
      //SelectLineByContent(TempLineContent);   //uncomment this, and remove/comment next line after fixing the parsing/updating issue

      FCachedLineContent := FSourceFiles[FSelectedSourceFileIndex].Content.Strings[RemoteLineNumber];
      vstPluginDebugging.RootNodeCount := FSourceFiles[FSelectedSourceFileIndex].Content.Count;  //Instead of function header, it should stop at first debug point, but that is not available yet. It requires a call to UpdateSelectedSourceFileAndLineFromDbgName with a valid debug point name.
      SelectNodeByIndex(vstPluginDebugging, FSelectedLine, True, True);  //"function" is case sensitive
      lblMsg.Caption := 'File: ' + FSourceFiles[FSelectedSourceFileIndex].Fnm;
      lblMsg.Hint := lblMsg.Hint + #13#10 + 'Line: ' + IntToStr(FSelectedLine + 1);

      spdbtnStopPlaying.Visible := True;
      spdbtnContinuePlayingAll.Visible := True;
      spdbtnStepOver.Visible := True;
    except
      on E: Exception do
      begin
        lblMsg.Hint := 'Ex on updating UI with line number (' + IntToStr(RemoteLineNumber) + ') from remote plugin : ' + E.Message;
        lblMsg.Color := clRed;
      end;
    end;
  end;
end;


procedure TfrClickerPlugin.vstPluginDebuggingBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  i: Integer;
begin
  case Column of
    0:
    begin
      //TargetCanvas.Pen.Color := clSilver;
      //TargetCanvas.Brush.Color := TargetCanvas.Pen.Color;
      //TargetCanvas.Rectangle(CellRect);
    end;

    1:
    begin
      if Integer(Node^.Index) = FSelectedLine then
      begin
        TargetCanvas.Pen.Color := $FFBBCC;
        TargetCanvas.Brush.Color := TargetCanvas.Pen.Color;
        TargetCanvas.Rectangle(CellRect);
      end
      else
        for i := 0 to Length(FAllDbgLinesFromSelectedFile) - 1 do
          if Integer(Node^.Index) = FAllDbgLinesFromSelectedFile[i] then
          begin
            TargetCanvas.Pen.Color := $CCFFCC;
            TargetCanvas.Brush.Color := TargetCanvas.Pen.Color;
            TargetCanvas.Rectangle(CellRect);
            Break;
          end;
    end;
  end; //case
end;


procedure TfrClickerPlugin.vstPluginDebuggingGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
var
  i: Integer;
begin
  case Column of
    0:
    begin
      ImageIndex := 0;

      if Integer(Node^.Index) = FSelectedLine then
      begin
        ImageIndex := 1;

        if (FSelectedSourceFileIndex < Length(FBreakPointsArr)) and (FSelectedSourceFileIndex > -1) then
          if FBreakPointsArr[FSelectedSourceFileIndex].IndexOf(IntToStr(Node^.Index)) > -1 then //a bit slow to call on every node.. :(
            ImageIndex := 3;
      end
      else
      begin
        for i := 0 to Length(FAllDbgLinesFromSelectedFile) - 1 do
          if Integer(Node^.Index) = FAllDbgLinesFromSelectedFile[i] then
          begin
            ImageIndex := 2;

            if (FSelectedSourceFileIndex < Length(FBreakPointsArr)) and (FSelectedSourceFileIndex > -1) then
              if FBreakPointsArr[FSelectedSourceFileIndex].IndexOf(IntToStr(Node^.Index)) > -1 then //a bit slow to call on every node.. :(
                ImageIndex := 4;

            Break;
          end;
      end;
    end; //0

  end;
end;


procedure TfrClickerPlugin.spdbtnContinuePlayingAllClick(Sender: TObject);
begin
  DoOnPluginDbgContinueAll;
end;


procedure TfrClickerPlugin.spdbtnGoToPrevDbgPointClick(Sender: TObject);
var
  ArrIdx: Integer;
  Node: PVirtualNode;
begin
  ArrIdx := GetIndexOfDbgLine(FLineIntoView);
  Dec(ArrIdx);

  if (ArrIdx > -1) and (ArrIdx < Length(FAllDbgLinesFromSelectedFile)) then
  begin
    FLineIntoView := FAllDbgLinesFromSelectedFile[ArrIdx];     //there can be source code at line 0, for *.inc files
    Node := GetNodeByIndex(vstPluginDebugging, FLineIntoView);

    if Node <> nil then
      vstPluginDebugging.ScrollIntoView(Node, True);
  end;
end;


procedure TfrClickerPlugin.spdbtnGoToNextDbgPointClick(Sender: TObject);
var
  ArrIdx: Integer;
  Node: PVirtualNode;
begin
  ArrIdx := GetIndexOfDbgLine(FLineIntoView);
  Inc(ArrIdx);

  if (ArrIdx > -1) and (ArrIdx < Length(FAllDbgLinesFromSelectedFile)) then
  begin
    FLineIntoView := FAllDbgLinesFromSelectedFile[ArrIdx];     //there can be source code at line 0, for *.inc files
    Node := GetNodeByIndex(vstPluginDebugging, FLineIntoView);

    if Node <> nil then
      vstPluginDebugging.ScrollIntoView(Node, True);
  end;
end;


procedure TfrClickerPlugin.spdbtnScrollToCurrentLineClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := GetNodeByIndex(vstPluginDebugging, FSelectedLine);

  if Node <> nil then
    vstPluginDebugging.ScrollIntoView(Node, True);
end;


procedure TfrClickerPlugin.spdbtnStepOverClick(Sender: TObject);
begin
  DoOnPluginDbgStepOver;
end;


procedure TfrClickerPlugin.DoOnPluginDbgStop;
begin
  if not Assigned(FOnPluginDbgStop) then
    raise Exception.Create('OnPluginDbgStop not assigned.');

  FOnPluginDbgStop();
end;


procedure TfrClickerPlugin.DoOnPluginDbgContinueAll;
begin
  if not Assigned(FOnPluginDbgContinueAll) then
    raise Exception.Create('OnPluginDbgContinueAll not assigned.');

  FOnPluginDbgContinueAll();
end;


procedure TfrClickerPlugin.DoOnPluginDbgStepOver;
begin
  if not Assigned(FOnPluginDbgStepOver) then
    raise Exception.Create('OnPluginDbgStepOver not assigned.');

  FOnPluginDbgStepOver();
end;


function TfrClickerPlugin.DoOnPluginDbgRequestLineNumber(ALineContent: string): Integer;
begin
  if not Assigned(FOnPluginDbgRequestLineNumber) then
    raise Exception.Create('PluginDbgRequestLineNumber not assigned.');

  Result := FOnPluginDbgRequestLineNumber(ALineContent);
end;


procedure TfrClickerPlugin.UpdateDebugSymbolsFileWithBreakPoints;
var
  Ini: TMemIniFile;
  CurrentSourceFileName: string;
  BreakPointCount, i: Integer;
begin
  if not FileExists(FDbgSymFnm) then
    Exit;

  if FSelectedSourceFileIndex = -1 then
    Exit;

  Ini := TMemIniFile.Create(FDbgSymFnm);
  try
    CurrentSourceFileName := FSourceFiles[FSelectedSourceFileIndex].Fnm;
    BreakPointCount := FBreakPointsArr[FSelectedSourceFileIndex].Count;

    Ini.WriteInteger(CurrentSourceFileName, 'BrkPointCount', BreakPointCount);

    for i := 0 to BreakPointCount - 1 do
      Ini.WriteString(CurrentSourceFileName, 'BrkPointIdx' + IntToStr(i), FBreakPointsArr[FSelectedSourceFileIndex].Strings[i]);   //   no '_' after 'BrkPointIdx', to avoid being loaded as simple debug point

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrClickerPlugin.LoadDebugSymbols(ADbgSymFnm: string);
var
  Ini: TClkIniReadonlyFile;
  i, j: Integer;
  BreakPointCount: Integer;
  CurrentSourceFileName: string;
begin
  spdbtnStopPlaying.Visible := False;
  spdbtnContinuePlayingAll.Visible := False;
  spdbtnStepOver.Visible := False;

  if FileExists(ADbgSymFnm) then
  begin
    lblMsg.Caption := 'Debug symbols file "' + ADbgSymFnm + '".';
    FDbgSymFnm := ADbgSymFnm;

    Ini := TClkIniReadonlyFile.Create(ADbgSymFnm);
    try
      ClearSourceFilesArr;
      SetLength(FSourceFiles, Ini.GetSectionCount);

      if Length(FBreakPointsArr) <> Ini.GetSectionCount then //all items must be reallocated and their contents be updated
      begin
        ClearBreakPointsArr;
        SetLength(FBreakPointsArr, Ini.GetSectionCount);

        for i := 0 to Length(FSourceFiles) - 1 do
        begin
          CurrentSourceFileName := Ini.GetSectionAtIndex(i);
          FBreakPointsArr[i] := TStringList.Create;

          BreakPointCount := Ini.ReadInteger(CurrentSourceFileName, 'BrkPointCount', 0);
          for j := 0 to BreakPointCount - 1 do
            FBreakPointsArr[i].Add(Ini.ReadString(CurrentSourceFileName, 'BrkPointIdx' + IntToStr(j), '0'));
        end;
      end;

      for i := 0 to Length(FSourceFiles) - 1 do
      begin
        FSourceFiles[i].Content := TStringList.Create;
        FSourceFiles[i].DbgLines := TStringList.Create;

        FSourceFiles[i].Fnm := Ini.GetSectionAtIndex(i);

        if FileExists(FSourceFiles[i].Fnm) then
        begin
          FSourceFiles[i].Content.LoadFromFile(FSourceFiles[i].Fnm);
          Ini.ReadSection(i, FSourceFiles[i].DbgLines);
        end
        else
          FSourceFiles[i].Content.Add('Source file "' + FSourceFiles[i].Fnm + '" not found.');
      end;

      FSelectedLine := -1;
      FLineIntoView := -1;
    finally
      Ini.Free;
    end;

    if FCachedLineContent <> '' then
      SelectLineByContent(FCachedLineContent);  //called when manually switching actions in list
  end
  else
    lblMsg.Caption := 'Debug symbols file "' + ADbgSymFnm + '" not found.';
end;


procedure TfrClickerPlugin.ExtractSelectedSourceFileAndLineFromDbgName(ALineContent: string; var ASelectedLine, ASelectedSourceFileIndex: Integer; var AAllDbgLinesFromSelectedFile: TIntArr);
var
  i, j, k: Integer;
  s, LineNo: string;
begin
  ASelectedLine := -1;
  ASelectedSourceFileIndex := -1;

  for i := 0 to Length(FSourceFiles) - 1 do
    for j := 0 to FSourceFiles[i].DbgLines.Count - 1 do
    begin
      s := FSourceFiles[i].DbgLines.ValueFromIndex[j];

      if Pos('DbgPoint(', s) > 0 then
        if (Pos(ALineContent, s) > 0) or (ALineContent = CBeforePluginExecution_DbgLineContent) then  //this may find subsets, which lead to wrong results
        begin
          ASelectedSourceFileIndex := i;

          SetLength(AAllDbgLinesFromSelectedFile, FSourceFiles[i].DbgLines.Count);
          for k := 0 to FSourceFiles[i].DbgLines.Count - 1 do
          begin
            LineNo := FSourceFiles[i].DbgLines.Names[k];

            if Pos('_', LineNo) > 0 then
            begin
              LineNo := Copy(LineNo, Pos('_', LineNo) + 1, MaxInt);
              AAllDbgLinesFromSelectedFile[k] := StrToIntDef(LineNo, -1);
            end;
          end;

          ASelectedLine := AAllDbgLinesFromSelectedFile[j];     //this can also be obtained as j, if j iterates through FSourceFiles[i].Content (which is longer)

          Exit;
        end;
    end;
end;


procedure TfrClickerPlugin.UpdateSelectedSourceFileAndLineFromDbgName(ALineContent: string);
begin
  ExtractSelectedSourceFileAndLineFromDbgName(ALineContent, FSelectedLine, FSelectedSourceFileIndex, FAllDbgLinesFromSelectedFile);
  if (FSelectedLine <> -1) and (FLineIntoView = -1) then
    FLineIntoView := FSelectedLine;
end;


procedure TfrClickerPlugin.SelectLineByContent(ALineContent: string);
begin
  vstPluginDebugging.RootNodeCount := 0;
  lblMsg.Hint := ALineContent;
  lblMsg.ShowHint := True;

  //vstPluginDebugging.SetFocus;  //this can be reenabled after implenting a property, which tells this frame that is is running in local/client mode

  UpdateSelectedSourceFileAndLineFromDbgName(ALineContent);

  if ALineContent = CBeforePluginExecution_DbgLineContent then
  begin
    vstPluginDebugging.Color := clDefault;

    if Length(FSourceFiles) > 0 then
    begin
      if FSelectedSourceFileIndex = -1 then
        FSelectedSourceFileIndex := 0;

      if FSelectedLine = -1 then
        FSelectedLine := FSourceFiles[FSelectedSourceFileIndex].Content.IndexOf('function ExecutePlugin(');

      FCachedLineContent := ALineContent;
      vstPluginDebugging.RootNodeCount := FSourceFiles[FSelectedSourceFileIndex].Content.Count;  //Instead of function header, it should stop at first debug point, but that is not available yet. It requires a call to UpdateSelectedSourceFileAndLineFromDbgName with a valid debug point name.
      SelectNodeByIndex(vstPluginDebugging, FSelectedLine, True, True);  //"function" is case sensitive
      lblMsg.Caption := 'File: ' + FSourceFiles[FSelectedSourceFileIndex].Fnm;
      lblMsg.Hint := lblMsg.Hint + #13#10 + 'Line: ' + IntToStr(FSelectedLine + 1);

      spdbtnStopPlaying.Visible := True;
      spdbtnContinuePlayingAll.Visible := True;
      spdbtnStepOver.Visible := True;
    end;

    Exit;
  end;

  if FSelectedSourceFileIndex > -1 then
  begin
    FCachedLineContent := ALineContent;
    vstPluginDebugging.Color := clDefault;
    vstPluginDebugging.RootNodeCount := FSourceFiles[FSelectedSourceFileIndex].Content.Count;
    SelectNodeByIndex(vstPluginDebugging, FSelectedLine, True, True);

    lblMsg.Caption := 'File: ' + FSourceFiles[FSelectedSourceFileIndex].Fnm;
    lblMsg.Hint := lblMsg.Hint + #13#10 + 'Line: ' + IntToStr(FSelectedLine + 1);
  end
  else
  begin
    vstPluginDebugging.Color := $CCCCFF;
    lblMsg.Caption := '';
  end;

  try
    vstPluginDebugging.Repaint;
  except
  end;

  spdbtnStopPlaying.Visible := True;
  spdbtnContinuePlayingAll.Visible := True;
  spdbtnStepOver.Visible := True;
end;


function TfrClickerPlugin.IsAtBreakPoint(ADebugPoint: string): Boolean;
var
  TempSelectedLine, TempSelectedSourceFileIndex: Integer;
  TempAllDbgLinesFromSelectedFile: TIntArr;
begin
  try
    ExtractSelectedSourceFileAndLineFromDbgName(ADebugPoint, TempSelectedLine, TempSelectedSourceFileIndex, TempAllDbgLinesFromSelectedFile);
    Result := FBreakPointsArr[TempSelectedSourceFileIndex].IndexOf(IntToStr(TempSelectedLine)) > -1;
  except
    Result := True;
  end;
end;


procedure TfrClickerPlugin.DoneDebuging;
begin
  vstPluginDebugging.RootNodeCount := 0;

  spdbtnStopPlaying.Visible := False;
  spdbtnContinuePlayingAll.Visible := False;
  spdbtnStepOver.Visible := False;

  FCachedLineContent := ''; //clear, so that next time, the frame won't load its content, unless required
end;


procedure TfrClickerPlugin.EnableRequestLineNumber;
begin
  tmrRequestLineNumber.Enabled := True;
end;


procedure TfrClickerPlugin.DisableRequestLineNumber;
begin
  tmrRequestLineNumber.Enabled := False;
  lblMsg.Color := clDefault;
end;

end.

