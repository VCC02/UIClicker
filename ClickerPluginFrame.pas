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
  VirtualTrees, Graphics, ClickerUtils, ClickerIniFiles;

type
  TOnPluginDbgStop = procedure of object;
  TOnPluginDbgContinueAll = procedure of object;
  TOnPluginDbgStepOver = procedure of object;
  TOnPluginDbgRequestLineNumber = function(out ALineContent, ADbgSymFile: string): Integer of object;
  TOnPluginDbgSetBreakpoint = procedure(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean) of object;

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
    FOnPluginDbgSetBreakpoint: TOnPluginDbgSetBreakpoint;

    FOnTClkIniFileCreate: TOnTClkIniFileCreate;

    function GetSelectedLine: string;

    procedure ExtractSelectedSourceFileAndLineFromDbgName(ALineContent: string; var ASelectedLine, ASelectedSourceFileIndex: Integer; var AAllDbgLinesFromSelectedFile: TIntArr);
    procedure UpdateSelectedSourceFileAndLineFromDbgName(ALineContent: string);
    procedure ClearSourceFilesArr;
    procedure ClearBreakPointsArr;

    function GetIndexOfDbgLine(ALineIndex: Integer): Integer;
    function LineIndexIsADebugPoint(AIndex: Integer): Boolean;
    procedure UpdateDebugSymbolsFileWithBreakPoints;
    function ToggleBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer): Boolean;  //returns the new state

    procedure DoOnPluginDbgStop;
    procedure DoOnPluginDbgContinueAll;
    procedure DoOnPluginDbgStepOver;
    function DoOnPluginDbgRequestLineNumber(out ALineContent, ADbgSymFile: string): Integer;
    procedure DoOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);

    function DoOnTClkIniFileCreate(AFileName: string): TClkIniFile;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadDebugSymbols(ADbgSymFnm: string);
    procedure SelectLineByContent(ALineContent: string);
    function IsAtBreakPoint(ADebugPoint: string): Boolean;
    procedure DoneDebuging; //clears vst and disables buttons

    procedure EnableRequestLineNumber;
    procedure DisableRequestLineNumber;
    procedure SetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);

    property SelectedLine: string read GetSelectedLine;
    property DbgSymFnm: string read FDbgSymFnm;

    property OnPluginDbgStop: TOnPluginDbgStop write FOnPluginDbgStop;
    property OnPluginDbgContinueAll: TOnPluginDbgContinueAll write FOnPluginDbgContinueAll;
    property OnPluginDbgStepOver: TOnPluginDbgStepOver write FOnPluginDbgStepOver;
    property OnPluginDbgRequestLineNumber: TOnPluginDbgRequestLineNumber write FOnPluginDbgRequestLineNumber;
    property OnPluginDbgSetBreakpoint: TOnPluginDbgSetBreakpoint write FOnPluginDbgSetBreakpoint;

    property OnTClkIniFileCreate: TOnTClkIniFileCreate write FOnTClkIniFileCreate;
  end;

implementation

{$R *.frm}


uses
  ClickerExtraUtils, ClickerActionPlugins;


constructor TfrClickerPlugin.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetLength(FSourceFiles, 0);
  SetLength(FBreakPointsArr, 0);
  SetLength(FAllDbgLinesFromSelectedFile, 0);

  FOnPluginDbgStop := nil;
  FOnPluginDbgContinueAll := nil;
  FOnPluginDbgStepOver := nil;
  FOnPluginDbgRequestLineNumber := nil;
  FOnPluginDbgSetBreakpoint := nil;

  FOnTClkIniFileCreate := nil;

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
  Result := IntToStr(FSelectedLine) + '=' + FCachedLineContent + #2 + FDbgSymFnm;
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


procedure TfrClickerPlugin.SetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);
var
  BreakpointLineStr: string;
  IndexOfBreakpointInList: Integer;
  Node: PVirtualNode;
begin
  if (ALineIndex = -1) or (ASelectedSourceFileIndex = -1) then
    Exit;

  BreakpointLineStr := IntToStr(ALineIndex);

  IndexOfBreakpointInList := FBreakPointsArr[ASelectedSourceFileIndex].IndexOf(BreakpointLineStr);

  if IndexOfBreakpointInList <> -1 then  //breakpoint already set
  begin
    if not AEnabled then
      FBreakPointsArr[ASelectedSourceFileIndex].Delete(IndexOfBreakpointInList)
  end
  else
    if AEnabled and LineIndexIsADebugPoint(ALineIndex) then
      FBreakPointsArr[ASelectedSourceFileIndex].Add(BreakpointLineStr);

  Node := GetNodeByIndex(vstPluginDebugging, ALineIndex);
  if Node <> nil then
    vstPluginDebugging.InvalidateNode(Node);

  UpdateDebugSymbolsFileWithBreakPoints;
end;


function TfrClickerPlugin.ToggleBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer): Boolean;
var
  BreakpointLineStr: string;
  IndexOfBreakpointInList: Integer;
begin
  Result := False;
  BreakpointLineStr := IntToStr(ALineIndex);

  IndexOfBreakpointInList := FBreakPointsArr[ASelectedSourceFileIndex].IndexOf(BreakpointLineStr);

  if IndexOfBreakpointInList <> -1 then  //breakpoint already set
    FBreakPointsArr[ASelectedSourceFileIndex].Delete(IndexOfBreakpointInList)
  else
    if LineIndexIsADebugPoint(ALineIndex) then
    begin
      FBreakPointsArr[ASelectedSourceFileIndex].Add(BreakpointLineStr);
      Result := True;
    end;

  vstPluginDebugging.InvalidateNode(FHitTestInfo.HitNode);
  UpdateDebugSymbolsFileWithBreakPoints;
end;


procedure TfrClickerPlugin.vstPluginDebuggingMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BreakPointEnabled: Boolean;
begin
  vstPluginDebugging.GetHitTestInfoAt(X, Y, True, FHitTestInfo);
  if FHitTestInfo.HitNode <> nil then
    if (FHitTestInfo.HitColumn = 0) and (X < imglstPluginDebugging.Width + Integer(vstPluginDebugging.Indent) + 12) then
    begin
      BreakPointEnabled := ToggleBreakpoint(FHitTestInfo.HitNode.Index, FSelectedSourceFileIndex);
      DoOnPluginDbgSetBreakpoint(FHitTestInfo.HitNode.Index, FSelectedSourceFileIndex, BreakPointEnabled);  //this notifies the server about changing the breakpoint
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
  TempLineContent, TempDbgSymFile: string;
begin
  RemoteLineNumber := DoOnPluginDbgRequestLineNumber(TempLineContent, TempDbgSymFile);

  if (RemoteLineNumber > -1) and (FSelectedLine <> RemoteLineNumber) then
  begin
    try
      FSelectedLine := RemoteLineNumber;

      if FDbgSymFnm <> TempDbgSymFile then
        LoadDebugSymbols(TempDbgSymFile);

      SelectLineByContent(TempLineContent);
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
  spdbtnStepOver.Enabled := False;
  try
    DoOnPluginDbgStepOver;
  finally
    spdbtnStepOver.Enabled := True;
  end;
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


function TfrClickerPlugin.DoOnPluginDbgRequestLineNumber(out ALineContent, ADbgSymFile: string): Integer;
begin
  if not Assigned(FOnPluginDbgRequestLineNumber) then
    raise Exception.Create('PluginDbgRequestLineNumber not assigned.');

  Result := FOnPluginDbgRequestLineNumber(ALineContent, ADbgSymFile);
end;


procedure TfrClickerPlugin.DoOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex: Integer; AEnabled: Boolean);
begin
  if not Assigned(FOnPluginDbgSetBreakpoint) then
    raise Exception.Create('OnSetPluginDbgBreakpoint not assigned.');

  FOnPluginDbgSetBreakpoint(ALineIndex, ASelectedSourceFileIndex, AEnabled);
end;


function TfrClickerPlugin.DoOnTClkIniFileCreate(AFileName: string): TClkIniFile;
begin
  if not Assigned(FOnTClkIniFileCreate) then
    raise Exception.Create('OnTClkIniFileCreate not assigned.');

  Result := FOnTClkIniFileCreate(AFileName);
end;


procedure TfrClickerPlugin.UpdateDebugSymbolsFileWithBreakPoints;
var
  Ini: TClkIniFile;
  CurrentSourceFileName: string;
  BreakPointCount, i: Integer;
begin
  if not FileExists(FDbgSymFnm) then
    Exit;

  if FSelectedSourceFileIndex = -1 then
    Exit;

  Ini := DoOnTClkIniFileCreate(FDbgSymFnm);
  try
    CurrentSourceFileName := FSourceFiles[FSelectedSourceFileIndex].Fnm;
    BreakPointCount := FBreakPointsArr[FSelectedSourceFileIndex].Count;

    Ini.WriteInteger(CurrentSourceFileName, 'BrkPointCount', BreakPointCount);

    for i := 0 to BreakPointCount - 1 do
      Ini.WriteString(CurrentSourceFileName, 'BrkPointIdx' + IntToStr(i), FBreakPointsArr[FSelectedSourceFileIndex].Strings[i]);   //   no '_' after 'BrkPointIdx', to avoid being loaded as simple debug point

    Ini.UpdateFile;  //this should not be called if both client and server are running on the same machine, because they would update the same file
  finally
    Ini.Free;
  end;
end;


procedure TfrClickerPlugin.LoadDebugSymbols(ADbgSymFnm: string);
var
  Ini: TClkIniFile;
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

    Ini := TClkIniFile.Create(ADbgSymFnm);
    try
      ClearSourceFilesArr;
      SetLength(FSourceFiles, Ini.GetSectionCount);

      if Length(FBreakPointsArr) <> Ini.GetSectionCount then //all items must be reallocated and their contents be updated
      begin
        ClearBreakPointsArr;
        SetLength(FBreakPointsArr, Ini.GetSectionCount);  //number of source files in the current plugin

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
  lblMsg.Hint := 'LineContent: "' + ALineContent + '"';
  lblMsg.ShowHint := True;

  if Pos(#2, ALineContent) > 0 then
    ALineContent := Copy(ALineContent, 1, Pos(#2, ALineContent) - 1);

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
    lblMsg.Caption := 'Source file is not selected.';
    lblMsg.Hint := lblMsg.Hint + #13#10 + 'The current debug symbols file: ' + #13#10 + '"' + FDbgSymFnm + '"' + #13#10 + 'may not match the source file.';
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

