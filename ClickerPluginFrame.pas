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

  TDbgSourceFile = record
    Fnm: string;
    Content: TStringList;  //this field can be removed if/when the vst is replaced by a SynEdit
    DbgLines: TStringList;
  end;

  TDbgSourceFileArr = array of TDbgSourceFile;

  { TfrClickerPlugin }

  TfrClickerPlugin = class(TFrame)
    imglstPluginDebugging: TImageList;
    imgPlugin: TImage;
    lblMsg: TLabel;
    lblPluginDebugging: TLabel;
    spdbtnContinuePlayingAll: TSpeedButton;
    spdbtnStepOver: TSpeedButton;
    spdbtnStopPlaying: TSpeedButton;
    vstPluginDebugging: TVirtualStringTree;
    procedure spdbtnContinuePlayingAllClick(Sender: TObject);
    procedure spdbtnStepOverClick(Sender: TObject);
    procedure spdbtnStopPlayingClick(Sender: TObject);
    procedure vstPluginDebuggingBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstPluginDebuggingGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: boolean; var ImageIndex: integer);
    procedure vstPluginDebuggingGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    FSourceFiles: TDbgSourceFileArr;
    FSelectedLine: Integer;
    FSelectedSourceFileIndex: Integer;
    FAllDbgLinesFromSelectedFile: TIntArr;

    FOnPluginDbgStop: TOnPluginDbgStop;
    FOnPluginDbgContinueAll: TOnPluginDbgContinueAll;
    FOnPluginDbgStepOver: TOnPluginDbgStepOver;

    procedure UpdateSelectedSourceFileAndLineFromDbgName(ALineContent: string);

    procedure DoOnPluginDbgStop;
    procedure DoOnPluginDbgContinueAll;
    procedure DoOnPluginDbgStepOver;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadDebugSymbols(ADbgSymFnm: string);
    procedure SelectLineByContent(ALineContent: string);
    procedure DoneDebuging; //clears vst and disables buttons

    property OnPluginDbgStop: TOnPluginDbgStop write FOnPluginDbgStop;
    property OnPluginDbgContinueAll: TOnPluginDbgContinueAll write FOnPluginDbgContinueAll;
    property OnPluginDbgStepOver: TOnPluginDbgStepOver write FOnPluginDbgStepOver;
  end;

implementation

{$R *.frm}


uses
  ClickerExtraUtils, ClickerIniFiles;


constructor TfrClickerPlugin.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetLength(FSourceFiles, 0);
  SetLength(FAllDbgLinesFromSelectedFile, 0);

  FOnPluginDbgStop := nil;
  FOnPluginDbgContinueAll := nil;
  FOnPluginDbgStepOver := nil;

  FSelectedLine := -1;
  FSelectedSourceFileIndex := -1;
end;


destructor TfrClickerPlugin.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FSourceFiles) - 1 do
  begin
    FSourceFiles[i].Content.Free;
    FSourceFiles[i].DbgLines.Free;
  end;

  SetLength(FSourceFiles, 0);
  SetLength(FAllDbgLinesFromSelectedFile, 0);
  inherited Destroy;
end;


procedure TfrClickerPlugin.vstPluginDebuggingGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  try
    if (FSelectedSourceFileIndex = -1) or (FSelectedLine = -1) then
      Exit;

    CellText := FSourceFiles[FSelectedSourceFileIndex].Content.Strings[Node^.Index];
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerPlugin.spdbtnStopPlayingClick(Sender: TObject);
begin
  DoOnPluginDbgStop;
end;


procedure TfrClickerPlugin.vstPluginDebuggingBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  i: Integer;
begin
  for i := 0 to Length(FAllDbgLinesFromSelectedFile) - 1 do
    if Integer(Node^.Index) = FAllDbgLinesFromSelectedFile[i] then
    begin
      TargetCanvas.Pen.Color := $CCFFCC;
      TargetCanvas.Brush.Color := TargetCanvas.Pen.Color;
      TargetCanvas.Rectangle(CellRect);
      Break;
    end;
end;


procedure TfrClickerPlugin.vstPluginDebuggingGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
var
  i: Integer;
begin
  ImageIndex := 0;

  if Integer(Node^.Index) = FSelectedLine then
    ImageIndex := 1
  else
  begin
    for i := 0 to Length(FAllDbgLinesFromSelectedFile) - 1 do
      if Integer(Node^.Index) = FAllDbgLinesFromSelectedFile[i] then
      begin
        ImageIndex := 2;
        Break;
      end;
  end;
end;


procedure TfrClickerPlugin.spdbtnContinuePlayingAllClick(Sender: TObject);
begin
  DoOnPluginDbgContinueAll;
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


procedure TfrClickerPlugin.LoadDebugSymbols(ADbgSymFnm: string);
var
  Ini: TClkIniReadonlyFile;
  i: Integer;
begin
  spdbtnStopPlaying.Visible := False;
  spdbtnContinuePlayingAll.Visible := False;
  spdbtnStepOver.Visible := False;

  if FileExists(ADbgSymFnm) then
  begin
    lblMsg.Caption := '';

    Ini := TClkIniReadonlyFile.Create(ADbgSymFnm);
    try
      SetLength(FSourceFiles, Ini.GetSectionCount);

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
    finally
      Ini.Free;
    end;
  end
  else
    lblMsg.Caption := 'Debug symbols file "' + ADbgSymFnm + '" not found.';
end;


procedure TfrClickerPlugin.UpdateSelectedSourceFileAndLineFromDbgName(ALineContent: string);
var
  i, j, k: Integer;
  s, LineNo: string;
begin
  FSelectedLine := -1;
  FSelectedSourceFileIndex := -1;

  for i := 0 to Length(FSourceFiles) - 1 do
    for j := 0 to FSourceFiles[i].DbgLines.Count - 1 do
    begin
      s := FSourceFiles[i].DbgLines.ValueFromIndex[j];

      if Pos('DbgPoint(', s) > 0 then
        if Pos(ALineContent, s) > 0 then  //this may find subsets, which lead to wrong results
        begin
          FSelectedSourceFileIndex := i;

          SetLength(FAllDbgLinesFromSelectedFile, FSourceFiles[i].DbgLines.Count);
          for k := 0 to FSourceFiles[i].DbgLines.Count - 1 do
          begin
            LineNo := FSourceFiles[i].DbgLines.Names[k];
            LineNo := Copy(LineNo, Pos('_', LineNo) + 1, MaxInt);
            FAllDbgLinesFromSelectedFile[k] := StrToIntDef(LineNo, -1);
          end;

          FSelectedLine := FAllDbgLinesFromSelectedFile[j];     //this can also be obtained as j, if j iterates through FSourceFiles[i].Content (which is longer)

          Exit;
        end;
    end;
end;


procedure TfrClickerPlugin.SelectLineByContent(ALineContent: string);
begin
  vstPluginDebugging.RootNodeCount := 0;
  UpdateSelectedSourceFileAndLineFromDbgName(ALineContent);

  if FSelectedSourceFileIndex > -1 then
  begin
    vstPluginDebugging.Color := clDefault;
    vstPluginDebugging.RootNodeCount := FSourceFiles[FSelectedSourceFileIndex].Content.Count;
    SelectNodeByIndex(vstPluginDebugging, FSelectedLine, True, True);

    lblMsg.Caption := 'File: ' + FSourceFiles[FSelectedSourceFileIndex].Fnm;
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


procedure TfrClickerPlugin.DoneDebuging;
begin
  vstPluginDebugging.RootNodeCount := 0;

  spdbtnStopPlaying.Visible := False;
  spdbtnContinuePlayingAll.Visible := False;
  spdbtnStepOver.Visible := False;
end;

end.

