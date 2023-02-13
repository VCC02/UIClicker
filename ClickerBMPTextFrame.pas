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


unit ClickerBMPTextFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Menus,
  ClickerUtils;

type
  TOnGetDisplayedText = function: string of object;
  TOnSetCroppingValuesToOtherFontProfiles = procedure(ACropLeft, ACropTop, ACropRight, ACropBottom: string; ASkipProfileIndex: Integer) of object;
  TOnGetCroppingLinesVisiblity = function: Boolean of object;

  { TfrClickerBMPText }

  TfrClickerBMPText = class(TFrame)
    btnBrowseFont: TButton;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkStrikeOut: TCheckBox;
    chkUnderline: TCheckBox;
    cmbMatchBitmapTextFontQuality: TComboBox;
    edtFontQualityReplacement: TEdit;
    imgPreview: TImage;
    lbeMatchBitmapTextBGColor: TLabeledEdit;
    lbeMatchBitmapTextFGColor: TLabeledEdit;
    lbeMatchBitmapTextFontName: TLabeledEdit;
    lbeMatchBitmapTextSize: TLabeledEdit;
    lblMatchBitmapTextFontQuality: TLabel;
    lblPreview: TLabel;
    MenuItemCopyPreviewImageAndCroppingLines: TMenuItem;
    MenuItemCopyCroppingValuesToOtherProfiles: TMenuItem;
    N1: TMenuItem;
    MenuItemCopyCroppedPreviewImage: TMenuItem;
    MenuItemColor_3DDkShadow: TMenuItem;
    MenuItemColor_3DLight: TMenuItem;
    MenuItemColor_ActiveCaption: TMenuItem;
    MenuItemColor_BtnFace: TMenuItem;
    MenuItemColor_GradientActiveCaption: TMenuItem;
    MenuItemColor_GradientInactiveCaption: TMenuItem;
    MenuItemColor_GrayText: TMenuItem;
    MenuItemColor_Highlight: TMenuItem;
    MenuItemColor_InactiveCaption: TMenuItem;
    MenuItemColor_ScrollBar: TMenuItem;
    MenuItemColor_Window: TMenuItem;
    MenuItemColor_WindowFrame: TMenuItem;
    MenuItemColor_WindowText: TMenuItem;
    MenuItemCopyColorToClipboard: TMenuItem;
    MenuItemCopyPreviewImage: TMenuItem;
    MenuItemErasePreviewImage: TMenuItem;
    MenuItemPasteColorFromClipboard: TMenuItem;
    MenuItemSavePreviewImage: TMenuItem;
    N2: TMenuItem;
    N5: TMenuItem;
    pnlProfileName: TPanel;
    pmPreviewImage: TPopupMenu;
    pmStandardColorVariables: TPopupMenu;
    pnlBG: TPanel;
    pnlFG: TPanel;
    scrboxPreview: TScrollBox;
    tmrStartup: TTimer;
    tmrUpdateCropEditBoxes: TTimer;
    procedure btnBrowseFontClick(Sender: TObject);
    procedure chkBoldClick(Sender: TObject);
    procedure chkItalicClick(Sender: TObject);
    procedure chkStrikeOutClick(Sender: TObject);
    procedure chkUnderlineClick(Sender: TObject);
    procedure cmbMatchBitmapTextFontQualityChange(Sender: TObject);
    procedure cmbMatchBitmapTextFontQualityDropDown(Sender: TObject);
    procedure imgPreviewResize(Sender: TObject);
    procedure lbeMatchBitmapTextBGColorChange(Sender: TObject);
    procedure lbeMatchBitmapTextBGColorMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbeMatchBitmapTextFGColorChange(Sender: TObject);
    procedure lbeMatchBitmapTextFGColorMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbeMatchBitmapTextFontNameChange(Sender: TObject);
    procedure lbeMatchBitmapTextSizeChange(Sender: TObject);
    procedure lblPreviewClick(Sender: TObject);
    procedure MenuItemColor_GenericClick(Sender: TObject);
    procedure MenuItemCopyColorToClipboardClick(Sender: TObject);
    procedure MenuItemCopyCroppedPreviewImageClick(Sender: TObject);
    procedure MenuItemCopyCroppingValuesToOtherProfilesClick(Sender: TObject);
    procedure MenuItemCopyPreviewImageAndCroppingLinesClick(Sender: TObject);
    procedure MenuItemCopyPreviewImageClick(Sender: TObject);
    procedure MenuItemErasePreviewImageClick(Sender: TObject);
    procedure MenuItemPasteColorFromClipboardClick(Sender: TObject);
    procedure MenuItemSavePreviewImageClick(Sender: TObject);
    procedure pmStandardColorVariablesPopup(Sender: TObject);
    procedure pnlBGDblClick(Sender: TObject);
    procedure pnlFGDblClick(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure tmrUpdateCropEditBoxesTimer(Sender: TObject);
  private
    FLastClickedLbe: TLabeledEdit;
    FProfileName: string; //font settings profile

    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;
    FOnEvaluateReplacements: TOnEvaluateReplacements;
    FOnGetDisplayedText: TOnGetDisplayedText;
    FOnSetCroppingValuesToOtherFontProfiles: TOnSetCroppingValuesToOtherFontProfiles;
    FOnGetCroppingLinesVisiblity: TOnGetCroppingLinesVisiblity;
    FOnUpdateTextCroppingLimitsInOIFromDraggingLines: TOnUpdateTextCroppingLimitsInOIFromDraggingLines;

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

    FMatchBitmapTextCropLeft: string;
    FMatchBitmapTextCropTop: string;
    FMatchBitmapTextCropRight: string;
    FMatchBitmapTextCropBottom: string;

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

    procedure SetProfileName(Value: string);

    procedure DoOnTriggerOnControlsModified;
    function DoOnGetDisplayedText: string;
    procedure DoOnSetCroppingValuesToOtherFontProfiles(ACropLeft, ACropTop, ACropRight, ACropBottom: string; ASkipProfileIndex: Integer);
    function DoOnGetCroppingLinesVisiblity: Boolean;
    procedure DoOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileName: string);

    function EvaluateReplacements(s: string): string;
    procedure CreateSelectionLabels;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PreviewText;
    procedure PreviewTextOnImage(AImg: TImage; ACroppedImg: TImage = nil);
    procedure ClearControls;
    procedure UpdateSelectionLabelsFromCropEditBoxes;
    procedure UpdateSelectionLabelsFromCropInfo(var ABMPText: TClkFindControlMatchBitmapText);
    procedure DisplayCroppingLines(AVisible: Boolean);

    property ProfileName: string read FProfileName write SetProfileName;

    property MatchBitmapTextCropLeft: string read FMatchBitmapTextCropLeft write FMatchBitmapTextCropLeft;
    property MatchBitmapTextCropTop: string read FMatchBitmapTextCropTop write FMatchBitmapTextCropTop;
    property MatchBitmapTextCropRight: string read FMatchBitmapTextCropRight write FMatchBitmapTextCropRight;
    property MatchBitmapTextCropBottom: string read FMatchBitmapTextCropBottom write FMatchBitmapTextCropBottom;

    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified read FOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
    property OnEvaluateReplacements: TOnEvaluateReplacements read FOnEvaluateReplacements write FOnEvaluateReplacements;
    property OnGetDisplayedText: TOnGetDisplayedText read FOnGetDisplayedText write FOnGetDisplayedText;
    property OnSetCroppingValuesToOtherFontProfiles: TOnSetCroppingValuesToOtherFontProfiles write FOnSetCroppingValuesToOtherFontProfiles;
    property OnGetCroppingLinesVisiblity: TOnGetCroppingLinesVisiblity write FOnGetCroppingLinesVisiblity;
    property OnUpdateTextCroppingLimitsInOIFromDraggingLines: TOnUpdateTextCroppingLimitsInOIFromDraggingLines write FOnUpdateTextCroppingLimitsInOIFromDraggingLines;
  end;

  //TfrClickerBMPTextArr = array of TfrClickerBMPText;


implementation

{$R *.frm}


uses
  Types, Graphics, Clipbrd, Dialogs;


function GetReplacementVarFromString(AString: string): string;   //if multiple var replacements are concatenated, only the first is returned
var
  First, Second: Integer;
begin
  Result := '';
  First := Pos('$', AString);
  if First > 0 then
  begin
    Result := Copy(AString, First, MaxInt);
    Delete(Result, 1, 1); //delete first '$' from result, to find the second one
    Second := Pos('$', Result);

    if Second > 0 then
      Result := '$' + Copy(Result, 1, Second);
  end;
end;


{ TfrClickerBMPText }

procedure TfrClickerBMPText.SetProfileName(Value: string);
begin
  if FProfileName <> Value then
  begin
    FProfileName := Value;
    pnlProfileName.Caption := 'Profile name: ' + FProfileName;
  end;
end;


procedure TfrClickerBMPText.DoOnTriggerOnControlsModified;
begin
  if Assigned(FOnTriggerOnControlsModified) then
    FOnTriggerOnControlsModified()
  else
    raise Exception.Create('OnTriggerOnControlsModified not assigned.');
end;


function TfrClickerBMPText.DoOnGetDisplayedText: string;
begin
  if Assigned(FOnGetDisplayedText) then
    Result := FOnGetDisplayedText()
  else
    raise Exception.Create('OnGetDisplayedText not assigned.');
end;


procedure TfrClickerBMPText.DoOnSetCroppingValuesToOtherFontProfiles(ACropLeft, ACropTop, ACropRight, ACropBottom: string; ASkipProfileIndex: Integer);
begin
  if Assigned(FOnSetCroppingValuesToOtherFontProfiles) then
    FOnSetCroppingValuesToOtherFontProfiles(ACropLeft, ACropTop, ACropRight, ACropBottom, ASkipProfileIndex)
  else
    raise Exception.Create('OnSetCroppingValuesToOtherFontProfiles not assigned.');
end;


function TfrClickerBMPText.DoOnGetCroppingLinesVisiblity: Boolean;
begin
  if Assigned(FOnGetCroppingLinesVisiblity) then
    Result := FOnGetCroppingLinesVisiblity()
  else
    raise Exception.Create('OnGetCroppingLinesVisiblity not assigned.');
end;


procedure TfrClickerBMPText.DoOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate: TLimitLabels; var AOffsets: TSimpleRectString; AFontProfileName: string);
begin
  if not Assigned(FOnUpdateTextCroppingLimitsInOIFromDraggingLines) then
    Exit;

  FOnUpdateTextCroppingLimitsInOIFromDraggingLines(ALimitLabelsToUpdate, AOffsets, AFontProfileName);
end;


function TfrClickerBMPText.EvaluateReplacements(s: string): string;
begin
  if Assigned(FOnEvaluateReplacements) then
    Result := FOnEvaluateReplacements(s)
  else
    raise Exception.Create('FOnEvaluateReplacements not assigned.');
end;


procedure TfrClickerBMPText.CreateSelectionLabels;
begin
  FSelectedComponentLeftLimitLabel := TLabel.Create(Self);
  FSelectedComponentTopLimitLabel := TLabel.Create(Self);
  FSelectedComponentRightLimitLabel := TLabel.Create(Self);
  FSelectedComponentBottomLimitLabel := TLabel.Create(Self);

  FSelectedComponentLeftLimitLabel.Parent := scrboxPreview;
  FSelectedComponentTopLimitLabel.Parent := scrboxPreview;
  FSelectedComponentRightLimitLabel.Parent := scrboxPreview;
  FSelectedComponentBottomLimitLabel.Parent := scrboxPreview;

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

  FTransparent_SelectedComponentLeftLimitLabel.Parent := scrboxPreview;
  FTransparent_SelectedComponentTopLimitLabel.Parent := scrboxPreview;
  FTransparent_SelectedComponentRightLimitLabel.Parent := scrboxPreview;
  FTransparent_SelectedComponentBottomLimitLabel.Parent := scrboxPreview;

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

  FTransparent_SelectedComponentLeftLimitLabel.Width := 3;
  FTransparent_SelectedComponentTopLimitLabel.Height := 3;
  FTransparent_SelectedComponentRightLimitLabel.Width := 3;
  FTransparent_SelectedComponentBottomLimitLabel.Height := 3;

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


constructor TfrClickerBMPText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastClickedLbe := nil;
  FProfileName := Name;

  FOnTriggerOnControlsModified := nil;
  FOnEvaluateReplacements := nil;
  FOnGetDisplayedText := nil;
  FOnSetCroppingValuesToOtherFontProfiles := nil;
  FOnGetCroppingLinesVisiblity := nil;
  FOnUpdateTextCroppingLimitsInOIFromDraggingLines := nil;

  CreateSelectionLabels;
  tmrStartup.Enabled := True; //after creating labels
end;


destructor TfrClickerBMPText.Destroy;
begin
  inherited Destroy;
end;


procedure TfrClickerBMPText.PreviewTextOnImage(AImg: TImage; ACroppedImg: TImage = nil);
var
  TextDimensions: TSize;
  TextToDisplay: string;
  FontQualityReplacement: Integer;
  FontQualityReplacementStr: string;
  CropLeft, CropTop, CropRight, CropBottom: Integer;
begin
  if AImg = nil then
    Exit;

  if AImg.Picture.Bitmap = nil then
    AImg.Picture.Bitmap := TBitmap.Create;

  TextToDisplay := EvaluateReplacements(DoOnGetDisplayedText); //lbeMatchBitmapText.Text; //ask once

  AImg.Picture.Bitmap.Width := AImg.Width;
  AImg.Picture.Bitmap.Height := AImg.Height;

  AImg.Picture.Bitmap.PixelFormat := pf24bit;

  AImg.Canvas.Font.Color := pnlFG.Color;
  AImg.Canvas.Font.Name := EvaluateReplacements(lbeMatchBitmapTextFontName.Text);

  AImg.Canvas.Font.Size := StrToIntDef(lbeMatchBitmapTextSize.Text, 8);

  AImg.Canvas.Font.Style := [];

  if chkBold.Checked then
    AImg.Canvas.Font.Style := AImg.Canvas.Font.Style + [fsBold];

  if chkItalic.Checked then
    AImg.Canvas.Font.Style := AImg.Canvas.Font.Style + [fsItalic];

  if chkUnderline.Checked then
    AImg.Canvas.Font.Style := AImg.Canvas.Font.Style + [fsUnderline];

  if chkStrikeOut.Checked then
    AImg.Canvas.Font.Style := AImg.Canvas.Font.Style + [fsStrikeOut];

  if cmbMatchBitmapTextFontQuality.ItemIndex = -1 then
    cmbMatchBitmapTextFontQuality.ItemIndex := 0;

  if cmbMatchBitmapTextFontQuality.ItemIndex = Integer(High(TFontQuality)) + 1 then
  begin
    FontQualityReplacementStr := EvaluateReplacements(edtFontQualityReplacement.Text);  //should return a string in the following set: 'Default', 'Draft', 'Proof', 'NonAntialiased', 'Antialiased', 'Cleartype', 'CleartypeNatural'
    FontQualityReplacement := cmbMatchBitmapTextFontQuality.Items.IndexOf(FontQualityReplacementStr);    // 'Var Replacement' string can also be matched here, but it wraps around to 0 (i.e. 'Default')
    if FontQualityReplacement = -1 then
      FontQualityReplacement := 0;  //default to fqDefault

    AImg.Canvas.Font.Quality := TFontQuality(FontQualityReplacement);
  end
  else
    AImg.Canvas.Font.Quality := TFontQuality(cmbMatchBitmapTextFontQuality.ItemIndex);

  AImg.Canvas.Brush.Color := pnlBG.Color;
  AImg.Canvas.Pen.Color := pnlBG.Color;  //yes, BG

  TextDimensions := AImg.Canvas.TextExtent(TextToDisplay);
  AImg.Width := TextDimensions.cx;
  AImg.Height := TextDimensions.cy;
  AImg.Picture.Bitmap.Width := AImg.Width;
  AImg.Picture.Bitmap.Height := AImg.Height;

  AImg.Canvas.Rectangle(0, 0, AImg.Width - 1, AImg.Height - 1);
  AImg.Canvas.TextOut(0, 0, TextToDisplay);     //Do not use replacements here. The editbox should already be updated with replaced strings.

  if Assigned(ACroppedImg) then
  begin
    CropLeft := Max(StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropLeft), 0), 0);
    CropTop := Max(StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropTop), 0), 0);
    CropRight := Max(StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropRight), 0), 0);
    CropBottom := Max(StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropBottom), 0), 0);

    if (CropLeft <> 0) or (CropTop <> 0) or (CropRight <> 0) or (CropBottom <> 0) then
    begin
      ACroppedImg.Width := TextDimensions.cx - (CropLeft + CropRight); //CropLeft is increased as left -> right (towards the text). CropRight is increased right-> left  (towards the text).
      ACroppedImg.Height := TextDimensions.cy - (CropTop + CropBottom);

      if ACroppedImg.Picture.Bitmap = nil then
        ACroppedImg.Picture.Bitmap := TBitmap.Create;

      ACroppedImg.Picture.Bitmap.PixelFormat := pf24bit;

      BitBlt(ACroppedImg.Picture.Bitmap.Canvas.Handle, 0, 0, ACroppedImg.Width, ACroppedImg.Height, AImg.Canvas.Handle, CropLeft, CropTop, SRCCOPY);

      //BitBlt param definition
      //HDC hdcDest, // handle to destination DC
      //int nXDest,  // x-coord of destination upper-left corner
      //int nYDest,  // y-coord of destination upper-left corner
      //int nWidth,  // width of destination rectangle
      //int nHeight, // height of destination rectangle
      //HDC hdcSrc,  // handle to source DC
      //int nXSrc,   // x-coordinate of source upper-left corner
      //int nYSrc,   // y-coordinate of source upper-left corner
      //DWORD dwRop  // raster operation code

      //Output cropped image to the main preview image, for debugging
      //AImg.Width := ACroppedImg.Width;
      //AImg.Height := ACroppedImg.Height;
      //AImg.Picture.Bitmap.Width := AImg.Width;
      //AImg.Picture.Bitmap.Height := AImg.Height;
      //AImg.Canvas.Rectangle(0, 0, AImg.Width - 1, AImg.Height - 1);
      //
      //BitBlt(AImg.Picture.Bitmap.Canvas.Handle, 0, 0, ACroppedImg.Width, ACroppedImg.Height, ACroppedImg.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  end;

  AImg.Repaint;
end;


procedure TfrClickerBMPText.PreviewText;
begin
  PreviewTextOnImage(imgPreview);
end;


procedure TfrClickerBMPText.ClearControls;
begin
  lbeMatchBitmapTextFontName.Text := 'Tahoma';
  lbeMatchBitmapTextSize.Text := '8';
  lbeMatchBitmapTextFGColor.Text := '';
  lbeMatchBitmapTextBGColor.Text := '';
  pnlFG.Color := clBtnFace;
  pnlBG.Color := clBtnFace;
  chkBold.Checked := False;
  chkItalic.Checked := False;
  chkUnderline.Checked := False;
  chkStrikeOut.Checked := False;
  cmbMatchBitmapTextFontQuality.ItemIndex := Integer(fqDefault);
  edtFontQualityReplacement.Visible := False;
end;


procedure TfrClickerBMPText.lbeMatchBitmapTextFGColorChange(Sender: TObject);
begin
  pnlFG.Color := HexToInt(EvaluateReplacements(lbeMatchBitmapTextFGColor.Text));  //moved from ClickActionsArrFrame.pas
  PreviewText;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.lbeMatchBitmapTextFGColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
    Exit;

  FLastClickedLbe := Sender as TLabeledEdit;
end;


procedure TfrClickerBMPText.lbeMatchBitmapTextFontNameChange(Sender: TObject);
begin
  PreviewText;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.lbeMatchBitmapTextSizeChange(Sender: TObject);
begin
  PreviewText;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.lblPreviewClick(Sender: TObject);
begin
  PreviewText;
end;


procedure TfrClickerBMPText.MenuItemColor_GenericClick(Sender: TObject);
begin
  FLastClickedLbe.Text := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
end;


procedure TfrClickerBMPText.MenuItemCopyColorToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := FLastClickedLbe.Text;
end;


procedure TfrClickerBMPText.MenuItemCopyCroppedPreviewImageClick(Sender: TObject);
var
  CroppedBmp: TBitmap;
  X, Y: Integer;
  CropLeft, CropTop, CropRight, CropBottom: Integer;
begin
  if (imgPreview.Picture.Bitmap.Width = 0) and (imgPreview.Picture.Bitmap.Height = 0) then
  begin
    MessageBox(Handle, 'Selected image is empty. Nothing to copy.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  CroppedBmp := TBitmap.Create;
  try
    CropLeft := Max(StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropLeft), 0), 0);
    CropTop := Max(StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropTop), 0), 0);
    CropRight := Max(StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropRight), 0), 0);
    CropBottom := Max(StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropBottom), 0), 0);

    CroppedBMP.Width := imgPreview.Width - (CropLeft + CropRight);
    CroppedBMP.Height := imgPreview.Height - (CropTop + CropBottom);
    X := FSelectedComponentLeftLimitLabel.Left;
    Y := FSelectedComponentTopLimitLabel.Top;

    CroppedBMP.PixelFormat := pf24bit;
    CroppedBMP.Canvas.Pen.Color := clWhite;
    CroppedBMP.Canvas.Brush.Color := clWhite;
    CroppedBMP.Canvas.Rectangle(0, 0, CroppedBMP.Width - 1, CroppedBMP.Height - 1);

    BitBlt(CroppedBMP.Canvas.Handle, 0, 0, CroppedBMP.Width, CroppedBMP.Height, imgPreview.Picture.Bitmap.Canvas.Handle, X, Y, SRCCOPY);

    Clipboard.Assign(CroppedBmp);
  finally
    CroppedBmp.Free;
  end;
end;


procedure TfrClickerBMPText.MenuItemCopyPreviewImageAndCroppingLinesClick(
  Sender: TObject);
var
  TempBmp: TBitmap;
begin
  if (imgPreview.Picture.Bitmap.Width = 0) and (imgPreview.Picture.Bitmap.Height = 0) then
  begin
    MessageBox(Handle, 'Selected image is empty. Nothing to copy.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  TempBmp := TBitmap.Create;
  try
    TempBmp.Width := imgPreview.Width + 2;
    TempBmp.Height := imgPreview.Height + 2;
    TempBmp.Canvas.Pen.Color := clWhite;
    TempBmp.Canvas.Brush.Color := clWhite;
    TempBmp.Canvas.Rectangle(0, 0, TempBmp.Width - 0, TempBmp.Height - 0);

    BitBlt(TempBmp.Canvas.Handle, 1, 1, TempBmp.Width, TempBmp.Height, imgPreview.Picture.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);

    TempBmp.Canvas.Pen.Color := FSelectedComponentLeftLimitLabel.Color;
    TempBmp.Canvas.Line(FSelectedComponentLeftLimitLabel.Left, 0, FSelectedComponentLeftLimitLabel.Left, TempBmp.Height);
    TempBmp.Canvas.Line(FSelectedComponentRightLimitLabel.Left, 0, FSelectedComponentRightLimitLabel.Left, TempBmp.Height);
    TempBmp.Canvas.Line(0, FSelectedComponentTopLimitLabel.Top, TempBmp.Width, FSelectedComponentTopLimitLabel.Top);
    TempBmp.Canvas.Line(0, FSelectedComponentBottomLimitLabel.Top, TempBmp.Width, FSelectedComponentBottomLimitLabel.Top);

    Clipboard.Assign(TempBmp);
  finally
    TempBmp.Free;
  end;
end;


procedure TfrClickerBMPText.MenuItemCopyCroppingValuesToOtherProfilesClick(
  Sender: TObject);
begin
  DoOnSetCroppingValuesToOtherFontProfiles(FMatchBitmapTextCropLeft,
                                           FMatchBitmapTextCropTop,
                                           FMatchBitmapTextCropRight,
                                           FMatchBitmapTextCropBottom,
                                           -1);
end;


procedure TfrClickerBMPText.MenuItemPasteColorFromClipboardClick(Sender: TObject);
begin
  FLastClickedLbe.Text := Clipboard.AsText;
end;


procedure TfrClickerBMPText.MenuItemCopyPreviewImageClick(Sender: TObject);
begin
  if (imgPreview.Picture.Bitmap.Width = 0) and (imgPreview.Picture.Bitmap.Height = 0) then
  begin
    MessageBox(Handle, 'Selected image is empty. Nothing to copy.', PChar(Caption), MB_ICONINFORMATION);
    Exit;
  end;

  Clipboard.Assign(imgPreview.Picture);
end;


procedure TfrClickerBMPText.MenuItemErasePreviewImageClick(Sender: TObject);
begin
  if MessageBox(Handle, 'Are you sure you want to erase the current image?', PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDNO then
    Exit;

  imgPreview.Canvas.Pen.Color := clWhite;
  imgPreview.Canvas.Brush.Color := clWhite;
  imgPreview.Canvas.Brush.Style := bsSolid;
  imgPreview.Canvas.Rectangle(0, 0, imgPreview.Width, imgPreview.Height);
end;


procedure TfrClickerBMPText.MenuItemSavePreviewImageClick(Sender: TObject);
var
  ASaveDialog: TSaveDialog;
begin
  ASaveDialog := TSaveDialog.Create(nil);
  try
    ASaveDialog.Filter := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';
    //ASaveDialog.InitialDir := FBMPsDir;
    if not ASaveDialog.Execute then
      Exit;

    if UpperCase(ExtractFileExt(ASaveDialog.FileName)) <> '.BMP' then
      ASaveDialog.FileName := ASaveDialog.FileName + '.bmp';

    if FileExists(ASaveDialog.FileName) then
      if MessageBox(Handle, 'File already exists. Replace?', PChar(Caption), MB_ICONWARNING + MB_YESNO) = IDNO then
        Exit;

    imgPreview.Picture.Bitmap.SaveToFile(ASaveDialog.FileName);
    //FBMPsDir := ExtractFileDir(ASaveDialog.FileName);
  finally
    ASaveDialog.Free;
  end;
end;


procedure TfrClickerBMPText.pmStandardColorVariablesPopup(Sender: TObject);
var
  i: Integer;
  s: string;
  TextColor: TColor;
begin
  for i := 0 to pmStandardColorVariables.Items.Count - 1 do
    if Pos('$', pmStandardColorVariables.Items.Items[i].Caption) > 0 then
    begin
      if pmStandardColorVariables.Items.Items[i].Bitmap <> nil then
        pmStandardColorVariables.Items.Items[i].Bitmap.Free;

      pmStandardColorVariables.Items.Items[i].Bitmap := TBitmap.Create;
      pmStandardColorVariables.Items.Items[i].Bitmap.Width := 16;
      pmStandardColorVariables.Items.Items[i].Bitmap.Height := 16;
      s := EvaluateReplacements(pmStandardColorVariables.Items.Items[i].Caption);
      TextColor := HexToInt(s);

      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Pen.Color := 1;  // > 0
      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Brush.Color := TextColor;
      pmStandardColorVariables.Items.Items[i].Bitmap.Canvas.Rectangle(0, 0, 16, 16);
    end;
end;


procedure TfrClickerBMPText.lbeMatchBitmapTextBGColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
    Exit;

  FLastClickedLbe := Sender as TLabeledEdit;
end;


procedure TfrClickerBMPText.lbeMatchBitmapTextBGColorChange(Sender: TObject);
begin
  pnlBG.Color := HexToInt(EvaluateReplacements(lbeMatchBitmapTextBGColor.Text));  //moved from ClickActionsArrFrame.pas
  PreviewText;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.btnBrowseFontClick(Sender: TObject);
var
  //ReplacementVar: string;
  AFontDialog: TFontDialog;
begin
  AFontDialog := TFontDialog.Create(nil);
  try
    AFontDialog.Font.Name := EvaluateReplacements(lbeMatchBitmapTextFontName.Text);
    AFontDialog.Font.Size := StrToIntDef(lbeMatchBitmapTextSize.Text, 8);

    //ReplacementVar := GetReplacementVarFromString(lbeMatchBitmapTextFontName.Text);

    AFontDialog.Font.Style := [];

    if chkBold.Checked then
      AFontDialog.Font.Style := AFontDialog.Font.Style + [fsBold];

    if chkItalic.Checked then
      AFontDialog.Font.Style := AFontDialog.Font.Style + [fsItalic];

    if chkUnderline.Checked then
      AFontDialog.Font.Style := AFontDialog.Font.Style + [fsUnderline];

    if chkStrikeOut.Checked then
      AFontDialog.Font.Style := AFontDialog.Font.Style + [fsStrikeOut];

    if not AFontDialog.Execute then
      Exit;

    //if ReplacementVar <> '' then
    //begin
    //  if MessageBox(Handle, PChar('A variable replacement (' + ReplacementVar + ') was found to be used as font name.' + #13#10 + 'Do you want to update it?'), PChar(Caption), MB_ICONQUESTION + MB_YESNO) = IDYES then
    //    vallstVariables.Values[ReplacementVar] := AFontDialog.Font.Name;
    //end
    //else
      lbeMatchBitmapTextFontName.Text := AFontDialog.Font.Name;

    lbeMatchBitmapTextSize.Text := IntToStr(AFontDialog.Font.Size);

    chkBold.Checked := fsBold in AFontDialog.Font.Style;
    chkItalic.Checked := fsItalic in AFontDialog.Font.Style;
    chkUnderline.Checked := fsUnderline in AFontDialog.Font.Style;
    chkStrikeOut.Checked := fsStrikeOut in AFontDialog.Font.Style;

    PreviewText;
  finally
    AFontDialog.Free;
  end;

  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.chkBoldClick(Sender: TObject);
begin
  PreviewText;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.chkItalicClick(Sender: TObject);
begin
  PreviewText;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.chkStrikeOutClick(Sender: TObject);
begin
  PreviewText;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.chkUnderlineClick(Sender: TObject);
begin
  PreviewText;
  DoOnTriggerOnControlsModified;
end;


procedure TfrClickerBMPText.cmbMatchBitmapTextFontQualityChange(Sender: TObject);
begin
  PreviewText;
  DoOnTriggerOnControlsModified;

  edtFontQualityReplacement.Visible := cmbMatchBitmapTextFontQuality.ItemIndex = Integer(High(TFontQuality)) + 1;
end;


procedure TfrClickerBMPText.cmbMatchBitmapTextFontQualityDropDown(
  Sender: TObject);
const
  CFontQualityNames: array[Low(TFontQuality)..High(TFontQuality)] of string = (
    'Default', 'Draft', 'Proof', 'NonAntialiased', 'Antialiased', 'Cleartype', 'CleartypeNatural');
var
  i: TFontQuality;
  OldItemIndex: Integer;
begin
  OldItemIndex := cmbMatchBitmapTextFontQuality.ItemIndex;
  try
    cmbMatchBitmapTextFontQuality.Clear;

    for i := Low(TFontQuality) to High(TFontQuality) do
      cmbMatchBitmapTextFontQuality.Items.Add(CFontQualityNames[i]);

    cmbMatchBitmapTextFontQuality.Items.Add('Var Replacement');
  finally
    cmbMatchBitmapTextFontQuality.ItemIndex := OldItemIndex;
  end;
end;


procedure TfrClickerBMPText.imgPreviewResize(Sender: TObject);
begin
  FSelectedComponentLeftLimitLabel.Height := Max(imgPreview.Height, scrboxPreview.Height) - 20;
  FSelectedComponentRightLimitLabel.Height := Max(imgPreview.Height, scrboxPreview.Height) - 20;
  FSelectedComponentTopLimitLabel.Width := Max(imgPreview.Width, scrboxPreview.Width) - 20;
  FSelectedComponentBottomLimitLabel.Width := Max(imgPreview.Width, scrboxPreview.Width) - 20;

  FTransparent_SelectedComponentLeftLimitLabel.Height := FSelectedComponentLeftLimitLabel.Height;
  FTransparent_SelectedComponentRightLimitLabel.Height := FSelectedComponentRightLimitLabel.Height;
  FTransparent_SelectedComponentTopLimitLabel.Width := FSelectedComponentTopLimitLabel.Width;
  FTransparent_SelectedComponentBottomLimitLabel.Width := FSelectedComponentBottomLimitLabel.Width;
end;


procedure TfrClickerBMPText.pnlFGDblClick(Sender: TObject);
var
  AColorDialog: TColorDialog;
begin
  AColorDialog := TColorDialog.Create(nil);
  try
    AColorDialog.Color := pnlFG.Color;

    if AColorDialog.Execute then
    begin
      pnlFG.Color := AColorDialog.Color;
      lbeMatchBitmapTextFGColor.Text := IntToHex(Int64(pnlFG.Color and $FFFFFF), 6);
      PreviewText;
      DoOnTriggerOnControlsModified;
    end;
  finally
    AColorDialog.Free;
  end;
end;


procedure TfrClickerBMPText.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  DisplayCroppingLines(DoOnGetCroppingLinesVisiblity);
end;


procedure TfrClickerBMPText.pnlBGDblClick(Sender: TObject);
var
  AColorDialog: TColorDialog;
begin
  AColorDialog := TColorDialog.Create(nil);
  try
    AColorDialog.Color := pnlBG.Color;

    if AColorDialog.Execute then
    begin
      pnlBG.Color := AColorDialog.Color;
      lbeMatchBitmapTextBGColor.Text := IntToHex(Int64(pnlBG.Color and $FFFFFF), 6);
      PreviewText;
      DoOnTriggerOnControlsModified;
    end;
  finally
    AColorDialog.Free;
  end;
end;


//=============


procedure TfrClickerBMPText.tmrUpdateCropEditBoxesTimer(Sender: TObject);
var
  Offsets: TSimpleRectString;
begin
  tmrUpdateCropEditBoxes.Enabled := False;

  if Pos('$', FMatchBitmapTextCropLeft) = 0 then
    FMatchBitmapTextCropLeft := IntToStr(FSelectedComponentLeftLimitLabel.Left);

  if Pos('$', FMatchBitmapTextCropRight) = 0 then
    FMatchBitmapTextCropRight := IntToStr(imgPreview.Width - FSelectedComponentRightLimitLabel.Left + 1);

  if Pos('$', FMatchBitmapTextCropTop) = 0 then
    FMatchBitmapTextCropTop := IntToStr(FSelectedComponentTopLimitLabel.Top);

  if Pos('$', FMatchBitmapTextCropBottom) = 0 then
    FMatchBitmapTextCropBottom := IntToStr(imgPreview.Height - FSelectedComponentBottomLimitLabel.Top + 1);

  Offsets.Left := FMatchBitmapTextCropLeft;
  Offsets.Top := FMatchBitmapTextCropTop;
  Offsets.Right := FMatchBitmapTextCropRight;
  Offsets.Bottom := FMatchBitmapTextCropBottom;
  DoOnUpdateTextCroppingLimitsInOIFromDraggingLines([llLeft, llTop, llRight, llBottom], Offsets, FProfileName);
end;


procedure TfrClickerBMPText.UpdateSelectionLabelsFromCropEditBoxes;
begin
  FSelectedComponentLeftLimitLabel.Left := StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropLeft), 0);
  FSelectedComponentTopLimitLabel.Top := StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropTop), 0);
  FSelectedComponentRightLimitLabel.Left := imgPreview.Width - StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropRight), 0) + 1;
  FSelectedComponentBottomLimitLabel.Top := imgPreview.Height - StrToIntDef(EvaluateReplacements(FMatchBitmapTextCropBottom), 0) + 1;

  FSelectedComponentLeftLimitLabel.Left := Max(0, Min(FSelectedComponentLeftLimitLabel.Left, imgPreview.Width - 3));
  FSelectedComponentTopLimitLabel.Top := Max(0, Min(FSelectedComponentTopLimitLabel.Top, imgPreview.Height - 3));

  FSelectedComponentRightLimitLabel.Left := Max(FSelectedComponentLeftLimitLabel.Left + 4, FSelectedComponentRightLimitLabel.Left);
  FSelectedComponentBottomLimitLabel.Top := Max(FSelectedComponentTopLimitLabel.Top + 4, FSelectedComponentBottomLimitLabel.Top);

  FSelectedComponentRightLimitLabel.Left := Max(0, Min(FSelectedComponentRightLimitLabel.Left, imgPreview.Width + 1));
  FSelectedComponentBottomLimitLabel.Top := Max(0, Min(FSelectedComponentBottomLimitLabel.Top, imgPreview.Height + 1));

  FTransparent_SelectedComponentLeftLimitLabel.Left := FSelectedComponentLeftLimitLabel.Left;
  FTransparent_SelectedComponentTopLimitLabel.Top := FSelectedComponentTopLimitLabel.Top;
  FTransparent_SelectedComponentRightLimitLabel.Left := FSelectedComponentRightLimitLabel.Left;
  FTransparent_SelectedComponentBottomLimitLabel.Top := FSelectedComponentBottomLimitLabel.Top;
end;


procedure TfrClickerBMPText.UpdateSelectionLabelsFromCropInfo(var ABMPText: TClkFindControlMatchBitmapText);
begin
  FSelectedComponentLeftLimitLabel.Left := StrToIntDef(EvaluateReplacements(ABMPText.CropLeft), 0);
  FSelectedComponentTopLimitLabel.Top := StrToIntDef(EvaluateReplacements(ABMPText.CropTop), 0);
  FSelectedComponentRightLimitLabel.Left := imgPreview.Width - StrToIntDef(EvaluateReplacements(ABMPText.CropRight), 0) + 1;
  FSelectedComponentBottomLimitLabel.Top := imgPreview.Height - StrToIntDef(EvaluateReplacements(ABMPText.CropBottom), 0) + 1;

  FSelectedComponentLeftLimitLabel.Left := Max(0, Min(FSelectedComponentLeftLimitLabel.Left, imgPreview.Width - 3));
  FSelectedComponentTopLimitLabel.Top := Max(0, Min(FSelectedComponentTopLimitLabel.Top, imgPreview.Height - 3));

  FSelectedComponentRightLimitLabel.Left := Max(FSelectedComponentLeftLimitLabel.Left + 4, FSelectedComponentRightLimitLabel.Left);
  FSelectedComponentBottomLimitLabel.Top := Max(FSelectedComponentTopLimitLabel.Top + 4, FSelectedComponentBottomLimitLabel.Top);

  FSelectedComponentRightLimitLabel.Left := Max(0, Min(FSelectedComponentRightLimitLabel.Left, imgPreview.Width + 1));
  FSelectedComponentBottomLimitLabel.Top := Max(0, Min(FSelectedComponentBottomLimitLabel.Top, imgPreview.Height + 1));

  FTransparent_SelectedComponentLeftLimitLabel.Left := FSelectedComponentLeftLimitLabel.Left;
  FTransparent_SelectedComponentTopLimitLabel.Top := FSelectedComponentTopLimitLabel.Top;
  FTransparent_SelectedComponentRightLimitLabel.Left := FSelectedComponentRightLimitLabel.Left;
  FTransparent_SelectedComponentBottomLimitLabel.Top := FSelectedComponentBottomLimitLabel.Top;
end;


procedure TfrClickerBMPText.FTransparent_LeftMouseDown(Sender: TObject;
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


procedure TfrClickerBMPText.FTransparent_LeftMouseMove(Sender: TObject;
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

    CurrentLabel.Left := Max(0, Min(FTransparent_SelectedComponentRightLimitLabel.Left - 4, NewLeft));
    FSelectedComponentLeftLimitLabel.Left := CurrentLabel.Left;

    tmrUpdateCropEditBoxes.Enabled := True;
  end;
end;


procedure TfrClickerBMPText.FTransparent_LeftMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerBMPText.FTransparent_RightMouseDown(Sender: TObject;
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


procedure TfrClickerBMPText.FTransparent_RightMouseMove(Sender: TObject;
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

    CurrentLabel.Left := Max(FTransparent_SelectedComponentLeftLimitLabel.Left + 4, Min(imgPreview.Width + 1, NewLeft));
    FSelectedComponentRightLimitLabel.Left := CurrentLabel.Left;

    tmrUpdateCropEditBoxes.Enabled := True;
  end;
end;


procedure TfrClickerBMPText.FTransparent_RightMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerBMPText.FTransparent_TopMouseDown(Sender: TObject;
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


procedure TfrClickerBMPText.FTransparent_TopMouseMove(Sender: TObject;
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

    CurrentLabel.Top := Max(0, Min(FTransparent_SelectedComponentBottomLimitLabel.Top - 4, NewTop));
    FSelectedComponentTopLimitLabel.Top := CurrentLabel.Top;

    tmrUpdateCropEditBoxes.Enabled := True;
  end;
end;


procedure TfrClickerBMPText.FTransparent_TopMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerBMPText.FTransparent_BottomMouseDown(Sender: TObject;
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


procedure TfrClickerBMPText.FTransparent_BottomMouseMove(Sender: TObject;
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

    CurrentLabel.Top := Max(FTransparent_SelectedComponentTopLimitLabel.Top + 4, Min(imgPreview.Height + 1, NewTop));
    FSelectedComponentBottomLimitLabel.Top := CurrentLabel.Top;

    tmrUpdateCropEditBoxes.Enabled := True;
  end;
end;


procedure TfrClickerBMPText.FTransparent_BottomMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectionHold := False;
end;


procedure TfrClickerBMPText.DisplayCroppingLines(AVisible: Boolean);
begin
  FSelectedComponentLeftLimitLabel.Visible := AVisible;
  FSelectedComponentTopLimitLabel.Visible := AVisible;
  FSelectedComponentRightLimitLabel.Visible := AVisible;
  FSelectedComponentBottomLimitLabel.Visible := AVisible;

  FTransparent_SelectedComponentLeftLimitLabel.Visible := AVisible;
  FTransparent_SelectedComponentTopLimitLabel.Visible := AVisible;
  FTransparent_SelectedComponentRightLimitLabel.Visible := AVisible;
  FTransparent_SelectedComponentBottomLimitLabel.Visible := AVisible;
end;

end.

