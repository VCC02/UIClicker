{
    Copyright (C) 2026 VCC
    creation date: 13 Jan 2026
    initial release date: 13 Jan 2026

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


unit ClickerIconsDM;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Controls, PopupNotifier;

type

  { TdmClickerIcons }

  TdmClickerIcons = class(TDataModule)
    imglstLoadSetVarFromIniFileProperties: TImageList;
    imglstScreenshotOptionsProperties: TImageList;
    imgLstMisc: TImageList;
    imglstActionExecution: TImageList;
    imglstActionProperties: TImageList;
    imglstActions: TImageList;
    imglstActions16: TImageList;
    imglstCallTemplateLoopProperties: TImageList;
    imglstCallTemplateProperties: TImageList;
    imglstClickProperties: TImageList;
    imglstEditTemplateOperationProperties: TImageList;
    imglstEditTemplateProperties: TImageList;
    imglstEditTemplateWhichTemplateProperties: TImageList;
    imglstExecAppProperties: TImageList;
    imglstFCMatchCriteriaProperties: TImageList;
    imglstFindControlProperties: TImageList;
    imglstFindSubControlProperties: TImageList;
    imglstFontColorProperties: TImageList;
    imglstFSCMatchCriteriaProperties: TImageList;
    imglstGPUSettingsProperties: TImageList;
    imglstImageEffectSettingsProperties: TImageList;
    imglstInitialRectangleProperties: TImageList;
    imglstLoadSetVarFromFileProperties: TImageList;
    imglstMatchBitmapAlgorithmSettingsProperties: TImageList;
    imglstMatchBitmapTextProperties: TImageList;
    imglstMatchByHistogramSettingsProperties: TImageList;
    imglstMatchPrimitiveFilesMenu: TImageList;
    imglstPluginProperties: TImageList;
    imglstRenderingInBrowserSettingsProperties: TImageList;
    imglstSaveSetVarToFileProperties: TImageList;
    imglstSetTextProperties: TImageList;
    imglstSetVar: TImageList;
    imglstSetVarProperties: TImageList;
    imglstSleepProperties: TImageList;
    imglstUsedMatchCriteria: TImageList;
    imglstUsedMatchCriteriaSub: TImageList;
    imglstWindowOperationsProperties: TImageList;
    pnSetVarFormat: TPopupNotifier;
  private

  public
    procedure DisplayVarFormatNotifier(AOverComponent: THandle);
  end;

var
  dmClickerIcons: TdmClickerIcons;



implementation

{$R *.frm}


uses
  Forms, ClickerUtils;


procedure TdmClickerIcons.DisplayVarFormatNotifier(AOverComponent: THandle);
var
  hwc: TCompRec;
  NotifierFormRect: TRect;
begin
  hwc := GetWindowClassRec(AOverComponent);

  pnSetVarFormat.vNotifierForm.BorderWidth := 1;
  pnSetVarFormat.vNotifierForm.BorderStyle := bsSizeToolWin;
  pnSetVarFormat.vNotifierForm.Width := 300;
  pnSetVarFormat.vNotifierForm.Height := 50;

  NotifierFormRect.Left := hwc.ComponentRectangle.Left;
  NotifierFormRect.Top := hwc.ComponentRectangle.Bottom + 3;
  NotifierFormRect.Width := pnSetVarFormat.vNotifierForm.Width;
  NotifierFormRect.Height := pnSetVarFormat.vNotifierForm.Height;

  if NotifierFormRect.Right > Screen.Width - 3 then
    Dec(NotifierFormRect.Left, Screen.Width - 3 - NotifierFormRect.Right);

  if NotifierFormRect.Bottom > Screen.Height then
    NotifierFormRect.Top := hwc.ComponentRectangle.Top - NotifierFormRect.Height - 3;

  pnSetVarFormat.ShowAtPos(NotifierFormRect.Left, NotifierFormRect.Top);
end;

end.

