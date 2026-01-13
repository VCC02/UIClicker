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
  Classes, SysUtils, Controls;

type

  { TdmClickerIcons }

  TdmClickerIcons = class(TDataModule)
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
    imglstSetVarProperties: TImageList;
    imglstSleepProperties: TImageList;
    imglstUsedMatchCriteria: TImageList;
    imglstUsedMatchCriteriaSub: TImageList;
    imglstWindowOperationsProperties: TImageList;
  private

  public

  end;

var
  dmClickerIcons: TdmClickerIcons;

implementation

{$R *.frm}

end.

