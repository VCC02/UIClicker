#   Copyright (C) 2022-2025 VCC
#   creation date: Aug 2022
#   initial release date: 10 Aug 2022
#
#   author: VCC
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"),
#   to deal in the Software without restriction, including without limitation
#   the rights to use, copy, modify, merge, publish, distribute, sublicense,
#   and/or sell copies of the Software, and to permit persons to whom the
#   Software is furnished to do so, subject to the following conditions:
#   The above copyright notice and this permission notice shall be included
#   in all copies or substantial portions of the Software.
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
#   OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


import sys
import ctypes
import ctypes.wintypes

from ctypes.wintypes import LPCSTR, LPCWSTR, BYTE, BOOLEAN, WORD, LONG, LARGE_INTEGER
from ctypes import Structure, POINTER
from enum import Enum


CClickType_Click = 0
CClickType_Drag = 1
CClickType_ButtonDown = 2
CClickType_ButtonUp = 3
CClickType_Wheel = 4

#Various datatypes, translated from ClickerUtils.pas

class TClkAction: #(Enum):
    acClick = 0
    acExecApp = 1
    acFindControl = 2
    acFindSubControl = 3
    acSetControlText = 4
    acCallTemplate = 5
    acSleep = 6
    acSetVar = 7
    acWindowOperations = 8
    acLoadSetVarFromFile = 9
    acSaveSetVarToFile = 10
    acPlugin = 11
    acEditTemplate = 12

class TXClickPointReference: #(Enum):
    xrefLeft = 0
    xrefRight = 1
    xrefWidth = 2
    xrefVar = 3
    xrefAbsolute = 4
    

class TYClickPointReference: #(Enum):
    yrefTop = 0
    yrefBottom = 1
    yrefHeight = 2
    yrefVar = 3
    yrefAbsolute = 4


#TMouseButton from controls.pp
class TMouseButton: #(Enum):
    mbLeft = 0
    mbRight = 1
    mbMiddle = 2
    mbExtra1 = 3
    mbExtra2 = 4


class TMouseWheelType: #(Enum):
    mwtVert = 0
    mwtHoriz = 1
    
    
class TExecAppUseInheritHandles: #(Enum):
    uihNo = 0
    uihYes = 1
    uihOnlyWithStdInOut = 2
    
    
class TSearchForControlMode: #(Enum):
    sfcmGenGrid = 0
    sfcmEnumWindows = 1
    sfcmFindWindow = 2
    
class TMatchBitmapAlgorithm: #(Enum):
    mbaBruteForce = 0
    mbaXYMultipleAndOffsets = 1
    mbaRawHistogramZones = 2
    
#TFontQuality from Graphics.pp
class TFontQuality: #(Enum):
    fqDefault = 0 
    fqDraft = 1
    fqProof = 2
    fqNonAntialiased = 3
    fqAntialiased = 4
    fqCleartype = 5
    fqCleartypeNatural = 6

class TImageSource: #(Enum):
    isScreenshot = 0
    isFile = 1

class TImageSourceFileNameLocation: #(Enum):
    isflDisk = 0
    isflMem = 1

class TRenderingRequestType: #(Enum):
    rrtShellExecute = 0
    rrtAction = 1

class TFontSizeUnit: #(Enum):
    fsuPt = 0
    fsuPx = 1


class TClkSetTextControlType: #(Enum):
    stEditBox = 0
    stComboBox = 1
    stKeystrokes = 2


class TWindowOperation: #(Enum):
    woBringToFront = 0
    woMoveResize = 1
    woClose = 2
    woFitIntoView = 3


class TLoopDirection:  #
    ldInc = 0
    ldDec = 1
    ldAuto = 2

class TLoopEvalBreakPosition:  #(Enum):
    lebpAfterContent = 0
    lebpBeforeContent = 1


class TEditTemplateOperation: #(Enum):
    etoNewAction = 0
    etoUpdateAction = 1
    etoMoveAction = 2
    etoDeleteAction = 3
    etoDuplicateAction = 4
    etoRenameAction = 5
    etoEnableAction = 6
    etoDisableAction = 7
    etoGetProperty = 8
    etoSetProperty = 9
    etoSetCondition = 10
    etoSetTimeout = 11
    etoExecuteAction = 12
    etoSaveTemplate = 13


class TEditTemplateWhichTemplate: #(Enum):
    etwtSelf = 0
    etwtOther = 1


class TClickOptions(Structure):
    _fields_ = [("XClickPointReference", BYTE), #LONG), #TXClickPointReference
               ("YClickPointReference", BYTE), #LONG), #TYClickPointReference
               ("XClickPointVar", LPCWSTR),
               ("YClickPointVar", LPCWSTR),
               ("XOffset", LPCWSTR),
               ("YOffset", LPCWSTR),
               ("MouseButton", BYTE), #LONG), #TMouseButton
               ("ClickWithCtrl", BOOLEAN),
               ("ClickWithAlt", BOOLEAN),
               ("ClickWithShift", BOOLEAN),
               ("ClickWithDoubleClick", BOOLEAN),  #not used anymore (field kept for backwards compatibility with old templates)
               ("Count", LONG),
               ("LeaveMouse", BOOLEAN),
               ("MoveWithoutClick", BOOLEAN),
               ("ClickType", LONG),    #see CClickType_Click and CClickType_DoubleClick
               ("XClickPointReferenceDest", BYTE), #LONG), #TXClickPointReference
               ("YClickPointReferenceDest", BYTE), #LONG), #TYClickPointReference
               ("XClickPointVarDest", LPCWSTR),
               ("YClickPointVarDest", LPCWSTR),
               ("XOffsetDest", LPCWSTR),
               ("YOffsetDest", LPCWSTR),
               ("MouseWheelType", BYTE), #LONG), #TMouseWheelType
               ("MouseWheelAmount", LPCWSTR),
               ("DelayAfterMovingToDestination", LPCWSTR),
               ("DelayAfterMouseDown", LPCWSTR),
               ("MoveDuration", LPCWSTR),
               ("UseClipCursor", BOOLEAN)]

PClickOptions = ctypes.POINTER(TClickOptions)

def GetDefaultClickOptions():
    ClickOptions = TClickOptions()
    ClickOptions.XClickPointReference = TXClickPointReference.xrefLeft
    ClickOptions.YClickPointReference = TYClickPointReference.yrefTop
    ClickOptions.XClickPointVar = '$Control_Left$'
    ClickOptions.YClickPointVar = '$Control_Top$'
    ClickOptions.XOffset = ''
    ClickOptions.YOffset = ''
    ClickOptions.MouseButton = TMouseButton.mbLeft
    ClickOptions.ClickWithCtrl = False
    ClickOptions.ClickWithAlt = False
    ClickOptions.ClickWithShift = False
    ClickOptions.ClickWithDoubleClick = False
    ClickOptions.Count = 1
    ClickOptions.LeaveMouse = False
    ClickOptions.MoveWithoutClick = False
    ClickOptions.ClickType = CClickType_Click
    ClickOptions.XClickPointReferenceDest = TXClickPointReference.xrefLeft
    ClickOptions.YClickPointReferenceDest = TYClickPointReference.yrefTop
    ClickOptions.XClickPointVarDest = '$Control_Left$'
    ClickOptions.YClickPointVarDest = '$Control_Top$'
    ClickOptions.XOffsetDest = ''
    ClickOptions.YOffsetDest = ''
    ClickOptions.MouseWheelType = TMouseWheelType.mwtVert
    ClickOptions.MouseWheelAmount = '0'
    ClickOptions.DelayAfterMovingToDestination = '50'
    ClickOptions.DelayAfterMouseDown = '200'
    ClickOptions.MoveDuration = '-1'
    ClickOptions.UseClipCursor = False
    return ClickOptions


class TExecAppOptions(Structure):
    _fields_ = [("PathToApp", LPCWSTR),
               ("ListOfParams", LPCWSTR),
               ("WaitForApp", BOOLEAN),
               ("AppStdIn", LPCWSTR),
               ("CurrentDir", LPCWSTR),
               ("UseInheritHandles", BYTE), #LONG) #TExecAppUseInheritHandless
               ("NoConsole", BOOLEAN),
               ("VerifyFileExistence", BOOLEAN),
               ("LeaveRunningAfterTimeout", BOOLEAN)] 
               

PExecAppOptions = ctypes.POINTER(TExecAppOptions)

def GetDefaultExecAppOptions():
    ExecAppOptions = TExecAppOptions()
    ExecAppOptions.PathToApp = ''
    ExecAppOptions.ListOfParams = ''
    ExecAppOptions.WaitForApp = False
    ExecAppOptions.AppStdIn = ''
    ExecAppOptions.CurrentDir = ''
    ExecAppOptions.UseInheritHandles = TExecAppUseInheritHandles.uihOnlyWithStdInOut
    ExecAppOptions.NoConsole = False
    ExecAppOptions.VerifyFileExistence = False
    ExecAppOptions.LeaveRunningAfterTimeout = False
    return ExecAppOptions


class TClkFindControlMatchCriteria(Structure):
    _fields_ = [("WillMatchText", BOOLEAN),
               ("WillMatchClassName", BOOLEAN),
               ("SearchForControlMode", LONG)] #TSearchForControlMode  #This one stays LONG

class TClkFindSubControlMatchCriteria(Structure):
    _fields_ = [("WillMatchBitmapText", BOOLEAN),
               ("WillMatchBitmapFiles", BOOLEAN),
               ("WillMatchPrimitiveFiles", BOOLEAN)]

class TMatchBitmapAlgorithmSettings(Structure):
    _fields_ = [("XMultipleOf", LONG),
               ("YMultipleOf", LONG),
               ("XOffset", LONG),
               ("YOffset", LONG)]

class TRectString(Structure):
    _fields_ = [("Left", LPCWSTR),
               ("Top", LPCWSTR),
               ("Right", LPCWSTR),
               ("Bottom", LPCWSTR),
               ("LeftOffset", LPCWSTR),
               ("TopOffset", LPCWSTR),
               ("RightOffset", LPCWSTR),
               ("BottomOffset", LPCWSTR)]


class TMatchByHistogramSettings(Structure):
    _fields_ = [("MinPercentColorMatch", LPCWSTR),
               ("MostSignificantColorCountInSubBmp", LPCWSTR),
               ("MostSignificantColorCountInBackgroundBmp", LPCWSTR)]

               
class TRenderingInBrowserSettings(Structure):
    _fields_ = [("RenderingRequestType", BYTE), #TRenderingRequestType
               ("ReceivingBitmapsTimeout", LONG),
               ("ActionForSendingRequest", LPCWSTR),
               ("UsePluginForReceivingBitmaps", BOOLEAN),
               ("PluginActionForReceivingBitmaps", LPCWSTR),
               ("FontSizeUnit", BYTE)] #TFontSizeUnit


###########/////////////////

#Font profile structure. MatchBitmapText field from TFindControlOptions is a dynamic array of TClkFindControlMatchBitmapText.
class TClkFindControlMatchBitmapText(Structure):
    _fields_ = [("ForegroundColor", LPCWSTR),
               ("BackgroundColor", LPCWSTR),
               ("FontName", LPCWSTR),
               ("FontSize", LONG),
               ("Bold", BOOLEAN),
               ("Italic", BOOLEAN),
               ("Underline", BOOLEAN),
               ("StrikeOut", BOOLEAN),
               ("FontQuality", BYTE), #LONG), #TFontQuality
               ("FontQualityUsesReplacement", BOOLEAN),
               ("FontQualityReplacement", LPCWSTR),
               ("CharSet", BYTE),
               ("Orientation", LONG),
               ("Pitch", BYTE),
               ("ProfileName", LPCWSTR),
               ("CropLeft", LPCWSTR),
               ("CropTop", LPCWSTR),
               ("CropRight", LPCWSTR),
               ("CropBottom", LPCWSTR),
               ("IgnoreBackgroundColor", BOOLEAN)]

PClkFindControlMatchBitmapText = ctypes.POINTER(TClkFindControlMatchBitmapText)

def GetDefaultMatchBitmapText():
    MatchBitmapTextProfile = TClkFindControlMatchBitmapText()
    MatchBitmapTextProfile.ForegroundColor = '$Color_WindowText$'
    MatchBitmapTextProfile.BackgroundColor = '$Color_BtnFace$'
    MatchBitmapTextProfile.FontName = 'Tahoma'
    MatchBitmapTextProfile.FontSize = 8
    MatchBitmapTextProfile.Bold = False
    MatchBitmapTextProfile.Italic = False
    MatchBitmapTextProfile.Underline = False
    MatchBitmapTextProfile.StrikeOut = False
    MatchBitmapTextProfile.FontQuality = TFontQuality.fqNonAntialiased
    MatchBitmapTextProfile.FontQualityUsesReplacement = False
    MatchBitmapTextProfile.FontQualityReplacement = ''
    MatchBitmapTextProfile.CharSet = 1
    MatchBitmapTextProfile.Orientation = 0
    MatchBitmapTextProfile.Pitch = 0
    MatchBitmapTextProfile.ProfileName = 'SomeFontProfileName'
    MatchBitmapTextProfile.CropLeft = '0'
    MatchBitmapTextProfile.CropTop = '0'
    MatchBitmapTextProfile.CropRight = '0'
    MatchBitmapTextProfile.CropBottom = '0'
    MatchBitmapTextProfile.IgnoreBackgroundColor = False
    return MatchBitmapTextProfile


class TMatchBitmapTextRec(Structure):
    _fields_ = [("ArrLen", LONG),
               ("Items", PClkFindControlMatchBitmapText)]

    def __init__(self, Count):
        TheItems = (TClkFindControlMatchBitmapText * Count)()
        self.Items = ctypes.cast(TheItems, PClkFindControlMatchBitmapText)
        self.ArrLen = Count
        
        for i in range(Count):
          self.Items[i] = GetDefaultMatchBitmapText()

PMatchBitmapTextRec = ctypes.POINTER(TMatchBitmapTextRec)


class TFindControlOptions(Structure):
    _fields_ = [("DummyField", LONG),
               ("MatchCriteria", TClkFindControlMatchCriteria),
               ("AllowToFail", BOOLEAN),
               ("MatchText", LPCWSTR),
               ("MatchClassName", LPCWSTR),
               ("MatchTextSeparator", LPCWSTR),
               ("MatchClassNameSeparator", LPCWSTR),
               ("InitialRectangle", TRectString),
               ("UseWholeScreen", BOOLEAN),
               ("WaitForControlToGoAway", BOOLEAN),
               ("StartSearchingWithCachedControl", BOOLEAN),
               ("CachedControlLeft", LPCWSTR),
               ("CachedControlTop", LPCWSTR),
               ("GetAllControls", BOOLEAN),
               ("PrecisionTimeout", BOOLEAN),
               ("EvaluateTextCount", LPCWSTR)
               ]

PFindControlOptions = ctypes.POINTER(TFindControlOptions)

def GetDefaultFindControlOptions():
    FindControlOptions = TFindControlOptions()
    FindControlOptions.MatchCriteria = TClkFindControlMatchCriteria()
    FindControlOptions.MatchCriteria.WillMatchText = True
    FindControlOptions.MatchCriteria.WillMatchClassName = True
    FindControlOptions.MatchCriteria.SearchForControlMode = TSearchForControlMode.sfcmGenGrid

    FindControlOptions.AllowToFail = False
    FindControlOptions.MatchText = 'SomeText'
    FindControlOptions.MatchClassName = 'TButton'
    FindControlOptions.MatchTextSeparator = ''
    FindControlOptions.MatchClassNameSeparator = ''

    FindControlOptions.InitialRectangle = TRectString()
    FindControlOptions.InitialRectangle.Left = '$Control_Left$'
    FindControlOptions.InitialRectangle.Top = '$Control_Top$'
    FindControlOptions.InitialRectangle.Right = '$Control_Right$'
    FindControlOptions.InitialRectangle.Bottom = '$Control_Bottom$'
    FindControlOptions.InitialRectangle.LeftOffset = '0'
    FindControlOptions.InitialRectangle.TopOffset = '0'
    FindControlOptions.InitialRectangle.RightOffset = '0'
    FindControlOptions.InitialRectangle.BottomOffset = '0'

    FindControlOptions.UseWholeScreen = True  #usually True for finding a window, and False, for finding a (sub)control on a window or another control.  
    FindControlOptions.WaitForControlToGoAway = False
    FindControlOptions.StartSearchingWithCachedControl = False
    FindControlOptions.CachedControlLeft = ''
    FindControlOptions.CachedControlTop = ''

    FindControlOptions.GetAllControls = False

    FindControlOptions.PrecisionTimeout = False
    FindControlOptions.EvaluateTextCount = "-1"

    return FindControlOptions


#def GetDefaultFindSubControlOptions():
#    FindSubControlOptions = GetDefaultFindControlOptions()
#    FindSubControlOptions.MatchCriteria.WillMatchText = False
#    FindSubControlOptions.MatchCriteria.WillMatchClassName = False
#    FindSubControlOptions.MatchCriteria.WillMatchBitmapText = True
#    FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles = False
#    FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles = False
#    FindSubControlOptions.UseWholeScreen = False
#    return FindSubControlOptions
#    


class TFindSubControlOptions(Structure):
    _fields_ = [("DummyField", LONG),
               ("MatchCriteria", TClkFindSubControlMatchCriteria),
               ("AllowToFail", BOOLEAN),
               ("MatchText", LPCWSTR),
               ("MatchBitmapText", PMatchBitmapTextRec), #TClkFindControlMatchBitmapTextArr;  #Can be updated, by a different call
               ("MatchBitmapFiles", LPCWSTR),
               ("MatchBitmapAlgorithm", BYTE), #LONG), #TMatchBitmapAlgorithm)
               ("MatchBitmapAlgorithmSettings", TMatchBitmapAlgorithmSettings),
               ("InitialRectangle", TRectString),
               ("UseWholeScreen", BOOLEAN),
               ("ColorError", LPCWSTR),
               ("AllowedColorErrorCount", LPCWSTR),
               ("WaitForControlToGoAway", BOOLEAN),
               ("StartSearchingWithCachedControl", BOOLEAN),
               ("CachedControlLeft", LPCWSTR),
               ("CachedControlTop", LPCWSTR),
               ("MatchPrimitiveFiles", LPCWSTR),
               ("GetAllControls", BOOLEAN),
               ("UseFastSearch", BOOLEAN),
               ("FastSearchAllowedColorErrorCount", LPCWSTR),
               ("IgnoredColors", LPCWSTR),
               ("SleepySearch", BOOLEAN),
               ("StopSearchOnMismatch", BOOLEAN),
               ("ImageSource", BYTE), #LONG), #TImageSource)
               ("SourceFileName", LPCWSTR),
               ("ImageSourceFileNameLocation", BYTE), #LONG),  #TImageSourceFileNameLocation)
               ("PrecisionTimeout", BOOLEAN),
               ("FullBackgroundImageInResult", BOOLEAN),
               ("MatchByHistogramSettings", TMatchByHistogramSettings),
               ("EvaluateTextCount", LPCWSTR),
               ("CropFromScreenshot", BOOLEAN),
               ("ThreadCount", LPCWSTR),
               ("UseTextRenderingInBrowser", BOOLEAN),
               ("RenderingInBrowserSettings", TRenderingInBrowserSettings)
               ]

PFindSubControlOptions = ctypes.POINTER(TFindSubControlOptions)

def GetDefaultFindSubControlOptions():
    FindSubControlOptions = TFindSubControlOptions()
    FindSubControlOptions.MatchCriteria = TClkFindSubControlMatchCriteria()
    FindSubControlOptions.MatchCriteria.WillMatchBitmapText = True
    FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles = False
    FindSubControlOptions.MatchCriteria.WillMatchPrimitiveFiles = False

    FindSubControlOptions.AllowToFail = False
    FindSubControlOptions.MatchText = 'SomeText'
    FindSubControlOptions.MatchBitmapText = ()  #(The content is updated separately. See TClkFindControlMatchBitmapText)
    FindSubControlOptions.MatchBitmapFiles = '' #'FileExample1.bmp\r\nFileExample2.bmp\r\nFileExample3.bmp'
    FindSubControlOptions.MatchBitmapAlgorithm = TMatchBitmapAlgorithm.mbaBruteForce

    FindSubControlOptions.MatchBitmapAlgorithmSettings = TMatchBitmapAlgorithmSettings()
    FindSubControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf = 1
    FindSubControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf = 1
    FindSubControlOptions.MatchBitmapAlgorithmSettings.XOffset = 0
    FindSubControlOptions.MatchBitmapAlgorithmSettings.YOffset = 0

    FindSubControlOptions.InitialRectangle = TRectString()
    FindSubControlOptions.InitialRectangle.Left = '$Control_Left$'
    FindSubControlOptions.InitialRectangle.Top = '$Control_Top$'
    FindSubControlOptions.InitialRectangle.Right = '$Control_Right$'
    FindSubControlOptions.InitialRectangle.Bottom = '$Control_Bottom$'
    FindSubControlOptions.InitialRectangle.LeftOffset = '0'
    FindSubControlOptions.InitialRectangle.TopOffset = '0'
    FindSubControlOptions.InitialRectangle.RightOffset = '0'
    FindSubControlOptions.InitialRectangle.BottomOffset = '0'

    FindSubControlOptions.UseWholeScreen = False  #usually True for finding a window, and False, for finding a (sub)control on a window or another control.  
    FindSubControlOptions.ColorError = '0'
    FindSubControlOptions.AllowedColorErrorCount = '0'
    FindSubControlOptions.WaitForControlToGoAway = False
    FindSubControlOptions.StartSearchingWithCachedControl = False
    FindSubControlOptions.CachedControlLeft = ''
    FindSubControlOptions.CachedControlTop = ''

    FindSubControlOptions.MatchPrimitiveFiles = '' #'FileExample1.pmtv\r\nFileExample2.pmtv\r\nFileExample3.pmtv'
    FindSubControlOptions.GetAllControls = False

    FindSubControlOptions.UseFastSearch = True
    FindSubControlOptions.FastSearchAllowedColorErrorCount = '10'
    FindSubControlOptions.IgnoredColors = ''
    FindSubControlOptions.SleepySearch = False
    FindSubControlOptions.StopSearchOnMismatch = True

    FindSubControlOptions.ImageSource = TImageSource.isScreenshot
    FindSubControlOptions.SourceFileName = ''
    FindSubControlOptions.ImageSourceFileNameLocation = TImageSourceFileNameLocation.isflMem

    FindSubControlOptions.PrecisionTimeout = False
    FindSubControlOptions.FullBackgroundImageInResult = True

    FindSubControlOptions.MatchByHistogramSettings.MinPercentColorMatch = '50'
    FindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp = '10'
    FindSubControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp = '15'

    FindSubControlOptions.EvaluateTextCount = "-1"
    FindSubControlOptions.CropFromScreenshot = False
    FindSubControlOptions.ThreadCount = "2"
    FindSubControlOptions.UseTextRenderingInBrowser = False
    
    FindSubControlOptions.RenderingInBrowserSettings = TRenderingInBrowserSettings()
    FindSubControlOptions.RenderingInBrowserSettings.RenderingRequestType = TRenderingRequestType.rrtShellExecute
    FindSubControlOptions.RenderingInBrowserSettings.ReceivingBitmapsTimeout = 3000
    FindSubControlOptions.RenderingInBrowserSettings.ActionForSendingRequest = ''
    FindSubControlOptions.RenderingInBrowserSettings.UsePluginForReceivingBitmaps = False
    FindSubControlOptions.RenderingInBrowserSettings.PluginActionForReceivingBitmaps = ''
    FindSubControlOptions.RenderingInBrowserSettings.FontSizeUnit = TFontSizeUnit.fsuPt

    return FindSubControlOptions


class TSetControlTextOptions(Structure):
    _fields_ = [("Text", LPCWSTR),
               ("ControlType", BYTE), #LONG), #TClkSetTextControlType
               ("DelayBetweenKeyStrokes", LPCWSTR),
               ("Count", LPCWSTR)]

PSetControlTextOptions = ctypes.POINTER(TSetControlTextOptions)

def GetDefaultSetControlTextOptions():
    SetControlTextOptions = TSetControlTextOptions()
    SetControlTextOptions.Text = 'New text'
    SetControlTextOptions.ControlType = TClkSetTextControlType.stEditBox
    SetControlTextOptions.DelayBetweenKeyStrokes = '0'
    SetControlTextOptions.Count = '1'
    return SetControlTextOptions


class TClkCallTemplateLoop(Structure):
    _fields_ = [("Enabled", BOOLEAN),
               ("Counter", LPCWSTR),
               ("InitValue", LPCWSTR),
               ("EndValue", LPCWSTR),
               ("Direction", BYTE), #LONG), #TLoopDirection
               ("BreakCondition", LPCWSTR),
               ("EvalBreakPosition", BYTE)] #LONG)] #TLoopEvalBreakPosition
               

class TCallTemplateOptions(Structure):
    _fields_ = [("TemplateFileName", LPCWSTR),
               ("ListOfCustomVarsAndValues", LPCWSTR),  #  example:  $VarA$=ValueA\r\n$VarB$=ValueB\r\n$VarC$=ValueC
               ("EvaluateBeforeCalling", BOOLEAN),
               ("CallTemplateLoop", TClkCallTemplateLoop)]

PCallTemplateOptions = ctypes.POINTER(TCallTemplateOptions)

def GetDefaultCallTemplateOptions():
    CallTemplateOptions = TCallTemplateOptions()
    CallTemplateOptions.TemplateFileName = '' #'PathToTemplate.clktmpl'
    CallTemplateOptions.ListOfCustomVarsAndValues = ''  #these vars will be added to the existing list of vars
    CallTemplateOptions.EvaluateBeforeCalling = False

    CallTemplateOptions.CallTemplateLoop = TClkCallTemplateLoop()
    CallTemplateOptions.CallTemplateLoop.Enabled = False
    CallTemplateOptions.CallTemplateLoop.Counter = ''  #  e.g. "$i$"
    CallTemplateOptions.CallTemplateLoop.InitValue = '0'
    CallTemplateOptions.CallTemplateLoop.EndValue = '3'
    CallTemplateOptions.CallTemplateLoop.Direction = TLoopDirection.ldInc
    CallTemplateOptions.CallTemplateLoop.BreakCondition = ''
    CallTemplateOptions.CallTemplateLoop.EvalBreakPosition = TLoopEvalBreakPosition.lebpAfterContent
    return CallTemplateOptions


class TSleepOptions(Structure):
    _fields_ = [("Value", LPCWSTR)]

PSleepOptions = ctypes.POINTER(TSleepOptions)

def GetDefaultSleepOptions():
    SleepOptions = TSleepOptions()
    SleepOptions.Value = '300'
    return SleepOptions


class TSetVarOptions(Structure):
    _fields_ = [("ListOfVarNames", LPCWSTR),
               ("ListOfVarValues", LPCWSTR),
               ("ListOfVarEvalBefore", LPCWSTR),
               ("FailOnException", BOOLEAN)]

PSetVarOptions = ctypes.POINTER(TSetVarOptions)

def GetDefaultSetVarOptions():
    SetVarOptions = TSetVarOptions()  #all of the following lists have to have the same number of items, separated by CRLF  (a.k.a. \r\n)
    SetVarOptions.ListOfVarNames = ''    #example  '$MyVar$1\r\n$MyVar$2\r\n$MyVar$3\r\n$MyVar$4'
    SetVarOptions.ListOfVarValues = '' #example  'Val1\r\nVal2\r\nVal3\r\nVal4'
    SetVarOptions.ListOfVarEvalBefore = ''     #example  '1\r\n1\r\n0\r\n1'
    SetVarOptions.FailOnException = False
    return SetVarOptions


class TWindowOperationsOptions(Structure):
    _fields_ = [("Operation", BYTE), #LONG),  #TWindowOperation
               ("NewX", LPCWSTR),
               ("NewY", LPCWSTR),
               ("NewWidth", LPCWSTR),
               ("NewHeight", LPCWSTR),
               ("NewPositionEnabled", BOOLEAN),
               ("NewSizeEnabled", BOOLEAN)]

PWindowOperationsOptions = ctypes.POINTER(TWindowOperationsOptions)

def GetDefaultWindowOperationsOptions():
    WindowOperationsOptions = TWindowOperationsOptions()
    WindowOperationsOptions.Operation = TWindowOperation.woBringToFront
    WindowOperationsOptions.NewX = ''  #'$NewX$'
    WindowOperationsOptions.NewY = ''  #'$NewY$'
    WindowOperationsOptions.NewWidth = ''  #'$NewWidth$'
    WindowOperationsOptions.NewHeight = ''  #'$NewHeight$'
    WindowOperationsOptions.NewPositionEnabled = False
    WindowOperationsOptions.NewSizeEnabled = False
    return WindowOperationsOptions


class TLoadSetVarFromFileOptions(Structure):
    _fields_ = [("FileName", LPCWSTR),
               ("SetVarActionName", LPCWSTR)]

PLoadSetVarFromFileOptions = ctypes.POINTER(TLoadSetVarFromFileOptions)

def GetDefaultLoadSetVarFromFileOptions():
    LoadSetVarFromFileOptions = TLoadSetVarFromFileOptions()
    LoadSetVarFromFileOptions.FileName = ''
    LoadSetVarFromFileOptions.SetVarActionName = ''
    return LoadSetVarFromFileOptions


class TSaveSetVarToFileOptions(Structure):
    _fields_ = [("FileName", LPCWSTR),
               ("SetVarActionName", LPCWSTR)]

PSaveSetVarToFileOptions = ctypes.POINTER(TSaveSetVarToFileOptions)

def GetDefaultSaveSetVarToFileOptions():
    SaveSetVarToFileOptions = TSaveSetVarToFileOptions()
    SaveSetVarToFileOptions.FileName = ''
    SaveSetVarToFileOptions.SetVarActionName = ''
    return SaveSetVarToFileOptions


class TPluginOptions(Structure):
    _fields_ = [("FileName", LPCWSTR),
               ("ListOfPropertiesAndValues", LPCWSTR)]

PPluginOptions = ctypes.POINTER(TPluginOptions)

def GetDefaultPluginOptions():
    PluginOptions = TPluginOptions()
    PluginOptions.FileName = ''
    PluginOptions.ListOfPropertiesAndValues = ''
    return PluginOptions


class TEditTemplateOptions(Structure):
    _fields_ = [("Operation", BYTE), #LONG),  #TEditTemplateOperation
               ("WhichTemplate", BYTE), #LONG),  #TEditTemplateWhichTemplate
               ("TemplateFileName", LPCWSTR),
               ("ListOfEditedProperties", LPCWSTR),   # ASCII-18 separated list of values
               ("ListOfEnabledProperties", LPCWSTR),
               ("EditedActionName", LPCWSTR),
               ("EditedActionType", BYTE), #LONG),  #TClkAction
               ("EditedActionCondition", LPCWSTR),
               ("EditedActionTimeout", LONG),
               ("NewActionName", LPCWSTR),
               ("ShouldSaveTemplate", BOOLEAN)]

PEditTemplateOptions = ctypes.POINTER(TEditTemplateOptions)

def GetDefaultEditTemplateOptions():
    EditTemplateOptions = TEditTemplateOptions()
    EditTemplateOptions.Operation = TEditTemplateOperation.etoNewAction
    EditTemplateOptions.WhichTemplate = TEditTemplateWhichTemplate.etwtOther
    EditTemplateOptions.TemplateFileName = ''
    EditTemplateOptions.ListOfEditedProperties = ''  # 'XClickPointReference=2YClickPointReference=3XClickPointVar=$Control_Left$YClickPointVar=$Control_Top$'  #All the properties for that action.
    EditTemplateOptions.ListOfEnabledProperties = 'XClickPointReferenceYClickPointReference'
    EditTemplateOptions.EditedActionName = ''
    EditTemplateOptions.EditedActionType = TClkAction.acClick
    EditTemplateOptions.EditedActionCondition = ''
    EditTemplateOptions.EditedActionTimeout = 1000
    EditTemplateOptions.NewActionName = ''
    EditTemplateOptions.ShouldSaveTemplate = False
    return EditTemplateOptions


########################
