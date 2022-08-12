#   Copyright (C) 2022 VCC
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

from ctypes.wintypes import LPCSTR, LPCWSTR, BYTE, BOOLEAN, LONG, LARGE_INTEGER
from ctypes import Structure, POINTER
from enum import Enum


CClickType_Click = 0;
CClickType_Drag = 1; 

#Various datatypes, translated from ClickerUtils.pas

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
    
#TFontQuality from Graphics.pp
class TFontQuality: #(Enum):
    fqDefault = 0 
    fqDraft = 1
    fqProof = 2
    fqNonAntialiased = 3
    fqAntialiased = 4
    fqCleartype = 5
    fqCleartypeNatural = 6

class TClkSetTextControlType: #(Enum):
    stEditBox = 0
    stComboBox = 1
    stKeystrokes = 2


class TWindowOperation: #(Enum):
    woBringToFront = 0
    woMoveResize = 1
    woClose = 2


class TClickOptions(Structure):
    _fields_ = [("XClickPointReference", BYTE), #LONG), #TXClickPointReference),  #this is defined as 32-bit in dll, but somehow, it wants 8-bit
               ("YClickPointReference", BYTE), #LONG), #TYClickPointReference),  #this is defined as 32-bit in dll, but somehow, it wants 8-bit
               ("XClickPointVar", LPCWSTR),
               ("YClickPointVar", LPCWSTR),
               ("XOffset", LPCWSTR),
               ("YOffset", LPCWSTR),
               ("MouseButton", LONG), #TMouseButton),  #not clear why TMouseButton has to be 32-bit, while TXClickPointReference is 8-bit
               ("ClickWithCtrl", BOOLEAN),
               ("ClickWithAlt", BOOLEAN),
               ("ClickWithShift", BOOLEAN),
               ("ClickWithDoubleClick", BOOLEAN),  #not used anymore (filed kept for backwards compatibility with old templates)
               ("Count", LONG),
               ("LeaveMouse", BOOLEAN),
               ("MoveWithoutClick", BOOLEAN),
               ("ClickType", LONG),    #see CClickType_Click and CClickType_DoubleClick
               ("XClickPointReferenceDest", BYTE), #TXClickPointReference),  #this is defined as 32-bit in dll, but somehow, it wants 8-bit
               ("YClickPointReferenceDest", BYTE), #TYClickPointReference),  #this is defined as 32-bit in dll, but somehow, it wants 8-bit
               ("XClickPointVarDest", LPCWSTR),
               ("YClickPointVarDest", LPCWSTR),
               ("XOffsetDest", LPCWSTR), 
               ("YOffsetDest", LPCWSTR)]

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
    return ClickOptions
    
    
class TExecAppOptions(Structure):
    _fields_ = [("PathToApp", LPCWSTR),
               ("ListOfParams", LPCWSTR),
               ("WaitForApp", BOOLEAN),
               ("AppStdIn", LPCWSTR),
               ("CurrentDir", LPCWSTR),
               ("UseInheritHandles", BYTE)] #TExecAppUseInheritHandles

PExecAppOptions = ctypes.POINTER(TExecAppOptions)

def GetDefaultExecAppOptions():
    ExecAppOptions = TExecAppOptions()
    ExecAppOptions.PathToApp = ''
    ExecAppOptions.ListOfParams = ''
    ExecAppOptions.WaitForApp = False
    ExecAppOptions.AppStdIn = ''
    ExecAppOptions.CurrentDir = ''
    ExecAppOptions.UseInheritHandles = TExecAppUseInheritHandles.uihOnlyWithStdInOut
    return ExecAppOptions


class TClkFindControlMatchCriteria(Structure):
    _fields_ = [("WillMatchText", BOOLEAN),
               ("WillMatchClassName", BOOLEAN),
               ("WillMatchBitmapText", BOOLEAN),
               ("WillMatchBitmapFiles", BOOLEAN),
               ("SearchForControlMode", BYTE)] #TSearchForControlMode
               
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

class TFindControlOptions(Structure):
    _fields_ = [("MatchCriteria", TClkFindControlMatchCriteria),
               ("AllowToFail", BOOLEAN),
               ("MatchText", LPCWSTR),
               ("MatchClassName", LPCWSTR),
               ("MatchTextSeparator", LPCWSTR),
               ("MatchClassNameSeparator", LPCWSTR),
               ("MatchBitmapText", LARGE_INTEGER), #TClkFindControlMatchBitmapTextArr;  #dummy field, can be updated, by a different call
               ("MatchBitmapFiles", LPCWSTR),
               ("MatchBitmapAlgorithm", BYTE), #TMatchBitmapAlgorithm)
               ("MatchBitmapAlgorithmSettings", TMatchBitmapAlgorithmSettings),
               ("InitialRectange", TRectString),
               ("UseWholeScreen", BOOLEAN),
               ("ColorError", LPCWSTR),
               ("AllowedColorErrorCount", LPCWSTR),
               ("WaitForControlToGoAway", BOOLEAN),
               ("StartSearchingWithCachedControl", BOOLEAN),
               ("CachedControlLeft", LPCWSTR),
               ("CachedControlTop", LPCWSTR)]
               
PFindControlOptions = ctypes.POINTER(TFindControlOptions)

def GetDefaultFindControlOptions():
    FindControlOptions = TFindControlOptions()
    FindControlOptions.MatchCriteria = TClkFindControlMatchCriteria()
    FindControlOptions.MatchCriteria.WillMatchText = True
    FindControlOptions.MatchCriteria.WillMatchClassName = True
    FindControlOptions.MatchCriteria.WillMatchBitmapText = False
    FindControlOptions.MatchCriteria.WillMatchBitmapFiles = False
    FindControlOptions.MatchCriteria.SearchForControlMode = TSearchForControlMode.sfcmGenGrid
    
    FindControlOptions.AllowToFail = False
    FindControlOptions.MatchText = 'SomeText'
    FindControlOptions.MatchClassName = 'TButton'
    FindControlOptions.MatchTextSeparator = ''
    FindControlOptions.MatchClassNameSeparator = ''
    FindControlOptions.MatchBitmapText = 0 #dummy field  (The content is updated separately. See TClkFindControlMatchBitmapText)
    FindControlOptions.MatchBitmapFiles = '' #'FileExample1.bmp\r\nFileExample2.bmp\r\nFileExample3.bmp'
    FindControlOptions.MatchBitmapAlgorithm = TMatchBitmapAlgorithm.mbaBruteForce
    
    FindControlOptions.MatchBitmapAlgorithmSettings = TMatchBitmapAlgorithmSettings()
    FindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf = 1
    FindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf = 1
    FindControlOptions.MatchBitmapAlgorithmSettings.XOffset = 0
    FindControlOptions.MatchBitmapAlgorithmSettings.YOffset = 0
    
    FindControlOptions.InitialRectange = TRectString()
    FindControlOptions.InitialRectange.Left = '$Control_Left$'
    FindControlOptions.InitialRectange.Top = '$Control_Top$'
    FindControlOptions.InitialRectange.Right = '$Control_Right$'
    FindControlOptions.InitialRectange.Bottom = '$Control_Bottom$'
    FindControlOptions.InitialRectange.LeftOffset = '0'
    FindControlOptions.InitialRectange.TopOffset = '0'
    FindControlOptions.InitialRectange.RightOffset = '0'
    FindControlOptions.InitialRectange.BottomOffset = '0'
    
    FindControlOptions.UseWholeScreen = True  #usually True for finding a window, and False, for finding a (sub)control on a window or another control.  
    FindControlOptions.ColorError = '0'
    FindControlOptions.AllowedColorErrorCount = '0'
    FindControlOptions.WaitForControlToGoAway = False
    FindControlOptions.StartSearchingWithCachedControl = False
    FindControlOptions.CachedControlLeft = ''
    FindControlOptions.CachedControlTop = ''
    return FindControlOptions


def GetDefaultFindSubControlOptions():
    FindSubControlOptions = GetDefaultFindControlOptions()
    FindSubControlOptions.MatchCriteria.WillMatchText = False
    FindSubControlOptions.MatchCriteria.WillMatchClassName = False
    FindSubControlOptions.MatchCriteria.WillMatchBitmapText = True
    FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles = False
    FindSubControlOptions.UseWholeScreen = False
    return FindSubControlOptions
    

class TSetControlTextOptions(Structure):
    _fields_ = [("Text", LPCWSTR),
               ("ControlType", BYTE)] #TClkSetTextControlType
               
PSetControlTextOptions = ctypes.POINTER(TSetControlTextOptions)

def GetDefaultSetControlTextOptions():
    SetControlTextOptions = TSetControlTextOptions()
    SetControlTextOptions.Text = 'New text'
    SetControlTextOptions.ControlType = TClkSetTextControlType.stEditBox
    return SetControlTextOptions


class TCallTemplateOptions(Structure):
    _fields_ = [("TemplateFileName", LPCWSTR),
               ("ListOfCustomVarsAndValues", LPCWSTR),  #  example:  $VarA$=ValueA\r\n$VarB$=ValueB\r\n$VarC$=ValueC
               ("EvaluateBeforeCalling", BOOLEAN)]

PCallTemplateOptions = ctypes.POINTER(TCallTemplateOptions)

def GetDefaultCallTemplateOptions():
    SetControlTextOptions = TCallTemplateOptions()
    SetControlTextOptions.TemplateFileName = 'PathToTemplate.clktmpl'
    SetControlTextOptions.ListOfCustomVarsAndValues = ''  #these vars will be added to the existing list of vars
    SetControlTextOptions.EvaluateBeforeCalling = False
    return SetControlTextOptions


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
               ("ListOfVarEvalBefore", LPCWSTR)]

PSetVarOptions = ctypes.POINTER(TSetVarOptions)

def GetDefaultSetVarOptions():
    SetVarOptions = TSetVarOptions()  #all of the following lists have to have the same number of items, separated by CRLF  (a.k.a. \r\n)
    SetVarOptions.ListOfVarNames = ''    #example  '$MyVar$1\r\n$MyVar$2\r\n$MyVar$3\r\n$MyVar$4'
    SetVarOptions.ListOfVarValues = '' #example  'Val1\r\nVal2\r\nVal3\r\nVal4'
    SetVarOptions.ListOfVarEvalBefore = ''     #example  '1\r\n1\r\n0\r\n1'
    return SetVarOptions


class TWindowOperationsOptions(Structure):
    _fields_ = [("Operation", BYTE),  #TWindowOperation
               ("NewX", LPCWSTR),
               ("NewY", LPCWSTR),
               ("NewWidth", LPCWSTR),
               ("NewHeight", LPCWSTR),
               ("NewPositionEabled", BOOLEAN),
               ("NewSizeEabled", BOOLEAN)]

PWindowOperationsOptions = ctypes.POINTER(TWindowOperationsOptions)

def GetDefaultWindowOperationsOptions():
    WindowOperationsOptions = TWindowOperationsOptions()
    WindowOperationsOptions.Operation = TWindowOperation.woBringToFront
    WindowOperationsOptions.NewX = ''  #'$NewX$'
    WindowOperationsOptions.NewY = ''  #'$NewY$'
    WindowOperationsOptions.NewWidth = ''  #'$NewWidth$'
    WindowOperationsOptions.NewHeight = ''  #'$NewHeight$'
    WindowOperationsOptions.NewPositionEabled = False
    WindowOperationsOptions.NewSizeEabled = False
    return WindowOperationsOptions


########################
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
               ("FontQuality", BYTE), #TFontQuality
               ("FontQualityUsesReplacement", BOOLEAN),
               ("FontQualityReplacement", LPCWSTR),
               ("ProfileName", LPCWSTR)]
               
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
    MatchBitmapTextProfile.ProfileName = 'SomeFontProfileName'
    return MatchBitmapTextProfile