#   Copyright (C) 2024 VCC
#   creation date: 19 Dec 2024
#   initial release date: 19 Dec 2024
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
import ctypes.wintypes  #without importing wintypes, python crashes when calling functions like TestConnectionToServerFunc
import os
ClickerClientAPIDir = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
ClickerClientAPIDir = os.path.join(ClickerClientAPIDir, 'py')
#print('--- ClickerClientAPIDir = ', ClickerClientAPIDir)
sys.path.append(ClickerClientAPIDir)

from ctypes.wintypes import LPCSTR, LPCWSTR, BYTE, BOOLEAN, LONG
from UIClickerTypes import *
from UIClickerTypes import TClickOptions
from UIClickerClient import *


import time

DllFuncs = TUIClickerDllFunctions() #use TUIClickerDllFunctions for Boolean results (True for success)
#DllFuncs = TDllFunctions() #use TDllFunctions for debugging (see functions implementation for details (some functions return 1 for success, others return 0 for success)

print("InitClickerClient: ", DllFuncs.InitClickerClient())
try:
    print("CreateLoggingWindow: ", DllFuncs.CreateLoggingWindow())

    print("SetServerAddress: ", DllFuncs.SetServerAddress('http://127.0.0.1:35444/'))
    print("TestConnectionToServer: ", DllFuncs.TestConnectionToServer())


    FindControlOptions = GetDefaultFindControlOptions()
    FindControlOptions.MatchCriteria.WillMatchText = False
    FindControlOptions.MatchCriteria.WillMatchClassName = False
    FindControlOptions.MatchCriteria.WillMatchBitmapText = True
    FindControlOptions.MatchCriteria.WillMatchBitmapFiles = True
    FindControlOptions.MatchCriteria.WillMatchPrimitiveFiles = True
    FindControlOptions.MatchCriteria.SearchForControlMode = TSearchForControlMode.sfcmEnumWindows

    FindControlOptions.AllowToFail = True
    FindControlOptions.MatchText = 'some text'
    FindControlOptions.MatchClassName = 'some class'
    FindControlOptions.MatchTextSeparator = 'txt sep'
    FindControlOptions.MatchClassNameSeparator = 'class sep'
    FindControlOptions.MatchBitmapText = ()  #(The content is updated separately. See TClkFindControlMatchBitmapText)
    FindControlOptions.MatchBitmapFiles = 'bmp'
    FindControlOptions.MatchBitmapAlgorithm = TMatchBitmapAlgorithm.mbaRawHistogramZones

    FindControlOptions.MatchBitmapAlgorithmSettings.XMultipleOf = 3
    FindControlOptions.MatchBitmapAlgorithmSettings.YMultipleOf = 4
    FindControlOptions.MatchBitmapAlgorithmSettings.XOffset = 5
    FindControlOptions.MatchBitmapAlgorithmSettings.YOffset = 6

    FindControlOptions.InitialRectangle.Left = '$Control_L$'
    FindControlOptions.InitialRectangle.Top = '$Control_T$'
    FindControlOptions.InitialRectangle.Right = '$Control_R$'
    FindControlOptions.InitialRectangle.Bottom = '$Control_B$'
    FindControlOptions.InitialRectangle.LeftOffset = '30'
    FindControlOptions.InitialRectangle.TopOffset = '40'
    FindControlOptions.InitialRectangle.RightOffset = '50'
    FindControlOptions.InitialRectangle.BottomOffset = '60'

    FindControlOptions.UseWholeScreen = False  #usually True for finding a window, and False, for finding a (sub)control on a window or another control.  
    FindControlOptions.ColorError = '70'
    FindControlOptions.AllowedColorErrorCount = '80'
    FindControlOptions.WaitForControlToGoAway = True
    FindControlOptions.StartSearchingWithCachedControl = True
    FindControlOptions.CachedControlLeft = '90'
    FindControlOptions.CachedControlTop = '100'

    FindControlOptions.MatchPrimitiveFiles = 'pmtv' #'FileExample1.pmtv\r\nFileExample2.pmtv\r\nFileExample3.pmtv'
    FindControlOptions.GetAllControls = True

    FindControlOptions.UseFastSearch = False
    FindControlOptions.FastSearchAllowedColorErrorCount = '700'
    FindControlOptions.IgnoredColors = '800'
    FindControlOptions.SleepySearch = True
    FindControlOptions.StopSearchOnMismatch = True

    FindControlOptions.ImageSource = TImageSource.isFile
    FindControlOptions.SourceFileName = 'Fnm'
    FindControlOptions.ImageSourceFileNameLocation = TImageSourceFileNameLocation.isflDisk

    FindControlOptions.PrecisionTimeout = True
    FindControlOptions.FullBackgroundImageInResult = False

    FindControlOptions.MatchByHistogramSettings.MinPercentColorMatch = '250'
    FindControlOptions.MatchByHistogramSettings.MostSignificantColorCountInSubBmp = '310'
    FindControlOptions.MatchByHistogramSettings.MostSignificantColorCountInBackgroundBmp = '415'

    FindControlOptions.EvaluateTextCount = "-17"
    FindControlOptions.CropFromScreenshot = True
    FindControlOptions.ThreadCount = "30"


    print("ExecuteFindControlAction: ", DllFuncs.ExecuteFindControlAction("Another FindControl", 100, ExecuteFindControlActionOptions, True, 'Mem'))
finally:
    print("DestroyLoggingWindow: ", DllFuncs.DestroyLoggingWindow())
    print("DoneClickerClient", DllFuncs.DoneClickerClient())
    print("end of script")