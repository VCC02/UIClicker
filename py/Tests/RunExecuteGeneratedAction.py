#   Copyright (C) 2025 VCC
#   creation date: 25 Jul 2025  (copied from .\RunExecuteFindSubControlAction.py)
#   initial release date: 25 Jul 2025
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


#This is an example of generated Python code, from UIClicker (by right-clicking an action).
#The following _FindSubControl_ function is the generated code.

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


def _FindSubControl_(ADllFuncs): # "FindSubControl"
    FindSubControl = GetDefaultFindSubControlOptions()
    FindSubControl.MatchBitmapText = ()  # The content is updated separately. See TClkFindControlMatchBitmapText
    FindSubControl.MatchText = '32'
    DestMatchBitmapTextArray = TMatchBitmapTextRec(2)
    DestMatchBitmapTextArray.Items[0].ForegroundColor = '$Color_WindowText$'
    DestMatchBitmapTextArray.Items[0].BackgroundColor = '$Color_BtnFace$'
    DestMatchBitmapTextArray.Items[0].FontName = 'Segoe UI'
    DestMatchBitmapTextArray.Items[0].FontSize = 9
    DestMatchBitmapTextArray.Items[0].FontQuality = 4 # Enum.TFontQuality
    DestMatchBitmapTextArray.Items[0].ProfileName = 'Profile [1]'
    DestMatchBitmapTextArray.Items[1].ForegroundColor = '$Color_WindowText$'
    DestMatchBitmapTextArray.Items[1].BackgroundColor = '$Color_BtnFace$'
    DestMatchBitmapTextArray.Items[1].FontName = 'Segoe UI'
    DestMatchBitmapTextArray.Items[1].FontSize = 9
    DestMatchBitmapTextArray.Items[1].FontQuality = 5 # Enum.TFontQuality
    DestMatchBitmapTextArray.Items[1].ProfileName = 'Profile [2]'
    FindSubControl.InitialRectangle.LeftOffset = '12'
    FindSubControl.InitialRectangle.TopOffset = '241'
    FindSubControl.InitialRectangle.RightOffset = '-285'
    FindSubControl.InitialRectangle.BottomOffset = '-14'
    FindSubControl.ColorError = '10'
    FindSubControl.AllowedColorErrorCount = '20'

    FindSubControl.MatchBitmapText = PMatchBitmapTextRec(DestMatchBitmapTextArray)
    return ADllFuncs.ExecuteFindSubControlAction('"FindSubControl"', 1000, FindSubControl, False, "Disk")



print("InitClickerClient: ", DllFuncs.InitClickerClient())
try:
    print("CreateLoggingWindow: ", DllFuncs.CreateLoggingWindow())

    print("SetServerAddress: ", DllFuncs.SetServerAddress('http://127.0.0.1:35444/'))
    print("TestConnectionToServer: ", DllFuncs.TestConnectionToServer())



    #print("ExecuteFindSubControlAction: ", _FindSubControl_(DllFuncs))
    
    Response = _FindSubControl_(DllFuncs)
    print("Response from ExecuteFindSubControlAction: ", Response)

finally:
    print("DestroyLoggingWindow: ", DllFuncs.DestroyLoggingWindow())
    print("DoneClickerClient", DllFuncs.DoneClickerClient())
    print("end of script")