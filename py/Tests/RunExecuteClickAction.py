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


    ClickOptions = GetDefaultClickOptions()
    ClickOptions.XClickPointReference = TXClickPointReference.xrefVar
    ClickOptions.YClickPointReference = TYClickPointReference.yrefAbsolute
    ClickOptions.XClickPointVar = 'FirstVar'
    ClickOptions.YClickPointVar = 'SecondVar'
    ClickOptions.XOffset = 'FirstOffset'
    ClickOptions.YOffset = 'SecondOffset'
    ClickOptions.MouseButton = TMouseButton.mbExtra1
    ClickOptions.ClickWithCtrl = True
    ClickOptions.ClickWithAlt = True
    ClickOptions.ClickWithShift = True
    ClickOptions.ClickWithDoubleClick = True
    ClickOptions.Count = 10
    ClickOptions.LeaveMouse = True
    ClickOptions.MoveWithoutClick = True
    ClickOptions.ClickType = CClickType_Wheel
    ClickOptions.XClickPointReferenceDest = TXClickPointReference.xrefWidth
    ClickOptions.YClickPointReferenceDest = TYClickPointReference.yrefHeight
    ClickOptions.XClickPointVarDest = 'ThirdVar'
    ClickOptions.YClickPointVarDest = 'FourthVar'
    ClickOptions.XOffsetDest = 'ThirdOffset'
    ClickOptions.YOffsetDest = 'FourthOffset'
    ClickOptions.MouseWheelType = TMouseWheelType.mwtHoriz
    ClickOptions.MouseWheelAmount = '30'
    ClickOptions.DelayAfterMovingToDestination = '40'
    ClickOptions.DelayAfterMouseDown = '50'
    ClickOptions.MoveDuration = '60'
    ClickOptions.UseClipCursor = True

    print("ExecuteClickAction: ", DllFuncs.ExecuteClickAction("Another Click", 100, ClickOptions, True))
finally:
    print("DestroyLoggingWindow: ", DllFuncs.DestroyLoggingWindow())
    print("DoneClickerClient", DllFuncs.DoneClickerClient())
    print("end of script")