#   Copyright (C) 2022-2025 VCC
#   creation date: Jul 2022
#   initial release date: 26 Jul 2022
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
from ctypes.wintypes import LPCSTR, LPCWSTR, BYTE, BOOLEAN, LONG, LPCVOID, LARGE_INTEGER
from UIClickerTypes import *
from UIClickerTypes import TClickOptions
from UIClickerClient import *


import time
import os

DllFuncs = TUIClickerDllFunctions() #use TUIClickerDllFunctions for Boolean results (True for success)
#DllFuncs = TDllFunctions() #use TDllFunctions for debugging (see functions implementation for details (some functions return 1 for success, others return 0 for success)

print("InitClickerClient: ", DllFuncs.InitClickerClient())
try:
    print("CreateLoggingWindow: ", DllFuncs.CreateLoggingWindow())
    print("TestConnectionToServer: ", DllFuncs.TestConnectionToServer())
    
    print("GetServerAddress: ", DllFuncs.GetServerAddress())
    
    print("SetServerAddress: ", DllFuncs.SetServerAddress('http://192.168.3.102:5444/'))
    print("GetServerAddress: ", DllFuncs.GetServerAddress())
    print("TestConnectionToServer after setting wrong address: ", DllFuncs.TestConnectionToServer())
    
    print("SetServerAddress: ", DllFuncs.SetServerAddress('http://127.0.0.1:5444/'))
    print("GetServerAddress: ", DllFuncs.GetServerAddress())
    print("TestConnectionToServer after setting a working address: ", DllFuncs.TestConnectionToServer())
    
    print("SetTemplatesDir: ", DllFuncs.SetTemplatesDir(os.getcwd())) #required for sending files
    
    #print("ClearClientInMemFS: ", DllFuncs.ClearClientInMemFS())  #For debugging only. The client should work without this call.
    #print("ClearServerInMemFS: ", DllFuncs.ClearServerInMemFS())  #For debugging only. The server should work without this call.
    
    ###########
    print("AddListOfAccessibleDirsToFileProviderClient: ", DllFuncs.AddListOfAccessibleDirsToFileProviderClient("bmps\\"))
    print("AddListOfAccessibleFileExtensionsToFileProviderClient: ", DllFuncs.AddListOfAccessibleFileExtensionsToFileProviderClient(".bmp\r\n.clktmpl\r\n.pmtv"))
    print("SetFileProviderClientConnectTimeout: ", DllFuncs.SetFileProviderClientConnectTimeout(3000))
    
    print("StartFileProviderClientThread: ", DllFuncs.StartFileProviderClientThread())
    ###########
    
    print("CreateNewTemplate: ", DllFuncs.CreateNewTemplate('VerifyClicking.clktmpl')) #creates a new template in dll's in-mem file system
    print("CreateNewTemplate: ", DllFuncs.CreateNewTemplate('VerifyClicking.clktmpl')) #the second call returns 1, because the file already exists
    
    ClickOptions = GetDefaultClickOptions()
    ClickOptions.XClickPointReference = TXClickPointReference.xrefVar
    ClickOptions.YClickPointReference = TYClickPointReference.yrefVar
    ClickOptions.XClickPointVar = '$SrcLeft$'
    ClickOptions.YClickPointVar = '$SrcTop$'
    ClickOptions.XOffset = '$XOffset$'
    ClickOptions.YOffset = '$YOffset$'
    print("AddClickActionToTemplate: ", DllFuncs.AddClickActionToTemplate('VerifyClicking.clktmpl', 'First', 0, True, '$a$==$b$', ctypes.byref(ClickOptions)))
    
    ExecAppOptions = GetDefaultExecAppOptions()
    ExecAppOptions.PathToApp = 'C:\\Windows\\Notepad.exe'
    ExecAppOptions.ListOfParams = 'SomeUnknownFile.txt'
    ExecAppOptions.WaitForApp = True
    ExecAppOptions.AppStdIn = 'NothingHere'
    ExecAppOptions.CurrentDir = 'C:\\Windows'
    ExecAppOptions.UseInheritHandles = TExecAppUseInheritHandles.uihYes
    print("AddExecAppActionToTemplate: ", DllFuncs.AddExecAppActionToTemplate('VerifyClicking.clktmpl', 'Second', 0, True, '$a$==$b$', ctypes.byref(ExecAppOptions)))
    
    FindControlOptions = GetDefaultFindControlOptions()
    FindControlOptions.MatchText = 'UI Clicker Main'
    FindControlOptions.MatchClassName = 'Window'
    print("AddFindControlActionToTemplate: ", DllFuncs.AddFindControlActionToTemplate('VerifyClicking.clktmpl', 'Third', 1000, True, '$a$<>$b$', ctypes.byref(FindControlOptions)))
    
    MatchBitmapText = GetDefaultMatchBitmapText()
    print("AddFontProfileToFindSubControlAction: ", DllFuncs.AddFontProfileToFindSubControlAction('VerifyClicking.clktmpl', 2, ctypes.byref(MatchBitmapText)))
    #-2 means the action index is out of range (negative or greater than the number of actions)
    #it may happen, if the above AddFindControlActionToTemplate calls fail, so there are no actions
    
    MatchBitmapText.ForegroundColor = 'FF8800'
    MatchBitmapText.BackgroundColor = '223344'
    MatchBitmapText.FontName = 'Segoe UI'
    MatchBitmapText.FontSize = 9
    MatchBitmapText.Bold = True
    MatchBitmapText.Italic = True
    MatchBitmapText.Underline = False
    MatchBitmapText.StrikeOut = True
    MatchBitmapText.FontQuality = TFontQuality.fqCleartype
    MatchBitmapText.FontQualityUsesReplacement = True
    MatchBitmapText.FontQualityReplacement = '$TheQuality'
    MatchBitmapText.ProfileName = 'Second profile'
    print("AddFontProfileToFindSubControlAction: ", DllFuncs.AddFontProfileToFindSubControlAction('VerifyClicking.clktmpl', 2, ctypes.byref(MatchBitmapText)))
    
    FindSubControlOptions = GetDefaultFindSubControlOptions()
    FindSubControlOptions.MatchBitmapFiles = "bmps\\ShowWindowInterpreter32.bmp\r\nbmps\\ShowWindowInterpreter64.bmp\r\nbmps\\ShowWindowInterpreter_Wine.bmp"   
    FindSubControlOptions.MatchCriteria.WillMatchBitmapText = False
    FindSubControlOptions.MatchCriteria.WillMatchBitmapFiles = True
    FindSubControlOptions.InitialRectangle.LeftOffset = '56'
    FindSubControlOptions.InitialRectangle.TopOffset = '92'
    FindSubControlOptions.InitialRectangle.RightOffset = '-70'
    FindSubControlOptions.InitialRectangle.BottomOffset = '-123'
    ###############
    print("AddFindSubControlActionToTemplate: ", DllFuncs.AddFindSubControlActionToTemplate('VerifyClicking.clktmpl', 'Fourth', 1000, True, '', ctypes.byref(FindSubControlOptions)))
    print("AddFontProfileToFindSubControlAction: ", DllFuncs.AddFontProfileToFindSubControlAction('VerifyClicking.clktmpl', 3, ctypes.byref(MatchBitmapText)))
    
    SetControlTextOptions = GetDefaultSetControlTextOptions()
    print("AddSetControlTextActionToTemplate: ", DllFuncs.AddSetControlTextActionToTemplate('VerifyClicking.clktmpl', 'Fifth', 0, True, '', ctypes.byref(SetControlTextOptions)))
    
    CallTemplateOptions = GetDefaultCallTemplateOptions()
    CallTemplateOptions.TemplateFileName = "bmps\\SleepABit.clktmpl"
    print("AddCallTemplateActionToTemplate: ", DllFuncs.AddCallTemplateActionToTemplate('VerifyClicking.clktmpl', 'Sixth', 0, True, '', ctypes.byref(CallTemplateOptions)))
    
    SleepOptions = GetDefaultSleepOptions()
    print("AddSleepActionToTemplate: ", DllFuncs.AddSleepActionToTemplate('VerifyClicking.clktmpl', 'Seventh', 0, True, '', ctypes.byref(SleepOptions)))
    
    SetVarOptions = GetDefaultSetVarOptions()
    SetVarOptions.ListOfVarNames = '$MyVar$'
    SetVarOptions.ListOfVarValues = '30'
    SetVarOptions.ListOfVarEvalBefore = '1'
    print("AddSetVarActionToTemplate: ", DllFuncs.AddSetVarActionToTemplate('VerifyClicking.clktmpl', 'Eighth', 0, True, '', ctypes.byref(SetVarOptions)))
    
    WindowOperationsOptions = GetDefaultWindowOperationsOptions()
    print("AddWindowOperationsActionToTemplate: ", DllFuncs.AddWindowOperationsActionToTemplate('VerifyClicking.clktmpl', 'Nineth', 0, True, '', ctypes.byref(WindowOperationsOptions)))
    
    print("Execute SetVar for saving to ini: ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 7, AStackLevel = 0)) #execute Eighth action
    SaveSetVarToFileOptions = GetDefaultSaveSetVarToFileOptions()
    SaveSetVarToFileOptions.FileName = '$AppDir$\\py\\bmps\\SetVar.ini'
    SaveSetVarToFileOptions.SetVarActionName = 'Eighth'
    print("AddSaveSetVarToFileActionToTemplate: ", DllFuncs.AddSaveSetVarToFileActionToTemplate('VerifyClicking.clktmpl', 'Tenth', 0, True, '', ctypes.byref(SaveSetVarToFileOptions)))
    
    LoadSetVarFromFileOptions = GetDefaultLoadSetVarFromFileOptions()
    LoadSetVarFromFileOptions.FileName = '$AppDir$\\py\\bmps\\SetVar.ini'
    LoadSetVarFromFileOptions.SetVarActionName = 'Eighth'
    print("AddLoadSetVarFromFileActionToTemplate: ", DllFuncs.AddLoadSetVarFromFileActionToTemplate('VerifyClicking.clktmpl', 'Eleventh', 0, True, '', ctypes.byref(LoadSetVarFromFileOptions)))
    
    PluginOptions = GetDefaultPluginOptions()
    PluginOptions.FileName = '$AppDir$\\..\\UIClickerFindWindowsPlugin\\lib\\x86_64-win64\\UIClickerFindWindows.dll'
    PluginOptions.ListOfPropertiesAndValues
    print("AddPluginActionToTemplate: ", DllFuncs.AddPluginActionToTemplate('VerifyClicking.clktmpl', 'Twelveth', 0, True, '', ctypes.byref(PluginOptions)))
    
    EditTemplateOptions = GetDefaultEditTemplateOptions()
    EditTemplateOptions.WhichTemplate = TEditTemplateWhichTemplate.etwtSelf
    EditTemplateOptions.TemplateFileName = ''
    EditTemplateOptions.ListOfEditedProperties = 'Text=Some text to be typed.ControlType=2DelayBetweenKeyStrokes=0Count=3'
    EditTemplateOptions.ListOfEnabledProperties = 'TextControlTypeCount'
    EditTemplateOptions.EditedActionName = 'My new action'
    EditTemplateOptions.EditedActionType = TClkAction.acSetControlText
    EditTemplateOptions.EditedActionCondition = '$LastAction_Status$==Successful'
    print("AddEditTemplateActionToTemplate: ", DllFuncs.AddEditTemplateActionToTemplate('VerifyClicking.clktmpl', 'Thirteenth', 0, True, '', ctypes.byref(EditTemplateOptions)))
    
    
    #///////////////
    
    print("Preparing a FindSubControl with pointer to structure. This one does not need AddFontProfileToFindSubControlAction.")
    FindSubControlOptions = GetDefaultFindControlOptions()
    FindSubControlOptions.MatchText = '64-bit'
    FindSubControlOptions.MatchClassName = ''
    
    TempMatchBitmapTextRec = TMatchBitmapTextRec(3)
    TempMatchBitmapTextRec.Items[0].ForegroundColor = '888888'
    TempMatchBitmapTextRec.Items[1].ForegroundColor = '3888888'
    TempMatchBitmapTextRec.Items[2].ForegroundColor = '7888888'
    TempMatchBitmapTextRec.Items[0].BackgroundColor = '444444'
    TempMatchBitmapTextRec.Items[1].BackgroundColor = '3444444'
    TempMatchBitmapTextRec.Items[2].BackgroundColor = '7444444'
    FindSubControlOptions.MatchBitmapText = PMatchBitmapTextRec(TempMatchBitmapTextRec)
    FindSubControlOptions.ThreadCount = '3'
    
    print("AddFindSubControlActionToTemplate: ", DllFuncs.AddFindSubControlActionToTemplate('VerifyClicking.clktmpl', 'AFindSubColntrol', 1000, True, '$a$<>$b$', ctypes.byref(FindSubControlOptions)))
    
    #///////////////////
    
    
    print("PrepareFilesInServer: ", DllFuncs.PrepareFilesInServer('VerifyClicking.clktmpl'))
    
    print("ExecuteActionAtIndex (Sleep): ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 6, AStackLevel = 0))
    print("ExecuteActionAtIndex (FindControl): ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 2, AStackLevel = 0))
    print("ExecuteActionAtIndex (FindSubControl): ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 3, AStackLevel = 0))
    
    #print("StopTemplateExecution: ", DllFuncs.StopTemplateExecution(0))
    
    print("GetListOfFilesFromClientInMem: ", DllFuncs.GetListOfFilesFromClientInMem())
    
    # #print("GetTemplateContentFromClientInMemAsString: ", DllFuncs.GetTemplateContentFromClientInMemAsString('VerifyClicking.clktmpl'))
    print("ExecuteActionAtIndex (SaveSetVarToFile): ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 9, AStackLevel = 0)) #Save SetVar vars to ini.
    ######## maybe modify the vars here, to verify later that loading them will update from ini
    print("ExecuteActionAtIndex (LoadSetVarFromFile): ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 10, AStackLevel = 0)) #Load SetVar vars from ini.
    
    print("ExecuteActionAtIndex (Plugin): ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 11, AStackLevel = 0)) #plugin.
    
    print("ExecuteActionAtIndex (EditTemplate): ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 12, AStackLevel = 0)) #EditTemplate.
    
    print("...Execute<ActionName>Action...")
    print("...waiting for user (to maybe live edit the action and) to manually click a debugging button on UIClicker...")
    print("ExecuteClickAction: ", DllFuncs.ExecuteClickAction("Another click", 100, ClickOptions, True))
    
    
    #It is expected that the plugins exist on disk.
    UIClickerBitness = DllFuncs.GetPluginBitnessDirName()
    PluginPath = os.path.join(os.path.abspath(os.path.join(os.path.dirname(__file__), os.path.join("..\\..\\UIClickerFindWindowsPlugin\\lib", UIClickerBitness))), "UIClickerFindWindows.dll")
    #PluginPath = os.path.join(os.path.abspath(os.path.join(os.path.dirname(__file__), os.path.join("..\\..\\UIClickerTypewriterPlugin\\lib", UIClickerBitness))), "UIClickerTypewriter.dll")
    print("Loading a plugin from disk... at ", PluginPath)
    
    
    f = open(PluginPath, 'rb')
    try:
        PluginContent = f.read()
    finally:
        f.close
    
    PluginFileSize = LARGE_INTEGER(len(PluginContent))
    print("SendMemPluginFileToServer: ", DllFuncs.SendMemPluginFileToServer('MyPlugin.dll', PluginContent, PluginFileSize))
    #################################### use TDllFunctions().SendMemPluginFileToServer for debugging
    
    
    print("FileProviderClientThreadDone: ", DllFuncs.FileProviderClientThreadDone())
    print("TerminateFileProviderClientThread: ", DllFuncs.TerminateFileProviderClientThread())
    time.sleep(1)
    print("FileProviderClientThreadDone: ", DllFuncs.FileProviderClientThreadDone())
finally:
    print("DestroyLoggingWindow: ", DllFuncs.DestroyLoggingWindow())
    print("DoneClickerClient", DllFuncs.DoneClickerClient())
    print("end of script")

