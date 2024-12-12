#   Copyright (C) 2022-2024 VCC
#   creation date: Dec 2024  (most content moved here from wrapper.py)
#   initial release date: 09 Dec 2024
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
from ctypes.wintypes import LPCSTR, LPCWSTR, BYTE, BOOLEAN, LONG
from UIClickerTypes import *
from UIClickerTypes import TClickOptions


import time


CMaxSharedStringLength = 10 * 1048576; #10MB

class TDllFunctionAddresses:
    def __init__(self):
        self.DllHandle = ctypes.CDLL("..\\ClickerClient\\ClickerClient.dll")  #CDLL is used for cdecl.   WinDLL is used for stdcall (and uses WINFUNCTYPE).


    def GetInitClickerClient(self):
        InitClickerClientProto = ctypes.CFUNCTYPE(None)
        InitClickerClientParams = ()
        InitClickerClientFuncRes = InitClickerClientProto(("InitClickerClient", self.DllHandle), InitClickerClientParams)
        return InitClickerClientFuncRes


    def GetDoneClickerClient(self):
        DoneClickerClientProto = ctypes.CFUNCTYPE(None)
        DoneClickerClientParams = ()
        DoneClickerClientFuncRes = DoneClickerClientProto(("DoneClickerClient", self.DllHandle), DoneClickerClientParams)
        return DoneClickerClientFuncRes


    def GetSetServerAddress(self):
        #SetServerAddressProto = ctypes.CFUNCTYPE(None, ctypes.c_char_p) #the SetServerAddress function returns void
        SetServerAddressProto = ctypes.CFUNCTYPE(None, LPCWSTR)
        SetServerAddressParams = (1, "AAddress", 0),
        SetServerAddressFuncRes = SetServerAddressProto(("SetServerAddress", self.DllHandle), SetServerAddressParams)
        return SetServerAddressFuncRes


    def GetGetServerAddress(self):
        GetServerAddressProto = ctypes.CFUNCTYPE(LONG, ctypes.c_char_p)
        GetServerAddressParams = (1, "AResponse", 0),
        GetServerAddressFuncRes = GetServerAddressProto(("GetServerAddress", self.DllHandle), GetServerAddressParams)
        return GetServerAddressFuncRes
    
    
    def GetTestConnectionToServerAddress(self):
        TestConnectionToServerProto = ctypes.CFUNCTYPE(LONG, ctypes.c_char_p)
        TestConnectionToServerParams = (1, "AResponse", 0),
        TestConnectionToServerFuncRes = TestConnectionToServerProto(("TestConnectionToServer", self.DllHandle), TestConnectionToServerParams)
        return TestConnectionToServerFuncRes


    def GetSetTemplatesDir(self):
        SetTemplatesDirProto = ctypes.CFUNCTYPE(None, LPCWSTR)
        SetTemplatesDirParams = (1, "AAddress", 0),
        SetTemplatesDirFuncRes = SetTemplatesDirProto(("SetTemplatesDir", self.DllHandle), SetTemplatesDirParams)
        return SetTemplatesDirFuncRes


    def GetCreateLoggingWindow(self):
        CreateLoggingWindowProto = ctypes.CFUNCTYPE(None)
        CreateLoggingWindowParams = ()
        CreateLoggingWindowFuncRes = CreateLoggingWindowProto(("CreateLoggingWindow", self.DllHandle), CreateLoggingWindowParams)
        return CreateLoggingWindowFuncRes


    def GetDestroyLoggingWindow(self):
        DestroyLoggingWindowProto = ctypes.CFUNCTYPE(None)
        DestroyLoggingWindowParams = ()
        DestroyLoggingWindowFuncRes = DestroyLoggingWindowProto(("DestroyLoggingWindow", self.DllHandle), DestroyLoggingWindowParams)
        return DestroyLoggingWindowFuncRes


    def GetCreateNewTemplate(self):
        CreateNewTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR)
        CreateNewTemplateParams = (1, "ATemplateFileName", 0),
        CreateNewTemplateFuncRes = CreateNewTemplateProto(("CreateNewTemplate", self.DllHandle), CreateNewTemplateParams)
        return CreateNewTemplateFuncRes


    def GetAddClickActionToTemplate(self):
        AddClickActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PClickOptions)
        AddClickActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "AClickOptions", 0),
        AddClickActionToTemplateFuncRes = AddClickActionToTemplateProto(("AddClickActionToTemplate", self.DllHandle), AddClickActionToTemplateParams)
        return AddClickActionToTemplateFuncRes
    
    
    def GetAddExecAppActionToTemplate(self):
        AddExecAppActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PExecAppOptions)
        AddExecAppActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "AExecAppOptions", 0),
        AddExecAppActionToTemplateFuncRes = AddExecAppActionToTemplateProto(("AddExecAppActionToTemplate", self.DllHandle), AddExecAppActionToTemplateParams)
        return AddExecAppActionToTemplateFuncRes
    
    
    def GetAddFindControlActionToTemplate(self):
        AddFindControlActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PFindControlOptions)
        AddFindControlActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "AFindControlOptions", 0),
        AddFindControlActionToTemplateFuncRes = AddFindControlActionToTemplateProto(("AddFindControlActionToTemplate", self.DllHandle), AddFindControlActionToTemplateParams)
        return AddFindControlActionToTemplateFuncRes
    
    
    def GetAddFindSubControlActionToTemplate(self):
        AddFindSubControlActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PFindControlOptions)
        AddFindSubControlActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "AFindControlOptions", 0),
        AddFindSubControlActionToTemplateFuncRes = AddFindSubControlActionToTemplateProto(("AddFindSubControlActionToTemplate", self.DllHandle), AddFindSubControlActionToTemplateParams)
        return AddFindSubControlActionToTemplateFuncRes
        
        
    def GetAddSetControlTextActionToTemplate(self):
        AddSetControlTextActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PSetControlTextOptions)
        AddSetControlTextActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "ASetControlTextOptions", 0),
        AddSetControlTextActionToTemplateFuncRes = AddSetControlTextActionToTemplateProto(("AddSetControlTextActionToTemplate", self.DllHandle), AddSetControlTextActionToTemplateParams)
        return AddSetControlTextActionToTemplateFuncRes
    
    
    def GetAddCallTemplateActionToTemplate(self):
        AddCallTemplateActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PCallTemplateOptions)
        AddCallTemplateActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "ACallTemplateOptions", 0),
        AddCallTemplateActionToTemplateFuncRes = AddCallTemplateActionToTemplateProto(("AddCallTemplateActionToTemplate", self.DllHandle), AddCallTemplateActionToTemplateParams)
        return AddCallTemplateActionToTemplateFuncRes
    
    
    def GetAddSleepActionToTemplate(self):
        AddSleepActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PSleepOptions)
        AddSleepActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "ASleepOptions", 0),
        AddSleepActionToTemplateFuncRes = AddSleepActionToTemplateProto(("AddSleepActionToTemplate", self.DllHandle), AddSleepActionToTemplateParams)
        return AddSleepActionToTemplateFuncRes
    
    
    def GetAddSetVarActionToTemplate(self):
        AddSetVarActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PSetVarOptions)
        AddSetVarActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "ASetVarOptions", 0),
        AddSetVarActionToTemplateFuncRes = AddSetVarActionToTemplateProto(("AddSetVarActionToTemplate", self.DllHandle), AddSetVarActionToTemplateParams)
        return AddSetVarActionToTemplateFuncRes
    
    
    def GetAddWindowOperationsActionToTemplate(self):
        AddWindowOperationsActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PWindowOperationsOptions)
        AddWindowOperationsActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "AWindowOperationsOptions", 0),
        AddWindowOperationsActionToTemplateFuncRes = AddWindowOperationsActionToTemplateProto(("AddWindowOperationsActionToTemplate", self.DllHandle), AddWindowOperationsActionToTemplateParams)
        return AddWindowOperationsActionToTemplateFuncRes
    
    
    def GetAddLoadSetVarFromFileActionToTemplate(self):
        AddLoadSetVarFromFileActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PLoadSetVarFromFileOptions)
        AddLoadSetVarFromFileActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "ALoadSetVarFromFileOptions", 0),
        AddLoadSetVarFromFileActionToTemplateFuncRes = AddLoadSetVarFromFileActionToTemplateProto(("AddLoadSetVarFromFileActionToTemplate", self.DllHandle), AddLoadSetVarFromFileActionToTemplateParams)
        return AddLoadSetVarFromFileActionToTemplateFuncRes
    
    
    def GetAddSaveSetVarToFileActionToTemplate(self):
        AddSaveSetVarToFileActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PSaveSetVarToFileOptions)
        AddSaveSetVarToFileActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "ASaveSetVarToFileOptions", 0),
        AddSaveSetVarToFileActionToTemplateFuncRes = AddSaveSetVarToFileActionToTemplateProto(("AddSaveSetVarToFileActionToTemplate", self.DllHandle), AddSaveSetVarToFileActionToTemplateParams)
        return AddSaveSetVarToFileActionToTemplateFuncRes
    
    
    def GetAddPluginActionToTemplate(self):
        AddPluginActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PPluginOptions)
        AddPluginActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "APluginOptions", 0),
        AddPluginActionToTemplateFuncRes = AddPluginActionToTemplateProto(("AddPluginActionToTemplate", self.DllHandle), AddPluginActionToTemplateParams)
        return AddPluginActionToTemplateFuncRes
    
    
    def GetAddEditTemplateActionToTemplate(self):
        AddEditTemplateActionToTemplateProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LPCWSTR, LONG, BOOLEAN, LPCWSTR, PEditTemplateOptions)
        AddEditTemplateActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0), (1, "AEditTemplateOptions", 0),
        AddEditTemplateActionToTemplateFuncRes = AddEditTemplateActionToTemplateProto(("AddEditTemplateActionToTemplate", self.DllHandle), AddEditTemplateActionToTemplateParams)
        return AddEditTemplateActionToTemplateFuncRes
    
    
    def GetAddFontProfileToFindSubControlAction(self):
        AddFontProfileToFindSubControlActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PClkFindControlMatchBitmapText)
        AddFontProfileToFindSubControlActionParams = (1, "ATemplateFileName", 0), (1, "AActionIndex", 0), (1, "AFindControlMatchBitmapText", 0),
        AddFontProfileToFindSubControlActionFuncRes = AddFontProfileToFindSubControlActionProto(("AddFontProfileToFindSubControlAction", self.DllHandle), AddFontProfileToFindSubControlActionParams)
        return AddFontProfileToFindSubControlActionFuncRes
    
    
    def GetPrepareFilesInServer(self):
        PrepareFilesInServerProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, ctypes.c_char_p)
        PrepareFilesInServerParams = (1, "ATemplateFileName", 0), (1, "AResponse", 0),
        PrepareFilesInServerFuncRes = PrepareFilesInServerProto(("PrepareFilesInServer", self.DllHandle), PrepareFilesInServerParams)
        return PrepareFilesInServerFuncRes
    
    
    def GetExecuteActionAtIndex(self):
        ExecuteActionAtIndexProto = ctypes.CFUNCTYPE(BOOLEAN, LONG, LONG)
        ExecuteActionAtIndexParams = (1, "AActionIndex", 0), (1, "AStackLevel", 0),
        ExecuteActionAtIndexFuncRes = ExecuteActionAtIndexProto(("ExecuteActionAtIndex", self.DllHandle), ExecuteActionAtIndexParams)
        return ExecuteActionAtIndexFuncRes
    
    
    def GetGetListOfFilesFromClientInMem(self):
        GetListOfFilesFromClientInMemProto = ctypes.CFUNCTYPE(LONG, ctypes.c_char_p)
        GetListOfFilesFromClientInMemParams = (1, "AResponse", 0),
        GetListOfFilesFromClientInMemFuncRes = GetListOfFilesFromClientInMemProto(("GetListOfFilesFromClientInMem", self.DllHandle), GetListOfFilesFromClientInMemParams)
        return GetListOfFilesFromClientInMemFuncRes
    
    
    def GetGetTemplateContentFromClientInMemAsString(self):
        GetTemplateContentFromClientInMemAsStringProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, ctypes.c_char_p)
        GetTemplateContentFromClientInMemAsStringParams = (1, "ATemplateFileName", 0), (1, "AResponse", 0),
        GetTemplateContentFromClientInMemAsStringFuncRes = GetTemplateContentFromClientInMemAsStringProto(("GetTemplateContentFromClientInMemAsString", self.DllHandle), GetTemplateContentFromClientInMemAsStringParams)
        return GetTemplateContentFromClientInMemAsStringFuncRes


    def GetStopTemplateExecution(self):
        StopTemplateExecutionProto = ctypes.CFUNCTYPE(LONG, LONG, ctypes.c_char_p)
        StopTemplateExecutionParams = (1, "AStackLevel", 0), (1, "AResponse", 0),
        StopTemplateExecutionFuncRes = StopTemplateExecutionProto(("StopTemplateExecution", self.DllHandle), StopTemplateExecutionParams)
        return StopTemplateExecutionFuncRes


    def GetStartFileProviderClientThread(self):
        StartFileProviderClientThreadProto = ctypes.CFUNCTYPE(None)
        StartFileProviderClientThreadParams = ()
        StartFileProviderClientThreadFuncRes = StartFileProviderClientThreadProto(("StartFileProviderClientThread", self.DllHandle), StartFileProviderClientThreadParams)
        return StartFileProviderClientThreadFuncRes


    def GetTerminateFileProviderClientThread(self):
        TerminateFileProviderClientThreadProto = ctypes.CFUNCTYPE(None)
        TerminateFileProviderClientThreadParams = ()
        TerminateFileProviderClientThreadFuncRes = TerminateFileProviderClientThreadProto(("TerminateFileProviderClientThread", self.DllHandle), TerminateFileProviderClientThreadParams)
        return TerminateFileProviderClientThreadFuncRes


    def GetAddListOfAccessibleDirsToFileProviderClient(self):
        AddListOfAccessibleDirsToFileProviderClientProto = ctypes.CFUNCTYPE(None, LPCWSTR)
        AddListOfAccessibleDirsToFileProviderClientParams = (1, "AListOfDirs", 0),
        AddListOfAccessibleDirsToFileProviderClientFuncRes = AddListOfAccessibleDirsToFileProviderClientProto(("AddListOfAccessibleDirsToFileProviderClient", self.DllHandle), AddListOfAccessibleDirsToFileProviderClientParams)
        return AddListOfAccessibleDirsToFileProviderClientFuncRes


    def GetAddListOfAccessibleFileExtensionsToFileProviderClient(self):
        AddListOfAccessibleFileExtensionsToFileProviderClientProto = ctypes.CFUNCTYPE(None, LPCWSTR)
        AddListOfAccessibleFileExtensionsToFileProviderClientParams = (1, "AListOfExtensions", 0),
        AddListOfAccessibleFileExtensionsToFileProviderClientFuncRes = AddListOfAccessibleFileExtensionsToFileProviderClientProto(("AddListOfAccessibleFileExtensionsToFileProviderClient", self.DllHandle), AddListOfAccessibleFileExtensionsToFileProviderClientParams)
        return AddListOfAccessibleFileExtensionsToFileProviderClientFuncRes


    def GetFileProviderClientThreadDone(self):
        FileProviderClientThreadDoneProto = ctypes.CFUNCTYPE(BOOLEAN)
        FileProviderClientThreadDoneParams = ()
        FileProviderClientThreadDoneFuncRes = FileProviderClientThreadDoneProto(("FileProviderClientThreadDone", self.DllHandle), FileProviderClientThreadDoneParams)
        return FileProviderClientThreadDoneFuncRes


    def GetSetFileProviderClientConnectTimeout(self):
        SetFileProviderClientConnectTimeoutProto = ctypes.CFUNCTYPE(None, LONG)
        SetFileProviderClientConnectTimeoutParams = (1, "ATimeout", 0),
        SetFileProviderClientConnectTimeoutFuncRes = SetFileProviderClientConnectTimeoutProto(("SetFileProviderClientConnectTimeout", self.DllHandle), SetFileProviderClientConnectTimeoutParams)
        return SetFileProviderClientConnectTimeoutFuncRes


    def GetClearClientInMemFS(self):
        ClearClientInMemFSProto = ctypes.CFUNCTYPE(None)
        ClearClientInMemFSParams = ()
        ClearClientInMemFSFuncRes = ClearClientInMemFSProto(("ClearClientInMemFS", self.DllHandle), ClearClientInMemFSParams)
        return ClearClientInMemFSFuncRes


    def GetClearServerInMemFS(self):
        ClearServerInMemFSProto = ctypes.CFUNCTYPE(None)
        ClearServerInMemFSParams = ()
        ClearServerInMemFSFuncRes = ClearServerInMemFSProto(("ClearServerInMemFS", self.DllHandle), ClearServerInMemFSParams)
        return ClearServerInMemFSFuncRes


#Execute<ActionName>Action functions. These allow using the "UseServerDebugging" flag, to see the action in ObjectInspector before executing it.

    def GetExecuteClickAction(self):
        ExecuteClickActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PClickOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteClickActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AClickOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteClickActionFuncRes = ExecuteClickActionProto(("ExecuteClickAction", self.DllHandle), ExecuteClickActionParams)
        return ExecuteClickActionFuncRes


    def GetExecuteExecAppAction(self):
        ExecuteExecAppActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PExecAppOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteExecAppActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AExecAppOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteExecAppActionFuncRes = ExecuteExecAppActionProto(("ExecuteExecAppAction", self.DllHandle), ExecuteExecAppActionParams)
        return ExecuteExecAppActionFuncRes


    def GetExecuteFindControlAction(self):
        ExecuteFindControlActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PFindControlOptions, BOOLEAN, LPCWSTR, ctypes.c_char_p)
        ExecuteFindControlActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AFindControlOptions", 0), (1, "AUseServerDebugging", 0), (1, "AFileLocation", 0), (1, "AResultStr", 0),
        ExecuteFindControlActionFuncRes = ExecuteFindControlActionProto(("ExecuteFindControlAction", self.DllHandle), ExecuteFindControlActionParams)
        return ExecuteFindControlActionFuncRes


    def GetExecuteFindSubControlAction(self):
        ExecuteFindSubControlActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PFindControlOptions, BOOLEAN, LPCWSTR, ctypes.c_char_p)
        ExecuteFindSubControlActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AFindSubControlOptions", 0), (1, "AUseServerDebugging", 0), (1, "AFileLocation", 0), (1, "AResultStr", 0),
        ExecuteFindSubControlActionFuncRes = ExecuteFindSubControlActionProto(("ExecuteFindSubControlAction", self.DllHandle), ExecuteFindSubControlActionParams)
        return ExecuteFindSubControlActionFuncRes


    def GetExecuteSetControlTextAction(self):
        ExecuteSetControlTextActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PSetControlTextOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteSetControlTextActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "ASetControlTextOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteSetControlTextActionFuncRes = ExecuteSetControlTextActionProto(("ExecuteSetControlTextAction", self.DllHandle), ExecuteSetControlTextActionParams)
        return ExecuteSetControlTextActionFuncRes


    def GetExecuteCallTemplateAction(self):
        ExecuteCallTemplateActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PCallTemplateOptions, BOOLEAN, LPCWSTR, ctypes.c_char_p)
        ExecuteCallTemplateActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "ACallTemplateOptions", 0), (1, "AUseServerDebugging", 0), (1, "AFileLocation", 0), (1, "AResultStr", 0),
        ExecuteCallTemplateActionFuncRes = ExecuteCallTemplateActionProto(("ExecuteCallTemplateAction", self.DllHandle), ExecuteCallTemplateActionParams)
        return ExecuteCallTemplateActionFuncRes


    def GetExecuteSleepAction(self):
        ExecuteSleepActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PSleepOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteSleepActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "ASleepOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteSleepActionFuncRes = ExecuteSleepActionProto(("ExecuteSleepAction", self.DllHandle), ExecuteSleepActionParams)
        return ExecuteSleepActionFuncRes


    def GetExecuteSetVarAction(self):
        ExecuteSetVarActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PSetVarOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteSetVarActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "ASetVarOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteSetVarActionFuncRes = ExecuteSetVarActionProto(("ExecuteSetVarAction", self.DllHandle), ExecuteSetVarActionParams)
        return ExecuteSetVarActionFuncRes


    def GetExecuteWindowOperationsAction(self):
        ExecuteWindowOperationsActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PWindowOperationsOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteWindowOperationsActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AWindowOperationsOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteWindowOperationsActionFuncRes = ExecuteWindowOperationsActionProto(("ExecuteWindowOperationsAction", self.DllHandle), ExecuteWindowOperationsActionParams)
        return ExecuteWindowOperationsActionFuncRes


    def GetExecuteLoadSetVarFromFileAction(self):
        ExecuteLoadSetVarFromFileActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PLoadSetVarFromFileOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteLoadSetVarFromFileActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "ALoadSetVarFromFileOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteLoadSetVarFromFileActionFuncRes = ExecuteLoadSetVarFromFileActionProto(("ExecuteLoadSetVarFromFileAction", self.DllHandle), ExecuteLoadSetVarFromFileActionParams)
        return ExecuteLoadSetVarFromFileActionFuncRes


    def GetExecuteSaveSetVarToFileAction(self):
        ExecuteSaveSetVarToFileActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PSaveSetVarToFileOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteSaveSetVarToFileActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "ASaveSetVarToFileOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteSaveSetVarToFileActionFuncRes = ExecuteSaveSetVarToFileActionProto(("ExecuteSaveSetVarToFileAction", self.DllHandle), ExecuteSaveSetVarToFileActionParams)
        return ExecuteSaveSetVarToFileActionFuncRes


    def GetExecutePluginAction(self):
        ExecutePluginActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PPluginOptions, BOOLEAN, BOOLEAN, ctypes.c_char_p)
        ExecutePluginActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "APluginOptions", 0), (1, "AUseServerDebugging", 0), (1, "AUseStepIntoDebugging", 0), (1, "AResultStr", 0),
        ExecutePluginActionFuncRes = ExecutePluginActionProto(("ExecutePluginAction", self.DllHandle), ExecutePluginActionParams)
        return ExecutePluginActionFuncRes


    def GetExecuteEditTemplateAction(self):
        ExecuteEditTemplateActionProto = ctypes.CFUNCTYPE(LONG, LPCWSTR, LONG, PEditTemplateOptions, BOOLEAN, ctypes.c_char_p)
        ExecuteEditTemplateActionParams = (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AEditTemplateOptions", 0), (1, "AUseServerDebugging", 0), (1, "AResultStr", 0),
        ExecuteEditTemplateActionFuncRes = ExecuteEditTemplateActionProto(("ExecuteEditTemplateAction", self.DllHandle), ExecuteEditTemplateActionParams)
        return ExecuteEditTemplateActionFuncRes


class TDllFunctions:
    def __init__(self):
        self.Addresses = TDllFunctionAddresses()
        
        self.InitClickerClientFunc = self.Addresses.GetInitClickerClient()
        self.DoneClickerClientFunc = self.Addresses.GetDoneClickerClient()
        
        self.SetServerAddressFunc = self.Addresses.GetSetServerAddress()
        self.GetServerAddressFunc = self.Addresses.GetGetServerAddress()
        self.TestConnectionToServerFunc = self.Addresses.GetTestConnectionToServerAddress()
        self.SetTemplatesDirFunc = self.Addresses.GetSetTemplatesDir()
        self.CreateLoggingWindowFunc = self.Addresses.GetCreateLoggingWindow()
        self.DestroyLoggingWindowFunc = self.Addresses.GetDestroyLoggingWindow()
        self.CreateNewTemplateFunc = self.Addresses.GetCreateNewTemplate()
        
        self.AddClickActionToTemplateFunc = self.Addresses.GetAddClickActionToTemplate()
        self.AddExecAppActionToTemplateFunc = self.Addresses.GetAddExecAppActionToTemplate()
        self.AddFindControlActionToTemplateFunc = self.Addresses.GetAddFindControlActionToTemplate()
        self.AddFindSubControlActionToTemplateFunc = self.Addresses.GetAddFindSubControlActionToTemplate()
        self.AddSetControlTextActionToTemplateFunc = self.Addresses.GetAddSetControlTextActionToTemplate()
        self.AddCallTemplateActionToTemplateFunc = self.Addresses.GetAddCallTemplateActionToTemplate()
        self.AddSleepActionToTemplateFunc = self.Addresses.GetAddSleepActionToTemplate()
        self.AddSetVarActionToTemplateFunc = self.Addresses.GetAddSetVarActionToTemplate()
        self.AddWindowOperationsActionToTemplateFunc = self.Addresses.GetAddWindowOperationsActionToTemplate()
        self.AddLoadSetVarFromFileActionToTemplateFunc = self.Addresses.GetAddLoadSetVarFromFileActionToTemplate()
        self.AddSaveSetVarToFileActionToTemplateFunc = self.Addresses.GetAddSaveSetVarToFileActionToTemplate()
        self.AddPluginActionToTemplateFunc = self.Addresses.GetAddPluginActionToTemplate()
        self.AddEditTemplateActionToTemplateFunc = self.Addresses.GetAddEditTemplateActionToTemplate()
        
        self.AddFontProfileToFindSubControlActionFunc = self.Addresses.GetAddFontProfileToFindSubControlAction()
        
        self.PrepareFilesInServerFunc = self.Addresses.GetPrepareFilesInServer()
        self.ExecuteActionAtIndexFunc = self.Addresses.GetExecuteActionAtIndex()
        self.GetListOfFilesFromClientInMemFunc = self.Addresses.GetGetListOfFilesFromClientInMem()
        self.GetTemplateContentFromClientInMemAsStringFunc = self.Addresses.GetGetTemplateContentFromClientInMemAsString()
        self.StopTemplateExecutionFunc = self.Addresses.GetStopTemplateExecution()
        
        self.GetStartFileProviderClientThreadFunc = self.Addresses.GetStartFileProviderClientThread()
        self.GetTerminateFileProviderClientThreadFunc = self.Addresses.GetTerminateFileProviderClientThread()
        self.GetAddListOfAccessibleDirsToFileProviderClientFunc = self.Addresses.GetAddListOfAccessibleDirsToFileProviderClient()
        self.GetAddListOfAccessibleFileExtensionsToFileProviderClientFunc = self.Addresses.GetAddListOfAccessibleFileExtensionsToFileProviderClient()
        self.GetFileProviderClientThreadDoneFunc = self.Addresses.GetFileProviderClientThreadDone()
        self.GetSetFileProviderClientConnectTimeoutFunc = self.Addresses.GetSetFileProviderClientConnectTimeout()
        
        self.GetClearClientInMemFSFunc = self.Addresses.GetClearClientInMemFS()
        self.GetClearServerInMemFSFunc = self.Addresses.GetClearServerInMemFS()
        
        self.ExecuteClickActionFunc = self.Addresses.GetExecuteClickAction()
        self.ExecuteExecAppActionFunc = self.Addresses.GetExecuteExecAppAction()
        self.ExecuteFindControlActionFunc = self.Addresses.GetExecuteFindControlAction()
        self.ExecuteFindSubControlActionFunc = self.Addresses.GetExecuteFindSubControlAction()
        self.ExecuteSetControlTextActionFunc = self.Addresses.GetExecuteSetControlTextAction()
        self.ExecuteCallTemplateActionFunc = self.Addresses.GetExecuteCallTemplateAction()
        self.ExecuteSleepActionFunc = self.Addresses.GetExecuteSleepAction()
        self.ExecuteSetVarActionFunc = self.Addresses.GetExecuteSetVarAction()
        self.ExecuteWindowOperationsActionFunc = self.Addresses.GetExecuteWindowOperationsAction()
        self.ExecuteLoadSetVarFromFileActionFunc = self.Addresses.GetExecuteLoadSetVarFromFileAction()
        self.ExecuteSaveSetVarToFileActionFunc = self.Addresses.GetExecuteSaveSetVarToFileAction()
        self.ExecutePluginActionFunc = self.Addresses.GetExecutePluginAction()
        self.ExecuteEditTemplateActionFunc = self.Addresses.GetExecuteEditTemplateAction()
        
    def InitClickerClient(self):
        try:
            self.InitClickerClientFunc()
            return 'OK'
        except:
            return 'AV on InitClickerClient'


    def DoneClickerClient(self):
        try:
            self.DoneClickerClientFunc()
            return 'OK'
        except:
            return 'AV on DoneClickerClient'


    def SetServerAddress(self, Address):
        try:
            self.SetServerAddressFunc(Address)  #sending PWideChar, and converting to ANSI at dll
            return 'OK'
        except:
            return 'AV on SetServerAddress'


    def GetServerAddress(self):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.GetServerAddressFunc(ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response
        except:
            return 'AV on GetServerAddress'


    def TestConnectionToServer(self):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.TestConnectionToServerFunc(ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response
        except:
            return 'AV on TestConnectionToServer'


    def SetTemplatesDir(self, ADir):
        try:
            self.SetTemplatesDirFunc(ADir)  #sending PWideChar, and converting to ANSI at dll
            return 'OK'
        except:
            return 'AV on SetTemplatesDir'


    def CreateLoggingWindow(self):
        try:
            self.CreateLoggingWindowFunc()
            return 'OK'
        except:
            return 'AV on CreateLoggingWindow'


    def DestroyLoggingWindow(self):
        try:
            self.DestroyLoggingWindowFunc()
            return 'OK'
        except:
            return 'AV on DestroyLoggingWindow'



    def CreateNewTemplate(self, ATemplateFileName):
        try:
            CreateInMemFileResult = self.CreateNewTemplateFunc(ATemplateFileName)  #sending PWideChar, and converting to ANSI at dll
            return CreateInMemFileResult
        except:
            return 'AV on CreateNewTemplate'
    
    
    def AddClickActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AClickOptions):
        try:
            AddClickActionToTemplateResult = self.AddClickActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AClickOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddClickActionToTemplateResult
        except:
            return 'AV on AddClickActionToTemplate'
    
    
    def AddExecAppActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AExecAppOptions):
        try:
            AddExecAppActionToTemplateResult = self.AddExecAppActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AExecAppOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddExecAppActionToTemplateResult
        except:
            return 'AV on AddExecAppActionToTemplate'


    def AddFindControlActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions):
        try:
            AddFindControlActionToTemplateResult = self.AddFindControlActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddFindControlActionToTemplateResult
        except Exception as e:
            return 'AV on AddFindControlActionToTemplate. {}'.format(e)
            
            
    def AddFindSubControlActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions):
        try:
            AddFindSubControlActionToTemplateResult = self.AddFindSubControlActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddFindSubControlActionToTemplateResult
        except Exception as e:
            return 'AV on AddFindSubControlActionToTemplate. {}'.format(e)
            
            
    def AddSetControlTextActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASetControlTextOptions):
        try:
            AddSetControlTextActionToTemplateResult = self.AddSetControlTextActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASetControlTextOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddSetControlTextActionToTemplateResult
        except:
            return 'AV on AddSetControlTextActionToTemplate'

            
    def AddCallTemplateActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ACallTemplateOptions):
        try:
            AddCallTemplateActionToTemplateResult = self.AddCallTemplateActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ACallTemplateOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddCallTemplateActionToTemplateResult
        except:
            return 'AV on AddCallTemplateActionToTemplate'


    def AddSleepActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASleepOptions):
        try:
            AddSleepActionToTemplateResult = self.AddSleepActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASleepOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddSleepActionToTemplateResult
        except:
            return 'AV on AddSleepActionToTemplate'


    def AddSetVarActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASetVarOptions):
        try:
            AddSetVarActionToTemplateResult = self.AddSetVarActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASetVarOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddSetVarActionToTemplateResult
        except:
            return 'AV on AddSetVarActionToTemplate'


    def AddWindowOperationsActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AWindowOperationsOptions):
        try:
            AddWindowOperationsActionToTemplateResult = self.AddWindowOperationsActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AWindowOperationsOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddWindowOperationsActionToTemplateResult
        except:
            return 'AV on AddWindowOperationsActionToTemplate'


    def AddLoadSetVarFromFileActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ALoadSetVarFromFileOptions):
        try:
            AddLoadSetVarFromFileActionToTemplateResult = self.AddLoadSetVarFromFileActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ALoadSetVarFromFileOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddLoadSetVarFromFileActionToTemplateResult
        except:
            return 'AV on AddLoadSetVarFromFileActionToTemplate'


    def AddSaveSetVarToFileActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASaveSetVarToFileOptions):
        try:
            AddSaveSetVarToFileActionToTemplateResult = self.AddSaveSetVarToFileActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASaveSetVarToFileOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddSaveSetVarToFileActionToTemplateResult
        except:
            return 'AV on AddSaveSetVarToFileActionToTemplate'


    def AddPluginActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, APluginOptions):
        try:
            AddPluginActionToTemplateResult = self.AddPluginActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, APluginOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddPluginActionToTemplateResult
        except:
            return 'AV on AddPluginActionToTemplate'


    def AddEditTemplateActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AEditTemplateOptions):
        try:
            AddEditTemplateActionToTemplateResult = self.AddEditTemplateActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AEditTemplateOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddEditTemplateActionToTemplateResult
        except:
            return 'AV on AddEditTemplateActionToTemplate'


    def AddFontProfileToFindSubControlAction(self, ATemplateFileName, AActionIndex, AFindControlMatchBitmapText):
        try:
            AddFontProfileToFindSubControlActionResult = self.AddFontProfileToFindSubControlActionFunc(ATemplateFileName, AActionIndex, AFindControlMatchBitmapText)  #sending PWideChar, and converting to ANSI at dll
            return AddFontProfileToFindSubControlActionResult
        except Exception as e:
            return 'AV on AddFontProfileToFindSubControlAction. {}'.format(e)

    
    def PrepareFilesInServer(self, ATemplateFileName):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.PrepareFilesInServerFunc(ATemplateFileName, ResponsePtr)  #sending PWideChar, and converting to ANSI at dll
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response
        except:
            return 'AV on PrepareFilesInServer'
            
    
    def ExecuteActionAtIndex(self, AActionIndex, AStackLevel):
        try:
            ExecuteActionAtIndexResult = self.ExecuteActionAtIndexFunc(AActionIndex, AStackLevel)
            return ExecuteActionAtIndexResult
        except:
            print('AV on ExecuteActionAtIndex')
            return False


    def GetListOfFilesFromClientInMem(self):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.GetListOfFilesFromClientInMemFunc(ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8')
        except:
            return 'AV on GetListOfFilesFromClientInMem'


    def GetTemplateContentFromClientInMemAsString(self, ATemplateFileName):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.GetTemplateContentFromClientInMemAsStringFunc(ATemplateFileName, ResponsePtr)  #sending PWideChar, and converting to ANSI at dll
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8')
        except:
            return 'AV on GetTemplateContentFromClientInMemAsString'


    def StopTemplateExecution(self, AStackLevel):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.StopTemplateExecutionFunc(AStackLevel, ResponsePtr)  #sending PWideChar, and converting to ANSI at dll
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8')
        except:
            return 'AV on StopTemplateExecution'


    def StartFileProviderClientThread(self):
        try:
            self.GetStartFileProviderClientThreadFunc()
            return 'OK'
        except:
            return 'AV on StartFileProviderClientThread'
            

    def TerminateFileProviderClientThread(self):
        try:
            self.GetTerminateFileProviderClientThreadFunc()
            return 'OK'
        except:
            return 'AV on TerminateFileProviderClientThread'


    def AddListOfAccessibleDirsToFileProviderClient(self, AListOfDirs):
        try:
            self.GetAddListOfAccessibleDirsToFileProviderClientFunc(AListOfDirs)  #CRLF separated dirs, accessible by the dll
            return 'OK'
        except:
            return 'AV on AddListOfAccessibleDirsToFileProviderClient'


    def AddListOfAccessibleFileExtensionsToFileProviderClient(self, AListOfExtensions):
        try:
            self.GetAddListOfAccessibleFileExtensionsToFileProviderClientFunc(AListOfExtensions)  #CRLF separated extensions, e.g. ".bmp\r\n.clktmpl"
            return 'OK'
        except:
            return 'AV on AddListOfAccessibleFileExtensionsToFileProviderClient'


    def FileProviderClientThreadDone(self):
        try:
            FileProviderClientThreadDoneResult = self.GetFileProviderClientThreadDoneFunc()
            return FileProviderClientThreadDoneResult
        except:
            print('AV on FileProviderClientThreadDone')
            return False


    def SetFileProviderClientConnectTimeout(self, ATimeout):
        try:
            self.GetSetFileProviderClientConnectTimeoutFunc(ATimeout)
            return 'OK'
        except:
            return 'AV on SetFileProviderClientConnectTimeout'


    def ClearClientInMemFS(self):
        try:
            self.GetClearClientInMemFSFunc()
            return 'OK'
        except:
            return 'AV on ClearClientInMemFS'


    def ClearServerInMemFS(self):
        try:
            self.GetClearServerInMemFSFunc()
            return 'OK'
        except:
            return 'AV on ClearServerInMemFS'



    def ExecuteClickAction(self, AActionName, AActionTimeout, AClickOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteClickActionFunc(AActionName, AActionTimeout, AClickOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteClickAction'


    def ExecuteExecAppAction(self, AActionName, AActionTimeout, AExecAppOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteExecAppActionFunc(AActionName, AActionTimeout, AExecAppOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteExecAppAction'


    def ExecuteFindControlAction(self, AActionName, AActionTimeout, AFindControlOptions, AUseServerDebugging, AFileLocation):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteFindControlActionFunc(AActionName, AActionTimeout, AFindControlOptions, AUseServerDebugging, AFileLocation, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteFindControlAction'


    def ExecuteFindSubControlAction(self, AActionName, AActionTimeout, AFindSubControlOptions, AUseServerDebugging, AFileLocation):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteFindSubControlActionFunc(AActionName, AActionTimeout, AFindSubControlOptions, AUseServerDebugging, AFileLocation, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteFindSubControlAction'


    def ExecuteSetControlTextAction(self, AActionName, AActionTimeout, ASetControlTextOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteSetControlTextActionFunc(AActionName, AActionTimeout, ASetControlTextOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteSetControlTextAction'


    def ExecuteCallTemplateAction(self, AActionName, AActionTimeout, ACallTemplateOptions, AUseServerDebugging, AFileLocation):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteCallTemplateActionFunc(AActionName, AActionTimeout, ACallTemplateOptions, AUseServerDebugging, AFileLocation, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteCallTemplateAction'


    def ExecuteSleepAction(self, AActionName, AActionTimeout, ASleepOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteSleepActionFunc(AActionName, AActionTimeout, ASleepOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteSleepAction'


    def ExecuteSetVarAction(self, AActionName, AActionTimeout, ASetVarOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteSetVarActionFunc(AActionName, AActionTimeout, ASetVarOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteSetVarAction'


    def ExecuteWindowOperationsAction(self, AActionName, AActionTimeout, AWindowOperationsOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteWindowOperationsActionFunc(AActionName, AActionTimeout, AWindowOperationsOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteWindowOperationsAction'


    def ExecuteLoadSetVarFromFileAction(self, AActionName, AActionTimeout, ALoadSetVarFromFileOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteLoadSetVarFromFileActionFunc(AActionName, AActionTimeout, ALoadSetVarFromFileOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteLoadSetVarFromFileAction'


    def ExecuteSaveSetVarToFileAction(self, AActionName, AActionTimeout, ASaveSetVarToFileOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.GetExecuteSaveSetVarToFileActionFunc(AActionName, AActionTimeout, ASaveSetVarToFileOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteSaveSetVarToFileAction'


    def ExecutePluginAction(self, AActionName, AActionTimeout, APluginOptions, AUseServerDebugging, AUseStepIntoDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecutePluginActionFunc(AActionName, AActionTimeout, APluginOptions, AUseServerDebugging, AUseStepIntoDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecutePluginAction'


    def ExecuteEditTemplateAction(self, AActionName, AActionTimeout, AEditTemplateOptions, AUseServerDebugging):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.ExecuteEditTemplateActionFunc(AActionName, AActionTimeout, AEditTemplateOptions, AUseServerDebugging, ResponsePtr)
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response.decode('utf-8') #Returns the list of variables, separated by ASCII8 ASCII7  (i.e. #8#7)
        except:
            return 'AV on ExecuteEditTemplateAction'


#A similar set of functions as above, but these ones return Boolean, instead of 0 or 1.
#More than that, their results are set to True for succes and False otherwise (without any other info).

class TUIClickerDllFunctions:
    def __init__(self):
        self.DllFuncs = TDllFunctions()

    def InitClickerClient(self):
        return self.DllFuncs.InitClickerClient() == 'OK'


    def DoneClickerClient(self):
        return self.DllFuncs.DoneClickerClient() == 'OK'


    def SetServerAddress(self, Address):
        return self.DllFuncs.SetServerAddress(Address) == 'OK'  #sending PWideChar, and converting to ANSI at dll


    def GetServerAddress(self):
        return self.DllFuncs.GetServerAddress()


    def TestConnectionToServer(self):
        return self.DllFuncs.TestConnectionToServer() == b'Connection ok'


    def SetTemplatesDir(self, ADir):
        return self.DllFuncs.SetTemplatesDir(ADir) == 'OK'  #sending PWideChar, and converting to ANSI at dll


    def CreateLoggingWindow(self):
        return self.DllFuncs.CreateLoggingWindow() == 'OK'


    def DestroyLoggingWindow(self):
        return self.DllFuncs.DestroyLoggingWindow() == 'OK'


    def CreateNewTemplate(self, ATemplateFileName):
        return self.DllFuncs.CreateNewTemplate(ATemplateFileName) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddClickActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AClickOptions):
        return self.DllFuncs.AddClickActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AClickOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddExecAppActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AExecAppOptions):
        return self.DllFuncs.AddExecAppActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AExecAppOptions) == 0   #sending PWideChar, and converting to ANSI at dll


    def AddFindControlActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions):
        return self.DllFuncs.AddFindControlActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddFindSubControlActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions):
        return self.DllFuncs.AddFindSubControlActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddSetControlTextActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASetControlTextOptions):
        return self.DllFuncs.AddSetControlTextActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASetControlTextOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddCallTemplateActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ACallTemplateOptions):
        return self.DllFuncs.AddCallTemplateActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ACallTemplateOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddSleepActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASleepOptions):
        return self.DllFuncs.AddSleepActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASleepOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddSetVarActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASetVarOptions):
        return self.DllFuncs.AddSetVarActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASetVarOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddWindowOperationsActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AWindowOperationsOptions):
        return self.DllFuncs.AddWindowOperationsActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AWindowOperationsOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddLoadSetVarFromFileActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ALoadSetVarFromFileOptions):
        return self.DllFuncs.AddLoadSetVarFromFileActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ALoadSetVarFromFileOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddSaveSetVarToFileActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASaveSetVarToFileOptions):
        return self.DllFuncs.AddSaveSetVarToFileActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, ASaveSetVarToFileOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddPluginActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, APluginOptions):
        return self.DllFuncs.AddPluginActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, APluginOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddEditTemplateActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AEditTemplateOptions):
        return self.DllFuncs.AddEditTemplateActionToTemplate(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AEditTemplateOptions) == 0  #sending PWideChar, and converting to ANSI at dll


    def AddFontProfileToFindSubControlAction(self, ATemplateFileName, AActionIndex, AFindControlMatchBitmapText):
        return self.DllFuncs.AddFontProfileToFindSubControlActionFunc(ATemplateFileName, AActionIndex, AFindControlMatchBitmapText) == 0  #sending PWideChar, and converting to ANSI at dll


    def PrepareFilesInServer(self, ATemplateFileName):
        return self.DllFuncs.PrepareFilesInServer(ATemplateFileName)  #sending PWideChar, and converting to ANSI at dll


    def ExecuteActionAtIndex(self, AActionIndex, AStackLevel):
        return self.DllFuncs.ExecuteActionAtIndex(AActionIndex, AStackLevel) == 1


    def GetListOfFilesFromClientInMem(self):
        return self.DllFuncs.GetListOfFilesFromClientInMem()


    def GetTemplateContentFromClientInMemAsString(self, ATemplateFileName):
        return self.DllFuncs.GetTemplateContentFromClientInMemAsString(ATemplateFileName)  #sending PWideChar, and converting to ANSI at dll


    def StopTemplateExecution(self, AStackLevel):
        return self.DllFuncs.StopTemplateExecution(AStackLevel) == 'Done'  #sending PWideChar, and converting to ANSI at dll


    def StartFileProviderClientThread(self):
        return self.DllFuncs.StartFileProviderClientThread() == 'OK'


    def TerminateFileProviderClientThread(self):
        return self.DllFuncs.TerminateFileProviderClientThread() == 'OK'


    def AddListOfAccessibleDirsToFileProviderClient(self, AListOfDirs):
        return self.DllFuncs.AddListOfAccessibleDirsToFileProviderClient(AListOfDirs) == 'OK'  #CRLF separated dirs, accessible by the dll


    def AddListOfAccessibleFileExtensionsToFileProviderClient(self, AListOfExtensions):
        return self.DllFuncs.AddListOfAccessibleFileExtensionsToFileProviderClient(AListOfExtensions) == 'OK'  #CRLF separated extensions, e.g. ".bmp\r\n.clktmpl"


    def FileProviderClientThreadDone(self):
        return self.DllFuncs.FileProviderClientThreadDone()


    def SetFileProviderClientConnectTimeout(self, ATimeout):
        return self.DllFuncs.SetFileProviderClientConnectTimeout(ATimeout) == 'OK'


    def ClearClientInMemFS(self):
        return self.DllFuncs.ClearClientInMemFS() == 'OK'


    def ClearServerInMemFS(self):
        return self.DllFuncs.ClearServerInMemFS() == 'OK'


    def ExecuteClickAction(self, AActionName, AActionTimeout, AClickOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteClickAction(AActionName, AActionTimeout, AClickOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteExecAppAction(self, AActionName, AActionTimeout, AExecAppOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteExecAppAction(AActionName, AActionTimeout, AExecAppOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteFindControlAction(self, AActionName, AActionTimeout, AFindControlOptions, AUseServerDebugging, AFileLocation):
        Res = self.DllFuncs.ExecuteFindControlAction(AActionName, AActionTimeout, AFindControlOptions, AUseServerDebugging, AFileLocation)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteFindSubControlAction(self, AActionName, AActionTimeout, AFindSubControlOptions, AUseServerDebugging, AFileLocation):
        Res = self.DllFuncs.ExecuteFindSubControlAction(AActionName, AActionTimeout, AFindSubControlOptions, AUseServerDebugging, AFileLocation)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteSetControlTextAction(self, AActionName, AActionTimeout, ASetControlTextOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteSetControlTextAction(AActionName, AActionTimeout, ASetControlTextOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteCallTemplateAction(self, AActionName, AActionTimeout, ACallTemplateOptions, AUseServerDebugging, AFileLocation):
        Res = self.DllFuncs.ExecuteCallTemplateAction(AActionName, AActionTimeout, ACallTemplateOptions, AUseServerDebugging, AFileLocation)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteSleepAction(self, AActionName, AActionTimeout, ASleepOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteSleepAction(AActionName, AActionTimeout, ASleepOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteSetVarAction(self, AActionName, AActionTimeout, ASetVarOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteSetVarAction(AActionName, AActionTimeout, ASetVarOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteWindowOperationsAction(self, AActionName, AActionTimeout, AWindowOperationsOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteWindowOperationsAction(AActionName, AActionTimeout, AWindowOperationsOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteLoadSetVarFromFileAction(self, AActionName, AActionTimeout, ALoadSetVarFromFileOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteLoadSetVarFromFileAction(AActionName, AActionTimeout, ALoadSetVarFromFileOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteSaveSetVarToFileAction(self, AActionName, AActionTimeout, ASaveSetVarToFileOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteSaveSetVarToFileAction(AActionName, AActionTimeout, ASaveSetVarToFileOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecutePluginAction(self, AActionName, AActionTimeout, APluginOptions, AUseServerDebugging, AUseStepIntoDebugging):
        Res = self.DllFuncs.ExecutePluginAction(AActionName, AActionTimeout, APluginOptions, AUseServerDebugging, AUseStepIntoDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


    def ExecuteEditTemplateAction(self, AActionName, AActionTimeout, AEditTemplateOptions, AUseServerDebugging):
        Res = self.DllFuncs.ExecuteEditTemplateAction(AActionName, AActionTimeout, AEditTemplateOptions, AUseServerDebugging)  #sending PWideChar, and converting to ANSI at dll
        return Res.find("Client exception: ") == -1


