#   Copyright (C) 2022 VCC
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


class TDllFunctions:
    def __init__(self):
        self.Addresses = TDllFunctionAddresses()
        
        self.InitClickerClientFunc = self.Addresses.GetInitClickerClient()
        self.DoneClickerClientFunc = self.Addresses.GetDoneClickerClient()
        
        self.SetServerAddressFunc = self.Addresses.GetSetServerAddress()
        self.GetServerAddressFunc = self.Addresses.GetGetServerAddress()
        self.TestConnectionToServerFunc = self.Addresses.GetTestConnectionToServerAddress()
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
        except:
            return 'AV on AddFindControlActionToTemplate'
            
            
    def AddFindSubControlActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions):
        try:
            AddFindSubControlActionToTemplateResult = self.AddFindSubControlActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition, AFindControlOptions)  #sending PWideChar, and converting to ANSI at dll
            return AddFindSubControlActionToTemplateResult
        except:
            return 'AV on AddFindSubControlActionToTemplate'
            
            
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
           
            
    def AddFontProfileToFindSubControlAction(self, ATemplateFileName, AActionIndex, AFindControlMatchBitmapText):
        try:
            AddFontProfileToFindSubControlActionResult = self.AddFontProfileToFindSubControlActionFunc(ATemplateFileName, AActionIndex, AFindControlMatchBitmapText)  #sending PWideChar, and converting to ANSI at dll
            return AddFontProfileToFindSubControlActionResult
        except:
            return 'AV on AddFontProfileToFindSubControlAction'

    
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


#    def StopTemplateExecution(self, AStackLevel):
#        try:
#            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
#            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
#            RespLen = self.StopTemplateExecutionFunc(AStackLevel, ResponsePtr)  #sending PWideChar, and converting to ANSI at dll
#            
#            Response = ctypes.string_at(ResponsePtr, RespLen)
#            return Response.decode('utf-8')
#        except:
#            return 'AV on StopTemplateExecution'


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


DllFuncs = TDllFunctions()

print("InitClickerClient: ", DllFuncs.InitClickerClient())
try:
    print("TestConnectionToServer: ", DllFuncs.TestConnectionToServer())
    
    print("GetServerAddress: ", DllFuncs.GetServerAddress())
    
    print("SetServerAddress: ", DllFuncs.SetServerAddress('http://192.168.3.102:5444/'))
    print("GetServerAddress: ", DllFuncs.GetServerAddress())
    print("TestConnectionToServer after setting wrong address: ", DllFuncs.TestConnectionToServer())
    
    print("SetServerAddress: ", DllFuncs.SetServerAddress('http://127.0.0.1:5444/'))
    print("GetServerAddress: ", DllFuncs.GetServerAddress())
    print("TestConnectionToServer after setting a working address: ", DllFuncs.TestConnectionToServer())
    
    #print("ClearClientInMemFS: ", DllFuncs.ClearClientInMemFS())  #For debugging only. The client should work without this call.
    #print("ClearServerInMemFS: ", DllFuncs.ClearServerInMemFS())  #For debugging only. The server should work without this call.
    
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
    FindSubControlOptions.InitialRectange.LeftOffset = '56'
    FindSubControlOptions.InitialRectange.TopOffset = '92'
    FindSubControlOptions.InitialRectange.RightOffset = '-70'
    FindSubControlOptions.InitialRectange.BottomOffset = '-123'
    ###############
    print("AddFindSubControlActionToTemplate: ", DllFuncs.AddFindSubControlActionToTemplate('VerifyClicking.clktmpl', 'Fourth', 1000, True, '', ctypes.byref(FindSubControlOptions)))
    print("AddFontProfileToFindSubControlAction: ", DllFuncs.AddFontProfileToFindSubControlAction('VerifyClicking.clktmpl', 3, ctypes.byref(MatchBitmapText)))
    
    SetControlTextOptions = GetDefaultSetControlTextOptions()
    print("AddSetControlTextActionToTemplate: ", DllFuncs.AddSetControlTextActionToTemplate('VerifyClicking.clktmpl', 'Fifth', 0, True, '', ctypes.byref(SetControlTextOptions)))
    
    CallTemplateOptions = GetDefaultCallTemplateOptions()
    print("AddCallTemplateActionToTemplate: ", DllFuncs.AddCallTemplateActionToTemplate('VerifyClicking.clktmpl', 'Sixth', 0, True, '', ctypes.byref(CallTemplateOptions)))
    
    SleepOptions = GetDefaultSleepOptions()
    print("AddSleepActionToTemplate: ", DllFuncs.AddSleepActionToTemplate('VerifyClicking.clktmpl', 'Seventh', 0, True, '', ctypes.byref(SleepOptions)))

    SetVarOptions = GetDefaultSetVarOptions()
    print("AddSetVarActionToTemplate: ", DllFuncs.AddSetVarActionToTemplate('VerifyClicking.clktmpl', 'Eighth', 0, True, '', ctypes.byref(SetVarOptions)))

    WindowOperationsOptions = GetDefaultWindowOperationsOptions()
    print("AddWindowOperationsActionToTemplate: ", DllFuncs.AddWindowOperationsActionToTemplate('VerifyClicking.clktmpl', 'Nineth', 0, True, '', ctypes.byref(WindowOperationsOptions)))


    ###########
    print("AddListOfAccessibleDirsToFileProviderClient: ", DllFuncs.AddListOfAccessibleDirsToFileProviderClient("bmps\\"))
    print("AddListOfAccessibleFileExtensionsToFileProviderClient: ", DllFuncs.AddListOfAccessibleFileExtensionsToFileProviderClient(".bmp\r\n.clktmpl"))
    print("SetFileProviderClientConnectTimeout: ", DllFuncs.SetFileProviderClientConnectTimeout(3000))

    print("StartFileProviderClientThread: ", DllFuncs.StartFileProviderClientThread())
    ###########

    print("PrepareFilesInServer: ", DllFuncs.PrepareFilesInServer('VerifyClicking.clktmpl'))
    
    print("ExecuteActionAtIndex: ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 6, AStackLevel = 0))
    print("ExecuteActionAtIndex: ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 2, AStackLevel = 0))
    print("ExecuteActionAtIndex: ", DllFuncs.ExecuteActionAtIndex(AActionIndex = 3, AStackLevel = 0))
    
    #print("StopTemplateExecution: ", DllFuncs.StopTemplateExecution(0))
    
    print("GetListOfFilesFromClientInMem: ", DllFuncs.GetListOfFilesFromClientInMem())
    
    #print("GetTemplateContentFromClientInMemAsString: ", DllFuncs.GetTemplateContentFromClientInMemAsString('VerifyClicking.clktmpl'))

    print("FileProviderClientThreadDone: ", DllFuncs.FileProviderClientThreadDone())
    print("TerminateFileProviderClientThread: ", DllFuncs.TerminateFileProviderClientThread())
    time.sleep(1)
    print("FileProviderClientThreadDone: ", DllFuncs.FileProviderClientThreadDone())
finally:
    print("DoneClickerClient", DllFuncs.DoneClickerClient())


