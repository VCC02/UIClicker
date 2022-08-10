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
from ctypes.wintypes import LPCSTR, LPCWSTR, BYTE, BOOLEAN, LARGE_INTEGER
DllHandle = ctypes.CDLL("..\\ClickerClient\\ClickerClient.dll")  #CDLL is used for cdecl.   WinDLL is used for stdcall (and uses WINFUNCTYPE).

CMaxSharedStringLength = 10 * 1048576; #10MB


def GetInitClickerClient():
    InitClickerClientProto = ctypes.CFUNCTYPE(None)
    InitClickerClientParams = ()
    InitClickerClientFuncRes = InitClickerClientProto(("InitClickerClient", DllHandle), InitClickerClientParams)
    return InitClickerClientFuncRes


def GetDoneClickerClient():
    DoneClickerClientProto = ctypes.CFUNCTYPE(None)
    DoneClickerClientParams = ()
    DoneClickerClientFuncRes = DoneClickerClientProto(("DoneClickerClient", DllHandle), DoneClickerClientParams)
    return DoneClickerClientFuncRes
 

def GetSetServerAddress():
    #SetServerAddressProto = ctypes.CFUNCTYPE(None, ctypes.c_char_p) #the SetServerAddress function returns void
    SetServerAddressProto = ctypes.CFUNCTYPE(None, LPCWSTR)
    SetServerAddressParams = (1, "AAddress", 0),
    SetServerAddressFuncRes = SetServerAddressProto(("SetServerAddress", DllHandle), SetServerAddressParams)
    return SetServerAddressFuncRes

       
def GetGetServerAddress():
    GetServerAddressProto = ctypes.CFUNCTYPE(LARGE_INTEGER, ctypes.c_char_p)
    GetServerAddressParams = (1, "AResponse", 0),
    GetServerAddressFuncRes = GetServerAddressProto(("GetServerAddress", DllHandle), GetServerAddressParams)
    return GetServerAddressFuncRes


def GetTestConnectionToServerAddress():
    TestConnectionToServerProto = ctypes.CFUNCTYPE(LARGE_INTEGER, ctypes.c_char_p)
    TestConnectionToServerParams = (1, "AResponse", 0),
    TestConnectionToServerFuncRes = TestConnectionToServerProto(("TestConnectionToServer", DllHandle), TestConnectionToServerParams)
    return TestConnectionToServerFuncRes


def GetCreateNewTemplate():
    CreateNewTemplateProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR)
    CreateNewTemplateParams = (1, "ATemplateFileName", 0),
    CreateNewTemplateFuncRes = CreateNewTemplateProto(("CreateNewTemplate", DllHandle), CreateNewTemplateParams)
    return CreateNewTemplateFuncRes
    
        
def GetAddClickActionToTemplate():
    AddClickActionToTemplateProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR, LPCWSTR, LARGE_INTEGER, BOOLEAN, LPCWSTR)
    AddClickActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0),
    AddClickActionToTemplateFuncRes = AddClickActionToTemplateProto(("AddClickActionToTemplate", DllHandle), AddClickActionToTemplateParams)
    return AddClickActionToTemplateFuncRes

       
def GetAddExecAppActionToTemplate():
    AddExecAppActionToTemplateProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR, LPCWSTR, LARGE_INTEGER, BOOLEAN, LPCWSTR)
    AddExecAppActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0),
    AddExecAppActionToTemplateFuncRes = AddExecAppActionToTemplateProto(("AddExecAppActionToTemplate", DllHandle), AddExecAppActionToTemplateParams)
    return AddExecAppActionToTemplateFuncRes

        
def GetPrepareFilesInServer():
    PrepareFilesInServerProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR, ctypes.c_char_p)
    PrepareFilesInServerParams = (1, "ATemplateFileName", 0), (1, "AResponse", 0),
    PrepareFilesInServerFuncRes = PrepareFilesInServerProto(("PrepareFilesInServer", DllHandle), PrepareFilesInServerParams)
    return PrepareFilesInServerFuncRes


def GetGetListOfFilesFromClientInMem():
    GetListOfFilesFromClientInMemProto = ctypes.CFUNCTYPE(LARGE_INTEGER, ctypes.c_char_p)
    GetListOfFilesFromClientInMemParams = (1, "AResponse", 0),
    GetListOfFilesFromClientInMemFuncRes = GetListOfFilesFromClientInMemProto(("GetListOfFilesFromClientInMem", DllHandle), GetListOfFilesFromClientInMemParams)
    return GetListOfFilesFromClientInMemFuncRes
    

def GetGetTemplateContentFromClientInMemAsString():
    GetTemplateContentFromClientInMemAsStringProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR, ctypes.c_char_p)
    GetTemplateContentFromClientInMemAsStringParams = (1, "ATemplateFileName", 0), (1, "AResponse", 0),
    GetTemplateContentFromClientInMemAsStringFuncRes = GetTemplateContentFromClientInMemAsStringProto(("GetTemplateContentFromClientInMemAsString", DllHandle), GetTemplateContentFromClientInMemAsStringParams)
    return GetTemplateContentFromClientInMemAsStringFuncRes
   
        
class TDllFunctions:
    def __init__(self):
        self.InitClickerClientFunc = GetInitClickerClient()
        self.DoneClickerClientFunc = GetDoneClickerClient()
        
        self.SetServerAddressFunc = GetSetServerAddress()
        self.GetServerAddressFunc = GetGetServerAddress()
        self.TestConnectionToServerFunc = GetTestConnectionToServerAddress()
        self.CreateNewTemplateFunc = GetCreateNewTemplate()
        self.AddClickActionToTemplateFunc = GetAddClickActionToTemplate()
        self.AddExecAppActionToTemplateFunc = GetAddExecAppActionToTemplate()
        self.PrepareFilesInServerFunc = GetPrepareFilesInServer()
        self.GetListOfFilesFromClientInMemFunc = GetGetListOfFilesFromClientInMem()
        self.GetTemplateContentFromClientInMemAsStringFunc = GetGetTemplateContentFromClientInMemAsString()
        
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
    
    
    def AddClickActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition):
        try:
            AddClickActionToTemplateResult = self.AddClickActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition)  #sending PWideChar, and converting to ANSI at dll
            return AddClickActionToTemplateResult
        except:
            return 'AV on AddClickActionToTemplate'
    
    
    def AddExecAppActionToTemplate(self, ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition):
        try:
            AddExecAppActionToTemplateResult = self.AddExecAppActionToTemplateFunc(ATemplateFileName, AActionName, AActionTimeout, AActionEnabled, AActionCondition)  #sending PWideChar, and converting to ANSI at dll
            return AddExecAppActionToTemplateResult
        except:
            return 'AV on AddExecAppActionToTemplate'
    
    
    def PrepareFilesInServer(self, ATemplateFileName):
        try:
            buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
            ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
            RespLen = self.PrepareFilesInServerFunc(ATemplateFileName, ResponsePtr)  #sending PWideChar, and converting to ANSI at dll
            
            Response = ctypes.string_at(ResponsePtr, RespLen)
            return Response
        except:
            return 'AV on PrepareFilesInServer'
            
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
    
    print("CreateNewTemplate: ", DllFuncs.CreateNewTemplate('VerifyClicking.clktmpl')) #creates a new template in dll's in-mem file system
    print("CreateNewTemplate: ", DllFuncs.CreateNewTemplate('VerifyClicking.clktmpl')) #the second call returns 1, because the file already exists
    
    print("AddClickActionToTemplate: ", DllFuncs.AddClickActionToTemplate('VerifyClicking.clktmpl', 'First', 0, True, '$a$==$b$'))
    
    print("AddExecAppActionToTemplate: ", DllFuncs.AddExecAppActionToTemplate('VerifyClicking.clktmpl', 'Second', 0, True, '$a$==$b$'))
    
    print("PrepareFilesInServer: ", DllFuncs.PrepareFilesInServer('VerifyClicking.clktmpl'))
    
    print("GetListOfFilesFromClientInMem: ", DllFuncs.GetListOfFilesFromClientInMem())
    
    #print("GetTemplateContentFromClientInMemAsString: ", DllFuncs.GetTemplateContentFromClientInMemAsString('VerifyClicking.clktmpl'))
finally:
    print("DoneClickerClient", DllFuncs.DoneClickerClient())


