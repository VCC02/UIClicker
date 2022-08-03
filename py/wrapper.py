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

InitClickerClientProto = ctypes.CFUNCTYPE(None)
InitClickerClientParams = ()
InitClickerClient = InitClickerClientProto(("InitClickerClient", DllHandle), InitClickerClientParams)

DoneClickerClientProto = ctypes.CFUNCTYPE(None)
DoneClickerClientParams = ()
DoneClickerClient = DoneClickerClientProto(("DoneClickerClient", DllHandle), DoneClickerClientParams)

SetServerAddressFunc = None
def GetSetServerAddressAddress():
    #SetServerAddressProto = ctypes.CFUNCTYPE(None, ctypes.c_char_p) #the SetServerAddress function returns void
    SetServerAddressProto = ctypes.CFUNCTYPE(None, LPCWSTR)
    SetServerAddressParams = (1, "AAddress", 0),
    SetServerAddressFunc = SetServerAddressProto(("SetServerAddress", DllHandle), SetServerAddressParams)
    return SetServerAddressFunc
    
def SetServerAddress(Address):
    try:
        #buffer = ctypes.create_string_buffer(10 * 1048576) #(CMaxSharedStringLength)
        #ctypes.memset(ctypes.addressof(buffer[0]), ctypes.addressof(Address[0]), len(Address) + 1)  #### not working
        #AddressPtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
        #SetServerAddressFunc(AddressPtr)
        SetServerAddressFunc(Address)  #sending PWideChar, and converting to ANSI at dll
        return 'OK'
    except:
        return 'AV on SetServerAddress'

TestConnectionToServerFunc = None
def GetTestConnectionToServerAddress():
  TestConnectionToServerProto = ctypes.CFUNCTYPE(LARGE_INTEGER, ctypes.c_char_p)
  TestConnectionToServerParams = (1, "AResponse", 0),
  TestConnectionToServerFunc = TestConnectionToServerProto(("TestConnectionToServer", DllHandle), TestConnectionToServerParams)
  return TestConnectionToServerFunc

def TestConnectionToServer():
    try:
        buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
        ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
        RespLen = TestConnectionToServerFunc(ResponsePtr)
        
        Response = ctypes.string_at(ResponsePtr, RespLen)
        return Response
    except:
        return 'AV on TestConnectionToServer'
        
        
GetServerAddressFunc = None
def GetGetServerAddress():
  GetServerAddressProto = ctypes.CFUNCTYPE(LARGE_INTEGER, ctypes.c_char_p)
  GetServerAddressParams = (1, "AResponse", 0),
  GetServerAddressFunc = GetServerAddressProto(("GetServerAddress", DllHandle), GetServerAddressParams)
  return GetServerAddressFunc

def GetServerAddress():
    try:
        buffer = ctypes.create_string_buffer(10 * 1048576) # #(CMaxSharedStringLength)
        ResponsePtr = buffer[0] #ctypes.c_char_p(buffer[0])  #address of first byte in the buffer
        RespLen = GetServerAddressFunc(ResponsePtr)
        
        Response = ctypes.string_at(ResponsePtr, RespLen)
        return Response
    except:
        return 'AV on GetServerAddress'
    

CreateNewTemplateFunc = None
def GetCreateNewTemplate():
    CreateNewTemplateProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR)
    CreateNewTemplateParams = (1, "ATemplateFileName", 0),
    CreateNewTemplateFunc = CreateNewTemplateProto(("CreateNewTemplate", DllHandle), CreateNewTemplateParams)
    return CreateNewTemplateFunc
    
def CreateNewTemplate(ATemplateFileName):
    try:
        CreateInMemFileResult = CreateNewTemplateFunc(ATemplateFileName)  #sending PWideChar, and converting to ANSI at dll
        return CreateInMemFileResult
    except:
        return 'AV on CreateNewTemplate'
        
        
AddClickActionToTemplateFunc = None
def GetAddClickActionToTemplate():
    AddClickActionToTemplateProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR, LPCWSTR, BYTE, LARGE_INTEGER, BOOLEAN, LPCWSTR)
    AddClickActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AAction", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0),
    AddClickActionToTemplateFunc = AddClickActionToTemplateProto(("AddClickActionToTemplate", DllHandle), AddClickActionToTemplateParams)
    return AddClickActionToTemplateFunc
    
def AddClickActionToTemplate(ATemplateFileName, AActionName, AAction, AActionTimeout, AActionEnabled, AActionCondition):
    try:
        AddClickActionToTemplateResult = AddClickActionToTemplateFunc(ATemplateFileName, AActionName, AAction, AActionTimeout, AActionEnabled, AActionCondition)  #sending PWideChar, and converting to ANSI at dll
        return AddClickActionToTemplateResult
    except:
        return 'AV on AddClickActionToTemplate'
        
        
AddExecAppActionToTemplateFunc = None
def GetAddExecAppActionToTemplate():
    AddExecAppActionToTemplateProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR, LPCWSTR, BYTE, LARGE_INTEGER, BOOLEAN, LPCWSTR)
    AddExecAppActionToTemplateParams = (1, "ATemplateFileName", 0), (1, "AActionName", 0), (1, "AAction", 0), (1, "AActionTimeout", 0), (1, "AActionEnabled", 0), (1, "AActionCondition", 0),
    AddExecAppActionToTemplateFunc = AddExecAppActionToTemplateProto(("AddExecAppActionToTemplate", DllHandle), AddExecAppActionToTemplateParams)
    return AddExecAppActionToTemplateFunc
    
def AddExecAppActionToTemplate(ATemplateFileName, AActionName, AAction, AActionTimeout, AActionEnabled, AActionCondition):
    try:
        AddExecAppActionToTemplateResult = AddExecAppActionToTemplateFunc(ATemplateFileName, AActionName, AAction, AActionTimeout, AActionEnabled, AActionCondition)  #sending PWideChar, and converting to ANSI at dll
        return AddExecAppActionToTemplateResult
    except:
        return 'AV on AddExecAppActionToTemplate'


PrepareFilesInServerFunc = None
def GetPrepareFilesInServer():
    PrepareFilesInServerProto = ctypes.CFUNCTYPE(LARGE_INTEGER, LPCWSTR)
    PrepareFilesInServerParams = (1, "ATemplateFileName", 0),
    PrepareFilesInServerFunc = PrepareFilesInServerProto(("PrepareFilesInServer", DllHandle), PrepareFilesInServerParams)
    return PrepareFilesInServerFunc
    
def PrepareFilesInServer(ATemplateFileName):
    try:
        PrepareFilesInServerResult = PrepareFilesInServerFunc(ATemplateFileName)  #sending PWideChar, and converting to ANSI at dll
        return PrepareFilesInServerResult
    except:
        return 'AV on PrepareFilesInServer'
        
        
TestConnectionToServerFunc = GetTestConnectionToServerAddress()
TestConnectionResult = TestConnectionToServer()  #add a call, to avoid being garbage collected, or what ever happens

GetServerAddressFunc = GetGetServerAddress()
GetServerAddressResult = GetServerAddress()  #add a call, to avoid being garbage collected, or what ever happens

SetServerAddressFunc = GetSetServerAddressAddress()
SetServerAddressResult = SetServerAddress('http://127.0.0.1:5444/')  #add a call, to avoid being garbage collected, or what ever happens

CreateNewTemplateFunc = GetCreateNewTemplate()
CreateNewTemplateResult = CreateNewTemplate('dummy.clktmpl')  #if called before InitClickerClient, it returns -1, because the internal in-mem file system, is not created yet

AddClickActionToTemplateFunc = GetAddClickActionToTemplate()
AddClickActionToTemplateResult = AddClickActionToTemplate('dummy.clktmpl', 'First', 0, 0, True, 'abc') #if called before InitClickerClient, it returns -1, because the internal in-mem file system, is not created yet

AddExecAppActionToTemplateFunc = GetAddExecAppActionToTemplate()
AddExecAppActionToTemplateResult = AddExecAppActionToTemplate('dummy.clktmpl', 'Second', 0, 0, True, 'abc') #if called before InitClickerClient, it returns -1, because the internal in-mem file system, is not created yet

PrepareFilesInServerFunc = GetPrepareFilesInServer()
PrepareFilesInServerResult = PrepareFilesInServer('dummy.clktmpl');


InitClickerClient()

print("TestConnectionToServer: ", TestConnectionToServer())

print("GetServerAddress: ", GetServerAddress())

print("SetServerAddress: ", SetServerAddress('http://192.168.3.102:5444/'))
print("GetServerAddress: ", GetServerAddress())
print("TestConnectionToServer after setting wrong address: ", TestConnectionToServer())

print("SetServerAddress: ", SetServerAddress('http://127.0.0.1:5444/'))
print("GetServerAddress: ", GetServerAddress())
print("TestConnectionToServer after setting a working address: ", TestConnectionToServer())

print("CreateNewTemplate: ", CreateNewTemplate('VerifyClicking.clktmpl')) #creates a new template in dll's in-mem file system
print("CreateNewTemplate: ", CreateNewTemplate('VerifyClicking.clktmpl')) #the second call returns 1, because the file already exists

print("AddClickActionToTemplate: ", AddClickActionToTemplate('VerifyClicking.clktmpl', 'First', 0, 0, True, '$a$==$b$'))

print("AddExecAppActionToTemplate: ", AddExecAppActionToTemplate('VerifyClicking.clktmpl', 'Second', 0, 0, True, '$a$==$b$'))

print("PrepareFilesInServer", PrepareFilesInServer('VerifyClicking.clktmpl'))


DoneClickerClient()
