{
    Copyright (C) 2025 VCC
    creation date: 23 Jul 2025
    initial release date: 23 Jul 2025

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit ClickerActionsCodeGen;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ClickerUtils;


function GenerateHTTPRequestFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean; AListeningPort: Word): string;
function GenerateClickerClientPascalRequestFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean): string;


implementation

uses
  ClickerActionProperties, ClickerActionsClient;


function GenerateHTTPRequestFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean; AListeningPort: Word): string;
var
  ActionType: TClkAction;
  Request, Properties: string;
begin
  ActionType := AAction.ActionOptions.Action;

  Request := 'http://127.0.0.1:' + IntToStr(AListeningPort) + '/' +
             'Execute' + CClkActionStr[ActionType] + 'Action?' +
             CREParam_StackLevel + '=0';


  if AWithAllProperties then
  begin   //all properties
    Request := Request + '&' + GetActionPropertiesByType(AAction, True);
    //
  end
  else
  begin   //different than default properties
    Properties := GetDifferentThanDefaultActionPropertiesByType(AAction, True);
    if Properties > '' then
      Request := Request + '&' + Properties;
  end;

  if ActionType in [acFindControl, acFindSubControl, acCallTemplate] then
    Request := Request + '&' + CREParam_FileLocation + '=' + CREParam_FileLocation_ValueDisk;

  if AWithDebugging then    //debugging
  begin
    if ActionType in [acPlugin, acCallTemplate] then
      Request := Request + '&' + CREParam_IsDebugging + '=1';  //required by plugin to be able to step into

    if ActionType = acCallTemplate then
      Request := Request + '&' + CREParam_UseLocalDebugger + '=1';  //debugging at server side, instead of being controlled by a client

    Request := Request + '&' + CREParam_UseServerDebugging + '=1';
  end;

  Request := Request + '&' + CPropertyName_ActionName + '=' + AAction.ActionOptions.ActionName; //implemented for some of the actions only
  Request := Request + '&' + CPropertyName_ActionTimeout + '=' + IntToStr(AAction.ActionOptions.ActionTimeout);  //required by a few actions only

  Result := Request;
end;


function FixFuncNameToValid(AFuncName: string): string;
var
  i: Integer;
begin
  for i := 1 to Length(AFuncName) do
    if not (AFuncName[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_']) then
      AFuncName[i] := '_';

  if AFuncName > '' then
    if not (AFuncName[1] in ['a'..'z', 'A'..'Z', '_']) then  //must start with a letter or '_'
      AFuncName := 'Func_' + AFuncName;

  Result := AFuncName;
end;


function GenerateClickerClientPascalRequestFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean): string;
var
  ActionType: TClkAction;
  Request, Properties, PropertyDataTypes: string;
  ListOfProperties, ListOfPropertyDataTypes: TStringList;
  i: Integer;
  IsDbg, IsFileLoc, IsStepIntoDbg: string;
  PropertyDataType: string;
  FuncName, TempActionCondition: string;
  ActionTypeStr: string;
begin
  ActionType := AAction.ActionOptions.Action;
  ActionTypeStr := CClkActionStr[ActionType];

  FuncName := FixFuncNameToValid(AAction.ActionOptions.ActionName);
  Request := '';
  Request := Request + 'function ' + FuncName + ': string; // ' + AAction.ActionOptions.ActionName + #13#10;
  Request := Request + 'var' + #13#10;
  Request := Request + '  ' + ActionTypeStr + 'API: TClk' + ActionTypeStr + 'OptionsAPI;' + #13#10;
  Request := Request + '  ' + ActionTypeStr + ': TClk' + ActionTypeStr + 'Options;' + #13#10;

  case ActionType of
    acFindControl:
    begin
      Request := Request + '  DummyDestMatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;' + #13#10;
      Request := Request + '  DummyDestMatchBitmapTextArray: TClkFindControlMatchBitmapTextAPIArr;' + #13#10;
    end;

    acFindSubControl:
    begin
      Request := Request + '  DestMatchBitmapTextRecAPI: TMatchBitmapTextRecAPI;' + #13#10;
      Request := Request + '  DestMatchBitmapTextArray: TClkFindControlMatchBitmapTextAPIArr;' + #13#10;
    end;

    else
      ; //no other special vars
  end;

  Request := StringReplace(Request, #5#6, ' ', [rfReplaceAll]);
  Request := Request + 'begin' + #13#10;
  Request := Request + '  GetDefaultPropertyValues_' + ActionTypeStr + '(' + ActionTypeStr + ');' + #13#10;

  ListOfProperties := TStringList.Create;
  ListOfPropertyDataTypes := TStringList.Create;
  try
    ListOfProperties.LineBreak := #13#10;
    ListOfPropertyDataTypes.LineBreak := #13#10;

    if AWithAllProperties then
    begin   //all properties
      Properties := GetActionPropertiesByType(AAction, True);
      PropertyDataTypes := GetActionPropertyDataTypesByType(AAction, True);
    end
    else
    begin   //different than default properties
      Properties := GetDifferentThanDefaultActionPropertiesByType(AAction, True);
      PropertyDataTypes := GetDifferentThanDefaultActionPropertyDataTypesByType(AAction, True);
    end;

    if ActionType in [acFindControl, acFindSubControl, acCallTemplate] then
      IsFileLoc := ', @WideString(''' + CREParam_FileLocation_ValueDisk + ''')[1]'
    else
      IsFileLoc := '';

    IsDbg := BoolToStr(AWithDebugging, ', True', ', False');  //debugging action

    if ActionType = acPlugin then
      IsStepIntoDbg := ', False';

    ListOfProperties.Text := StringReplace(Properties, '&', #13#10, [rfReplaceAll]);
    ListOfPropertyDataTypes.Text := StringReplace(PropertyDataTypes, '&', #13#10, [rfReplaceAll]);
    for i := 0 to ListOfProperties.Count - 1 do
    begin
      if (ActionType = acFindSubControl) and (ListOfProperties.Names[i] = 'MatchBitmapText.Count') then
      begin
        Request := Request + '  SetLength(' + ActionTypeStr + '.MatchBitmapText, ' + ListOfProperties.ValueFromIndex[i] + ');' + #13#10;
        Continue;
      end;

      try
        PropertyDataType := ListOfPropertyDataTypes.ValueFromIndex[i];
      except
        PropertyDataType := '';
      end;

      if PropertyDataType = CDTString then
        Request := Request + '  ' + ActionTypeStr + '.' + ListOfProperties.Names[i] + ' := ''' + ListOfProperties.ValueFromIndex[i] + ''';' + #13#10
      else
        if PropertyDataType = CDTInteger then
          Request := Request + '  ' + ActionTypeStr + '.' + ListOfProperties.Names[i] + ' := ' + ListOfProperties.ValueFromIndex[i] + ';' + #13#10
        else
          if Pos(CDTEnum, PropertyDataType + '.') = 1 then     // CDTEnum + '.<EnumDataType>'
            Request := Request + '  ' + ActionTypeStr + '.' + ListOfProperties.Names[i] + ' := ' + Copy(PropertyDataType, Length(CDTEnum) + 2, MaxInt) + '(' + ListOfProperties.ValueFromIndex[i] + ');' + #13#10
          else
            if PropertyDataType = CDTBool then
              Request := Request + '  ' + ActionTypeStr + '.' + ListOfProperties.Names[i] + ' := ' + BoolToStr(ListOfProperties.ValueFromIndex[i] = '1', 'True', 'False') + ';' + #13#10
            else  //structure or array
              Request := Request + '  ' + ActionTypeStr + '.' + ListOfProperties.Names[i] + ' := ' + ListOfProperties.ValueFromIndex[i] + ';' + #13#10; //same as int
    end;

    Request := Request + #13#10;
    case ActionType of
      acFindControl:
        Request := Request + '  Set' + ActionTypeStr + 'OptionsToAPI(' + ActionTypeStr + ', ' + ActionTypeStr + 'API, DummyDestMatchBitmapTextRecAPI, DummyDestMatchBitmapTextArray);' + #13#10;

      acFindSubControl:
        Request := Request + '  Set' + ActionTypeStr + 'OptionsToAPI(' + ActionTypeStr + ', ' + ActionTypeStr + 'API, DestMatchBitmapTextRecAPI, DestMatchBitmapTextArray);' + #13#10;

      else
        Request := Request + '  Set' + ActionTypeStr + 'OptionsToAPI(' + ActionTypeStr + ', ' + ActionTypeStr + 'API);' + #13#10;
    end;

    Request := Request + #13#10;

    Request := Request + '  SetLength(Result, CMaxSharedStringLength);' + #13#10;
    Request := Request + '  SetLength(Result, Execute' + CClkActionStr[ActionType] + 'Action(@WideString(''' + AAction.ActionOptions.ActionName + ''')[1], ' + IntToStr(AAction.ActionOptions.ActionTimeout) + ', @' + CClkActionStr[ActionType] + 'API' + IsDbg + IsFileLoc + IsStepIntoDbg + ', @Result[1])); ' + #13#10;

    Request := Request + 'end;';
    Request := Request + #13#10#13#10;

    //The following StringReplace call should be replaced with a better option, which is able to convert a complex condition to a valid string.
    TempActionCondition := AAction.ActionOptions.ActionCondition;
    if Length(TempActionCondition) > 1 then
      if (TempActionCondition[Length(TempActionCondition) - 1] = #13) and (TempActionCondition[Length(TempActionCondition)] = #10) then
        Delete(TempActionCondition, Length(TempActionCondition) - 1, 2);

    if TempActionCondition <> '' then
    begin
      //ToDo: use #13#10 to separate into "or" rows, and #5#6, to separate into "and" columns, using TStringList.
      TempActionCondition := '(' + StringReplace(TempActionCondition, #5#6, ') and (', [rfReplaceAll]) + ')';
      TempActionCondition := 'if (' + StringReplace(StringReplace(TempActionCondition, '==', ' = ', [rfReplaceAll]), #13#10, ') or (', [rfReplaceAll]) + ') then';
      TempActionCondition := StringReplace(TempActionCondition, ' and ()', '', [rfReplaceAll]);
      Request := Request + TempActionCondition + #13#10;
    end;

    Request := Request + '  Response := ' + FuncName + ';' + #13#10;
    Request := Request + '  //Result := GetErrorMessageFromResponse(Response);' + #13#10;
    Request := Request + #13#10;
  finally
    ListOfProperties.Free;
    ListOfPropertyDataTypes.Free;
  end;

  Result := Request;
end;


end.

