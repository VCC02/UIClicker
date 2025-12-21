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

function GenerateClickerClientPascalRequestFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean; ATemplateName: string): string;
function GenerateClickerClientPascalRequestCallFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean): string;
function GenerateGetVarValueFromResponsePascalFunc: string;

function GenerateClickerClientPythonRequestFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean; ATemplateName: string): string;
function GenerateClickerClientPythonRequestCallFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean; AIncludeDllFuncsLoading: Boolean = False): string;
function GenerateGetVarValueFromResponsePythonFunc: string;

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


function IsStringCmpOperator(AOp: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to 5 do  //the first 6 operators are string comparison operators
    if CComparisonOperators[i] = AOp then
    begin
      Result := True;
      Break;
    end;
end;


function IsIntCmpOperator(AOp: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 6 to 11 do  //the second chunk of 6 operators are integer comparison operators
    if CComparisonOperators[i] = AOp then
    begin
      Result := True;
      Break;
    end;
end;


function IsExtCmpOperator(AOp: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 12 to 17 do  //the thirds chunk of 6 operators are integer comparison operators
    if CComparisonOperators[i] = AOp then
    begin
      Result := True;
      Break;
    end;
end;


function IsVar(AOp: string): Boolean;
begin
  Result := (AOp > '') and (AOp[1] = '$') and (AOp[Length(AOp)] = '$');
end;


function UIClickerOperatorToPascal(AOp: string): string;
begin
  Result := '??';

  if (AOp = CCompNotEqual) or (AOp = CIntCompNotEqual) or (AOp = CExtCompNotEqual) then
  begin
    Result := '<>';
    Exit;
  end;

  if (AOp = CCompEqual) or (AOp = CIntCompEqual) or (AOp = CExtCompEqual) then
  begin
    Result := '=';
    Exit;
  end;

  if (AOp = CCompLessThan) or (AOp = CIntCompLessThan) or (AOp = CExtCompLessThan) then
  begin
    Result := '<';
    Exit;
  end;

  if (AOp = CCompGreaterThan) or (AOp = CIntCompGreaterThan) or (AOp = CExtCompGreaterThan) then
  begin
    Result := '>';
    Exit;
  end;

  if (AOp = CCompLessThanOrEqual) or (AOp = CIntCompLessThanOrEqual) or (AOp = CExtCompLessThanOrEqual) then
  begin
    Result := '<=';
    Exit;
  end;

  if (AOp = CCompGreaterThanOrEqual) or (AOp = CIntCompGreaterThanOrEqual) or (AOp = CExtCompGreaterThanOrEqual) then
  begin
    Result := '>=';
    Exit;
  end;
end;


function ExpressionToPascal(AActionConditionExpression: string): string;
var
  Op1, Op2, OpEq: string;
begin
  RawExpressionToParts(AActionConditionExpression, Op1, Op2, OpEq);

  if IsStringCmpOperator(OpEq) then
  begin
    if IsVar(Op1) then
      Op1 := 'GetVarValueFromResponse(Response, ''' + Op1 + ''')'
    else
      Op1 := '''' + Op1 + '''';

    if IsVar(Op2) then
      Op2 := 'GetVarValueFromResponse(Response, ''' + Op2 + ''')'
    else
      Op2 := '''' + Op2 + '''';

    Result := Op1 + ' ' + UIClickerOperatorToPascal(OpEq) + ' ' + Op2;
  end
  else
  begin
    if IsVar(Op1) then
    begin
      if IsIntCmpOperator(OpEq) then
        Op1 := 'StrToIntDef(GetVarValueFromResponse(Response, ''' + Op1 + '''), -1)'
      else
        if IsExtCmpOperator(OpEq) then
          Op1 := 'StrToFloatDef(GetVarValueFromResponse(Response, ''' + Op1 + '''), -1)'
        else
          Op1 := 'StrToUnknownDef(GetVarValueFromResponse(Response, ''' + Op1 + '''), -1)'
    end;

    if IsVar(Op2) then
    begin
      if IsIntCmpOperator(OpEq) then
        Op2 := 'StrToIntDef(GetVarValueFromResponse(Response, ''' + Op2 + '''), -1)'
      else
        if IsExtCmpOperator(OpEq) then
          Op2 := 'StrToFloatDef(GetVarValueFromResponse(Response, ''' + Op2 + '''), -1)'
        else
          Op2 := 'StrToUnknownDef(GetVarValueFromResponse(Response, ''' + Op2 + '''), -1)'
    end;

    Result := Op1 + ' ' + UIClickerOperatorToPascal(OpEq) + ' ' + Op2;
  end;
end;


function ActionConditionToPascal(AActionCondition: string): string;
var
  Rows, Columns: TStringList;
  i, j: Integer;
  TempRow, TempRowPas, TempCol: string;
begin
  Result := 'if ';
  Rows := TStringList.Create;
  try
    Rows.LineBreak := #13#10;
    Rows.Text := AActionCondition;

    for i := 0 to Rows.Count - 1 do
    begin
      TempRow := Rows.Strings[i];
      TempRowPas := '';
      Columns := TStringList.Create;
      try
        Columns.LineBreak := #5#6;
        Columns.Text := TempRow;

        for j := 0 to Columns.Count - 1 do
        begin
          TempCol := ExpressionToPascal(Columns.Strings[j]);
          TempRowPas := TempRowPas + '(' + TempCol + ')';
          if j < Columns.Count - 1 then
            TempRowPas := TempRowPas + ' and ';
        end;
      finally
        Columns.Free;
      end;

      Result := Result + '(' + TempRowPas + ')';
      if i < Rows.Count - 1 then
        Result := Result + ' or ';
    end;

    Result := Result + ' then'#13#10;
  finally
    Rows.Free;
  end;
end;


function GenerateClickerClientPascalRequestFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean; ATemplateName: string): string;
var
  ActionType: TClkAction;
  Request, Properties, PropertyDataTypes: string;
  ListOfProperties, ListOfPropertyDataTypes: TStringList;
  i: Integer;
  IsDbg, IsFileLoc, IsStepIntoDbg: string;
  PropertyDataType: string;
  FuncName: string;
  ActionTypeStr: string;
begin
  ActionType := AAction.ActionOptions.Action;
  ActionTypeStr := CClkActionStr[ActionType];

  if ATemplateName > '' then
    ATemplateName := ' from ' + ExtractFileName(ATemplateName);

  FuncName := FixFuncNameToValid(AAction.ActionOptions.ActionName);
  Request := '';
  Request := Request + 'function ' + FuncName + ': string; // ' + AAction.ActionOptions.ActionName + ATemplateName + #13#10;
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


  finally
    ListOfProperties.Free;
    ListOfPropertyDataTypes.Free;
  end;

  Result := Request;
end;


function GenerateClickerClientPascalRequestCallFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean): string;
var
  FuncName, TempActionCondition: string;
begin
  Result := '';
  FuncName := FixFuncNameToValid(AAction.ActionOptions.ActionName);

  TempActionCondition := AAction.ActionOptions.ActionCondition;
  if Length(TempActionCondition) > 1 then
    if (TempActionCondition[Length(TempActionCondition) - 1] = #13) and (TempActionCondition[Length(TempActionCondition)] = #10) then
      Delete(TempActionCondition, Length(TempActionCondition) - 1, 2);

  if TempActionCondition <> '' then
    Result := Result + ActionConditionToPascal(TempActionCondition);

  Result := Result + '  Response := ' + FuncName + ';' + #13#10;
  Result := Result + '  //Result := GetVarValueFromResponse(Response, ''$LastAction_Status$'');' + #13#10;
  //Result := Result + #13#10;
end;


function GenerateGetVarValueFromResponsePascalFunc: string;
begin
  Result :=
    '  function GetVarValueFromResponse(AResponse, AVarName: string): string;' + #13#10 +
    '  var' + #13#10 +
    '    ListOfStrings: TStringList;' + #13#10 +
    '  begin' + #13#10 +
    '    ListOfStrings := TStringList.Create;' + #13#10 +
    '    try' + #13#10 +
    '      ListOfStrings.Text := FastReplace_87ToReturn(AResponse);' + #13#10 +
    '      Result := ListOfStrings.Values[AVarName];' + #13#10 +
    '    finally' + #13#10 +
    '      ListOfStrings.Free;' + #13#10 +
    '    end;' + #13#10 +
    '  end;' + #13#10;
end;


function UIClickerOperatorToPython(AOp: string): string;
begin
  Result := '??';

  if (AOp = CCompNotEqual) or (AOp = CIntCompNotEqual) or (AOp = CExtCompNotEqual) then
  begin
    Result := '!=';
    Exit;
  end;

  if (AOp = CCompEqual) or (AOp = CIntCompEqual) or (AOp = CExtCompEqual) then
  begin
    Result := '==';
    Exit;
  end;

  if (AOp = CCompLessThan) or (AOp = CIntCompLessThan) or (AOp = CExtCompLessThan) then
  begin
    Result := '<';
    Exit;
  end;

  if (AOp = CCompGreaterThan) or (AOp = CIntCompGreaterThan) or (AOp = CExtCompGreaterThan) then
  begin
    Result := '>';
    Exit;
  end;

  if (AOp = CCompLessThanOrEqual) or (AOp = CIntCompLessThanOrEqual) or (AOp = CExtCompLessThanOrEqual) then
  begin
    Result := '<=';
    Exit;
  end;

  if (AOp = CCompGreaterThanOrEqual) or (AOp = CIntCompGreaterThanOrEqual) or (AOp = CExtCompGreaterThanOrEqual) then
  begin
    Result := '>=';
    Exit;
  end;
end;


function ExpressionToPython(AActionConditionExpression: string): string;
var
  Op1, Op2, OpEq: string;
begin
  RawExpressionToParts(AActionConditionExpression, Op1, Op2, OpEq);

  if IsStringCmpOperator(OpEq) then
  begin
    if IsVar(Op1) then
      Op1 := 'GetVarValueFromResponse(Response, ''' + Op1 + ''')'
    else
      Op1 := '''' + Op1 + '''';

    if IsVar(Op2) then
      Op2 := 'GetVarValueFromResponse(Response, ''' + Op2 + ''')'
    else
      Op2 := '''' + Op2 + '''';

    Result := Op1 + ' ' + UIClickerOperatorToPython(OpEq) + ' ' + Op2;
  end
  else
  begin
    if IsVar(Op1) then
    begin
      if IsIntCmpOperator(OpEq) then
        Op1 := 'int(GetVarValueFromResponse(Response, ''' + Op1 + '''))'
      else
        if IsExtCmpOperator(OpEq) then
          Op1 := 'float(GetVarValueFromResponse(Response, ''' + Op1 + '''))'
        else
          Op1 := 'unknown(GetVarValueFromResponse(Response, ''' + Op1 + '''))'
    end;

    if IsVar(Op2) then
    begin
      if IsIntCmpOperator(OpEq) then
        Op2 := 'int(GetVarValueFromResponse(Response, ''' + Op2 + '''))'
      else
        if IsExtCmpOperator(OpEq) then
          Op2 := 'float(GetVarValueFromResponse(Response, ''' + Op2 + '''))'
        else
          Op2 := 'unknown(GetVarValueFromResponse(Response, ''' + Op2 + '''))'
    end;

    Result := Op1 + ' ' + UIClickerOperatorToPython(OpEq) + ' ' + Op2;
  end;
end;


function ActionConditionToPython(AActionCondition: string): string;
var
  Rows, Columns: TStringList;
  i, j: Integer;
  TempRow, TempRowPas, TempCol: string;
begin
  Result := 'if ';
  Rows := TStringList.Create;
  try
    Rows.LineBreak := #13#10;
    Rows.Text := AActionCondition;

    for i := 0 to Rows.Count - 1 do
    begin
      TempRow := Rows.Strings[i];
      TempRowPas := '';
      Columns := TStringList.Create;
      try
        Columns.LineBreak := #5#6;
        Columns.Text := TempRow;

        for j := 0 to Columns.Count - 1 do
        begin
          TempCol := ExpressionToPython(Columns.Strings[j]);
          TempRowPas := TempRowPas + '(' + TempCol + ')';
          if j < Columns.Count - 1 then
            TempRowPas := TempRowPas + ' and ';
        end;
      finally
        Columns.Free;
      end;

      Result := Result + '(' + TempRowPas + ')';
      if i < Rows.Count - 1 then
        Result := Result + ' or ';
    end;

    Result := Result + ':'#13#10;
  finally
    Rows.Free;
  end;
end;


function GenerateClickerClientPythonRequestFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean; ATemplateName: string): string;
var
  ActionType: TClkAction;
  Request, Properties, PropertyDataTypes: string;
  ListOfProperties, ListOfPropertyDataTypes: TStringList;
  i: Integer;
  IsDbg, IsFileLoc, IsStepIntoDbg: string;
  PropertyDataType: string;
  FuncName: string;
  ActionTypeStr, ActionNameStr: string;
begin
  ActionType := AAction.ActionOptions.Action;
  ActionTypeStr := CClkActionStr[ActionType];

  if ATemplateName > '' then
    ATemplateName := ' from ' + ExtractFileName(ATemplateName);

  FuncName := FixFuncNameToValid(AAction.ActionOptions.ActionName);
  Request := '';
  Request := Request + 'def ' + FuncName + '(ADllFuncs): # ' + AAction.ActionOptions.ActionName + ATemplateName + #13#10;
  Request := Request + '    #Python code in work...' + #13#10;

  Request := StringReplace(Request, #5#6, ' ', [rfReplaceAll]);
  Request := Request + '    ' + ActionTypeStr + ' = GetDefault' + ActionTypeStr + 'Options()' + #13#10;

  ListOfProperties := TStringList.Create;
  ListOfPropertyDataTypes := TStringList.Create;
  try
    ListOfProperties.LineBreak := #13#10;
    ListOfPropertyDataTypes.LineBreak := #13#10;

    case ActionType of
      //acFindControl:
      //  Request := Request + '    DummyDestMatchBitmapTextArray = TMatchBitmapTextRec()' + #13#10;

      acFindSubControl:
        Request := Request + '    ' + ActionTypeStr + '.MatchBitmapText = ()  # The content is updated separately. See TClkFindControlMatchBitmapText' + #13#10;

      else
        ; //no other special vars
    end;

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
      IsFileLoc := ', "' + CREParam_FileLocation_ValueDisk + '"'
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
        Request := Request + '    DestMatchBitmapTextArray = TMatchBitmapTextRec(' + ListOfProperties.ValueFromIndex[i] + ')' + #13#10;
        Continue;
      end;

      ActionNameStr := ActionTypeStr;
      if (ActionType = acFindSubControl) and (Pos('MatchBitmapText[', ListOfProperties.Names[i]) = 1) then
      begin
        ListOfProperties.Strings[i] := StringReplace(ListOfProperties.Names[i], 'MatchBitmapText[', 'Items[', [rfReplaceAll]) + '=' + ListOfProperties.ValueFromIndex[i];
        ActionNameStr := 'DestMatchBitmapTextArray';
      end;

      try
        PropertyDataType := ListOfPropertyDataTypes.ValueFromIndex[i];
      except
        PropertyDataType := '';
      end;

      if PropertyDataType = CDTString then
        Request := Request + '    ' + ActionNameStr + '.' + ListOfProperties.Names[i] + ' = ''' + ListOfProperties.ValueFromIndex[i] + '''' + #13#10
      else
        if PropertyDataType = CDTInteger then
          Request := Request + '    ' + ActionNameStr + '.' + ListOfProperties.Names[i] + ' = ' + ListOfProperties.ValueFromIndex[i]  + #13#10
        else
          if Pos(CDTEnum, PropertyDataType + '.') = 1 then     // CDTEnum + '.<EnumDataType>'
            Request := Request + '    ' + ActionNameStr + '.' + ListOfProperties.Names[i] + ' = ' + ListOfProperties.ValueFromIndex[i] + ' # ' + PropertyDataType + #13#10
          else
            if PropertyDataType = CDTBool then
              Request := Request + '    ' + ActionNameStr + '.' + ListOfProperties.Names[i] + ' = ' + BoolToStr(ListOfProperties.ValueFromIndex[i] = '1', 'True', 'False') + #13#10
            else  //structure or array
              Request := Request + '    ' + ActionNameStr + '.' + ListOfProperties.Names[i] + ' = ' + ListOfProperties.ValueFromIndex[i] + #13#10; //same as int
    end;

    Request := Request + #13#10;

    if ActionType = acFindSubControl then
      Request := Request + '    ' + ActionTypeStr + '.MatchBitmapText = PMatchBitmapTextRec(DestMatchBitmapTextArray)' + #13#10;

    Request := Request + '    return ADllFuncs.Execute' + CClkActionStr[ActionType] + 'Action(''' + AAction.ActionOptions.ActionName + ''', ' + IntToStr(AAction.ActionOptions.ActionTimeout) + ', ' + CClkActionStr[ActionType] + IsDbg + IsFileLoc + IsStepIntoDbg + ')' + #13#10;
    Request := Request + #13#10;
  finally
    ListOfProperties.Free;
    ListOfPropertyDataTypes.Free;
  end;

  Result := Request;
end;


function GenerateClickerClientPythonRequestCallFromAction(var AAction: TClkActionRec; AWithAllProperties, AWithDebugging: Boolean; AIncludeDllFuncsLoading: Boolean = False): string;
var
  FuncName, TempActionCondition, TextOffset: string;
begin
  Result := '';
  FuncName := FixFuncNameToValid(AAction.ActionOptions.ActionName);

  TempActionCondition := AAction.ActionOptions.ActionCondition;
  if Length(TempActionCondition) > 1 then
    if (TempActionCondition[Length(TempActionCondition) - 1] = #13) and (TempActionCondition[Length(TempActionCondition)] = #10) then
      Delete(TempActionCondition, Length(TempActionCondition) - 1, 2);

  TextOffset := '';
  if TempActionCondition <> '' then
  begin
    Result := Result + ActionConditionToPython(TempActionCondition);
    TextOffset := '    ';
  end;

  if AIncludeDllFuncsLoading then
  begin
    Result := Result + TextOffset + '###DllFuncs = TUIClickerDllFunctions()' + #13#10;
    Result := Result + TextOffset + '#DllFuncs = TDllFunctions() #this type can be used by GetVarValueFromResponse function' + #13#10;
  end;

  Result := Result + TextOffset + 'Response = ' + FuncName + '(DllFuncs)' + #13#10;
  Result := Result + TextOffset + '#Result = GetVarValueFromResponse(Response, ''$LastAction_Status$'')' + #13#10;
  //Result := Result + #13#10;
end;


function GenerateGetVarValueFromResponsePythonFunc: string;
begin
  Result :=
    '    def GetVarValueFromResponse(AResponse, AVarName):' + #13#10 +
    '        AResponseStr = AResponse.replace("", "\r\n")' + #13#10 +
    '        ListOfVars = AResponseStr.splitlines()' + #13#10 +
    '        for Item in ListOfVars:' + #13#10 +
    '            PosEq = Item.index("=")' + #13#10 +
    '            VarValue = ''''' + #13#10 +
    '            if PosEq > -1:' + #13#10 +
    '                CurrentVarName = Item[0 : PosEq]' + #13#10 +
    '                if CurrentVarName == AVarName:' + #13#10 +
    '                    VarValue = Item[PosEq + 1: len(Item)]' + #13#10 +
    '                    return VarValue' + #13#10 +
    '        return ''0'' # a valid int as string' + #13#10;
end;

end.

