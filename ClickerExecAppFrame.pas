{
    Copyright (C) 2022 VCC
    creation date: Feb 2023
    initial release date: 07 Feb 2023

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


unit ClickerExecAppFrame;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ClickerUtils;

type

  { TfrClickerExecApp }

  TfrClickerExecApp = class(TFrame)
    lblExecAppParams: TLabel;
    memExecAppParams: TMemo;
    procedure memExecAppParamsChange(Sender: TObject);
  private
    FOnTriggerOnControlsModified: TOnTriggerOnControlsModified;

    procedure DoOnTriggerOnControlsModified;
  public
    constructor Create(AOwner: TComponent); override;
    property OnTriggerOnControlsModified: TOnTriggerOnControlsModified write FOnTriggerOnControlsModified;
  end;

implementation

{$R *.frm}

{ TfrClickerExecApp }


constructor TfrClickerExecApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnTriggerOnControlsModified := nil;
end;


procedure TfrClickerExecApp.DoOnTriggerOnControlsModified;
begin
  if not Assigned(FOnTriggerOnControlsModified) then
    raise Exception.Create('OnTriggerOnControlsModified not assigned.')
  else
    FOnTriggerOnControlsModified;
end;


procedure TfrClickerExecApp.memExecAppParamsChange(Sender: TObject);
begin
  DoOnTriggerOnControlsModified;
end;

end.

