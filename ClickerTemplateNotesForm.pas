{
    Copyright (C) 2022 VCC
    creation date: Dec 2019
    initial release date: 13 Sep 2022

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


unit ClickerTemplateNotesForm;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmClickerTemplateNotes }

  TfrmClickerTemplateNotes = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    memNotes: TMemo;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private

  public

  end;


function EditTemplateNotes(var ANotesContent: string): Boolean;

implementation

{$R *.frm}


function EditTemplateNotes(var ANotesContent: string): Boolean;
var
  frmClickerTemplateNotes: TfrmClickerTemplateNotes;
begin
  Result := False;
  Application.CreateForm(TfrmClickerTemplateNotes, frmClickerTemplateNotes);
  frmClickerTemplateNotes.memNotes.Lines.Text := ANotesContent;
  frmClickerTemplateNotes.ShowModal;

  if frmClickerTemplateNotes.Tag = 1 then
  begin
    ANotesContent := frmClickerTemplateNotes.memNotes.Lines.Text;
    Result := True;
  end;
end;

{ TfrmClickerTemplateNotes }

procedure TfrmClickerTemplateNotes.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TfrmClickerTemplateNotes.btnCancelClick(Sender: TObject);
begin
  Tag := 0;
  Close;
end;

end.

