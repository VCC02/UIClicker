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


unit ClickerActionsPaletteFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, VirtualTrees, ExtCtrls;

type

  { TfrClickerActionsPalette }

  TfrClickerActionsPalette = class(TFrame)
    tmrHide: TTimer;
    vstActionsPalette: TVirtualStringTree;
    procedure tmrHideTimer(Sender: TObject);
    procedure vstActionsPaletteGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstActionsPaletteGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
  private
    FImages: TImageList;
    FActions: TStringList;

    FOwnerPanel: TPanel;

    procedure SetImages(Value: TImageList);
    procedure HideOwnerPanel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetActionNames(AListOfActions: TStringList);
    procedure StartAutoHide;
    procedure ResetAutoHideTimer;

    property Images: TImageList read FImages write SetImages;
    property OwnerPanel: TPanel write FOwnerPanel;
  end;

implementation

{$R *.frm}

{ TfrClickerActionsPalette }

constructor TfrClickerActionsPalette.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActions := TStringList.Create;
  FImages := nil;
  FOwnerPanel := nil;
  vstActionsPalette.Caption := 'ActionsPalette';
end;


destructor TfrClickerActionsPalette.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;


procedure TfrClickerActionsPalette.vstActionsPaletteGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
begin
  try
    CellText := FActions.Strings[Node^.Index];
  except
    CellText := 'bug';
  end;
end;


procedure TfrClickerActionsPalette.vstActionsPaletteGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := Node^.Index;
end;


procedure TfrClickerActionsPalette.HideOwnerPanel;
begin
  if Assigned(FOwnerPanel) then
    FOwnerPanel.Hide;
end;


procedure TfrClickerActionsPalette.StartAutoHide;
begin
  tmrHide.Tag := 0;
  tmrHide.Enabled := True;
end;


procedure TfrClickerActionsPalette.ResetAutoHideTimer;
begin
  tmrHide.Enabled := False;
  tmrHide.Tag := 0;
end;


procedure TfrClickerActionsPalette.tmrHideTimer(Sender: TObject);
begin
  tmrHide.Tag := tmrHide.Tag + 1;

  if tmrHide.Tag >= 5 then
  begin
    ResetAutoHideTimer;
    HideOwnerPanel;
  end;
end;


procedure TfrClickerActionsPalette.SetActionNames(AListOfActions: TStringList);
begin
  vstActionsPalette.BeginUpdate;
  try
    FActions.Clear;
    FActions.AddStrings(AListOfActions);
    vstActionsPalette.RootNodeCount := FActions.Count;
  finally
    vstActionsPalette.EndUpdate;
  end;
end;


procedure TfrClickerActionsPalette.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    vstActionsPalette.StateImages := Value;
  end;
end;

end.

