{
    Copyright (C) 2025 VCC
    creation date: Mar 2023
    initial release date: 10 Mar 2023

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


unit ClickerOIUtils;

{$mode Delphi}

interface


uses
  Classes, SysUtils, Menus, Controls, ClickerUtils, Graphics;


type
  TOIMenuItemData = record
    OwnerMenu: TPopupMenu;
    NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
    MenuItemCaption: string;
    TempEditingAction: PClkActionRec;
  end;
  POIMenuItemData = ^TOIMenuItemData;


function AddMenuItemToAnotherMenuItem(APopupMenu: TPopupMenu; AParentMenuItem: TMenuItem; ACaption: TCaption; AHandler: TNotifyEvent;
  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec): TMenuItem;

function AddMenuItemToPopupMenu(APopupMenu: TPopupMenu; ACaption: TCaption; AHandler: TNotifyEvent;
  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec): TMenuItem;

procedure BuildFontColorIcons(AImgLst: TImageList; var AFindSubControlOptions: TClkFindSubControlOptions; AEvaluateReplacementsFunc: TEvaluateReplacementsFunc);


implementation


function AddMenuItemToAnotherMenuItem(APopupMenu: TPopupMenu; AParentMenuItem: TMenuItem; ACaption: TCaption; AHandler: TNotifyEvent;
  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec): TMenuItem;
var
  MenuData: POIMenuItemData;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := ACaption;
  Result.OnClick := AHandler;

  New(MenuData);
  Result.Tag := {%H-}PtrInt(MenuData);
  MenuData^.OwnerMenu := APopupMenu;
  MenuData^.NodeLevel := ANodeLevel;
  MenuData^.CategoryIndex := ACategoryIndex;
  MenuData^.PropertyIndex := APropertyIndex;
  MenuData^.PropertyItemIndex := AItemIndex;
  MenuData^.MenuItemCaption := ACaption;
  MenuData^.TempEditingAction := AEditingAction;

  if AParentMenuItem <> nil then
    AParentMenuItem.Add(Result);
end;


function AddMenuItemToPopupMenu(APopupMenu: TPopupMenu; ACaption: TCaption; AHandler: TNotifyEvent;
  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec): TMenuItem;
begin
  Result := AddMenuItemToAnotherMenuItem(APopupMenu, nil, ACaption, AHandler, ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEditingAction);
  APopupMenu.Items.Add(Result);
end;


procedure BuildFontColorIcons(AImgLst: TImageList; var AFindSubControlOptions: TClkFindSubControlOptions; AEvaluateReplacementsFunc: TEvaluateReplacementsFunc);
var
  i: Integer;
  FontProfilesCount: Integer;
  FG, BG: TColor;
  Bmp: TBitmap;
begin
  FontProfilesCount := Length(AFindSubControlOptions.MatchBitmapText);

  AImgLst.Clear;
  for i := 0 to FontProfilesCount - 1 do
  begin
    FG := HexToInt(AEvaluateReplacementsFunc(AFindSubControlOptions.MatchBitmapText[i].ForegroundColor));
    BG := HexToInt(AEvaluateReplacementsFunc(AFindSubControlOptions.MatchBitmapText[i].BackgroundColor));

    Bmp := TBitmap.Create;
    try
      Bmp.Width := 16;
      Bmp.Height := 16;
      Bmp.Canvas.Pen.Color := 1;

      Bmp.Canvas.Brush.Color := FG;
      Bmp.Canvas.Rectangle(0, 0, Bmp.Width, Bmp.Height);
      AImgLst.AddMasked(Bmp, 2);

      Bmp.Canvas.Brush.Color := BG;
      Bmp.Canvas.Rectangle(0, 0, Bmp.Width, Bmp.Height);
      AImgLst.AddMasked(Bmp, 2);
    finally
      Bmp.Free;
    end;
  end;
end;

end.

