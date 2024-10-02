{
    Copyright (C) 2024 VCC
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


procedure AddMenuItemToPopupMenu(APopupMenu: TPopupMenu; ACaption: TCaption; AHandler: TNotifyEvent;
  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec);

procedure BuildFontColorIcons(AImgLst: TImageList; var AFindControlOptions: TClkFindControlOptions; AEvaluateReplacementsFunc: TEvaluateReplacementsFunc);


implementation


procedure AddMenuItemToPopupMenu(APopupMenu: TPopupMenu; ACaption: TCaption; AHandler: TNotifyEvent;
  ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; AEditingAction: PClkActionRec);
var
  MenuData: POIMenuItemData;
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(nil);
  MenuItem.Caption := ACaption;
  MenuItem.OnClick := AHandler;

  New(MenuData);
  MenuItem.Tag := {%H-}PtrInt(MenuData);
  MenuData^.OwnerMenu := APopupMenu;
  MenuData^.NodeLevel := ANodeLevel;
  MenuData^.CategoryIndex := ACategoryIndex;
  MenuData^.PropertyIndex := APropertyIndex;
  MenuData^.PropertyItemIndex := AItemIndex;
  MenuData^.MenuItemCaption := ACaption;
  MenuData^.TempEditingAction := AEditingAction;

  APopupMenu.Items.Add(MenuItem);
end;


procedure BuildFontColorIcons(AImgLst: TImageList; var AFindControlOptions: TClkFindControlOptions; AEvaluateReplacementsFunc: TEvaluateReplacementsFunc);
var
  i: Integer;
  FontProfilesCount: Integer;
  FG, BG: TColor;
  Bmp: TBitmap;
begin
  FontProfilesCount := Length(AFindControlOptions.MatchBitmapText);

  AImgLst.Clear;
  for i := 0 to FontProfilesCount - 1 do
  begin
    FG := HexToInt(AEvaluateReplacementsFunc(AFindControlOptions.MatchBitmapText[i].ForegroundColor));
    BG := HexToInt(AEvaluateReplacementsFunc(AFindControlOptions.MatchBitmapText[i].BackgroundColor));

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

