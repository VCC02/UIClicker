unit GradientTextMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TfrmGradientTextMain }

  TfrmGradientTextMain = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmGradientTextMain: TfrmGradientTextMain;

implementation

{$R *.frm}

{ TfrmGradientTextMain }

procedure TfrmGradientTextMain.FormCreate(Sender: TObject);
begin
  Image1.Canvas.Font.Color := clBlack;
  Image1.Canvas.Font.Name := 'Tahoma';
  Image1.Canvas.Font.Size := 36;
  Image1.Canvas.Font.Quality := fqNonAntialiased;
  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.TextOut(10, 150, 'This is the searched text.');
end;

end.

