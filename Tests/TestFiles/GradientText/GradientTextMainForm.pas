unit GradientTextMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls;

type

  { TfrmGradientTextMain }

  TfrmGradientTextMain = class(TForm)
    btnResetTrackBar: TButton;
    edtButtonClickDuration: TEdit;
    edtTrackBarDragDuration: TEdit;
    grpButtonClickDuration: TGroupBox;
    grpTrackBarDragDuration: TGroupBox;
    grpClickTesting: TGroupBox;
    Image1: TImage;
    trbTestTrackBar: TTrackBar;
    procedure btnResetTrackBarClick(Sender: TObject);
    procedure btnResetTrackBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnResetTrackBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure trbTestTrackBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure trbTestTrackBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public

  end;

var
  frmGradientTextMain: TfrmGradientTextMain;

implementation

{$R *.frm}

var
  ButtonTk: QWord;
  TrackBarTk: QWord;

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


procedure TfrmGradientTextMain.trbTestTrackBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TrackBarTk := GetTickCount64;
end;


procedure TfrmGradientTextMain.trbTestTrackBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  edtTrackBarDragDuration.Text := IntToStr(GetTickCount64 - TrackBarTk);
end;


procedure TfrmGradientTextMain.btnResetTrackBarClick(Sender: TObject);
begin
  trbTestTrackBar.Position := 0;
end;


procedure TfrmGradientTextMain.btnResetTrackBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ButtonTk := GetTickCount64;
end;


procedure TfrmGradientTextMain.btnResetTrackBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  edtButtonClickDuration.Text := IntToStr(GetTickCount64 - ButtonTk);
end;

end.

