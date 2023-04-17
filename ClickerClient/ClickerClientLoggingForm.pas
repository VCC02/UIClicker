unit ClickerClientLoggingForm;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmClickerClientLogging }

  TfrmClickerClientLogging = class(TForm)
    memLog: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public
    procedure AddToLog(AMsg: string);
    procedure HandleOnLogMissingServerFile(AMsg: string);
  end;

var
  frmClickerClientLogging: TfrmClickerClientLogging;

implementation

{$R *.frm}

{ TfrmClickerClientLogging }

procedure TfrmClickerClientLogging.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;


procedure TfrmClickerClientLogging.AddToLog(AMsg: string);
begin
  memLog.Lines.Add(DateTimeToStr(Now) + '  ' + AMsg);
end;


procedure TfrmClickerClientLogging.HandleOnLogMissingServerFile(AMsg: string);
begin
  AddToLog(AMsg);
end;

end.

