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

