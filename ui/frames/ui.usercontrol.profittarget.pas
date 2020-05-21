unit ui.usercontrol.profittarget;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ui.usercontrol,
  ui.usercontrol.profittarget.control;

type

  { TProfitTarget }

  TProfitTarget = class(TUserControl)
  private
    function GetPerc: Single;
    procedure SetPerc(const AValue: Single);

  public
    property Percent : Single read GetPerc write SetPerc;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TProfitTarget }

function TProfitTarget.GetPerc: Single;
begin
  if not Assigned(Control) then
    Exit(0);

  Result := TProfitTargetControl(Control).Percent;
end;

procedure TProfitTarget.SetPerc(const AValue: Single);
begin
  if not Assigned(Control) then
      Exit;

  TProfitTargetControl(Control).Percent := AValue;
end;

constructor TProfitTarget.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Control := TProfitTargetControl.Create(nil);
end;

end.

