unit ui.usercontrol.profittarget.control;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ExtCtrls, StdCtrls;

type

  { TProfitTargetControl }

  TProfitTargetControl = class(TFrame)
    edit_custom: TEdit;
    lbl_info: TLabel;
    radio_group: TRadioGroup;
  private
    FPerc : Single;

    function GetPerc: Single;
    procedure SetPerc(const AValue: Single);

  public
    property Percent : Single read GetPerc write SetPerc;
  end;

implementation

{$R *.lfm}

{ TProfitTargetControl }

function TProfitTargetControl.GetPerc: Single;
begin
  Result := FPerc;
end;

procedure TProfitTargetControl.SetPerc(const AValue: Single);
begin
  FPerc := AValue;

  if (AValue > 0) and ((AValue < 0.005) or (AValue > 0.10)) then
    radio_group.ItemIndex := Pred(radio_group.Items.Count)
  else if AValue = 0.10 then
    radio_group.ItemIndex := 5
  else if AValue = 0.05 then
    radio_group.ItemIndex := 4
  else if AValue = 0.03 then
    radio_group.ItemIndex := 3
  else if AValue = 0.02 then
    radio_group.ItemIndex := 2
  else if AValue = 0.01 then
    radio_group.ItemIndex := 1
  else
    radio_group.ItemIndex := 0;
end;

end.

