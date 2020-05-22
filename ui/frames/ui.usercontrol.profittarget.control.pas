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
  if radio_group.ItemIndex = Pred(radio_group.Items.Count) then
    FPerc := StrToFloatDef(edit_custom.Text, 0.005)
  else
    FPerc := StrToFloatDef(radio_group.Items[radio_group.ItemIndex].Replace('%', ''), 0) / 100;

  Result := FPerc;
  edit_custom.Text := FloatToStr(FPerc);
end;

procedure TProfitTargetControl.SetPerc(const AValue: Single);
begin
  FPerc := AValue;

  if (AValue > 0) and ((AValue < 0.005) or (AValue > 0.10)) then
    radio_group.ItemIndex := Pred(radio_group.Items.Count)
  else if Round(AValue * 100) = 10 then
    radio_group.ItemIndex := 5
  else if Round(AValue * 100) = 5 then
    radio_group.ItemIndex := 4
  else if Round(AValue * 100) = 3 then
    radio_group.ItemIndex := 3
  else if Round(AValue * 100) = 2 then
    radio_group.ItemIndex := 2
  else if Round(AValue * 100) = 1 then
    radio_group.ItemIndex := 1
  else
    radio_group.ItemIndex := 0;

  edit_custom.Text := FloatToStr(FPerc);
end;

end.

