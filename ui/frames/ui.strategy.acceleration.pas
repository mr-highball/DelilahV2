unit ui.strategy.acceleration;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ui.strategy, delilah.types;

type

  { TConfigureAcceleration }

  TConfigureAcceleration = class(TConfigureStrategy)
    chk_dynamic: TCheckBox;
    edit_chop_thresh: TEdit;
    edit_lead_starting_percent: TEdit;
    edit_cross_thresh: TEdit;
    edit_risky_pos_percent: TEdit;
    edit_lead_end_percent: TEdit;
    edit_pos_percent: TEdit;
    edit_cross_down_thresh: TEdit;
    edit_window_size: TEdit;
  private
  protected
    procedure UpdateAccel(Sender : TObject); virtual;
    procedure DoReload(const AStrategy: IStrategy); override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  delilah.strategy.acceleration;

{$R *.lfm}

{ TConfigureAcceleration }

procedure TConfigureAcceleration.UpdateAccel(Sender: TObject);
var
  LStrat: IAccelerationStrategy;
begin
  LStrat := Strategy as IAccelerationStrategy;

  if edit_window_size.Text <> '' then
    LStrat.WindowSizeInMilli := StrToInt(edit_window_size.Text);

  if edit_lead_starting_percent.Text <> '' then
    LStrat.LeadStartPercent := StrToFloat(edit_lead_starting_percent.Text);

  if edit_lead_end_percent.Text <> '' then
    LStrat.LeadEndPercent := StrToFloat(edit_lead_end_percent.Text);

  if edit_pos_percent.Text <> '' then
    LStrat.PositionPercent := StrToFloat(edit_pos_percent.Text);

  if edit_risky_pos_percent.Text <> '' then
    LStrat.RiskyPositionPercent := StrToFloat(edit_risky_pos_percent.Text);

  if edit_cross_thresh.Text <> '' then
    LStrat.CrossThresholdPercent := StrToFloat(edit_cross_thresh.Text);

  if edit_cross_down_thresh.Text <> '' then
    LStrat.CrossDownThresholdPercent := StrToFloat(edit_cross_down_thresh.Text);

  if edit_chop_thresh.Text <> '' then
    LStrat.AvoidChopThreshold := StrToFloat(edit_chop_thresh.Text);

  LStrat.UseDynamicPositions := chk_dynamic.Checked;
end;

procedure TConfigureAcceleration.DoReload(const AStrategy: IStrategy);
var
  LStrat: IAccelerationStrategy;
begin
  LStrat := Strategy as IAccelerationStrategy;

  edit_window_size.Text := FloatToStr(LStrat.WindowSizeInMilli);
  edit_lead_starting_percent.Text := FloatToStr(LStrat.LeadStartPercent);
  edit_lead_end_percent.Text := FloatToStr(LStrat.LeadEndPercent);
  edit_pos_percent.Text := FloatToStr(LStrat.PositionPercent);
  edit_risky_pos_percent.Text := FloatToStr(LStrat.RiskyPositionPercent);
  edit_cross_thresh.Text := FloatToStr(LStrat.CrossThresholdPercent);
  edit_cross_down_thresh.Text := FloatToStr(LStrat.CrossDownThresholdPercent);
  edit_chop_thresh.Text := FloatToStr(LStrat.AvoidChopThreshold);
  chk_dynamic.Checked := LStrat.UseDynamicPositions;
end;

constructor TConfigureAcceleration.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Strategy := TAccelerationStrategyImpl.Create;
  AddBeforeSaveSubscriber(UpdateAccel);
end;

end.

