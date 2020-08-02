unit ui.strategy.bobber;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ui.strategy,
  delilah.types;

type

  { TConfigureBobber }

  TConfigureBobber = class(TConfigureStrategy)
    chk_limit_buy: TCheckBox;
    chk_limit_sell: TCheckBox;
    edit_funds: TEdit;
    edit_threshold: TEdit;
    edit_anch_threshold: TEdit;
    radio_funds_mode: TRadioGroup;
  private
    procedure UpdateBobber(Sender : TObject);
  protected
    procedure DoReload(const AStrategy: IStrategy); override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  delilah.strategy.bobber,
  delilah.strategy.bobber.gdax;

{$R *.lfm}

{ TConfigureBobber }

procedure TConfigureBobber.UpdateBobber(Sender: TObject);
var
  LStrat: IGDAXBobberStrategy;
begin
  LStrat := Strategy as IGDAXBobberStrategy;

  if edit_threshold.Text <> '' then
    LStrat.Threshold := StrToFloat(edit_threshold.Text)
  else
    LStrat.Threshold := 0;

  if edit_anch_threshold.Text <> '' then
    LStrat.AdjustAnchorThreshold := StrToFloat(edit_anch_threshold.Text)
  else
    LStrat.AdjustAnchorThreshold := 0;

  if edit_funds.Text <> '' then
    LStrat.Funds := StrToFloat(edit_funds.Text)
  else
    LStrat.Funds := 0;

  if radio_funds_mode.ItemIndex < 0 then
    radio_funds_mode.ItemIndex := 0;

  if radio_funds_mode.Items[radio_funds_mode.ItemIndex] = 'bmPercentTotal' then
    LStrat.FundsMode := bmPercentTotal
  else if radio_funds_mode.Items[radio_funds_mode.ItemIndex] = 'bmPercentAvailable' then
    LStrat.FundsMode := bmPercentAvailable
  else if radio_funds_mode.Items[radio_funds_mode.ItemIndex] = 'bmFixedBase' then
    LStrat.FundsMode := bmFixedBase
  else if radio_funds_mode.Items[radio_funds_mode.ItemIndex] = 'bmFixedCoin' then
    LStrat.FundsMode := bmFixedCoin;

  LStrat.UseLimitBuy := chk_limit_buy.Checked;
  LStrat.UseLimitSell := chk_limit_sell.Checked;
end;

procedure TConfigureBobber.DoReload(const AStrategy: IStrategy);
var
  LStrat: IGDAXBobberStrategy;
begin
  LStrat := Strategy as IGDAXBobberStrategy;

  edit_threshold.Text := FloatToStr(LStrat.Threshold);
  edit_anch_threshold.Text := FloatToStr(LStrat.AdjustAnchorThreshold);
  edit_funds.Text := FloatToStr(LStrat.Funds);

  if LStrat.FundsMode = bmPercentTotal then
    radio_funds_mode.ItemIndex := radio_funds_mode.Items.IndexOf('bmPercentTotal')
  else if LStrat.FundsMode = bmPercentAvailable then
    radio_funds_mode.ItemIndex := radio_funds_mode.Items.IndexOf('bmPercentAvailable')
  else if LStrat.FundsMode = bmFixedBase then
    radio_funds_mode.ItemIndex := radio_funds_mode.Items.IndexOf('bmFixedBase')
  else if LStrat.FundsMode = bmFixedCoin then
    radio_funds_mode.ItemIndex := radio_funds_mode.Items.IndexOf('bmFixedCoin');

  chk_limit_buy.Checked := LStrat.UseLimitBuy;
  chk_limit_sell.Checked := LStrat.UseLimitSell;
end;

constructor TConfigureBobber.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Strategy := TGDAXBobberStrategyImpl.Create(nil, nil, nil);
  AddBeforeSaveSubscriber(UpdateBobber);
end;

end.

