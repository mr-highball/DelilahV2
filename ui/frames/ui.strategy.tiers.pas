unit ui.strategy.tiers;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ui.strategy, ui.usercontrol.singleline, delilah.types;

type

  { TConfigureTiers }

  TConfigureTiers = class(TConfigureStrategy)
    chk_avoid_chop: TCheckBox;
    chk_only_profit: TCheckBox;
    chk_only_lower: TCheckBox;
    chk_market_buy: TCheckBox;
    chk_market_sell: TCheckBox;
    edit_max_scale_buy_percent: TEdit;
    edit_window_size: TEdit;
    edit_min_profit: TEdit;
    edit_ignore_profit_threshold: TEdit;
    edit_min_reduction: TEdit;
    edit_market_fee: TEdit;
    edit_limit_fee: TEdit;
    edit_small_sell_perc: TEdit;
    edit_large_sell_percent: TEdit;
    edit_small_buy_percent: TEdit;
    edit_large_buy_percent: TEdit;
  private
    procedure UpdateTiers(Sender : TObject);
  protected
    procedure DoReload(const AStrategy: IStrategy); override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  delilah.strategy.gdax.tiers;

{$R *.lfm}

{ TConfigureTiers }

procedure TConfigureTiers.UpdateTiers(Sender: TObject);
var
  LStrat: ITierStrategyGDAX;
begin
  LStrat := Strategy as ITierStrategyGDAX;

  if edit_window_size.Text <> '' then
    LStrat.ChannelStrategy.WindowSizeInMilli := StrToInt(edit_window_size.Text);

  if edit_ignore_profit_threshold.Text <> '' then
    LStrat.IgnoreOnlyProfitThreshold := StrToFloat(edit_ignore_profit_threshold.Text);

  if edit_min_profit.Text <> '' then
    LStrat.MinProfit := StrToFloat(edit_min_profit.Text);

  if edit_min_reduction.Text <> '' then
    LStrat.MinReduction := StrToFloat(edit_min_reduction.Text);

  if edit_market_fee.Text <> '' then
    LStrat.MarketFee := StrToFloat(edit_market_fee.Text);

  if edit_limit_fee.Text <> '' then
    LStrat.LimitFee := StrToFloat(edit_limit_fee.Text);

  if edit_small_sell_perc.Text <> '' then
    LStrat.SmallTierSellPerc := StrToFloat(edit_small_sell_perc.Text);

  if edit_large_sell_percent.Text <> '' then
    LStrat.LargeTierSellPerc := StrToFloat(edit_large_sell_percent.Text);

  if edit_small_buy_percent.Text <> '' then
    LStrat.SmallTierPerc := StrToFloat(edit_small_buy_percent.Text);

  if edit_large_buy_percent.Text <> '' then
    LStrat.LargeTierPerc := StrToFloat(edit_large_buy_percent.Text);

  if edit_max_scale_buy_percent.Text <> '' then
    LStrat.MaxScaledBuyPerc := StrToFloat(edit_max_scale_buy_percent.Text);

  LStrat.OnlyProfit := chk_only_profit.Checked;
  LStrat.OnlyLowerAAC := chk_only_lower.Checked;
  LStrat.UseMarketBuy := chk_market_buy.Checked;
  LStrat.UseMarketSell := chk_market_sell.Checked;
  LStrat.AvoidChop := chk_avoid_chop.Checked;
end;

procedure TConfigureTiers.DoReload(const AStrategy: IStrategy);
var
  LStrat: ITierStrategyGDAX;
begin
  LStrat := Strategy as ITierStrategyGDAX;

  edit_window_size.Text := FloatToStr(LStrat.ChannelStrategy.WindowSizeInMilli);
  edit_ignore_profit_threshold.Text := FloatToStr(LStrat.IgnoreOnlyProfitThreshold);
  edit_min_profit.Text := FloatToStr(LStrat.MinProfit);
  edit_min_reduction.Text := FloatToStr(LStrat.MinReduction);
  edit_market_fee.Text := FloatToStr(LStrat.MarketFee);
  edit_limit_fee.Text := FloatToStr(LStrat.LimitFee);
  edit_small_sell_perc.Text := FloatToStr(LStrat.SmallTierSellPerc);
  edit_large_sell_percent.Text := FloatToStr(LStrat.LargeTierSellPerc);
  edit_small_buy_percent.Text := FloatToStr(LStrat.SmallTierPerc);
  edit_large_buy_percent.Text := FloatToStr(LStrat.LargeTierPerc);
  edit_max_scale_buy_percent.Text := FloatToStr(LStrat.MaxScaledBuyPerc);
  chk_only_profit.Checked := LStrat.OnlyProfit ;
  chk_only_lower.Checked := LStrat.OnlyLowerAAC ;
  chk_market_buy.Checked := LStrat.UseMarketBuy ;
  chk_market_sell.Checked := LStrat.UseMarketSell ;
  chk_avoid_chop.Checked := LStrat.AvoidChop ;
end;

constructor TConfigureTiers.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Strategy := TTierStrategyGDAXImpl.Create(nil, nil, nil);
  AddBeforeSaveSubscriber(UpdateTiers);
end;

end.

