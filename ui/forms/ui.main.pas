unit ui.main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TATools, Forms,
  Controls, Graphics, Dialogs, JSONPropStorage, ExtCtrls, ComCtrls, StdCtrls,
  Menus, ui.ignition, ui.authenticator, ui.usercontrol.multiline,
  ui.usercontrol.products, ui.usercontrol, gdax.api.types, delilah.order.gdax,
  ui.usercontrol.singleline, ui.gunslinger.gdax, delilah.types, ui.email,
  delilah.strategy.gdax.tiers, gdax.api.ticker, delilah.strategy.acceleration,
  ui.usercontrol.slider, ui.usercontrol.profittarget, utilities.tickerparser.main,
  ui.usercontrol.boolean, delilah.strategy.bobber.gdax, ui.strategy.bobber
  {, ezthreads};

type

  //alias due to same names... my bad
  TPreloadTick = TGDAXTickerImpl;

  { TSimpleBot }
  (*
    this form allows for configuring a IDelilah engine for GDAX and offers
    some conveniences such as chart, visual logging, etc...
    not all features of the engine are utilized, and currently this is tailored
    to GDAX. ultimately this is designed to be a simple ui wrapper to get someone
    up and running a strategy with minimal hassle as well as showing a use case
    for the GDAX api and engine core classes.
  *)
  TSimpleBot = class(TForm)
    btn_log_clear: TButton;
    chart_tools: TChartToolset;
    chart_toolsDataPointCrosshairTool1: TDataPointCrosshairTool;
    chart_toolsZoomMouseWheelTool1: TZoomMouseWheelTool;
    chart_source: TListChartSource;
    chart_ticker: TChart;
    chart_tickerLineSeries1: TLineSeries;
    chk_log_info: TCheckBox;
    chk_log_error: TCheckBox;
    chk_log_warn: TCheckBox;
    grp_log_options: TGroupBox;
    Gunslinger1: TGDAXGunslinger;
    ignition_main: TIgnition;
    icons: TImageList;
    menu: TImageList;
    json_main: TJSONPropStorage;
    mi_tools: TMenuItem;
    mi_sim: TMenuItem;
    mi_gunslinger: TMenuItem;
    mi_email_setup: TMenuItem;
    mi_email_enabled: TMenuItem;
    mi_file_settings: TMenuItem;
    mi_log_tab: TMenuItem;
    mi_email: TMenuItem;
    mi_logging: TMenuItem;
    mi_auto_start: TMenuItem;
    mi_configure: TMenuItem;
    menu_main: TMainMenu;
    memo_licenses: TMemo;
    multi_log: TMultiLine;
    pnl_strat_ctrl_container: TPanel;
    pnl_log_clear: TPanel;
    pctrl_main: TPageControl;
    scroll_strategy: TScrollBox;
    Splitter1: TSplitter;
    status_main: TStatusBar;
    ts_product: TTabSheet;
    ts_about: TTabSheet;
    ts_chart: TTabSheet;
    ts_log: TTabSheet;
    ts_strategy: TTabSheet;
    ts_auth: TTabSheet;
    procedure btn_log_clearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure json_mainRestoringProperties(Sender: TObject);
    procedure json_mainSavingProperties(Sender: TObject);
    procedure mi_gunslingerClick(Sender: TObject);
    procedure mi_auto_startClick(Sender: TObject);
    procedure mi_email_enabledClick(Sender: TObject);
    procedure mi_email_setupClick(Sender: TObject);
    procedure mi_file_settingsClick(Sender: TObject);
    procedure mi_log_tabClick(Sender: TObject);
    procedure mi_simClick(Sender: TObject);
    procedure pctrl_mainChange(Sender: TObject);
    procedure scroll_strategyResize(Sender: TObject);
  private
    FAuth : TAuthenticator;
    FProducts : TProducts;
    FProductInit,
    FLoaded: Boolean;
    FFundsCtrl,
    FMarketFeeCtrl,
    FLimitFeeCtrl,
    FScaledBuyCtrl: TSingleLine;
    FBuyOnUpCtrl,
    FBuyOnDownCtrl,
    FIgnoreOnUpCtrl,
    FIgnoreOnDownCtrl,
    FBuyBobberIn,
    FBuyBobberOut: TBool;
    FBobberCtrl: TUserControl;
    FBobberInnerCtrl: TConfigureBobber;
    FTimeFrameCtrl,
    FPosSizeCtrl,
    FSellPosSizeCtrl,
    FDCASizeCtrl,
    FUpFundCtrl,
    FDownFundCtrl: TSlider;
    FProfitCtrl,
    FMinUpRedCtrl,
    FMinDownRedCtrl: TProfitTarget;
    FInit : Boolean;
    FEngine,
    FTempEngine: IDelilah;
    FCompletedOrders,
    FHighWindowSize: Cardinal;
    FHighPosSize,
    FHighSellPosSize,
    FHighDCASize,
    FHighUpMinRed,
    FHighDownMinRed: Single;
    FMarketFee,
    FLimitFee,
    FHighTakeProfit,
    FFundsBalance: Single;
    FLowestTier,
    FLowTier,
    FTier: ITierStrategyGDAX;
    FBobber: IGDAXBobberStrategy;
    FAccelStrategy,
    FLowAccelStrategy,
    FLowestAccelStrategy : IAccelerationStrategy;
    FUseMarketBuy,
    FUseMarketSell: Boolean;
    FFunds: Single;
    FTradingPair: String;
    //FDoggy : IEZThread;
    procedure SetupEmail;
    procedure EnableEmail;
    procedure SetupLogFile;
    procedure SetupLogTab;
    procedure InitControls;
    procedure InitFundsLedger;
    procedure EnableAutoStart;
    procedure PreloadTickers;
    procedure ValidateLicense;
    procedure LaunchWatchdog;
    procedure HandleAutoStart;
    function LoadProduct(out Error : String) : Boolean;

    procedure SimulateStrategy;
    procedure AddStrategiesToSim(const ASim : TTickerParser);
    procedure DoOnStartSim(Sender : TObject);
    procedure DoOnFinishSim(Sender : TObject);

    procedure CheckCanStart(Sender:TObject;Var Continue:Boolean);
    procedure CheckCanStop(Sender:TObject;Var Continue:Boolean);

    procedure StartStrategy(Sender:TObject);
    procedure StopStrategy(Sender:TObject);

    procedure ProductError(Const AProductID:String;Const AError:String);
    procedure ProductTick(Sender : TObject; Const ATick : IGDAXTicker);
    procedure EngineStatus(Const ADetails:IOrderDetails;Const AID:String;
      Const AOldStatus,ANewStatus:TOrderManagerStatus);

    procedure LogError(Const AMessage:String);
    procedure LogInfo(Const AMessage:String);
    procedure LogWarn(Const AMessage:String);

    function GetAccelCriteria : TActiveCriteriaCallbackArray;
    function GetLowAccelCriteria : TActiveCriteriaCallbackArray;
    function GetLowestAccelCriteria : TActiveCriteriaCallbackArray;
  private
    const
      (*
        min and max times associated to the strategy time slider
      *)
      MIN_TIME = 86400000; //24 hours
      MAX_TIME = 432000000; //5 days

      (*
        min and max main position sizes associated to the strategy pos size slider
      *)
      MIN_POS_SIZE = 0.001; //min sell percent
      MAX_POS_SIZE = 1.0; //max sell percent

      (*
        min and max sell position sizes associated to the strategy sell pos size slider
      *)
      MIN_SELL_POS_SIZE = 0.001; //min sell percent
      MAX_SELL_POS_SIZE = 1.0; //max sell percent

      (*
        min and max dca position sizes associated to the strategy dca pos size slider
      *)
      MIN_DCA_SIZE = 0.001; //0.1% applied to highest dca strat
      MAX_DCA_SIZE = 0.03; //3% applied to highest dca strat
  protected
    (*
      provided an input time (read from setting) will output the interpolated
      value against the new slider settings
    *)
    procedure InterpolateTimeSetting(var Time : Cardinal; const AIsReload : Boolean = False);

    (*
      provided an input position size (read from setting) will output the interpolated
      value against the new slider settings
    *)
    procedure InterpolatePositionSetting(var Position : Single; const AIsReload : Boolean = False);

    (*
      provided an input sell position size (read from setting) will output the interpolated
      value against the new slider settings
    *)
    procedure InterpolateSellPositionSetting(var Position : Single; const AIsReload : Boolean = False);

    (*
      provided an input DCA position size (read from setting) will output the interpolated
      value against the new slider settings
    *)
    procedure InterpolateDCAPositionSetting(var Position : Single; const AIsReload : Boolean = False);
  public
  end;

var
  SimpleBot: TSimpleBot;
  EXPIRED : Boolean;

implementation
uses
  //UITypes,
  delilah, delilah.strategy.gdax, delilah.ticker.gdax, delilah.strategy.window,
  delilah.strategy.gdax.sample, delilah.manager.gdax, ledger,
  delilah.strategy.gdax.sample.extended, delilah.strategy.channels,
  math, dateutils, delilah.strategy.acceleration.gdax, ui.license,
  delilah.strategy.bobber
  {$IFDEF WINDOWS}
  ,JwaWindows
  {$ENDIF};

{$R *.lfm}

const
  LOG_ENTRY = '%s %s - %s';

function StrategiesInPosition : Boolean;
begin
  if not (
    Assigned(SimpleBot.FAccelStrategy)
    or Assigned(SimpleBot.FLowAccelStrategy)
    or Assigned(SimpleBot.FLowestAccelStrategy))
  then
    Exit(False);

  //if we have one strategy in position, return true
  Result := (SimpleBot.FAccelStrategy.Position <> TAccelPosition.apNone)
    or (SimpleBot.FLowAccelStrategy.Position <> TAccelPosition.apNone)
    or (SimpleBot.FLowestAccelStrategy.Position <> TAccelPosition.apNone)
end;

procedure BobberInOutPosition(const ADetails : PActiveCriteriaDetails;
  var Active : Boolean);
begin
  //when we are checking to buy based on the bobber we see if
  //a.) the settings enabled and b.) we're in or out of pos
  Active :=
    (SimpleBot.FBuyBobberIn.Checked and (SimpleBot.FBobber.State = bsInPos))
    or
    (SimpleBot.FBuyBobberOut.Checked and (SimpleBot.FBobber.State = bsOutPos));
end;

procedure AccelLowestStrategyInPosition(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
var
  LUptrend: Boolean;
begin
  try
    Active := False;

    if not PAccelerationStrategy(ADetails.Data)^.IsReady then
      Exit;

    //if we're trending upwards using "our" acceleration strategy, then onlyprofit
    //will be "on", otherwise, allow for selling at a loss
    LUptrend := (PAccelerationStrategy(ADetails.Data)^.CurLagAccel + PAccelerationStrategy(ADetails.Data)^.CurLeadAccel > 0);

    //default to only profit
    SimpleBot.FLowestTier.OnlyProfit := True;

    //toggly the aac based on the trend
    if LUptrend then
    begin
      if SimpleBot.FHighUpMinRed > 0 then
      begin
        SimpleBot.FLowestTier.OnlyLowerAAC := True;
        SimpleBot.FLowestTier.MinReduction := SimpleBot.FHighUpMinRed / 3;
      end
      else
        SimpleBot.FLowestTier.OnlyLowerAAC := False;
    end
    else
    begin
      if SimpleBot.FHighDownMinRed > 0 then
      begin
        SimpleBot.FLowestTier.OnlyLowerAAC := True;
        SimpleBot.FLowestTier.MinReduction := SimpleBot.FHighDownMinRed / 3;
      end
      else
        SimpleBot.FLowestTier.OnlyLowerAAC := False;
    end;

    //when we are selling and in position, allow the sell
    if not ADetails^.IsBuy then
      Active := True
    //otherwise, buying is determined based on the trend / selection
    else
      Active := (LUptrend and SimpleBot.FBuyOnUpCtrl.Checked) or (not LUptrend and SimpleBot.FBuyOnDownCtrl.Checked);
  finally
    SimpleBot.LogInfo(Format('LowestTier::[active]:%s [only-profit]:%s', [BoolToStr(Active, True), BoolToStr(SimpleBot.FLowestTier.OnlyProfit, True)]));
  end;
end;

procedure AccelLowStrategyInPosition(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
var
  LUptrend: Boolean;
begin
  try
    Active := False;

    if not PAccelerationStrategy(ADetails.Data)^.IsReady then
      Exit;

    //if we're trending upwards using "our" acceleration strategy, then onlyprofit
    //will be "on", otherwise, allow for selling at a loss
    LUptrend := (PAccelerationStrategy(ADetails.Data)^.CurLagAccel + PAccelerationStrategy(ADetails.Data)^.CurLeadAccel > 0);

    //default to only profit
    SimpleBot.FLowTier.OnlyProfit := True;

    //toggly the aac based on the trend
    if LUptrend then
    begin
      if SimpleBot.FHighUpMinRed > 0 then
      begin
        SimpleBot.FLowTier.OnlyLowerAAC := True;
        SimpleBot.FLowTier.MinReduction := SimpleBot.FHighUpMinRed / 2;
      end
      else
        SimpleBot.FLowestTier.OnlyLowerAAC := False;
    end
    else
    begin
      if SimpleBot.FHighDownMinRed > 0 then
      begin
        SimpleBot.FLowTier.OnlyLowerAAC := True;
        SimpleBot.FLowTier.MinReduction := SimpleBot.FHighDownMinRed / 2;
      end
      else
        SimpleBot.FLowTier.OnlyLowerAAC := False;
    end;

    //when we are selling and in position, allow the sell
    if not ADetails^.IsBuy then
      Active := True
    //otherwise, buying is determined based on the trend / selection
    else
      Active := (LUptrend and SimpleBot.FBuyOnUpCtrl.Checked) or (not LUptrend and SimpleBot.FBuyOnDownCtrl.Checked);
  finally
    SimpleBot.LogInfo(Format('Lowest::[active]:%s [only-profit]:%s', [BoolToStr(Active, True), BoolToStr(SimpleBot.FLowTier.OnlyProfit, True)]));
  end;
end;

procedure AccelStrategyInPosition(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
var
  LUptrend: Boolean;
begin
  try
    Active := False;

    if not PAccelerationStrategy(ADetails.Data)^.IsReady then
      Exit;

    //if we're trending upwards using "our" acceleration strategy, then onlyprofit
    //will be "on", otherwise, allow for selling at a loss
    LUptrend := (PAccelerationStrategy(ADetails.Data)^.CurLagAccel + PAccelerationStrategy(ADetails.Data)^.CurLeadAccel > 0);

    //default to only profit
    SimpleBot.FTier.OnlyProfit := True;

    //toggly the aac based on the trend
    if LUptrend then
    begin
      if SimpleBot.FHighUpMinRed > 0 then
      begin
        SimpleBot.FTier.OnlyLowerAAC := True;
        SimpleBot.FTier.MinReduction := SimpleBot.FHighUpMinRed;
      end
      else
        SimpleBot.FTier.OnlyLowerAAC := False;
    end
    else
    begin
      if SimpleBot.FHighDownMinRed > 0 then
      begin
        SimpleBot.FTier.OnlyLowerAAC := True;
        SimpleBot.FTier.MinReduction := SimpleBot.FHighDownMinRed;
      end
      else
        SimpleBot.FTier.OnlyLowerAAC := False;
    end;

    //depending on price direction, adjust the strategy
    if LUptrend then
    begin
      //configure the ignore profit threshold
      SimpleBot.FTier.IgnoreOnlyProfitThreshold := SimpleBot.FUpFundCtrl.Value / 100;

      //set the ignore profit using the only profit property
      if SimpleBot.FIgnoreOnUpCtrl.Checked then
        SimpleBot.FTier.OnlyProfit := False;
    end
    else
    begin
      SimpleBot.FTier.IgnoreOnlyProfitThreshold := SimpleBot.FDownFundCtrl.Value / 100;

      //set the ignore profit using the only profit property
      if SimpleBot.FIgnoreOnDownCtrl.Checked then
        SimpleBot.FTier.OnlyProfit := False;
    end;

    //if the position is zero, then set it as close to zero but above since
    //zero is used as a way of not using this functionality
    if SimpleBot.FDownFundCtrl.Value <= 0 then
      SimpleBot.FTier.IgnoreOnlyProfitThreshold := 0.0000001
    else if SimpleBot.FDownFundCtrl.Value >= 100 then
      SimpleBot.FTier.IgnoreOnlyProfitThreshold := 0;

    //when we are selling and in position, allow the sell
    if not ADetails^.IsBuy then
      Active := True
    //otherwise, buying is determined based on the trend / selection
    else
      Active := (LUptrend and SimpleBot.FBuyOnUpCtrl.Checked) or (not LUptrend and SimpleBot.FBuyOnDownCtrl.Checked);
  finally
    SimpleBot.LogInfo(Format('Tier::[active]:%s [only-profit]:%s [uptrend]:%s [ignore-profit]:%f', [BoolToStr(Active, True), BoolToStr(SimpleBot.FLowTier.OnlyProfit, True), BoolToStr(LUptrend, True), SimpleBot.FTier.IgnoreOnlyProfitThreshold]));
  end;
end;

procedure MoonParentReady(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
begin
  //wait for our parent strategy to be ready before we go calling moonshots
  Active:=PTierStrategyGDAX(ADetails.Data)^.ChannelStrategy.IsReady;
end;

procedure MoonLowInventory(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
begin
  Active:=False;

  //below we check to see if we have a small inventory (more than 85% free funds)
  if ADetails^.TotalFunds < 0 then
    Exit
  //allocate up to 5% of funds to moon strategy
  else if (ADetails^.Funds / ADetails^.TotalFunds) >= 0.95 then
    Active:=True;
end;

function GetAverageAccelPerTick(Const AStrategy : PTierStrategyGDAX;
  Out TimePerTick : Extended) : Extended;
var
  LTickers: TTickers;
  LDelta: TArray<Extended>;
  I: Integer;
begin
  Result:=0;
  TimePerTick:=0;
  LTickers:=AStrategy^.ChannelStrategy.Tickers;

  //we need at least two tickerrs
  if LTickers.Count < 2 then
    Exit;

  //find the average time between each tick in the set so we can use it
  //to normalize our average accel "per tick" as opposed to per msec
  TimePerTick:=MilliSecondsBetween(LTickers[Pred(LTickers.Count)].Time, LTickers[0].Time) / LTickers.Count;
  SetLength(LDelta,LTickers.Count - 1);
  for I := 1 to Pred(LTickers.Count) do
    if LTickers[I].Time - LTickers[I - 1].Time > 0 then
      LDelta[I - 1]:=(LTickers[I].Price - LTickers[I - 1].Price) / MilliSecondsBetween(LTickers[I].Time, LTickers[I - 1].Time);

  Result:=mean(LDelta) * TimePerTick;
end;

procedure MoonRisingPrice(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
var
  LAvg, LProjectedPrice, LTimePerTick: Extended;
  LParentWindow: Cardinal;
  LMinProfit: Single;
begin
  if not Assigned(ADetails^.Data) then
    Exit;

  Active:=False;

  //if the price has steadily been rising using our configurable strategy
  //as the baseline, then go ahead and say we're "mooning". we'll use
  //average accel for this
  LAvg:=GetAverageAccelPerTick(PTierStrategyGDAX(ADetails^.Data),LTimePerTick);
  if LAvg < 0 then
  begin
    SimpleBot.LogInfo('Main::MoonRisingPrice::negative average acceleration per tick: ' + FloatToStr(LAvg));
    Exit;
  end;

  SimpleBot.LogInfo('Main::MoonRisingPrice::average acceleration per tick: ' + FloatToStr(LAvg));

  //no need to project if the parent doesn't care about profit
  if not PTierStrategyGDAX(ADetails^.Data)^.OnlyProfit then
  begin
    Active:=True;
    SimpleBot.LogInfo('Main::MoonRisingPrice::parent OnlyProfit is false, no projection');
    Exit;
  end;

  //to project the price we need to look at the parent window's size
  //and min profit, and see if with the current average accelaration, the profit
  //"should" be reached
  LParentWindow:=PTierStrategyGDAX(ADetails^.Data)^.ChannelStrategy.WindowSizeInMilli;
  LMinProfit:=PTierStrategyGDAX(ADetails^.Data)^.MinProfit;

  //(ticksInWindow * averageAccel) + currentPrice = projected
  LProjectedPrice:=(LParentWindow / LTimePerTick) * LAvg + ADetails^.Ticker^.Price;
  SimpleBot.LogInfo('Main::MoonRisingPrice::projected price ' + FloatToStr(LProjectedPrice));

  //find delta of current price and see if the percentage gain is at least
  //half min profit (doesn't account for fees etc...)
  LAvg:=LProjectedPrice - ADetails^.Ticker^.Price;//yeah yeah don't reuse variables

  if (LAvg / ADetails^.Ticker^.Price) < (LMinProfit / 2) then
  begin
    SimpleBot.LogInfo('Main::MoonRisingPrice::projected price is not high enough for min profit');
    Exit;
  end;

  //otherwise all good to buy
  Active:=True;
end;

procedure MoonSideBuy(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
begin
  //moon strategy is only buy strategy
  Active:=ADetails^.IsBuy;
end;

procedure AccelDisableTrades(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
begin
  //acceleration strategy can never do shit, errp beer
  Active:=False;
end;

procedure MidAccelStratPositve(Const ADetails : PActiveCriteriaDetails;
  Var Active : Boolean);
var
  LAccel: PTierStrategyGDAX;
  LTimePerTick, LAvg: Extended;
begin
  //this criteria is only checked during buy positions
  if not ADetails^.IsBuy then
    Exit;

  if not Assigned(ADetails^.Data) then
    Exit;

  Active:=False;
  LAccel:=PTierStrategyGDAX(ADetails^.Data);

  //find the average accel per tick and if we are postive, return true
  LAvg:=GetAverageAccelPerTick(LAccel,LTimePerTick);
  SimpleBot.LogInfo('Main::MidAccelStratPositive::average acceleration per tick: ' + FloatToStr(LAvg));
  Active:=LAvg > 0;
end;

{ TSimpleBot }

procedure TSimpleBot.json_mainRestoringProperties(Sender: TObject);
var
  LBal:Extended;
begin
  if not FInit or FLoaded then
    Exit;

  //get our ledgers setup correctly
  FEngine.InventoryLedger.Clear;
  FEngine.FundsLedger.Clear;
  FEngine.HoldsLedger.Clear;
  FEngine.HoldsInventoryLedger.Clear;

  //now read from our json file
  FAuth.Secret := json_main.ReadString('secret','');
  FAuth.Key := json_main.ReadString('key','');
  FAuth.Passphrase := json_main.ReadString('pass','');

  FFunds := StrToFloatDef(json_main.ReadString('funds','0.0'), 0);
  FFundsCtrl.Text := FloatToStr(FFunds);
  FAuth.IsSanboxMode := json_main.ReadBoolean('sandbox_mode',True);
  FEngine.AAC := StrToFloatDef(json_main.ReadString('aac','0.0'),0);

  //restoring funds requires us to see what was recorded in the ledger
  //as the balance, then subtract (balance - start funds) and then
  //credit by this amount (could be negative credit)
  FFundsBalance := StrToFloatDef(json_main.ReadString('funds_ledger','0.0'), 0);

  //set the actual ledger to the balance
  FEngine.Funds := FFundsBalance;

  //record to holds
  LBal := FEngine.HoldsLedger.RecordEntry(
    StrToFloatDef(json_main.ReadString('holds_ledger','0.0'),0),
    ltDebit
  ).Balance;

  //record to inventory
  LBal := FEngine.InventoryLedger.RecordEntry(
    StrToFloatDef(json_main.ReadString('inventory_ledger','0.0'),0),
    ltCredit
  ).Balance;

  //record to holds inventory
  LBal := FEngine.HoldsInventoryLedger.RecordEntry(
    StrToFloatDef(json_main.ReadString('inventory_holds_ledger','0.0'),0),
    ltDebit
  ).Balance;

  //simple counter for completed orders
  FCompletedOrders:=json_main.ReadInteger('completed_orders',0);

  FMarketFee := StrToFloatDef(json_main.ReadString('market_fee','0.005'),0.005);
  FMarketFeeCtrl.Text := FloatToStr(FMarketFee);
  FLimitFee := StrToFloatDef(json_main.ReadString('limit_fee','0.005'),0.005);
  FLimitFeeCtrl.Text := FloatToStr(FLimitFee);

  //temp settings
  FHighWindowSize := StrToIntDef(json_main.ReadString('high_window_size', ''), Round(MIN_TIME + (MAX_TIME - MIN_TIME) / 2));
  InterpolateTimeSetting(FHighWindowSize, True);

  FHighPosSize := StrToFloatDef(json_main.ReadString('high_position_size', ''), MIN_POS_SIZE + (MAX_POS_SIZE - MIN_POS_SIZE) / 2);
  InterpolatePositionSetting(FHighPosSize, True);

  FHighSellPosSize := StrToFloatDef(json_main.ReadString('high_sell_position_size', ''), MIN_POS_SIZE + (MAX_POS_SIZE - MIN_POS_SIZE) / 2);
  InterpolateSellPositionSetting(FHighSellPosSize, True);

  FHighDCASize := StrToFloatDef(json_main.ReadString('high_dca_size', ''), MIN_DCA_SIZE + (MAX_DCA_SIZE - MIN_DCA_SIZE) / 2);
  InterpolateDCAPositionSetting(FHighDCASize, True);

  FUpFundCtrl.Value := Round(StrToFloatDef(json_main.ReadString('max_uptrend_fund_usage', '1'), 1) * 100);
  FDownFundCtrl.Value := Round(StrToFloatDef(json_main.ReadString('max_downtrend_fund_usage', '1'), 1) * 100);

  FHighTakeProfit := StrToFloatDef(json_main.ReadString('high_take_profit','0.03'), 0.03);
  FProfitCtrl.Percent := FHighTakeProfit;

  FHighUpMinRed := StrToFloatDef(json_main.ReadString('high_min_reduction','0.0'), 0.0);
  FMinUpRedCtrl.Percent := FHighUpMinRed;

  FHighDownMinRed := StrToFloatDef(json_main.ReadString('high_min_reduction_downtrend','0.0'), 0.0);
  FMinDownRedCtrl.Percent := FHighDownMinRed;

  FUseMarketBuy := StrToBoolDef(json_main.ReadString('market_buy','false'), False);
  FUseMarketSell := StrToBoolDef(json_main.ReadString('market_sell','false'), False);

  FScaledBuyCtrl.Text := FloatToStr(StrToFloatDef(json_main.ReadString('scaled_buy_percent', '0.0'), 0));

  FBuyOnUpCtrl.Checked := StrToBoolDef(json_main.ReadString('buy_uptrend','true'), True);
  FBuyOnDownCtrl.Checked := StrToBoolDef(json_main.ReadString('buy_downtrend','true'), True);
  FIgnoreOnUpCtrl.Checked := StrToBoolDef(json_main.ReadString('ignore_uptrend','false'), False);
  FIgnoreOnDownCtrl.Checked := StrToBoolDef(json_main.ReadString('ignore_downtrend','false'), False);
  FBuyBobberIn.Checked := StrToBoolDef(json_main.ReadString('buy_bobber_in_pos','true'), True);
  FBuyBobberOut.Checked := StrToBoolDef(json_main.ReadString('buy_bobber_out_pos','true'), True);

  //bobber
  FBobberInnerCtrl.edit_funds.Text := FloatToStr(StrToFloatDef(json_main.ReadString('bobber_funds','0.0'), 0.0));
  FBobberInnerCtrl.edit_threshold.Text := FloatToStr(StrToFloatDef(json_main.ReadString('bobber_threshold','0.0'), 0.0));
  FBobberInnerCtrl.edit_anch_threshold.Text := FloatToStr(StrToFloatDef(json_main.ReadString('bobber_anchor_threshold','0.0'), 0.0));
  FBobberInnerCtrl.edit_threshold_profit.Text := FloatToStr(StrToFloatDef(json_main.ReadString('bobber_threshold_profit','0.0'), 0.0));
  FBobberInnerCtrl.edit_profit_percent.Text := FloatToStr(StrToFloatDef(json_main.ReadString('bobber_profit_percent','0.0'), 0.0));
  FBobberInnerCtrl.radio_funds_mode.ItemIndex := StrToIntDef(json_main.ReadString('bobber_mode','0'), 0);
  FBobberInnerCtrl.chk_limit_buy.Checked := StrToBoolDef(json_main.ReadString('bobber_limit_buy','true'), True);
  FBobberInnerCtrl.chk_limit_sell.Checked := StrToBoolDef(json_main.ReadString('bobber_limit_sell','true'), True);

  //menu items
  mi_auto_start.Checked := StrToBoolDef(json_main.ReadString('auto_start','false'), False);
  FProducts.ProductFrame.edit_interval.Value := json_main.ReadInteger('poll_interval', 1500);
  FTradingPair := json_main.ReadString('trading_pair', '');

  FLoaded := True;
end;

procedure TSimpleBot.FormCreate(Sender: TObject);
var
  LManager:IGDAXOrderManager;
begin
  FInit := False;
  FLoaded := False;
  FCompletedOrders:=0;

  //create an engine
  FEngine:=TDelilahImpl.Create(LogInfo, LogError, LogWarn);

  //since we are dealing with GDAX assign the order manager
  LManager:=TGDAXOrderManagerImpl.Create(LogInfo, LogError, LogWarn);
  FEngine.OrderManager:=LManager;
  FEngine.OnStatus:=EngineStatus;

  InitControls;
  LaunchWatchdog;
  HandleAutoStart;
end;

procedure TSimpleBot.btn_log_clearClick(Sender: TObject);
begin
  multi_log.Lines.Clear;
end;

procedure TSimpleBot.FormDestroy(Sender: TObject);
begin
  FAccelStrategy := nil;
  FLowAccelStrategy := nil;
  FBobber := nil;
end;

procedure TSimpleBot.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //enter full screen mode with F11 key
  if Key = VK_F11 then
  begin
    if Self.WindowState = wsFullScreen then
    begin
      Self.WindowState := wsNormal;
      Self.BorderStyle := bsSizeable;
    end
    else
    begin
      Self.WindowState := wsFullScreen;
      Self.BorderStyle := bsNone;
    end;
  end;
end;

procedure TSimpleBot.json_mainSavingProperties(Sender: TObject);
begin
  json_main.WriteString('secret',FAuth.Secret);
  json_main.WriteString('key',FAuth.Key);
  json_main.WriteString('pass',FAuth.Passphrase);
  json_main.WriteString('funds',FloatToStr(StrToFloatDef(FFundsCtrl.Text,0)));
  json_main.WriteBoolean('sandbox_mode',FAuth.IsSanboxMode);
  json_main.WriteString('aac',FloatToStr(FEngine.AAC));
  json_main.WriteInteger('completed_orders',FCompletedOrders);
  json_main.WriteString('funds_ledger',FloatToStr(FEngine.FundsLedger.Balance));
  json_main.WriteString('holds_ledger',FloatToStr(FEngine.HoldsLedger.Balance));
  json_main.WriteString('inventory_ledger',FloatToStr(FEngine.InventoryLedger.Balance));
  json_main.WriteString('inventory_holds_ledger',FloatToStr(FEngine.HoldsInventoryLedger.Balance));

  FMarketFee := StrToFloatDef(FMarketFeeCtrl.Text, 0.005);
  json_main.WriteString('market_fee',FloatToStr(FMarketFee));

  FLimitFee := StrToFloatDef(FLimitFeeCtrl.Text, 0.005);
  json_main.WriteString('limit_fee',FloatToStr(FLimitFee));

  //dca
  InterpolateTimeSetting(FHighWindowSize);
  json_main.WriteString('high_window_size', IntToStr(FHighWindowSize));

  InterpolatePositionSetting(FHighPosSize);
  json_main.WriteString('high_position_size', FloatToStr(FHighPosSize));

  InterpolateSellPositionSetting(FHighSellPosSize);
  json_main.WriteString('high_sell_position_size', FloatToStr(FHighSellPosSize));

  InterpolateDCAPositionSetting(FHighDCASize);
  json_main.WriteString('high_dca_size', FloatToStr(FHighDCASize));

  json_main.WriteString('max_uptrend_fund_usage', FloatToStr(FUpFundCtrl.Value / 100));
  json_main.WriteString('max_downtrend_fund_usage', FloatToStr(FDownFundCtrl.Value / 100));

  FHighTakeProfit := FProfitCtrl.Percent;
  json_main.WriteString('high_take_profit', FloatToStr(FHighTakeProfit));

  FHighUpMinRed := FMinUpRedCtrl.Percent;
  json_main.WriteString('high_min_reduction', FloatToStr(FHighUpMinRed));

  FHighDownMinRed := FMinDownRedCtrl.Percent;
  json_main.WriteString('high_min_reduction_downtrend', FloatToStr(FHighDownMinRed));

  json_main.WriteString('market_buy', BoolToStr(FUseMarketBuy, True));
  json_main.WriteString('market_sell', BoolToStr(FUseMarketSell, True));

  json_main.WriteString('scaled_buy_percent', FloatToStr(StrToFloatDef(FScaledBuyCtrl.Text, 0)));

  json_main.WriteString('buy_uptrend', BoolToStr(FBuyOnUpCtrl.Checked, True));
  json_main.WriteString('buy_downtrend', BoolToStr(FBuyOnDownCtrl.Checked, True));
  json_main.WriteString('ignore_uptrend', BoolToStr(FIgnoreOnUpCtrl.Checked, True));
  json_main.WriteString('ignore_downtrend', BoolToStr(FIgnoreOnDownCtrl.Checked, True));
  json_main.WriteString('buy_bobber_in_pos', BoolToStr(FBuyBobberIn.Checked, True));
  json_main.WriteString('buy_bobber_out_pos', BoolToStr(FBuyBobberOut.Checked, True));


  //bobber
  json_main.WriteString('bobber_funds', FloatToStr(StrToFloatDef(FBobberInnerCtrl.edit_funds.Text, 0)));
  json_main.WriteString('bobber_threshold', FloatToStr(StrToFloatDef(FBobberInnerCtrl.edit_threshold.Text, 0)));
  json_main.WriteString('bobber_anchor_threshold', FloatToStr(StrToFloatDef(FBobberInnerCtrl.edit_anch_threshold.Text, 0)));
  json_main.WriteString('bobber_threshold_profit', FloatToStr(StrToFloatDef(FBobberInnerCtrl.edit_threshold_profit.Text, 0)));
  json_main.WriteString('bobber_profit_percent', FloatToStr(StrToFloatDef(FBobberInnerCtrl.edit_profit_percent.Text, 0)));
  json_main.WriteString('bobber_mode', IntToStr(FBobberInnerCtrl.radio_funds_mode.ItemIndex));
  json_main.WriteString('bobber_limit_buy', BoolToStr(FBobberInnerCtrl.chk_limit_buy.Checked, True));
  json_main.WriteString('bobber_limit_sell', BoolToStr(FBobberInnerCtrl.chk_limit_sell.Checked, True));

  //menu/ui items
  json_main.WriteString('poll_interval', IntToStr(FProducts.ProductFrame.edit_interval.Value));
  json_main.WriteString('auto_start', BoolToStr(mi_auto_start.Checked, True));

  if Assigned(FProducts.ProductFrame) and Assigned(FProducts.ProductFrame.Product) then
    json_main.WriteString('trading_pair', FProducts.ProductFrame.Product.ID);
end;

procedure TSimpleBot.mi_gunslingerClick(Sender: TObject);
begin
  Gunslinger1.Visible := mi_gunslinger.Checked;
end;

procedure TSimpleBot.mi_auto_startClick(Sender: TObject);
begin
  EnableAutoStart;
end;

procedure TSimpleBot.mi_email_enabledClick(Sender: TObject);
begin
  EnableEmail;
end;

procedure TSimpleBot.mi_email_setupClick(Sender: TObject);
begin
  SetupEmail;
end;

procedure TSimpleBot.mi_file_settingsClick(Sender: TObject);
begin
  SetupLogFile;
end;

procedure TSimpleBot.mi_log_tabClick(Sender: TObject);
begin
  SetupLogTab;
end;

procedure TSimpleBot.mi_simClick(Sender: TObject);
begin
  SimulateStrategy;
end;

procedure TSimpleBot.pctrl_mainChange(Sender: TObject);
begin
  if pctrl_main.ActivePage=ts_product then
    if not FProductInit then
    begin
      MessageDlg(
        'Products',
        'please wait while we fetch products from GDAX...',
        TMsgDlgType.mtInformation,
        [TMsgDlgBtn.mbOK],
        ''
      );
      FProducts.ProductFrame.Init;
      FProductInit:=True;
      MessageDlg(
        'Products',
        'Finished',
        TMsgDlgType.mtInformation,
        [TMsgDlgBtn.mbOK],
        ''
      );
    end;
end;

procedure TSimpleBot.scroll_strategyResize(Sender: TObject);
begin
  pnl_strat_ctrl_container.Width := scroll_strategy.Width;
  pnl_strat_ctrl_container.Left := 0;
end;

procedure TSimpleBot.SetupEmail;
var
  LEmail:TEmailSetup;
begin
  //ShowMessage('not implemented');
  LEmail:=TEmailSetup.Create(nil);
  try
    LEmail.ShowModal;
  finally
    LEmail.Free;
  end;
end;

procedure TSimpleBot.EnableEmail;
begin
  ShowMessage('not implemented');
end;

procedure TSimpleBot.SetupLogFile;
begin
  ShowMessage('not implemented');
end;

procedure TSimpleBot.SetupLogTab;
begin
  ShowMessage('not implemented');
end;

procedure TSimpleBot.InitControls;
var
  I: Integer;
begin
  if not FInit then
  begin
    //SimpleBot tab
    pctrl_main.ActivePage:=ts_auth;

    //logger
    multi_log.Options:=multi_log.Options - [ucAuthor];
    multi_log.Title:='Strategy Logger';
    multi_log.Description:='logging for the strategy';
    chk_log_error.Checked:=False;
    chk_log_warn.Checked:=False;
    chk_log_info.Checked:=False;
    multi_log.ControlWidthPercent := 1;
    multi_log.ControlHeightPercent := 1;

    //gunslinger
    Gunslinger1.Visible := False;
    mi_gunslinger.Checked := False;

    //authenticator
    FAuth:=TAuthenticator.Create(Self);
    FAuth.Parent:=ts_auth;
    FAuth.AnchorHorizontalCenterTo(ts_auth);
    FAuth.AnchorVerticalCenterTo(ts_auth);

    //products
    FProducts:=TProducts.Create(Self);
    FProducts.Parent:=ts_product;
    FProducts.Align:=TAlign.alClient;
    FProducts.ControlWidthPercent := 1;
    FProducts.ControlHeightPercent := 1;
    FProducts.ProductFrame.Authenticator:=FAuth.Authenticator;
    FProducts.ProductFrame.OnError:=ProductError;
    FProducts.ProductFrame.OnTick:=ProductTick;
    FProducts.ProductFrame.TickerInterval:=1500;
    FProducts.ProductFrame.LogTickers := True;
    FProductInit:=False;

    //strategy
    ignition_main.OnRequestStart:=CheckCanStart;
    ignition_main.OnRequestStop:=CheckCanStop;
    ignition_main.OnStart:=StartStrategy;
    ignition_main.OnStop:=StopStrategy;
    ignition_main.Status:='Stopped';

    FFundsCtrl:=TSingleLine.Create(Self);
    FFundsCtrl.Name := 'Funds';
    FFundsCtrl.Align:=TAlign.alTop;
    FFundsCtrl.Title:='Funds';
    FFundsCtrl.Description:='specify "how much" quote currency is available to spend trading';
    FFundsCtrl.Height:=300;
    FFundsCtrl.ControlWidthPercent := 0.3;
    FFundsCtrl.Options:=FFundsCtrl.Options - [ucAuthor];
    FFundsCtrl.Text := FLoatToStr(FFunds);

    FLimitFeeCtrl := TSingleLine.Create(Self);
    FLimitFeeCtrl.Name := 'LimitFee';
    FLimitFeeCtrl.Align := TAlign.alTop;
    FLimitFeeCtrl.Title := 'Limit Fee';
    FLimitFeeCtrl.Description := 'specify the fee incurred on a "limit" order (based on your volume under the "orders" -> "fees" tab)';
    FLimitFeeCtrl.Height := 300;
    FLimitFeeCtrl.ControlWidthPercent := 0.30;
    FLimitFeeCtrl.Options := FLimitFeeCtrl.Options - [ucAuthor];
    FLimitFeeCtrl.Text := FloatToStr(FLimitFee);

    FMarketFeeCtrl := TSingleLine.Create(Self);
    FMarketFeeCtrl.Name := 'MarketFee';
    FMarketFeeCtrl.Align := TAlign.alTop;
    FMarketFeeCtrl.Title := 'Market Fee';
    FMarketFeeCtrl.Description := 'specify the fee incurred on a "market" order (based on your volume under the "orders" -> "fees" tab)';
    FMarketFeeCtrl.Height := 300;
    FMarketFeeCtrl.ControlWidthPercent := 0.30;
    FMarketFeeCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FMarketFeeCtrl.Text := FloatToStr(FMarketFee);

    FTimeFrameCtrl := TSlider.Create(Self);
    FTimeFrameCtrl.Name := 'TimeFrame';
    FTimeFrameCtrl.Align := TAlign.alTop;
    FTimeFrameCtrl.Height := 300;
    FTimeFrameCtrl.ControlWidthPercent := 1;
    FTimeFrameCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FTimeFrameCtrl.MinValue := 1;
    FTimeFrameCtrl.MaxValue := 100;
    FTimeFrameCtrl.Title := 'Time Frame';
    FTimeFrameCtrl.Description := 'adjusts the timeframe used to make trades. longer timeframes are less likely to exit earlier at the expense of possibly entering later';
    FTimeFrameCtrl.MinDescr := 'Shorter';
    FTimeFrameCtrl.MaxDescr := 'Longer';

    FPosSizeCtrl := TSlider.Create(Self);
    FPosSizeCtrl.Name := 'PositionSize';
    FPosSizeCtrl.Align := TAlign.alTop;
    FPosSizeCtrl.Height := 300;
    FPosSizeCtrl.ControlWidthPercent := 1;
    FPosSizeCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FPosSizeCtrl.MinValue := 1;
    FPosSizeCtrl.MaxValue := 100;
    FPosSizeCtrl.Title := 'Position Size';
    FPosSizeCtrl.Description := 'adjust how large main positions should be relative to the amount of inventory';
    FPosSizeCtrl.MinDescr := 'Smaller';
    FPosSizeCtrl.MaxDescr := 'Larger';

    FSellPosSizeCtrl := TSlider.Create(Self);
    FSellPosSizeCtrl.Name := 'SellPositionSize';
    FSellPosSizeCtrl.Align := TAlign.alTop;
    FSellPosSizeCtrl.Height := 300;
    FSellPosSizeCtrl.ControlWidthPercent := 1;
    FSellPosSizeCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FSellPosSizeCtrl.MinValue := 1;
    FSellPosSizeCtrl.MaxValue := 100;
    FSellPosSizeCtrl.Title := 'Sell Position Size';
    FSellPosSizeCtrl.Description := 'adjust how large sell positions should be relative to the amount of inventory';
    FSellPosSizeCtrl.MinDescr := 'Smaller';
    FSellPosSizeCtrl.MaxDescr := 'Larger';

    FDCASizeCtrl := TSlider.Create(Self);
    FDCASizeCtrl.Name := 'DCASize';
    FDCASizeCtrl.Align := TAlign.alTop;
    FDCASizeCtrl.Height := 300;
    FDCASizeCtrl.ControlWidthPercent := 1;
    FDCASizeCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FDCASizeCtrl.MinValue := 1;
    FDCASizeCtrl.MaxValue := 100;
    FDCASizeCtrl.Title := 'DCA Size';
    FDCASizeCtrl.Description := 'adjust how large DCA positions should be relative to the amount of funds';
    FDCASizeCtrl.MinDescr := 'Smaller';
    FDCASizeCtrl.MaxDescr := 'Larger';

    FUpFundCtrl := TSlider.Create(Self);
    FUpFundCtrl.Name := 'UpFundUsage';
    FUpFundCtrl.Align := TAlign.alTop;
    FUpFundCtrl.Height := 300;
    FUpFundCtrl.ControlWidthPercent := 1;
    FUpFundCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FUpFundCtrl.MinValue := 0;
    FUpFundCtrl.MaxValue := 100;
    FUpFundCtrl.Title := 'Max Uptrend Fund Usage';
    FUpFundCtrl.Description := 'when trending up, this controls the maximum percentage of funds to use. if funds fall below this, inventory may sell at a loss';
    FUpFundCtrl.MinDescr := '0%';
    FUpFundCtrl.MaxDescr := '100%';

    FDownFundCtrl := TSlider.Create(Self);
    FDownFundCtrl.Name := 'DownFundUsage';
    FDownFundCtrl.Align := TAlign.alTop;
    FDownFundCtrl.Height := 300;
    FDownFundCtrl.ControlWidthPercent := 1;
    FDownFundCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FDownFundCtrl.MinValue := 0;
    FDownFundCtrl.MaxValue := 100;
    FDownFundCtrl.Title := 'Max Downtrend Fund Usage';
    FDownFundCtrl.Description := 'when trending down, this controls the maximum percentage of funds to use. if funds fall below this, inventory may sell at a loss';
    FDownFundCtrl.MinDescr := '0%';
    FDownFundCtrl.MaxDescr := '100%';

    FProfitCtrl := TProfitTarget.Create(Self);
    FProfitCtrl.Name := 'Profit';
    FProfitCtrl.Align := TAlign.alTop;
    FProfitCtrl.Height := 350;
    FProfitCtrl.ControlWidthPercent := 1;
    FProfitCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FProfitCtrl.Title := 'Target Profit';
    FProfitCtrl.Description := 'used to allow the strategy to begin scaling out of a position. sell positions may vary though, depending on market conditions';

    FMinUpRedCtrl := TProfitTarget.Create(Self);
    FMinUpRedCtrl.Name := 'MinUpReduction';
    FMinUpRedCtrl.Align := TAlign.alTop;
    FMinUpRedCtrl.Height := 350;
    FMinUpRedCtrl.ControlWidthPercent := 1;
    FMinUpRedCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FMinUpRedCtrl.Title := 'Minimum Uptrend Reduction';
    FMinUpRedCtrl.Description := 'specify a minimum percentage to lower your cost during an uptrend. if you wish to allow trading without lowering, specify "0" in the "custom" box';

    FMinDownRedCtrl := TProfitTarget.Create(Self);
    FMinDownRedCtrl.Name := 'MinDownReduction';
    FMinDownRedCtrl.Align := TAlign.alTop;
    FMinDownRedCtrl.Height := 350;
    FMinDownRedCtrl.ControlWidthPercent := 1;
    FMinDownRedCtrl.Options := FMarketFeeCtrl.Options - [ucAuthor];
    FMinDownRedCtrl.Title := 'Minimum Downtrend Reduction';
    FMinDownRedCtrl.Description := 'specify a minimum percentage to lower your cost during a downtrend. if you wish to allow trading without lowering, specify "0" in the "custom" box';

    FScaledBuyCtrl := TSingleLine.Create(Self);
    FScaledBuyCtrl.Name := 'ScaledBuy';
    FScaledBuyCtrl.Align := TAlign.alTop;
    FScaledBuyCtrl.Title := 'Scaled Buy %';
    FScaledBuyCtrl.Description := 'sets the buy scaling percentage to use as more inventory is accumulated. as inventory is accumulated this scale is proportionally applied to orders (future order are larger/smaller as inventory grows)';
    FScaledBuyCtrl.Height := 300;
    FScaledBuyCtrl.ControlWidthPercent := 0.30;
    FScaledBuyCtrl.Options := FLimitFeeCtrl.Options - [ucAuthor];

    FBuyOnUpCtrl := TBool.Create(Self);
    FBuyOnUpCtrl.Name := 'BuyUp';
    FBuyOnUpCtrl.Align := TAlign.alTop;
    FBuyOnUpCtrl.Title := 'Buy on Uptrend';
    FBuyOnUpCtrl.Description := 'when enabled, DCA buys will be performed on the uptrend';
    FBuyOnUpCtrl.Height := 300;
    FBuyOnUpCtrl.ControlWidthPercent := 0.30;
    FBuyOnUpCtrl.Options := FLimitFeeCtrl.Options - [ucAuthor];

    FBuyOnDownCtrl := TBool.Create(Self);
    FBuyOnDownCtrl.Name := 'BuyDown';
    FBuyOnDownCtrl.Align := TAlign.alTop;
    FBuyOnDownCtrl.Title := 'Buy on Downtrend';
    FBuyOnDownCtrl.Description := 'when enabled, DCA buys will be performed on the downtrend';
    FBuyOnDownCtrl.Height := 300;
    FBuyOnDownCtrl.ControlWidthPercent := 0.30;
    FBuyOnDownCtrl.Options := FLimitFeeCtrl.Options - [ucAuthor];

    FIgnoreOnUpCtrl := TBool.Create(Self);
    FIgnoreOnUpCtrl.Name := 'IgnoreUp';
    FIgnoreOnUpCtrl.Align := TAlign.alTop;
    FIgnoreOnUpCtrl.Title := 'Ignore Profit on Uptrend';
    FIgnoreOnUpCtrl.Description := 'when enabled, sells will be made without taking profit into consideration on the uptrend';
    FIgnoreOnUpCtrl.Height := 300;
    FIgnoreOnUpCtrl.ControlWidthPercent := 0.30;
    FIgnoreOnUpCtrl.Options := FLimitFeeCtrl.Options - [ucAuthor];

    FIgnoreOnDownCtrl := TBool.Create(Self);
    FIgnoreOnDownCtrl.Name := 'IgnoreDown';
    FIgnoreOnDownCtrl.Align := TAlign.alTop;
    FIgnoreOnDownCtrl.Title := 'Ignore Profit on Uptrend';
    FIgnoreOnDownCtrl.Description := 'when enabled, sells will be made without taking profit into consideration on the downtrend';
    FIgnoreOnDownCtrl.Height := 300;
    FIgnoreOnDownCtrl.ControlWidthPercent := 0.30;
    FIgnoreOnDownCtrl.Options := FLimitFeeCtrl.Options - [ucAuthor];

    FBuyBobberIn := TBool.Create(Self);
    FBuyBobberIn.Name := 'BuyBobberIn';
    FBuyBobberIn.Align := TAlign.alTop;
    FBuyBobberIn.Title := 'Buy When Bobber in Position';
    FBuyBobberIn.Description := 'when enabled, DCA buys will be performed when the bobber strategy is in position';
    FBuyBobberIn.Height := 300;
    FBuyBobberIn.ControlWidthPercent := 0.30;
    FBuyBobberIn.Options := FLimitFeeCtrl.Options - [ucAuthor];

    FBuyBobberOut := TBool.Create(Self);
    FBuyBobberOut.Name := 'BuyBobberOut';
    FBuyBobberOut.Align := TAlign.alTop;
    FBuyBobberOut.Title := 'Buy When Bobber out Position';
    FBuyBobberOut.Description := 'when enabled, DCA buys will be performed when the bobber strategy is out of position';
    FBuyBobberOut.Height := 300;
    FBuyBobberOut.ControlWidthPercent := 0.30;
    FBuyBobberOut.Options := FLimitFeeCtrl.Options - [ucAuthor];

    FBobberCtrl := TUserControl.Create(Self);
    FBobberCtrl.Name := 'Bobber';
    FBobberCtrl.Align := TAlign.alTop;
    FBobberCtrl.Title := 'Floating Position';
    FBobberCtrl.Description := 'holds a floating position of inventory based on specified rules';
    FBobberCtrl.Height := 600;
    FBobberCtrl.ControlWidthPercent := 1;
    FBobberCtrl.Options := FLimitFeeCtrl.Options - [ucAuthor];
    FBobberInnerCtrl := TConfigureBobber.Create(FBobberCtrl);
    FBobberInnerCtrl.scroll_controls.Parent := FBobberCtrl.pnl_control; //not normal way to do this but didn't want to dupe ui stuff
    FBobberInnerCtrl.edit_name.Text := 'BobberInner';

    //reverse order (bottom first to show)
    FBobberCtrl.Parent := pnl_strat_ctrl_container;
    FBuyBobberOut.Parent := pnl_strat_ctrl_container;
    FBuyBobberIn.Parent := pnl_strat_ctrl_container;
    FIgnoreOnDownCtrl.Parent := pnl_strat_ctrl_container;
    FIgnoreOnUpCtrl.Parent := pnl_strat_ctrl_container;
    FBuyOnDownCtrl.Parent := pnl_strat_ctrl_container;
    FBuyOnUpCtrl.Parent := pnl_strat_ctrl_container;
    FScaledBuyCtrl.Parent := pnl_strat_ctrl_container;
    FMinDownRedCtrl.Parent := pnl_strat_ctrl_container;
    FMinUpRedCtrl.Parent := pnl_strat_ctrl_container;
    FProfitCtrl.Parent := pnl_strat_ctrl_container;
    FSellPosSizeCtrl.Parent := pnl_strat_ctrl_container;
    FDownFundCtrl.Parent := pnl_strat_ctrl_container;
    FUpFundCtrl.Parent := pnl_strat_ctrl_container;
    FDCASizeCtrl.Parent := pnl_strat_ctrl_container;
    FPosSizeCtrl.Parent := pnl_strat_ctrl_container;
    FTimeFrameCtrl.Parent := pnl_strat_ctrl_container;
    FLimitFeeCtrl.Parent := pnl_strat_ctrl_container;
    FMarketFeeCtrl.Parent := pnl_strat_ctrl_container;
    FFundsCtrl.Parent := pnl_strat_ctrl_container;

    //hot keys
    for I := 0 to Pred(Self.ControlCount) do
      if Assigned(Self.Controls[I]) then
        Self.Controls[I].AddHandlerOnKeyDown(FormKeyDown);

    if EXPIRED then
      status_main.Panels[0].Text := 'LICENSE EXPIRED';
    FInit:=True;

    json_main.Restore;
    FLoaded := True;
  end;
end;

procedure TSimpleBot.InitFundsLedger;
begin
  FFundsBalance := FEngine.FundsLedger.Balance;
  FFunds := StrToFloatDef(FFundsCtrl.Text, 0.0);
end;

procedure TSimpleBot.EnableAutoStart;
begin
  //nothing to do right now since we key of the ui control, may change
end;

procedure TSimpleBot.PreloadTickers;
var
  LFile: TOpenDialog;
  I: Integer;
  LGDAXTick : IGDAXTicker;
  LTick : ITicker;
  LError, LInput: String;
  LParser : TTickerParser;
  LDecimation: Single;
  LLogError,
  LLogInfo,
  LLogWarn: Boolean;
begin
  LLogError := chk_log_error.Checked;
  chk_log_error.Checked := False;

  LLogInfo := chk_log_info.Checked;
  chk_log_info.Checked := False;

  LLogWarn := chk_log_warn.Checked;
  chk_log_warn.Checked := False;

  try
    if MessageDlg(
        'Preload Tickers',
        'Would you like to preload ticker data?',
        TMsgDlgType.mtConfirmation,
        [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes],
        ''
      ) = mrYes
    then
    begin
      LFile := TOpenDialog.Create(nil);
      LFile.Options := LFile.Options + [TOpenOption.ofAllowMultiSelect];

      if LFile.Execute then
      begin
        //prompt for decimation, default to none if cancelled
        LDecimation := 0;
        if DefaultInputDialog(
          'Decimation %',
          'Input a decimation percent:',
          False,
          LInput
        )
        then
          LDecimation := StrToFloatDef(LInput, 0);

        LParser := TTickerParser.Create(nil);
        LParser.LoadFiles(LFile.Files, LDecimation);

        for I := 0 to Pred(LParser.LoadedTickers.Count) do
        begin
          //create a gdax ticker
          LGDAXTick := TPreloadTick.Create;

          //assign the selected product
          LGDAXTick.Product := FProducts.ProductFrame.Product;

          //attempt to load from json
          if not LGDAXTick.LoadFromJSON(LParser.LoadedTickers[I].JSON, LError) then
            raise Exception.Create(LError);

          //make a delilah ticker
          LTick := TGDAXTickerImpl.Create(LGDAXTick);

          //feed the engine (disregard errors)
          FEngine.Feed(LTick, LError);

          //cleanup local since we reuse this per iteration
          LGDAXTick := nil;
          LTick := nil;

          if I mod 100 = 0 then
          begin
            ignition_main.lbl_status.Caption := 'loaded: ' + IntToStr(Succ(I));
            Application.ProcessMessages;
          end;
        end;

        LParser.Free;
      end;
      LFile.Free;
    end;
  finally
    chk_log_error.Checked := LLogError;
    chk_log_info.Checked := LLogInfo;
    chk_log_warn.Checked := LLogWarn;
  end;
end;

procedure TSimpleBot.ValidateLicense;
begin
  //don't terminate if our authenticator isn't initialized
  if not Assigned(FAuth) or (FAuth.Authenticator.Key.IsEmpty) or (FAuth.Authenticator.Secret.IsEmpty) then
    Exit;

  HashCreds(FAuth.Key, FAuth.Secret);
  ValidateOrKill;
end;

procedure TSimpleBot.LaunchWatchdog;
begin
  //todo - below is an implementation for monitoring, but needs either
  //       changes in ezthreads to check for latest compiler (needs generic.collection)
  //       or need to finish porting this to be trunk compatible

//  procedure Sniffer(const AThread : IEZThread);
//  begin
//    while Assigned(Application) and (not Application.Terminated) then
//    begin
//      if SimpleBot.FEngine.EngineState = TEngineState.esStarted then
//      begin
//        HashCreds(SimpleBot.FAuth.Key, SimpleBot.FAuth.Secret);
//        ValidateOrKill;
//        Sleep(30000);
//      end;
//    end;
//  end;
//
//begin
//  //create a watchdog thread to have secondary license checking while running
//  FDoggy := NewEZThread;
//  FDoggy
//    .Setup(Sniffer)
//    .Start;
end;

procedure TSimpleBot.HandleAutoStart;
var
  LError: String;
begin
  //first try to load the product regardless of auto start
  if not LoadProduct(LError) then
  begin
    LogError('HandleAutoStart::' + LError);
    Exit;
  end;

  //if not enabled, then bail
  if not mi_auto_start.Checked then
    Exit;

  //use the button click to trigger proper state
  ignition_main.btn_start.Click();
end;

function TSimpleBot.LoadProduct(out Error: String): Boolean;
var
  I: Integer;
begin
  Result := False;

  if Trim(FTradingPair) = '' then
  begin
    Error := 'LoadProduct::unable to auto start';
    Exit;
  end;

  //ensure we've loaded products
  FProducts.ProductFrame.Authenticator:=FAuth.Authenticator;
  FProducts.ProductFrame.Init;
  FProductInit := True;

  //get the index of the pair
  I := FProducts.ProductFrame.combo_products.Items.IndexOf(FTradingPair);

  if I < 0 then
  begin
    Error := 'LoadProduct::trading pair not found [' + FTradingPair + ']';
    Exit;
  end;

  //set the product
  FProducts.ProductFrame.combo_products.ItemIndex := I;
  Result := True;
end;

procedure TSimpleBot.SimulateStrategy;
var
  LSim : TTickerParser;
begin
  ValidateLicense;

  //we can't run while we're running the full strategy because I cheated and
  //copy pasta'd this to get simulation working in simplebot
  if FEngine.EngineState = esStarted then
  begin
    MessageDlg(
      'Simulator cannot run while main strategy is running.',
      TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK],
      -1
    );

    Exit;
  end;

  //create a simulation window
  LSim := TTickerParser.Create(nil);
  try
    LSim.OnStartSimulate := DoOnStartSim;
    LSIm.OnFinishSimulate := DoOnFinishSim;
    LSim.Position := poMainFormCenter;
    LSim.Width := Trunc(Self.Width * 0.8);
    LSim.Height := Trunc(Self.Height * 0.8);
    LSim.DemoMode := True;
    AddStrategiesToSim(LSim);
    LSim.ShowModal;
  finally
    LSim.Free;
  end;
end;

procedure TSimpleBot.DoOnStartSim(Sender: TObject);
begin
  //store the current engine to temp
  FTempEngine := FEngine;
  FEngine := TTickerParser(Sender).Engine;
end;

procedure TSimpleBot.DoOnFinishSim(Sender: TObject);
begin
  FEngine := FTempEngine;
  FTempEngine := nil
end;

procedure TSimpleBot.AddStrategiesToSim(const ASim: TTickerParser);
var
  LSellForMonies : ITierStrategyGDAX;
  LAccelHighest : IAccelerationStrategy;
  LSellForMoniesLow: ITierStrategyGDAX;
  LAccelLow: IAccelerationStrategy;
  LSellForMoniesLowest: ITierStrategyGDAX;
  LAccelLowest: IAccelerationStrategy;
  LHighDCAWindow: Cardinal;
  LScaledBuy: Extended;
  LBobber : IGDAXBobberStrategy;
const
  DCA_PERC = 0.0333;
  LEAD_POS_PERC = 0.75;
begin
  //shameless copy paste from start strategy, downside is now there are
  //two places to copy and paste to/from.... oh well


  //set the engine specific data
  ASim.edit_funds.Text := FLoatToStr(StrToFloatDef(FFundsCtrl.Text, 0.0));
  ASim.edit_fee_perc.Text := FloatToStr(StrToFloatDef(FMarketFeeCtrl.Text, 0.0));

  //preload some ui settings if a product is specified
  if Assigned(FProducts.ProductFrame.Product) then
  begin
    ASim.edit_product_min.Text := FloatToStr(FProducts.ProductFrame.Product.BaseMinSize);
    ASim.edit_product.Text := FProducts.ProductFrame.Product.ID;
  end;

  //init strategies
  LSellForMoniesLowest := TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);
  LAccelLowest := TGDAXAccelerationStrategyImpl.Create(LogInfo,LogError,LogInfo);
  LSellForMoniesLow := TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);
  LAccelLow := TGDAXAccelerationStrategyImpl.Create(LogInfo,LogError,LogInfo);
  LSellForMonies := TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);
  LAccelHighest := TGDAXAccelerationStrategyImpl.Create(LogInfo,LogError,LogInfo);
  LBobber := TGDAXBobberStrategyImpl.Create(LogInfo, LogError, LogInfo);

  //make sure we have the latest settings
  InterpolateTimeSetting(FHighWindowSize);
  InterpolatePositionSetting(FHighPosSize);
  InterpolateSellPositionSetting(FHighSellPosSize);
  InterpolateDCAPositionSetting(FHighDCASize);
  FHighTakeProfit := FProfitCtrl.Percent;
  FHighUpMinRed := FMinUpRedCtrl.Percent;
  LHighDCAWindow := Round(FHighWindowSize * DCA_PERC);
  LScaledBuy := StrToFloatDef(FScaledBuyCtrl.Text, 0);

  //----------------------------------------------------------------------------
  //configure the bobberstrategy
  FBobberInnerCtrl.btn_saveClick(Self); //bad practice, but meh
  LBobber.Funds := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).Funds;
  LBobber.Threshold := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).Threshold;
  LBobber.AdjustAnchorThreshold := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).AdjustAnchorThreshold;
  LBobber.ProfitThreshold := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).ProfitThreshold;
  LBobber.ProfitPercent := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).ProfitPercent;
  LBobber.FundsMode := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).FundsMode;
  LBobber.UseLimitBuy := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).UseLimitBuy;
  LBobber.UseLimitSell := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).UseLimitSell;
  FBobber := LBobber;

  //----------------------------------------------------------------------------
  //configure the sell for monies to sell for higher profits
  LSellForMoniesLowest.UseMarketBuy :=  FUseMarketBuy;
  LSellForMoniesLowest.UseMarketSell := FUseMarketSell;
  LSellForMoniesLowest.ChannelStrategy.WindowSizeInMilli := Round(LHighDCAWindow / 3);
  LSellForMoniesLowest.AvoidChop := False;
  LSellForMoniesLowest.GTFOPerc := 0;
  LSellForMoniesLowest.SmallTierPerc := (FHighDCASize / 3) / 2;
  LSellForMoniesLowest.MidTierPerc := (FHighDCASize / 3) / 2;
  LSellForMoniesLowest.LargeTierPerc := FHighDCASize / 3;
  LSellForMoniesLowest.SmallTierSellPerc := (FHighSellPosSize / 3) / 2;
  LSellForMoniesLowest.MidTierSellPerc := (FHighSellPosSize / 3) / 2;
  LSellForMoniesLowest.LargeTierSellPerc := FHighSellPosSize / 3;
  LSellForMoniesLowest.IgnoreOnlyProfitThreshold := 0;
  LSellForMoniesLowest.LimitFee := FLimitFee;
  LSellForMoniesLowest.MarketFee := FMarketFee;
  LSellForMoniesLowest.OnlyProfit := True;
  LSellForMoniesLowest.MinProfit := FHighTakeProfit / 3;
  LSellForMoniesLowest.MaxScaledBuyPerc := LScaledBuy;
  LSellForMoniesLowest.FixedProfit := False; //todo - add?
  FLowestTier := LSellForMoniesLowest;

  //configure the lowest acceleration
  LAccelLowest.OnlyBuy := True;
  LAccelLowest.WindowSizeInMilli := Round(FHighWindowSize / 3);
  LAccelLowest.LeadStartPercent := 0.635;
  LAccelLowest.LeadEndPercent := 1.0;

  if FBuyOnUpCtrl.Checked then
    LAccelLowest.PositionPercent := (FHighPosSize / 3) * LEAD_POS_PERC
  else
    LAccelLowest.PositionPercent := 0;

  if FBuyOnDownCtrl.Checked then
    LAccelLowest.RiskyPositionPercent := FHighPosSize / 3
  else
    LAccelLowest.RiskyPositionPercent := 0;

  LAccelLowest.CrossThresholdPercent := 3.5;
  LAccelLowest.CrossDownThresholdPercent := 2;
  //LAccelLowest.AvoidChopThreshold := 0.0000025;//0.035; (old price based number)
  LAccelLowest.UseDynamicPositions := False; //fixed

  //now setup the tier strategy with a pointer to the acceleration "parent"
  FLowestAccelStrategy := LAccelLowest;
  LSellForMoniesLowest.ActiveCriteria := GetLowestAccelCriteria;
  LSellForMoniesLowest.ActiveCriteriaData := @FLowestAccelStrategy;

  //----------------------------------------------------------------------------
  //configure the sell for monies to sell for higher profits
  LSellForMoniesLow.UseMarketBuy := FUseMarketBuy;
  LSellForMoniesLow.UseMarketSell := FUseMarketSell;
  LSellForMoniesLow.ChannelStrategy.WindowSizeInMilli := Round(LHighDCAWindow / 2);
  LSellForMoniesLow.AvoidChop := False;
  LSellForMoniesLow.GTFOPerc := 0;
  LSellForMoniesLow.SmallTierPerc := (FHighDCASize / 2) / 2;
  LSellForMoniesLow.MidTierPerc := (FHighDCASize / 2) / 2;
  LSellForMoniesLow.LargeTierPerc := FHighDCASize / 2;
  LSellForMoniesLow.SmallTierSellPerc := (FHighSellPosSize / 2) / 2;
  LSellForMoniesLow.MidTierSellPerc := (FHighSellPosSize / 2) / 2;
  LSellForMoniesLow.LargeTierSellPerc := FHighSellPosSize / 2;
  LSellForMoniesLow.IgnoreOnlyProfitThreshold := 0;
  LSellForMoniesLow.LimitFee := FLimitFee;
  LSellForMoniesLow.MarketFee := FMarketFee;
  LSellForMoniesLow.OnlyProfit := True;
  LSellForMoniesLow.MinProfit := FHighTakeProfit / 2;
  LSellForMoniesLow.MaxScaledBuyPerc := LScaledBuy;
  LSellForMoniesLow.FixedProfit := False; //todo - add?
  FLowTier := LSellForMoniesLow;

  //configure the low acceleration
  LAccelLow.OnlyBuy := True;
  LAccelLow.WindowSizeInMilli := Round(FHighWindowSize / 2);
  LAccelLow.LeadStartPercent := 0.635;
  LAccelLow.LeadEndPercent := 1.0;

  if FBuyOnUpCtrl.Checked then
    LAccelLow.PositionPercent := (FHighPosSize / 2) * LEAD_POS_PERC
  else
    LAccelLow.PositionPercent := 0;

  if FBuyOnDownCtrl.Checked then
    LAccelLow.RiskyPositionPercent := FHighPosSize / 2
  else
    LAccelLow.RiskyPositionPercent := 0;

  LAccelLow.CrossThresholdPercent := 3.5;
  LAccelLow.CrossDownThresholdPercent := 2;
  //LAccelLow.AvoidChopThreshold := 0.000003;//0.035; (old price based number)
  LAccelLow.UseDynamicPositions := False; //fixed

  //now setup the tier strategy with a pointer to the acceleration "parent"
  FLowAccelStrategy := LAccelLow;
  LSellForMoniesLow.ActiveCriteria := GetLowAccelCriteria;
  LSellForMoniesLow.ActiveCriteriaData := @FLowAccelStrategy;

  //----------------------------------------------------------------------------
  //configure the sell for monies to sell for higher profits
  LSellForMonies.UseMarketBuy := FUseMarketBuy;
  LSellForMonies.UseMarketSell := FUseMarketSell;
  LSellForMonies.ChannelStrategy.WindowSizeInMilli := LHighDCAWindow;
  LSellForMonies.AvoidChop := False;
  LSellForMonies.GTFOPerc := 0;
  LSellForMonies.SmallTierPerc := FHighDCASize / 2;
  LSellForMonies.MidTierPerc := FHighDCASize / 2;
  LSellForMonies.LargeTierPerc := FHighDCASize;
  LSellForMonies.SmallTierSellPerc := (FHighSellPosSize / 2);
  LSellForMonies.MidTierSellPerc := (FHighSellPosSize / 2);
  LSellForMonies.LargeTierSellPerc := FHighSellPosSize;
  LSellForMonies.IgnoreOnlyProfitThreshold := 0;
  LSellForMonies.LimitFee := FLimitFee;
  LSellForMonies.MarketFee := FMarketFee;
  LSellForMonies.OnlyProfit := True;
  LSellForMonies.MinProfit := FHighTakeProfit;
  LSellForMonies.MaxScaledBuyPerc := LScaledBuy;
  LSellForMonies.FixedProfit := True; //todo - add?
  FTier := LSellForMonies;

  //configure the highest acceleration
  LAccelHighest.OnlyBuy := True;
  LAccelHighest.WindowSizeInMilli := FHighWindowSize;
  LAccelHighest.LeadStartPercent := 0.635;
  LAccelHighest.LeadEndPercent := 1.0;

  if FBuyOnUpCtrl.Checked then
    LAccelHighest.PositionPercent := FHighPosSize * LEAD_POS_PERC
  else
    LAccelHighest.PositionPercent := 0;

  if FBuyOnDownCtrl.Checked then
    LAccelHighest.RiskyPositionPercent := FHighPosSize
  else
    LAccelHighest.RiskyPositionPercent := 0;

  LAccelHighest.CrossThresholdPercent := 3.5;
  LAccelHighest.CrossDownThresholdPercent := 2;
  //LAccelHighest.AvoidChopThreshold := 0.0000035;//0.035; (old price based number)
  LAccelHighest.UseDynamicPositions := False; //fixed

  //now setup the tier strategy with a pointer to the acceleration "parent"
  FAccelStrategy := LAccelHighest;
  LSellForMonies.ActiveCriteria := GetAccelCriteria;
  LSellForMonies.ActiveCriteriaData := @FAccelStrategy;

  //add all strategies
  ASim.Strategies.Add(LAccelLowest);
  ASim.Strategies.Add(LSellForMoniesLowest);
  ASim.Strategies.Add(LAccelLow);
  ASim.Strategies.Add(LSellForMoniesLow);
  ASim.Strategies.Add(LAccelHighest);
  ASim.Strategies.Add(LSellForMonies);
  ASim.Strategies.Add(LBobber);
end;

procedure TSimpleBot.CheckCanStart(Sender: TObject; var Continue: Boolean);
begin
  //products need to be initialized before we can start the strategy
  if not FProductInit then
  begin
    Continue:=False;
    pctrl_main.ActivePage:=ts_product;
    MessageDlg(
      'Invalid Product',
      'please setup the product page',
      TMsgDlgType.mtWarning,
      [TMsgDlgBtn.mbOK],
      ''
    );
    pctrl_main.OnChange(ts_product);
  end;
  //make sure our authenticator has some values
  if FAuth.Secret.IsEmpty or FAuth.Passphrase.IsEmpty or FAuth.Key.IsEmpty then
  begin
    Continue:=False;
    pctrl_main.ActivePage:=ts_auth;
    MessageDlg(
      'Invalid Authenticator',
      'please setup the authenticator page',
      TMsgDlgType.mtWarning,
      [TMsgDlgBtn.mbOK],
      ''
    );
    pctrl_main.OnChange(ts_auth);
    Exit;
  end;
  //now make sure we actually have a product selected
  if not Assigned(FProducts.ProductFrame.Product) then
  begin
    Continue:=False;
    pctrl_main.ActivePage:=ts_product;
    MessageDlg(
      'Invalid Product',
      'please select a product',
      TMsgDlgType.mtWarning,
      [TMsgDlgBtn.mbOK],
      ''
    );
    pctrl_main.OnChange(ts_product);
  end;
end;

procedure TSimpleBot.CheckCanStop(Sender: TObject; var Continue: Boolean);
var
  LError:String;
begin
  if not FEngine.Stop(LError) then
  begin
    Continue:=False;
    LogError(LError);
  end
  else
    Continue:=True;
end;

procedure TSimpleBot.StartStrategy(Sender: TObject);
var
  LError : String;
  LSellForMonies : ITierStrategyGDAX;
  LAccelHighest : IAccelerationStrategy;
  LSellForMoniesLow: ITierStrategyGDAX;
  LAccelLow: IAccelerationStrategy;
  LSellForMoniesLowest: ITierStrategyGDAX;
  LAccelLowest: IAccelerationStrategy;
  LHighDCAWindow: Cardinal;
  LScaledBuy: Extended;
  LBobber : IGDAXBobberStrategy;
const
  DCA_PERC = 0.0333;
  LEAD_POS_PERC = 0.75;
begin
  ValidateLicense;

  //clear chart source
  chart_source.Clear;

  //set the engine specific data
  InitFundsLedger;
  FLimitFee := StrToFloatDef(FLimitFeeCtrl.Text, 0.0);
  FMarketFee := StrToFloatDef(FMarketFeeCtrl.Text, 0.0);

  //init strategies
  LSellForMoniesLowest := TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);
  LAccelLowest := TGDAXAccelerationStrategyImpl.Create(LogInfo,LogError,LogInfo);
  LSellForMoniesLow := TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);
  LAccelLow := TGDAXAccelerationStrategyImpl.Create(LogInfo,LogError,LogInfo);
  LSellForMonies := TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);
  LAccelHighest := TGDAXAccelerationStrategyImpl.Create(LogInfo,LogError,LogInfo);
  LBobber := TGDAXBobberStrategyImpl.Create(LogInfo, LogError, LogInfo);

  //make sure we have the latest settings
  InterpolateTimeSetting(FHighWindowSize);
  InterpolatePositionSetting(FHighPosSize);
  InterpolateSellPositionSetting(FHighSellPosSize);
  InterpolateDCAPositionSetting(FHighDCASize);
  FHighTakeProfit := FProfitCtrl.Percent;
  FHighUpMinRed := FMinUpRedCtrl.Percent;
  LHighDCAWindow := Round(FHighWindowSize * DCA_PERC);
  LScaledBuy := StrToFloatDef(FScaledBuyCtrl.Text, 0);

  //----------------------------------------------------------------------------
  //configure the bobberstrategy
  FBobberInnerCtrl.btn_saveClick(Self); //bad practice, but meh
  LBobber.Funds := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).Funds;
  LBobber.Threshold := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).Threshold;
  LBobber.AdjustAnchorThreshold := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).AdjustAnchorThreshold;
  LBobber.ProfitThreshold := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).ProfitThreshold;
  LBobber.ProfitPercent := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).ProfitPercent;
  LBobber.FundsMode := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).FundsMode;
  LBobber.UseLimitBuy := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).UseLimitBuy;
  LBobber.UseLimitSell := (FBobberInnerCtrl.Strategy as IGDAXBobberStrategy).UseLimitSell;
  FBobber := LBobber;

  //----------------------------------------------------------------------------
  //configure the sell for monies to sell for higher profits
  LSellForMoniesLowest.UseMarketBuy :=  FUseMarketBuy;
  LSellForMoniesLowest.UseMarketSell := FUseMarketSell;
  LSellForMoniesLowest.ChannelStrategy.WindowSizeInMilli := Round(LHighDCAWindow / 3);
  LSellForMoniesLowest.AvoidChop := False;
  LSellForMoniesLowest.GTFOPerc := 0;
  LSellForMoniesLowest.SmallTierPerc := (FHighDCASize / 3) / 2;
  LSellForMoniesLowest.MidTierPerc := (FHighDCASize / 3) / 2;
  LSellForMoniesLowest.LargeTierPerc := FHighDCASize / 3;
  LSellForMoniesLowest.SmallTierSellPerc := (FHighSellPosSize / 3) / 2;
  LSellForMoniesLowest.MidTierSellPerc := (FHighSellPosSize / 3) / 2;
  LSellForMoniesLowest.LargeTierSellPerc := FHighSellPosSize / 3;
  LSellForMoniesLowest.IgnoreOnlyProfitThreshold := 0;
  LSellForMoniesLowest.LimitFee := FLimitFee;
  LSellForMoniesLowest.MarketFee := FMarketFee;
  LSellForMoniesLowest.OnlyProfit := True;
  LSellForMoniesLowest.MinProfit := FHighTakeProfit / 3;
  LSellForMoniesLowest.MaxScaledBuyPerc := LScaledBuy;
  LSellForMoniesLowest.FixedProfit := False; //todo - add?
  FLowestTier := LSellForMoniesLowest;

  //configure the lowest acceleration
  LAccelLowest.OnlyBuy := True;
  LAccelLowest.WindowSizeInMilli := Round(FHighWindowSize / 3);
  LAccelLowest.LeadStartPercent := 0.635;
  LAccelLowest.LeadEndPercent := 1.0;

  if FBuyOnUpCtrl.Checked then
    LAccelLowest.PositionPercent := (FHighPosSize / 3) * LEAD_POS_PERC
  else
    LAccelLowest.PositionPercent := 0;

  if FBuyOnDownCtrl.Checked then
    LAccelLowest.RiskyPositionPercent := FHighPosSize / 3
  else
    LAccelLowest.RiskyPositionPercent := 0;

  LAccelLowest.CrossThresholdPercent := 3.5;
  LAccelLowest.CrossDownThresholdPercent := 2;
  //LAccelLowest.AvoidChopThreshold := 0.0000025;//0.035; (old price based number)
  LAccelLowest.UseDynamicPositions := False; //fixed

  //now setup the tier strategy with a pointer to the acceleration "parent"
  FLowestAccelStrategy := LAccelLowest;
  LSellForMoniesLowest.ActiveCriteria := GetLowestAccelCriteria;
  LSellForMoniesLowest.ActiveCriteriaData := @FLowestAccelStrategy;

  //----------------------------------------------------------------------------
  //configure the sell for monies to sell for higher profits
  LSellForMoniesLow.UseMarketBuy := FUseMarketBuy;
  LSellForMoniesLow.UseMarketSell := FUseMarketSell;
  LSellForMoniesLow.ChannelStrategy.WindowSizeInMilli := Round(LHighDCAWindow / 2);
  LSellForMoniesLow.AvoidChop := False;
  LSellForMoniesLow.GTFOPerc := 0;
  LSellForMoniesLow.SmallTierPerc := (FHighDCASize / 2) / 2;
  LSellForMoniesLow.MidTierPerc := (FHighDCASize / 2) / 2;
  LSellForMoniesLow.LargeTierPerc := FHighDCASize / 2;
  LSellForMoniesLow.SmallTierSellPerc := (FHighSellPosSize / 2) / 2;
  LSellForMoniesLow.MidTierSellPerc := (FHighSellPosSize / 2) / 2;
  LSellForMoniesLow.LargeTierSellPerc := FHighSellPosSize / 2;
  LSellForMoniesLow.IgnoreOnlyProfitThreshold := 0;
  LSellForMoniesLow.LimitFee := FLimitFee;
  LSellForMoniesLow.MarketFee := FMarketFee;
  LSellForMoniesLow.OnlyProfit := True;
  LSellForMoniesLow.MinProfit := FHighTakeProfit / 2;
  LSellForMoniesLow.MaxScaledBuyPerc := LScaledBuy;
  LSellForMoniesLow.FixedProfit := False; //todo - add?
  FLowTier := LSellForMoniesLow;

  //configure the low acceleration
  LAccelLow.OnlyBuy := True;
  LAccelLow.WindowSizeInMilli := Round(FHighWindowSize / 2);
  LAccelLow.LeadStartPercent := 0.635;
  LAccelLow.LeadEndPercent := 1.0;

  if FBuyOnUpCtrl.Checked then
    LAccelLow.PositionPercent := (FHighPosSize / 2) * LEAD_POS_PERC
  else
    LAccelLow.PositionPercent := 0;

  if FBuyOnDownCtrl.Checked then
    LAccelLow.RiskyPositionPercent := FHighPosSize / 2
  else
    LAccelLow.RiskyPositionPercent := 0;

  LAccelLow.CrossThresholdPercent := 3.5;
  LAccelLow.CrossDownThresholdPercent := 2;
  //LAccelLow.AvoidChopThreshold := 0.000003;//0.035; (old price based number)
  LAccelLow.UseDynamicPositions := False; //fixed

  //now setup the tier strategy with a pointer to the acceleration "parent"
  FLowAccelStrategy := LAccelLow;
  LSellForMoniesLow.ActiveCriteria := GetLowAccelCriteria;
  LSellForMoniesLow.ActiveCriteriaData := @FLowAccelStrategy;

  //----------------------------------------------------------------------------
  //configure the sell for monies to sell for higher profits
  LSellForMonies.UseMarketBuy := FUseMarketBuy;
  LSellForMonies.UseMarketSell := FUseMarketSell;
  LSellForMonies.ChannelStrategy.WindowSizeInMilli := LHighDCAWindow;
  LSellForMonies.AvoidChop := False;
  LSellForMonies.GTFOPerc := 0;
  LSellForMonies.SmallTierPerc := FHighDCASize / 2;
  LSellForMonies.MidTierPerc := FHighDCASize / 2;
  LSellForMonies.LargeTierPerc := FHighDCASize;
  LSellForMonies.SmallTierSellPerc := (FHighSellPosSize / 2);
  LSellForMonies.MidTierSellPerc := (FHighSellPosSize / 2);
  LSellForMonies.LargeTierSellPerc := FHighSellPosSize;
  LSellForMonies.IgnoreOnlyProfitThreshold := 0;
  LSellForMonies.LimitFee := FLimitFee;
  LSellForMonies.MarketFee := FMarketFee;
  LSellForMonies.OnlyProfit := True;
  LSellForMonies.MinProfit := FHighTakeProfit;
  LSellForMonies.MaxScaledBuyPerc := LScaledBuy;
  LSellForMonies.FixedProfit := True; //todo - add?
  FTier := LSellForMonies;

  //configure the highest acceleration
  LAccelHighest.OnlyBuy := True;
  LAccelHighest.WindowSizeInMilli := FHighWindowSize;
  LAccelHighest.LeadStartPercent := 0.635;
  LAccelHighest.LeadEndPercent := 1.0;

  if FBuyOnUpCtrl.Checked then
    LAccelHighest.PositionPercent := FHighPosSize * LEAD_POS_PERC
  else
    LAccelHighest.PositionPercent := 0;

  if FBuyOnDownCtrl.Checked then
    LAccelHighest.RiskyPositionPercent := FHighPosSize
  else
    LAccelHighest.RiskyPositionPercent := 0;

  LAccelHighest.CrossThresholdPercent := 3.5;
  LAccelHighest.CrossDownThresholdPercent := 2;
  //LAccelHighest.AvoidChopThreshold := 0.0000035;//0.035; (old price based number)
  LAccelHighest.UseDynamicPositions := False; //fixed

  //now setup the tier strategy with a pointer to the acceleration "parent"
  FAccelStrategy := LAccelHighest;
  LSellForMonies.ActiveCriteria := GetAccelCriteria;
  LSellForMonies.ActiveCriteriaData := @FAccelStrategy;

  //also update the strategy to hold any balance if a restart occurred
  FAccelStrategy.UpdateCurrentPosition(FEngine.AvailableInventory);

  //add all strategies
  FEngine.Strategies.Add(LAccelLowest);
  FEngine.Strategies.Add(LSellForMoniesLowest);
  FEngine.Strategies.Add(LAccelLow);
  FEngine.Strategies.Add(LSellForMoniesLow);
  FEngine.Strategies.Add(LAccelHighest);
  FEngine.Strategies.Add(LSellForMonies);
  FEngine.Strategies.Add(LBobber);

  //also assign the authenticator
  (FEngine.OrderManager as IGDAXOrderManager).Authenticator:=FAuth.Authenticator;

  //prompt for pre-loading of tickers if not auto starting
  if not mi_auto_start.Checked then
    PreloadTickers;

  //update again after preload due to positions being postponed
  FAccelStrategy.UpdateCurrentPosition(FEngine.AvailableInventory);

  //start the engine to accept tickers
  if not FEngine.Start(LError) then
    LogError(LError);

  //start collecting tickers for the product
  FProducts.ProductFrame.Authenticator:=FAuth.Authenticator;
  FProducts.ProductFrame.Running:=True;

  //update the status
  ignition_main.Status:='Started';

  //disable all controls that would cause issues
  FAuth.Enabled := False;
  FProducts.Enabled := False;

  LogInfo('Strategy Started');
end;

procedure TSimpleBot.StopStrategy(Sender: TObject);
begin
  //stop the ticker collection
  FProducts.ProductFrame.Running:=False;
  //for now clear the strategy from the engine
  FEngine.Strategies.Clear;
  //update status and re-enable controls
  ignition_main.Status:='Stopped';
  FAuth.Enabled:=True;
  FProducts.Enabled:=True;
  LogInfo('Strategy Stopped');
end;

procedure TSimpleBot.ProductError(const AProductID: String; const AError: String);
begin
  LogError(Format('%s - %s',[AProductID,AError]));
end;

procedure TSimpleBot.ProductTick(Sender: TObject; const ATick: IGDAXTicker);
var
  I:Integer;
  LError:String;
  LTick:ITicker;
  LFunds,
  LInventory,
  LTickPrice,
  LAAC,
  LFundsLed:Extended;
  LFile:String;
begin
  {$IFDEF WINDOWS}
  SetThreadExecutionState(ES_CONTINUOUS OR ES_SYSTEM_REQUIRED OR DWORD($00000040));
  {$ENDIF}

  //only keep so much before purging visual log
  if multi_log.Lines.Count>5000 then
  begin
    LFile:='log_'+FormatDateTime('mm_dd_yyyy.log',Now);
    with TStringList.Create do
    begin
      try
        if FileExists(LFile) then
          LoadFromFile(LFile);
        Append(multi_log.Lines.Text);
        SaveToFile(LFile);
      finally
        Free;
      end;
    end;
    multi_log.Lines.Clear;
  end;

  //purge chart old entries after 10k
  if chart_source.Count>10000 then
    for I:=0 to 1000 do
      chart_source.Delete(0);

  //sandbox mode apparently is reporting incorrect times from GDAX...
  //for everything to work as production does, assign the time manually
  if FAuth.IsSanboxMode then
    ATick.Time:=Now;
  LogInfo(
    Format('%s - %s',
      [
        ATick.Product.ID,'ticker price:' + FloatToStr(ATick.Price) +
        ' ticker time: ' + DateTimeToStr(ATick.Time)
      ]
    )
  );

  //add the ticker price
  chart_source.Add(ATick.Time,ATick.Price);

  //feed the engine a tick
  LTick:=TGDAXTickerImpl.Create(ATick);
  if not FEngine.Feed(
    LTick,
    LError
  ) then
    LogError(LError);
  //add any additional info from strategy
  //todo - let strategy draw on the chart in some way (channels etc...)

  //refresh the chart
  chart_ticker.Refresh;

  //for debugging assign local variables
  LFunds:=FFunds;
  LInventory:=FEngine.AvailableInventory;
  LTickPrice:=LTick.Price;
  LAAC:=FEngine.AAC;
  LFundsLed:=FEngine.FundsLedger.Balance;

  //update status panels
  status_main.Panels[0].Text:='Funds ' + FloatToStr(FEngine.AvailableFunds);
  status_main.Panels[1].Text:='Inventory ' + FloatToStr(FEngine.AvailableInventory);
  status_main.Panels[2].Text:='AAC ' + FloatToStr(FEngine.AAC);
  status_main.Panels[3].Text:='Profit ' + FloatToStr(
    (
      (LFundsLed + (LAAC * LInventory)) +
      LInventory * (LTickPrice - LAAC)
    ) - LFunds
  );
  status_main.Panels[4].Text:='Completed ' + IntToStr(FCompletedOrders);
end;

procedure TSimpleBot.EngineStatus(const ADetails: IOrderDetails; const AID: String;
  const AOldStatus, ANewStatus: TOrderManagerStatus);
begin
  if ANewStatus=omCompleted then
  begin
    Inc(FCompletedOrders);
    json_main.Save;
  end;
end;

procedure TSimpleBot.LogError(const AMessage: String);
begin
  if chk_log_error.Checked then
    multi_log.Lines.Append(
      Format(LOG_ENTRY,['-ERROR-',DateTimeToStr(Now),AMessage])
    );
end;

procedure TSimpleBot.LogInfo(const AMessage: String);
begin
  if chk_log_info.Checked then
    multi_log.Lines.Append(
      Format(LOG_ENTRY,['-INFO-',DateTimeToStr(Now),AMessage])
    );
end;

procedure TSimpleBot.LogWarn(const AMessage: String);
begin
  if chk_log_warn.Checked then
    multi_log.Lines.Append(
      Format(LOG_ENTRY,['-WARN-',DateTimeToStr(Now),AMessage])
    );
end;

function TSimpleBot.GetAccelCriteria: TActiveCriteriaCallbackArray;
begin
  SetLength(Result, 2);
  Result[0] := @AccelStrategyInPosition;
  Result[1] := @BobberInOutPosition;
end;

function TSimpleBot.GetLowAccelCriteria: TActiveCriteriaCallbackArray;
begin
  SetLength(Result, 2);
  Result[0] := @AccelLowStrategyInPosition;
  Result[1] := @BobberInOutPosition;
end;

function TSimpleBot.GetLowestAccelCriteria: TActiveCriteriaCallbackArray;
begin
  SetLength(Result, 2);
  Result[0] := @AccelLowestStrategyInPosition;
  Result[1] := @BobberInOutPosition;
end;

procedure TSimpleBot.InterpolateTimeSetting(var Time: Cardinal;
  const AIsReload: Boolean);
begin
  //if input is larger or smaller then someone has chosen to use a custom setting outside
  //of the bounds specified
  if Time > MAX_TIME then
  begin
    if AIsReload then
    begin
      FTimeFrameCtrl.Value := FTimeFrameCtrl.MaxValue;
      Exit;
    end
    //allow fallthrough if the user adjusted
    else if FTimeFrameCtrl.Value >= FTimeFrameCtrl.MaxValue then
      Exit;
  end
  else if Time < MIN_TIME then
  begin
    if AIsReload then
    begin
      FTimeFrameCtrl.Value := FTimeFrameCtrl.MinValue;
      Exit;
    end
    else if FTimeFrameCtrl.Value <= FTimeFrameCtrl.MinValue then
      Exit;
  end;

  //otherwise we need to set the time according to the slider value
  if not AIsReload then
    Time := MIN_TIME + Trunc((MAX_TIME - MIN_TIME) * (FTimeFrameCtrl.Value / FTimeFrameCtrl.MaxValue))
  else
    FTimeFrameCtrl.Value := FTimeFrameCtrl.MinValue + Trunc((FTimeFrameCtrl.MaxValue - FTimeFrameCtrl.MinValue) * ((Time - MIN_TIME) / (MAX_TIME - MIN_TIME)));
end;

procedure TSimpleBot.InterpolatePositionSetting(var Position: Single;
  const AIsReload: Boolean);
begin
  //if input is larger or smaller then someone has chosen to use a custom setting outside
  //of the bounds specified
  if Position > MAX_POS_SIZE then
  begin
    if AIsReload then
    begin
      FPosSizeCtrl.Value := FPosSizeCtrl.MaxValue;
      Exit;
    end
    else if FPosSizeCtrl.Value >= FPosSizeCtrl.MaxValue then
      Exit;;
  end
  else if Position < MIN_POS_SIZE then
  begin
    if AIsReload then
    begin
      FPosSizeCtrl.Value := FPosSizeCtrl.MinValue;
      Exit;
    end
    else if FPosSizeCtrl.Value <= FPosSizeCtrl.MinValue then
      Exit;
  end;

  //otherwise we need to set the position according to the slider value
  if not AIsReload then
    Position := MIN_POS_SIZE + (MAX_POS_SIZE - MIN_POS_SIZE) * (FPosSizeCtrl.Value / FPosSizeCtrl.MaxValue)
  else
    FPosSizeCtrl.Value := FPosSizeCtrl.MinValue + Trunc((FPosSizeCtrl.MaxValue - FPosSizeCtrl.MinValue) * ((Position - MIN_POS_SIZE) / (MAX_POS_SIZE - MIN_POS_SIZE)));
end;

procedure TSimpleBot.InterpolateSellPositionSetting(var Position: Single;
  const AIsReload: Boolean);
begin
  //if input is larger or smaller then someone has chosen to use a custom setting outside
  //of the bounds specified
  if Position > MAX_SELL_POS_SIZE then
  begin
    if AIsReload then
    begin
      FSellPosSizeCtrl.Value := FSellPosSizeCtrl.MaxValue;
      Exit;
    end
    else if FSellPosSizeCtrl.Value >= FSellPosSizeCtrl.MaxValue then
      Exit;
  end
  else if Position < MIN_SELL_POS_SIZE then
  begin
    if AIsReload then
    begin
      FSellPosSizeCtrl.Value := FSellPosSizeCtrl.MinValue;
      Exit;
    end
    else if FSellPosSizeCtrl.Value <= FSellPosSizeCtrl.MinValue then
      Exit;
  end;

  //otherwise we need to set the position according to the slider value
  if not AIsReload then
    Position := MIN_SELL_POS_SIZE + (MAX_SELL_POS_SIZE - MIN_SELL_POS_SIZE) * (FSellPosSizeCtrl.Value / FSellPosSizeCtrl.MaxValue)
  else
    FSellPosSizeCtrl.Value := FPosSizeCtrl.MinValue + Trunc((FSellPosSizeCtrl.MaxValue - FSellPosSizeCtrl.MinValue) * ((Position - MIN_SELL_POS_SIZE) / (MAX_SELL_POS_SIZE - MIN_SELL_POS_SIZE)));
end;

procedure TSimpleBot.InterpolateDCAPositionSetting(var Position: Single;
  const AIsReload: Boolean);
begin
  //if input is larger or smaller then someone has chosen to use a custom setting outside
  //of the bounds specified
  if Position > MAX_DCA_SIZE then
  begin
    if AIsReload then
    begin
      FDCASizeCtrl.Value := FDCASizeCtrl.MaxValue;
      Exit;
    end
    else if FDCASizeCtrl.Value >= FDCASizeCtrl.MaxValue then
      Exit;
  end
  else if Position < MIN_DCA_SIZE then
  begin
    if AIsReload then
    begin
      FDCASizeCtrl.Value := FDCASizeCtrl.MinValue;
      Exit;
    end
    else if FDCASizeCtrl.Value <= FDCASizeCtrl.MinValue then
      Exit;
  end;

  //otherwise we need to set the position according to the slider value
  if not AIsReload then
    Position := MIN_DCA_SIZE + (MAX_DCA_SIZE - MIN_DCA_SIZE) * (FDCASizeCtrl.Value / FDCASizeCtrl.MaxValue)
  else
    FDCASizeCtrl.Value := FDCASizeCtrl.MinValue + Trunc((FDCASizeCtrl.MaxValue - FDCASizeCtrl.MinValue) * ((Position - MIN_DCA_SIZE) / (MAX_DCA_SIZE - MIN_DCA_SIZE)));
end;

initialization
  HashCreds('', '');
  EXPIRED := IsLicenseExpired;
end.

