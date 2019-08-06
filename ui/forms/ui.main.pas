unit ui.main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TATools,
  Forms, Controls, Graphics, Dialogs, JSONPropStorage, ExtCtrls, ComCtrls,
  StdCtrls, Menus, ui.ignition, ui.authenticator, ui.usercontrol.multiline,
  ui.usercontrol.products, ui.usercontrol, gdax.api.types, delilah.order.gdax,
  ui.usercontrol.singleline, delilah.types, ui.email, delilah.strategy.gdax.tiers;

type


  { TMain }
  (*
    this form allows for configuring a IDelilah engine for GDAX and offers
    some conveniences such as chart, visual logging, etc...
    not all features of the engine are utilized, and currently this is tailored
    to GDAX. ultimately this is designed to be a simple ui wrapper to get someone
    up and running a strategy with minimal hassle as well as showing a use case
    for the GDAX api and engine core classes.
  *)
  TMain = class(TForm)
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
    ignition_main: TIgnition;
    icons: TImageList;
    menu: TImageList;
    json_main: TJSONPropStorage;
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
    pnl_log_clear: TPanel;
    pctrl_main: TPageControl;
    scroll_strategy: TScrollBox;
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
    procedure json_mainRestoringProperties(Sender: TObject);
    procedure json_mainSavingProperties(Sender: TObject);
    procedure mi_auto_startClick(Sender: TObject);
    procedure mi_email_enabledClick(Sender: TObject);
    procedure mi_email_setupClick(Sender: TObject);
    procedure mi_file_settingsClick(Sender: TObject);
    procedure mi_log_tabClick(Sender: TObject);
    procedure pctrl_mainChange(Sender: TObject);
  private
    FAuth : TAuthenticator;
    FOnlyLower: Boolean;
    FMoonMan: Boolean;
    FProducts : TProducts;
    FProductInit : Boolean;
    FFunds : TSingleLine;
    FInit : Boolean;
    FEngine : IDelilah;
    FCompletedOrders,
    FTempWindowSetting : Cardinal;
    FMarketFee,
    FLimitFee,
    FMinProfit,
    FMinReduce,
    FSmallBuyPerc,
    FMedBuyPerc,
    FLargeBuyPerc,
    FSmallSellPerc,
    FMedSellPerc,
    FLargeSellPerc,
    FIgnoreProfitPerc,
    FGTFOPerc: Single;
    FUseMarketBuy,
    FUseMarketSell,
    FAvoidChop: Boolean;
    FMidStrategy: ITierStrategyGDAX;
    FAccelStrategy: ITierStrategyGDAX;
    FAccelWindowFactor: Integer;
    procedure SetupEmail;
    procedure EnableEmail;
    procedure SetupLogFile;
    procedure SetupLogTab;
    procedure InitControls;
    procedure EnableAutoStart;
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
    function GetMoonCriteria : TActiveCriteriaCallbackArray;
    function GetMidCriteria : TActiveCriteriaCallbackArray;
    function GetAccelCriteria : TActiveCriteriaCallbackArray;
  public
  end;

var
  Main: TMain;

implementation
uses
  delilah, delilah.strategy.gdax, delilah.ticker.gdax, delilah.strategy.window,
  delilah.strategy.gdax.sample, delilah.manager.gdax, ledger,
  delilah.strategy.gdax.sample.extended, delilah.strategy.channels,
  math, dateutils
  {$IFDEF WINDOWS}
  ,JwaWindows
  {$ENDIF};

{$R *.lfm}

const
  LOG_ENTRY = '%s %s - %s';

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
    Main.LogInfo('Main::MoonRisingPrice::negative average acceleration per tick: ' + FloatToStr(LAvg));
    Exit;
  end;

  Main.LogInfo('Main::MoonRisingPrice::average acceleration per tick: ' + FloatToStr(LAvg));

  //no need to project if the parent doesn't care about profit
  if not PTierStrategyGDAX(ADetails^.Data)^.OnlyProfit then
  begin
    Active:=True;
    Main.LogInfo('Main::MoonRisingPrice::parent OnlyProfit is false, no projection');
    Exit;
  end;

  //to project the price we need to look at the parent window's size
  //and min profit, and see if with the current average accelaration, the profit
  //"should" be reached
  LParentWindow:=PTierStrategyGDAX(ADetails^.Data)^.ChannelStrategy.WindowSizeInMilli;
  LMinProfit:=PTierStrategyGDAX(ADetails^.Data)^.MinProfit;

  //(ticksInWindow * averageAccel) + currentPrice = projected
  LProjectedPrice:=(LParentWindow / LTimePerTick) * LAvg + ADetails^.Ticker^.Price;
  Main.LogInfo('Main::MoonRisingPrice::projected price ' + FloatToStr(LProjectedPrice));

  //find delta of current price and see if the percentage gain is at least
  //half min profit (doesn't account for fees etc...)
  LAvg:=LProjectedPrice - ADetails^.Ticker^.Price;//yeah yeah don't reuse variables

  if (LAvg / ADetails^.Ticker^.Price) < (LMinProfit / 2) then
  begin
    Main.LogInfo('Main::MoonRisingPrice::projected price is not high enough for min profit');
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
  Main.LogInfo('Main::MidAccelStratPositive::average acceleration per tick: ' + FloatToStr(LAvg));
  Active:=LAvg > 0;
end;

{ TMain }

procedure TMain.json_mainRestoringProperties(Sender: TObject);
var
  LBal:Extended;
begin
  //todo - check if a json settings file exists, if so read/assign values
  if not FInit then
    Exit;
   //get our ledgers setup correctly
  FEngine.InventoryLedger.Clear;
  FEngine.FundsLedger.Clear;
  FEngine.HoldsLedger.Clear;
  FEngine.HoldsInventoryLedger.Clear;

  //now read from our json file
  FAuth.Secret:=json_main.ReadString('secret','');
  FAuth.Key:=json_main.ReadString('key','');
  FAuth.Passphrase:=json_main.ReadString('pass','');;
  FFunds.Text:=json_main.ReadString('funds','0.0');
  FEngine.Funds:=StrToFloatDef(FFunds.Text,0);
  FAuth.IsSanboxMode:=json_main.ReadBoolean('sandbox_mode',True);
  FEngine.AAC:=StrToFloatDef(json_main.ReadString('aac','0.0'),0);

  //restoring funds requires us to see what was recorded in the ledger
  //as the balance, then subtract (balance - start funds) and then
  //credit by this amount (could be negative credit)
  LBal:=StrToFloatDef(json_main.ReadString('funds_ledger','0.0'),0);
  if LBal>0 then
    LBal:=LBal - FEngine.Funds;
  //record to funds
  if LBal<>0 then
    LBal:=FEngine.FundsLedger.RecordEntry(
      LBal,
      ltCredit
    ).Balance;
  //record to holds
  LBal:=FEngine.HoldsLedger.RecordEntry(
    StrToFloatDef(json_main.ReadString('holds_ledger','0.0'),0),
    ltDebit
  ).Balance;
  //record to inventory
  LBal:=FEngine.InventoryLedger.RecordEntry(
    StrToFloatDef(json_main.ReadString('inventory_ledger','0.0'),0),
    ltCredit
  ).Balance;
  //record to holds inventory
  LBal:=FEngine.HoldsInventoryLedger.RecordEntry(
    StrToFloatDef(json_main.ReadString('inventory_holds_ledger','0.0'),0),
    ltDebit
  ).Balance;
  //simple counter for completed orders
  FCompletedOrders:=json_main.ReadInteger('completed_orders',0);

  //todo - remove these once strategies can persist
  FTempWindowSetting:=json_main.ReadInteger('temp_window_setting',20 * 60 * 1000);
  FMarketFee:=StrToFloatDef(json_main.ReadString('market_fee','0.005'),0.005);
  FLimitFee:=StrToFloatDef(json_main.ReadString('limit_fee','0.0015'),0.0015);
  FUseMarketBuy:=json_main.ReadBoolean('market_buy',False);
  FUseMarketSell:=json_main.ReadBoolean('market_sell',False);
  FMinReduce:=StrToFloatDef(json_main.ReadString('min_reduction','0'),0);
  FMinProfit:=StrToFloatDef(json_main.ReadString('min_profit','0'),0);
  FSmallBuyPerc:=StrToFloatDef(json_main.ReadString('small_buy_perc','0'),0);
  FMedBuyPerc:=StrToFloatDef(json_main.ReadString('med_buy_perc','0'),0);
  FLargeBuyPerc:=StrToFloatDef(json_main.ReadString('large_buy_perc','0'),0);
  FSmallSellPerc:=StrToFloatDef(json_main.ReadString('small_sell_perc','0'),0);
  FMedSellPerc:=StrToFloatDef(json_main.ReadString('med_sell_perc','0'),0);
  FLargeSellPerc:=StrToFloatDef(json_main.ReadString('large_sell_perc','0'),0);
  FIgnoreProfitPerc:=StrToFloatDef(json_main.ReadString('ignore_profit_perc','0'),0);
  FGTFOPerc:=StrToFloatDef(json_main.ReadString('gtfo_perc','0'),0);
  FOnlyLower:=json_main.ReadBoolean('only_lower',True);
  FAvoidChop:=json_main.ReadBoolean('avoid_chop',False);
  FMoonMan:=json_main.ReadBoolean('lunar_mission',False);
  FAccelWindowFactor:=json_main.ReadInteger('accel_window_factor',4);
end;

procedure TMain.FormCreate(Sender: TObject);
var
  LError:String;
  LManager:IGDAXOrderManager;
begin
  FMidStrategy:=nil;;
  FCompletedOrders:=0;
  //create an engine
  FEngine:=TDelilahImpl.Create(LogInfo, LogError, LogWarn);
  //since we are dealing with GDAX assign the order manager
  LManager:=TGDAXOrderManagerImpl.Create(LogInfo, LogError, LogWarn);
  FEngine.OrderManager:=LManager;
  FEngine.OnStatus:=EngineStatus;

  InitControls;
end;

procedure TMain.btn_log_clearClick(Sender: TObject);
begin
  multi_log.Lines.Clear;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FMidStrategy:=nil;
  FAccelStrategy:=nil;
end;

procedure TMain.json_mainSavingProperties(Sender: TObject);
begin
  //todo - attempt to save a settings file based on options set
  json_main.WriteString('secret',FAuth.Secret);
  json_main.WriteString('key',FAuth.Key);
  json_main.WriteString('pass',FAuth.Passphrase);
  json_main.WriteString('funds',FloatToStr(StrToFloatDef(FFunds.Text,0)));
  json_main.WriteBoolean('sandbox_mode',FAuth.IsSanboxMode);
  json_main.WriteString('aac',FloatToStr(FEngine.AAC));
  json_main.WriteInteger('completed_orders',FCompletedOrders);
  json_main.WriteString('funds_ledger',FloatToStr(FEngine.FundsLedger.Balance));
  json_main.WriteString('holds_ledger',FloatToStr(FEngine.HoldsLedger.Balance));
  json_main.WriteString('inventory_ledger',FloatToStr(FEngine.InventoryLedger.Balance));
  json_main.WriteString('inventory_holds_ledger',FloatToStr(FEngine.HoldsInventoryLedger.Balance));

  //todo - temporary, will remove
  json_main.WriteInteger('temp_window_setting',FTempWindowSetting);
  json_main.WriteBoolean('market_sell',FUseMarketSell);
  json_main.WriteBoolean('market_buy',FUseMarketBuy);
  json_main.WriteBoolean('only_lower',FOnlyLower);
  json_main.WriteBoolean('avoid_chop',FAvoidChop);
  json_main.WriteBoolean('lunar_mission',FMoonMan);
  json_main.WriteString('market_fee',FloatToStr(FMarketFee));
  json_main.WriteString('limit_fee',FloatToStr(FLimitFee));
  json_main.WriteString('min_reduction',FloatToStr(FMinReduce));
  json_main.WriteString('min_profit',FloatToStr(FMinProfit));
  json_main.WriteString('small_buy_perc',FloatToStr(FSmallBuyPerc));
  json_main.WriteString('med_buy_perc',FloatToStr(FMedBuyPerc));
  json_main.WriteString('large_buy_perc',FloatToStr(FLargeBuyPerc));
  json_main.WriteString('small_sell_perc',FloatToStr(FSmallSellPerc));
  json_main.WriteString('med_sell_perc',FloatToStr(FMedSellPerc));
  json_main.WriteString('large_sell_perc',FloatToStr(FLargeSellPerc));
  json_main.WriteString('ignore_profit_perc',FloatToStr(FIgnoreProfitPerc));
  json_main.WriteString('gtfo_perc',FloatToStr(FGTFOPerc));
  json_main.WriteInteger('accel_window_factor',FAccelWindowFactor);
end;

procedure TMain.mi_auto_startClick(Sender: TObject);
begin
  EnableAutoStart;
end;

procedure TMain.mi_email_enabledClick(Sender: TObject);
begin
  EnableEmail;
end;

procedure TMain.mi_email_setupClick(Sender: TObject);
begin
  SetupEmail;
end;

procedure TMain.mi_file_settingsClick(Sender: TObject);
begin
  SetupLogFile;
end;

procedure TMain.mi_log_tabClick(Sender: TObject);
begin
  SetupLogTab;
end;

procedure TMain.pctrl_mainChange(Sender: TObject);
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

procedure TMain.SetupEmail;
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

procedure TMain.EnableEmail;
begin
  ShowMessage('not implemented');
end;

procedure TMain.SetupLogFile;
begin
  ShowMessage('not implemented');
end;

procedure TMain.SetupLogTab;
begin
  ShowMessage('not implemented');
end;

procedure TMain.InitControls;
begin
  if not FInit then
  begin
    json_main.Restore;
    //main tab
    pctrl_main.ActivePage:=ts_auth;
    //logger
    multi_log.Options:=multi_log.Options - [ucAuthor];
    multi_log.Title:='Strategy Logger';
    multi_log.Description:='logging for the strategy';
    chk_log_error.Checked:=True;
    chk_log_warn.Checked:=True;
    chk_log_info.Checked:=True;
    //authenticator
    FAuth:=TAuthenticator.Create(Self);
    FAuth.Parent:=ts_auth;
    FAuth.AnchorHorizontalCenterTo(ts_auth);
    FAuth.AnchorVerticalCenterTo(ts_auth);
    //products
    FProducts:=TProducts.Create(Self);
    FProducts.Parent:=ts_product;
    FProducts.Align:=TAlign.alClient;
    FProducts.ProductFrame.Authenticator:=FAuth.Authenticator;
    FProducts.ProductFrame.OnError:=ProductError;
    FProducts.ProductFrame.OnTick:=ProductTick;
    FProducts.ProductFrame.TickerInterval:=1500;
    FProductInit:=False;
    //strategy
    ignition_main.OnRequestStart:=CheckCanStart;
    ignition_main.OnRequestStop:=CheckCanStop;
    ignition_main.OnStart:=StartStrategy;
    ignition_main.OnStop:=StopStrategy;
    ignition_main.Status:='Stopped';
    FFunds:=TSingleLine.Create(Self);
    FFunds.Parent:=scroll_strategy;
    FFunds.Align:=TAlign.alTop;
    FFunds.Options:=FFunds.Options - [ucAuthor];
    FFunds.Title:='Funds';
    FFunds.Description:='specify "how much" quote currency is available to spend trading';
    FFunds.Height:=100;
    FFunds.Control.Constraints.MaxWidth:=200;
    //FFunds.Text:='0.0';
    FInit:=True;
  end;
end;

procedure TMain.EnableAutoStart;
begin
  ShowMessage('not implemented');
end;

procedure TMain.CheckCanStart(Sender: TObject; var Continue: Boolean);
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

procedure TMain.CheckCanStop(Sender: TObject; var Continue: Boolean);
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

procedure TMain.StartStrategy(Sender: TObject);
var
  LError:String;
  LConfigStrategy,
  LMoonStrategy:ITierStrategyGDAX;
const
  MOON_MIN_WINDOW = 20 * 60 * 1000;
begin
  //clear chart source
  chart_source.Clear;

  //todo - right now just adding an sample strategy, but need to choose
  //from the selected strategy in some dropdown once fully implemented.
  //also allow easy ui binding, and registering...
  LConfigStrategy:=TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);
  LMoonStrategy:=TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);

  //todo - currently using a config to pull window, but this needs
  //to be dynamic based on strategy (since not all strategies utilize a window)
  LConfigStrategy.ChannelStrategy.WindowSizeInMilli:=FTempWindowSetting;
  LConfigStrategy.SmallTierPerc:=FSmallBuyPerc;
  LConfigStrategy.MidTierPerc:=FMedBuyPerc;
  LConfigStrategy.LargeTierPerc:=FLargeBuyPerc;
  LConfigStrategy.SmallTierSellPerc:=FSmallSellPerc;
  LConfigStrategy.MidTierSellPerc:=FMedSellPerc;
  LConfigStrategy.LargeTierSellPerc:=FLargeSellPerc;
  LConfigStrategy.UseMarketBuy:=FUseMarketBuy;
  LConfigStrategy.UseMarketSell:=FUseMarketSell;
  LConfigStrategy.OnlyLowerAAC:=FOnlyLower;
  LConfigStrategy.AvoidChop:=FAvoidChop;
  LConfigStrategy.MinReduction:=FMinReduce;
  LConfigStrategy.MinProfit:=FMinProfit;
  LConfigStrategy.OnlyProfit:=True;
  LConfigStrategy.MarketFee:=FMarketFee;
  LConfigStrategy.LimitFee:=FLimitFee;
  LConfigStrategy.IgnoreOnlyProfitThreshold:=FIgnoreProfitPerc;
  LConfigStrategy.GTFOPerc:=FGTFOPerc;
  FMidStrategy:=nil;
  FMidStrategy:=LConfigStrategy;
  FMidStrategy.ActiveCriteria:=GetMidCriteria;

  //log some quick info for strategy
  LogInfo(
    Format(
      'Strategy::[UseMarketBuy]-%s [UseMarketSell]-%s [MarketFee]-%s [LimitFee]-%s [MinReduce]-%s [MinProfit]-%s',
      [BoolToStr(FUseMarketBuy,True),BoolToStr(FUseMarketSell,True),FloatToStr(FMarketFee),FloatToStr(FLimitFee),FloatToStr(FMinReduce),FloatToStr(FMinProfit)]
    )
  );

  //as you can tell... this is a permanent strategy that will never ever change in the future
  if FMoonMan then
  begin
    (*
      below we tailor these properties to the current set of moonman
      criteria. the idea is to only operate when we run out of inventory
      and to let our parent strategy do the selling. this should fix the case
      where our parent (being lagging) has sold everything but bitcoin
      jumps on the first falcon heavy it can get a ticket to. in this case
      our parent strategy will almost never buy any coinage and miss
      out on fat stacks
    *)

    //use 20% of parent size unless it's smaller than the min
    if (FMidStrategy.ChannelStrategy.WindowSizeInMilli * 0.20) >= MOON_MIN_WINDOW then
      LMoonStrategy.ChannelStrategy.WindowSizeInMilli:=Round(FMidStrategy.ChannelStrategy.WindowSizeInMilli * 0.2)
    else
      LMoonStrategy.ChannelStrategy.WindowSizeInMilli:=MOON_MIN_WINDOW;

    LMoonStrategy.SmallTierPerc:=FMidStrategy.SmallTierPerc * 0.20;
    LMoonStrategy.MidTierPerc:=FMidStrategy.MidTierPerc * 0.20;
    LMoonStrategy.LargeTierPerc:=FMidStrategy.LargeTierPerc * 0.20;
    LMoonStrategy.SmallTierSellPerc:=0;
    LMoonStrategy.MidTierSellPerc:=0;
    LMoonStrategy.LargeTierSellPerc:=0;
    LMoonStrategy.UseMarketBuy:=FMidStrategy.UseMarketBuy;
    LMoonStrategy.UseMarketSell:=FMidStrategy.UseMarketSell;
    LMoonStrategy.OnlyLowerAAC:=FMidStrategy.OnlyLowerAAC;
    LMoonStrategy.MinReduction:=FMidStrategy.MinReduction;
    LMoonStrategy.OnlyProfit:=True;
    LMoonStrategy.MarketFee:=0.000;
    LMoonStrategy.AvoidChop:=False;
    LMoonStrategy.MinProfit:=0.004;
    LMoonStrategy.ActiveCriteria:=GetMoonCriteria;
    LMoonStrategy.ActiveCriteriaData:=@FMidStrategy;
  end;

  //add both config/moon strategies
  FEngine.Strategies.Add(
    LConfigStrategy
  );

  if FMoonMan then
    FEngine.Strategies.Add(
      LMoonStrategy
    );

  (*
    as long as we have an acceleration factor above zero then
    go ahead and make a strategy using this factor amount
  *)
  if FAccelWindowFactor > 0 then
  begin
    FAccelStrategy:=nil;
    FAccelStrategy:=TTierStrategyGDAXImpl.Create(LogInfo,LogError,LogInfo);
    FAccelStrategy.ChannelStrategy.WindowSizeInMilli:=FMidStrategy.ChannelStrategy.WindowSizeInMilli * FAccelWindowFactor;
    FAccelStrategy.ActiveCriteria:=GetAccelCriteria;

    //update our mid-tier strategy's data to this strategy
    FMidStrategy.ActiveCriteriaData:=@FAccelStrategy;

    //add strategy to the engine
    FEngine.Strategies.Add(FAccelStrategy);
  end;

  //also assign the authenticator
  (FEngine.OrderManager as IGDAXOrderManager).Authenticator:=FAuth.Authenticator;

  //if the funds has changed since the last time this was run, reset it
  if FEngine.Funds<>StrToFloatDef(FFunds.Text,0) then
    FEngine.Funds:=StrToFloatDef(FFunds.Text,0);

  //start the engine to accept tickers
  if not FEngine.Start(LError) then
    LogError(LError);

  //start collecting tickers for the product
  FProducts.ProductFrame.Authenticator:=FAuth.Authenticator;
  FProducts.ProductFrame.Running:=True;

  //update the status
  ignition_main.Status:='Started';

  //disable all controls that would cause issues
  FAuth.Enabled:=False;
  FProducts.Enabled:=False;
  LogInfo('Strategy Started');
end;

procedure TMain.StopStrategy(Sender: TObject);
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

procedure TMain.ProductError(const AProductID: String; const AError: String);
begin
  LogError(Format('%s - %s',[AProductID,AError]));
end;

procedure TMain.ProductTick(Sender: TObject; const ATick: IGDAXTicker);
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
  LFunds:=FEngine.Funds;
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

procedure TMain.EngineStatus(const ADetails: IOrderDetails; const AID: String;
  const AOldStatus, ANewStatus: TOrderManagerStatus);
begin
  if ANewStatus=omCompleted then
  begin
    Inc(FCompletedOrders);
    json_main.Save;
  end;
end;

procedure TMain.LogError(const AMessage: String);
begin
  if chk_log_error.Checked then
    multi_log.Lines.Append(
      Format(LOG_ENTRY,['-ERROR-',DateTimeToStr(Now),AMessage])
    );
end;

procedure TMain.LogInfo(const AMessage: String);
begin
  if chk_log_info.Checked then
    multi_log.Lines.Append(
      Format(LOG_ENTRY,['-INFO-',DateTimeToStr(Now),AMessage])
    );
end;

procedure TMain.LogWarn(const AMessage: String);
begin
  if chk_log_warn.Checked then
    multi_log.Lines.Append(
      Format(LOG_ENTRY,['-WARN-',DateTimeToStr(Now),AMessage])
    );
end;

function TMain.GetMoonCriteria: TActiveCriteriaCallbackArray;
begin
  SetLength(Result,4);
  Result[0]:=MoonSideBuy;
  Result[1]:=MoonParentReady;
  Result[2]:=MoonLowInventory;
  Result[3]:=MoonRisingPrice;
end;

function TMain.GetMidCriteria: TActiveCriteriaCallbackArray;
begin
  SetLength(Result,0);
  if FAccelWindowFactor < 1 then
    Exit
  else
  begin
    SetLength(Result,1);
    Result[0]:=MidAccelStratPositve;
  end;
end;

function TMain.GetAccelCriteria: TActiveCriteriaCallbackArray;
begin
  SetLength(Result,1);
  Result[0]:=AccelDisableTrades;
end;


end.

