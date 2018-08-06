unit ui.main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TATools,
  Forms, Controls, Graphics, Dialogs, JSONPropStorage, ExtCtrls, ComCtrls,
  StdCtrls, Menus, ui.ignition, ui.authenticator, ui.usercontrol.multiline,
  ui.usercontrol.products, ui.usercontrol, gdax.api.types, delilah.order.gdax,
  ui.usercontrol.singleline, delilah.types;

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
    chart_tools: TChartToolset;
    chart_toolsDataPointCrosshairTool1: TDataPointCrosshairTool;
    chart_toolsZoomMouseWheelTool1: TZoomMouseWheelTool;
    chart_source: TListChartSource;
    chart_ticker: TChart;
    chart_tickerLineSeries1: TLineSeries;
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
    pctrl_main: TPageControl;
    scroll_strategy: TScrollBox;
    status_main: TStatusBar;
    ts_product: TTabSheet;
    ts_about: TTabSheet;
    ts_chart: TTabSheet;
    ts_log: TTabSheet;
    ts_strategy: TTabSheet;
    ts_auth: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure json_mainRestoringProperties(Sender: TObject);
    procedure json_mainSavingProperties(Sender: TObject);
    procedure pctrl_mainChange(Sender: TObject);
  private
    FAuth : TAuthenticator;
    FProducts : TProducts;
    FProductInit : Boolean;
    FFunds : TSingleLine;
    FInit : Boolean;
    FEngine : IDelilah;
    procedure InitControls;
    procedure CheckCanStart(Sender:TObject;Var Continue:Boolean);
    procedure CheckCanStop(Sender:TObject;Var Continue:Boolean);
    procedure StartStrategy(Sender:TObject);
    procedure StopStrategy(Sender:TObject);
    procedure ProductError(Const AProductID:String;Const AError:String);
    procedure ProductTick(Sender : TObject; Const ATick : IGDAXTicker);
    procedure LogError(Const AMessage:String);
    procedure LogInfo(Const AMessage:String);
  public
  end;

var
  Main: TMain;

implementation
uses
  delilah, delilah.strategy.gdax, delilah.ticker.gdax, delilah.strategy.window,
  delilah.strategy.gdax.sample, delilah.manager.gdax;

{$R *.lfm}

const
  LOG_ENTRY = '%s %s - %s';

{ TMain }

procedure TMain.json_mainRestoringProperties(Sender: TObject);
begin
  //todo - check if a json settings file exists, if so read/assign values
end;

procedure TMain.FormCreate(Sender: TObject);
var
  LError:String;
  LManager:IGDAXOrderManager;
begin
  //create an engine
  FEngine:=TDelilahImpl.Create;
  //since we are dealing with GDAX assign the order manager
  LManager:=TGDAXOrderManagerImpl.Create;
  FEngine.OrderManager:=LManager;

  InitControls;
end;

procedure TMain.json_mainSavingProperties(Sender: TObject);
begin
  //todo - attempt to save a settings file based on options set
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

procedure TMain.InitControls;
begin
  if not FInit then
  begin
    //main tab
    pctrl_main.ActivePage:=ts_auth;
    //logger
    multi_log.Options:=multi_log.Options - [ucAuthor];
    multi_log.Title:='Strategy Logger';
    multi_log.Description:='logging for the strategy';
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
    FFunds.Text:='0.0';
    FInit:=True;
  end;
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
  LStrategy:ISampleGDAX;
begin
  //clear chart source
  chart_source.Clear;
  //todo - right now just adding an sample strategy, but need to choose
  //from the selected strategy in some dropdown once fully implemented.
  //also allow easy ui binding, and registering...
  LStrategy:=TSampleGDAXImpl.Create;
  //during testing don't require a size, but this is where we would
  //put for example, 1hr worth of time, or a min.. or whatever.
  LStrategy.WindowSizeInMilli:=0;
  FEngine.Strategies.Add(
    LStrategy
  );
  //also assign the authenticator
  (FEngine.OrderManager as IGDAXOrderManager).Authenticator:=FAuth.Authenticator;
  //start the engine to accept tickers
  FEngine.Funds:=StrToFloatDef(FFunds.Text,0);
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
  LError:String;
  LTick:ITicker;
begin
  //only keep so much before purging visual log
  if multi_log.Lines.Count>5000 then
    multi_log.Lines.Clear;
  LogInfo(
    Format('%s - %s',
      [ATick.Product.ID,'ticker price:' + FloatToStr(ATick.Price)]
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
end;

procedure TMain.LogError(const AMessage: String);
begin
  multi_log.Lines.Append(
    Format(LOG_ENTRY,['-ERROR-',DateTimeToStr(Now),AMessage])
  );
end;

procedure TMain.LogInfo(const AMessage: String);
begin
  multi_log.Lines.Append(
    Format(LOG_ENTRY,['-INFO-',DateTimeToStr(Now),AMessage])
  );
end;


end.

