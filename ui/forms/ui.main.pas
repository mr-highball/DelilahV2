unit ui.main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries,
  Forms, Controls, Graphics, Dialogs, JSONPropStorage, ExtCtrls, ComCtrls,
  StdCtrls, ui.ignition, ui.authenticator, ui.usercontrol.multiline,
  ui.usercontrol.products, ui.usercontrol, gdax.api.types;

type


  { TMain }

  TMain = class(TForm)
    chart_source: TListChartSource;
    chart_ticker: TChart;
    chart_tickerLineSeries1: TLineSeries;
    ignition_main: TIgnition;
    icons: TImageList;
    json_main: TJSONPropStorage;
    memo_licenses: TMemo;
    multi_log: TMultiLine;
    pctrl_main: TPageControl;
    scroll_strategy: TScrollBox;
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

{$R *.lfm}

const
  LOG_ENTRY = '%s %s - %s';

{ TMain }

procedure TMain.json_mainRestoringProperties(Sender: TObject);
begin
  //todo
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  InitControls;
end;


procedure TMain.json_mainSavingProperties(Sender: TObject);
begin
  //todo
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
  FProductInit:=False;
  //strategy
  ignition_main.OnRequestStart:=CheckCanStart;
  ignition_main.OnRequestStop:=CheckCanStop;
  ignition_main.OnStart:=StartStrategy;
  ignition_main.OnStop:=StopStrategy;
  ignition_main.Status:='Stopped';
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
begin
  //for now we can always stop
  Continue:=True;
end;

procedure TMain.StartStrategy(Sender: TObject);
begin
  //clear chart source
  chart_source.Clear;
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
begin
  LogInfo(
    Format('%s - %s',
      [ATick.Product.ID,'ticker price:' + FloatToStr(ATick.Price)]
    )
  );
  //add the ticker price
  chart_source.Add(ATick.Time,ATick.Price);
  chart_ticker.Refresh;
  //add any additional info from strategy
  //todo...
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

