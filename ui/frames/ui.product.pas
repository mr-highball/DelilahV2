unit ui.product;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Spin, ExtCtrls,
  gdax.api.types;

type

  (*
    event triggered when a tick is recorded successfully
  *)
  TTickEvent = procedure(Sender : TObject; Const ATick : IGDAXTicker) of object;

  (*
    event triggered when an error occurs for a related product
  *)
  TProductErrorEvent = procedure(Const AProductID:String;Const AError:String) of object;

  { TProductFrame }

  TProductFrame = class(TFrame)
    chk_log_tickers: TCheckBox;
    combo_products: TComboBox;
    edit_interval: TSpinEdit;
    lbl_interval: TLabel;
    timer_ticker: TTimer;
    procedure timer_tickerTimer(Sender: TObject);
  private
    FAuth : IGDAXAuthenticator;
    FOnTick : TTickEvent;
    FRunning : Boolean;
    FProducts : IGDAXProducts;
    FOnError : TProductErrorEvent;
    function GetAuth: IGDAXAuthenticator;
    function GetInterval: Cardinal;
    function GetLogTickers: Boolean;
    function GetOnError: TProductErrorEvent;
    function GetOnTick: TTickEvent;
    function GetProduct: IGDAXProduct;
    function GetRunning: Boolean;
    procedure SetAuth(AValue: IGDAXAuthenticator);
    procedure SetInterval(AValue: Cardinal);
    procedure SetLogTickers(AValue: Boolean);
    procedure SetOnError(AValue: TProductErrorEvent);
    procedure SetOnTick(AValue: TTickEvent);
    procedure SetRunning(AValue: Boolean);
    procedure Start;
    procedure Stop;
    procedure InitCombo;
    procedure LogTick(Const ATicker:IGDAXTicker; Const AContent:String);
  protected
    procedure Loaded; override;
    procedure LogError(Const AMessage:String);
  public
    //events
    property OnTick : TTickEvent read GetOnTick write SetOnTick;
    property OnError : TProductErrorEvent read GetOnError write SetOnError;
  public
    //properties/methods
    property TickerInterval : Cardinal read GetInterval write SetInterval;
    property LogTickers : Boolean read GetLogTickers write SetLogTickers;
    property Authenticator : IGDAXAuthenticator read GetAuth write SetAuth;
    property Running : Boolean read GetRunning write SetRunning;
    property Product : IGDAXProduct read GetProduct;
    procedure Init;
    destructor Destroy; override;
  end;

implementation
uses
  DateUtils, gdax.api.products, gdax.api.ticker;

{$R *.lfm}

{ TProductFrame }

procedure TProductFrame.timer_tickerTimer(Sender: TObject);
var
  LProduct : IGDAXProduct;
  LTick : IGDAXTicker;
  LError,
  LContent : String;
begin
  LProduct:=Product;
  try
    if FRunning then
      if Assigned(FAuth) and Assigned(LProduct) then
      begin
        LTick:=TGDAXTickerImpl.Create;
        LTick.Product:=LProduct;
        //call to log error so we don't disrupt the next tick
        if not LTick.Get(LContent,LError) then
        begin
          LogError(LError);
          Exit;
        end;
        //for any listener, pass down the tick
        if Assigned(FOnTick) then
          FOnTick(Self,LTick);
        //log the tick if we are being requested to do so
        if LogTickers then
          LogTick(LTick,LContent);
      end
      else
        LogError('invalid authenticator or product');
  except on E:Exception do
    LogError(E.Message);
  end;
end;

function TProductFrame.GetAuth: IGDAXAuthenticator;
begin
  Result:=FAuth;
end;

function TProductFrame.GetInterval: Cardinal;
begin
  Result:=Abs(edit_interval.Value);
end;

function TProductFrame.GetLogTickers: Boolean;
begin
  Result:=chk_log_tickers.Checked;
end;

function TProductFrame.GetOnError: TProductErrorEvent;
begin
  Result:=FOnError;
end;

function TProductFrame.GetOnTick: TTickEvent;
begin
  Result:=FOnTick;
end;

function TProductFrame.GetProduct: IGDAXProduct;
begin
  Result:=nil;
  if combo_products.ItemIndex<0 then
    Exit;
  Result:=FProducts.Products[combo_products.ItemIndex];
end;

function TProductFrame.GetRunning: Boolean;
begin
  Result:=FRunning;
end;

procedure TProductFrame.SetAuth(AValue: IGDAXAuthenticator);
begin
  FAuth:=nil;
  FAuth:=AValue;
end;

procedure TProductFrame.SetInterval(AValue: Cardinal);
begin
  edit_interval.Value:=Abs(AValue);
end;

procedure TProductFrame.SetLogTickers(AValue: Boolean);
begin
  chk_log_tickers.Checked:=AValue;
end;

procedure TProductFrame.SetOnError(AValue: TProductErrorEvent);
begin
  FOnError:=AValue;
end;

procedure TProductFrame.SetOnTick(AValue: TTickEvent);
begin
  FOnTick:=AValue;
end;

procedure TProductFrame.SetRunning(AValue: Boolean);
begin
  if AValue then
    Start
  else
    Stop;
end;

procedure TProductFrame.Start;
begin
  Stop;
  timer_ticker.Interval:=TickerInterval;
  timer_ticker.Enabled:=True;
  FRunning:=True;
end;

procedure TProductFrame.Stop;
begin
  timer_ticker.Enabled:=False;
  FRunning:=False;
end;

procedure TProductFrame.InitCombo;
var
  LContent,
  LError : String;
  I : Integer;
begin
  //clear combo before continuing
  combo_products.Clear;
  if not Assigned(FProducts) then
    raise Exception.Create('products is invalid');
  if not FProducts.Get(LContent,LError) then
    raise Exception.Create(LError);
  //add all of the product ids to the combo
  for I:=0 to Pred(FProducts.Products.Count) do
    combo_products.Items.Add(FProducts.Products[I].ID);
end;

procedure TProductFrame.LogTick(const ATicker: IGDAXTicker;
  const AContent: String);
var
  LLog:TStringList;
  LFile:String;
begin
  //below is a very simple logging of tickers, needs to be updated later
  LLog:=TStringList.Create;
  try
    LFile:=ATicker.Product.ID + FormatDateTime('mm_dd_yyyy',Now) + '.log';
    if FileExists(LFile) then
    begin
      LLog.LoadFromFile(LFile);
      LLog.Add(AContent);
      LLog.SaveToFile(LFile);
    end
    else
    begin
      LLog.Add(AContent);
      LLog.SaveToFile(LFile);
    end;
  finally
    LLog.Free;
  end;
end;

procedure TProductFrame.Loaded;
begin
  inherited Loaded;
  combo_products.Clear;
  Align:=TAlign.alClient;
  Stop;
end;

procedure TProductFrame.LogError(const AMessage: String);
begin
  if Assigned(FOnError) then
    FOnError(Product.ID,AMessage)
  else
    raise Exception.Create('product ' + Product.ID + ' with error ' + AMessage);
end;

procedure TProductFrame.Init;
begin
  if not Assigned(FAuth) then
    raise Exception.Create('no authenticator assigned to ' + Self.Classname);
  FProducts:=nil;
  FProducts:=TGDAXProductsImpl.Create;
  FProducts.Authenticator:=FAuth;
  InitCombo;
end;

destructor TProductFrame.Destroy;
begin
  FAuth:=nil;
  FProducts:=nil;
  inherited Destroy;
end;

end.

