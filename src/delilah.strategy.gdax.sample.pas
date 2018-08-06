unit delilah.strategy.gdax.sample;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, delilah.strategy.gdax,
  delilah.strategy.window;

type

  { ISampleGDAX }

  ISampleGDAX = interface(IWindowStrategy)
    ['{0BC943DB-FA2F-4C42-BF55-83C22C82123D}']
    //here we would add any additional properties or method
    //that pertain to your strategy, such as IWindowStrategy if
    //we didn't want to go the delegate route
  end;

  { TSampleGDAXImpl }
  (*
    below we inherit from TStrategyGDAXImpl because we are targeting the
    GDAX exchange, but we also realize the IWindowStrategy. to make use
    of base classes functionality, we will use a TStrategyGDAXImpl as base
    and a delegate class for the window.
  *)
  TSampleGDAXImpl = class(TStrategyGDAXImpl,IWindowStrategy,ISampleGDAX)
  strict private
    FWindow: IWindowStrategy;
    FID: String;
    function GetWindow: IWindowStrategy;
    //methods delegates by window, compiler yells if not present.
    //alternatively we could've just added the Window property to ISampleGDAX
    //and use it, but is slightly inconvenient when have to call
    //Strategy.Window.Size vs Strategy.Size, also I already started to
    //do it this way...
    function GetCleanPerc: Single;
    function GetCleanThresh: Single;
    function GetCollected: Cardinal;
    function GetHighest: Extended;
    function GetIsReady: Boolean;
    function GetLowest: Extended;
    function GetStdDev: Single;
    function GetWindowSize: Cardinal;
    procedure SetCleanPerc(Const AValue: Single);
    procedure SetCleanThresh(Const AValue: Single);
    procedure SetWindowSize(Const AValue: Cardinal);
    function GetTickers: TTickers;
  strict protected
    (*
      this method is the primary method used to operate on "the tick" and
      where most logic will be implemented (or called via other methods)
    *)
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      const AFunds, AInventory: Extended; out Error: String): Boolean; override;
  public
    property Window : IWindowStrategy read GetWindow implements IWindowStrategy;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  delilah.ticker.gdax,//gdax ticker for engine
  delilah.order.gdax,//order detail implementation for engine
  gdax.api.types,//contains common types associated to GDAX
  gdax.api.orders,//contains implementation of order specific stuff
  gdax.api.consts;//enums/consts etc...

{ TSampleGDAXImpl }

function TSampleGDAXImpl.GetWindow: IWindowStrategy;
begin
  Result:=FWindow;
end;

function TSampleGDAXImpl.GetCleanPerc: Single;
begin
  Result:=FWindow.CleanupPercentage;
end;

function TSampleGDAXImpl.GetCleanThresh: Single;
begin
  Result:=FWindow.CleanupThreshold;
end;

function TSampleGDAXImpl.GetCollected: Cardinal;
begin
  Result:=FWindow.CollectedSizeInMilli;
end;

function TSampleGDAXImpl.GetHighest: Extended;
begin
  Result:=FWindow.HighestPrice;
end;

function TSampleGDAXImpl.GetIsReady: Boolean;
begin
  Result:=FWindow.IsReady;
end;

function TSampleGDAXImpl.GetLowest: Extended;
begin
  Result:=FWindow.LowestPrice;
end;

function TSampleGDAXImpl.GetStdDev: Single;
begin
  Result:=FWindow.StdDev;
end;

function TSampleGDAXImpl.GetWindowSize: Cardinal;
begin
  Result:=FWindow.WindowSizeInMilli;
end;

procedure TSampleGDAXImpl.SetCleanPerc(const AValue: Single);
begin
  FWindow.CleanupPercentage:=AValue;
end;

procedure TSampleGDAXImpl.SetCleanThresh(const AValue: Single);
begin
  FWindow.CleanupThreshold:=AValue;
end;

procedure TSampleGDAXImpl.SetWindowSize(const AValue: Cardinal);
begin
  FWindow.WindowSizeInMilli:=AValue;
end;

function TSampleGDAXImpl.GetTickers: TTickers;
begin
  Result:=FWindow.Tickers;
end;

function TSampleGDAXImpl.DoFeed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory: Extended; out
  Error: String): Boolean;
var
  LGDAXOrder:IGDAXOrder;
  LDetails:IGDAXOrderDetails;
  LTicker:ITickerGDAX;
begin
  //****************************************************************************
  //below we are broken into two main sections, the "house keeping" stuff
  //which just determines whether or not we should hit our main logic,
  //and of course the main logic
  //****************************************************************************

  //make a call to our parent and check the result to make sure everything
  //is fine to move ahead with
  Result:=inherited DoFeed(ATicker, AManager, AFunds, AInventory, Error);

  //if not, we'll terminate
  if not Result then
    Exit;

  //now set result back to false, since we are performing new logic
  Result:=False;

  //because we are using delegation we need to also "feed" our delegate
  //strategy in order for it's "plumbing" to work. for custom solution
  //or strategies not relying on other base classes, this step in not necessary
  if not FWindow.Feed(ATicker,AManager,AFunds,AInventory,Error) then
    Exit;

  //cast the incoming ticker to a gdax ticker for more useful information
  //(we can do this, because we inherit from the GDAX implementation which
  //guarantees we are provided this before continuing)
  LTicker:=ATicker as ITickerGDAX;

  //window strategies have a "ready" property to specify that enough ticker
  //data has been collected, so lets check before we do any real work.
  //exit "true" here because a failure didn't actually happen, just postponing
  //the work for later
  if not FWindow.IsReady then
    Exit(True);

  //another simple check is to make sure we have enough funds to even place
  //the size order (again just soft exit unless an error is what we want)
  if AFunds<(LTicker.Ticker.Product.BaseMinSize * LTicker.Ticker.Bid) then
    Exit(True);

  //****************************************************************************
  //our simple example simply attempts to place an order for the smallest
  //amount of base currency and then cancel
  //after a certain amount of time if not filled, then repeat the process
  //****************************************************************************

  //check to see if we have placed an order with the manager by using it's
  //count property
  if (AManager.Count>0) and (AManager.Exists[FID]) then
  begin
    case AManager.Status[FID] of
      //in our sample, we simply attempt to cancel the order
      omActive:
        begin
          if not AManager.Cancel(FID,IOrderDetails(LDetails),Error) then
            Exit;
        end;
      //on cancel order or completed, just remove so we can purchase more
      //orders during the next tick
      omCanceled,omCompleted:
        begin
          if not AManager.Delete(FID,Error) then
            Exit;
        end;
    end;
  end
  //otherwise we should try and place an order, then record the ID generated
  //by the manager for later use
  else
  begin
    //create a gdax order
    LGDAXOrder:=TGDAXOrderImpl.Create;

    //next set the product we are operating with from the ticker
    LGDAXOrder.Product:=LTicker.Ticker.Product;

    //set to the minimum size allowed for the product
    LGDAXOrder.Size:=LTicker.Ticker.Product.BaseMinSize;

    //use the "ask" price to determine the current quickest likely
    LGDAXOrder.Price:=LTicker.Ticker.Bid;

    //set the order to a "limit" type which on GDAX currently has no fees
    LGDAXOrder.OrderType:=TOrderType.otLimit;

    //now create the details for the order manager to work with
    LDetails:=TGDAXOrderDetailsImpl.Create(LGDAXOrder);

    //attempt to place the order with the manager
    if not AManager.Place(
      LDetails,
      FID,
      Error
    ) then
      Exit;
  end;

  //lastly, since everything was performed successfully, exit as true
  Result:=True;
end;

constructor TSampleGDAXImpl.Create;
begin
  inherited Create;

  //create an instance of TWindowStrategyImpl to handle the delegation.
  //also to note, that this strategy doesn't actually make use of the window,
  //but thought it may be useful to demonstrate how to save a lot of time
  //utilizing base classes not related to a particular crypto exchange
  FWindow:=TWindowStrategyImpl.Create;
end;

destructor TSampleGDAXImpl.Destroy;
begin
  //don't forget to free your resources
  FWindow:=nil;
  inherited Destroy;
end;

end.

