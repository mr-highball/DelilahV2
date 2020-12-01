unit delilah.strategy.gdax.sample;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, delilah.strategy.gdax,
  delilah.strategy.window, DateUtils;

type

  //here we would add any additional properties or method
  //that pertain to your strategy, such as IWindowStrategy if
  //we didn't want to go the delegate route

  { ISampleGDAX }

  ISampleGDAX = interface(IStrategyGDAX)
    ['{0BC943DB-FA2F-4C42-BF55-83C22C82123D}']
    //property methods
    function GetMultiplier: Cardinal;
    function GetWindow: IWindowStrategy;
    procedure SetMultiplier(Const AValue: Cardinal);

    //properties
    (*
      demonstrating adding a new property.
      the multiplier will attempt to "multiply" the base minimum size
      of a given product for buy orders where funds will permit, otherwise
      the minimum size will be used
    *)
    property Multiplier : Cardinal read GetMultiplier write SetMultiplier;

    (*
      uses a window strategy for collecting a range of tickers
    *)
    property Window : IWindowStrategy read GetWindow;
  end;

  { TSampleGDAXImpl }
  (*
    implementation of our sample strategy
  *)
  TSampleGDAXImpl = class(TStrategyGDAXImpl, ISampleGDAX)
  strict private
    FWindow: IWindowStrategy;
    FID: String;
    FTime: TDateTime;
    FMultiplier,
    FAccumulate: Cardinal;
  protected
    function GetMultiplier: Cardinal;
    procedure SetMultiplier(Const AValue: Cardinal);
    function GetWindow: IWindowStrategy;
  strict protected
    (*
      this method is the primary method used to operate on "the tick" and
      where most logic will be implemented (or called via other methods)
    *)
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      const AFunds, AInventory, AAAC: Extended; out Error: String): Boolean; override;

    (*
      below are two virtual methods we introduce in this class to further
      extend functionality of the sample strategy. since we've written
      a lot of boiler plate safety checking code, we want to avoid duplicating
      it as much as possible and reuse any niceties that we introduced in
      our base sample strategy
    *)
    function DoAllowBuy(Const AFunds,AInventory,AAC,ATickerPrice:Extended;Out Reason:String):Boolean;virtual;
    function DoAllowSell(Const AFunds,AInventory,AAC,ATickerPrice:Extended;Out Reason:String):Boolean;virtual;
  public
    property Window : IWindowStrategy read GetWindow;
    property Multiplier : Cardinal read GetMultiplier write SetMultiplier;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  Math,
  delilah.ticker.gdax,//gdax ticker for engine
  delilah.order.gdax,//order detail implementation for engine
  gdax.api.types,//contains common types associated to GDAX
  gdax.api.orders,//contains implementation of order specific stuff
  gdax.api.consts;//enums/consts etc...

{ TSampleGDAXImpl }

function TSampleGDAXImpl.GetMultiplier: Cardinal;
begin
  Result := FMultiplier;
end;

procedure TSampleGDAXImpl.SetMultiplier(const AValue: Cardinal);
begin
  FMultiplier := AValue;
end;

function TSampleGDAXImpl.GetWindow: IWindowStrategy;
begin
  Result := FWindow;
end;

function TSampleGDAXImpl.DoFeed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LGDAXOrder:IGDAXOrder;
  LDetails:IGDAXOrderDetails;
  LTicker:ITickerGDAX;
  LPriceDiff:Single;
  LPrice,
  LFunds,
  LMin,
  LInv,
  LSize,
  LAAC:Extended;
  LSecondsBetween: Integer;
  LReason:String;
begin
  //****************************************************************************
  //below we are broken into two main sections, the "house keeping" stuff
  //which just determines whether or not we should hit our main logic,
  //and of course the main logic
  //****************************************************************************

  {%region Pre-Checks}
  //make a call to our parent and check the result to make sure everything
  //is fine to move ahead with
  Result:=inherited DoFeed(ATicker, AManager, AFunds, AInventory, AAAC, Error);

  //if not, we'll terminate
  if not Result then
    Exit;

  //now set result back to false, since we are performing new logic
  Result:=False;

  //because we are using delegation we need to also "feed" our delegate
  //strategy in order for it's "plumbing" to work. for custom solution
  //or strategies not relying on other base classes, this step in not necessary
  if not FWindow.Feed(ATicker,AManager,AFunds,AInventory,AAAC,Error) then
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
  begin
    LogInfo(
      Format(
        'window is not ready [size]:%d [collected]:%d',
        [
          FWindow.WindowSizeInMilli,
          FWindow.CollectedSizeInMilli
        ]
      )
    );
    Exit(True);
  end;
  {%endregion}
  //****************************************************************************
  //our simple example simply attempts to place an order for the smallest
  //amount of base currency and then cancel
  //after a certain amount of time if not filled, then repeat the process.
  //if the AAC is lower then the current ticker price, then the strategy will
  //prioritize a sell instead for the same minimum amount allowed.
  //****************************************************************************

  //check to see if we have placed an order with the manager by using it's
  //count property
  {%region Monitor-Order}
  if (AManager.Count>0) and (AManager.Exists[FID]) then
  begin
    LogInfo('order id exists and we have an open order');
    if not AManager.Details(FID,IOrderDetails(LDetails),Error) then
      Exit;
    case AManager.Status[FID] of
      //in our sample, we simply attempt to cancel the order
      omActive:
        begin
          //simple calculation to see if we should cancel and try to purchase
          //for a price which will lead to a quicker order (expanded for easy
          //debugging values)
          if LDetails.Order.Side=osBuy then
            LPriceDiff:=RoundTo(LTicker.Ticker.Bid,-8) - RoundTo(LDetails.Price,-8)
          else
            LPriceDiff:=RoundTo(LTicker.Ticker.Ask,-8) - RoundTo(LDetails.Price,-8);
          LSecondsBetween:=SecondsBetween(Now,FTime);

          //see if the ticker price is different and that some time
          //has elapsed since we created before cancelling (this could be
          //a property of the sample strategy, but for now, simply hardcode)
          if (LPriceDiff<>0) and (LSecondsBetween>30) then
          begin
            LogInfo('attempting to cancel order');
            if not AManager.Cancel(FID,IOrderDetails(LDetails),Error) then
              Exit;
            LogInfo('order cancelled');
          end;
        end;
      //on cancel order or completed, just remove so we can purchase more
      //orders during the next tick
      omCanceled,omCompleted:
        begin
          LogInfo('order completed or cancelled, removing from manager');
          if not AManager.Delete(FID,Error) then
            Exit;
        end;
    end;
  end
  {%endregion}
  //otherwise we should try and place an order, then record the ID generated
  //by the manager for later use
  else
  begin
    LogInfo('looking to see if we should place an order');

    //create a gdax order
    LGDAXOrder:=TGDAXOrderImpl.Create;

    //next set the product we are operating with from the ticker
    LGDAXOrder.Product:=LTicker.Ticker.Product;

    //set these for easy debug. rounding here, because engine
    //may provide too precise measurements for simple equality checks when
    //working with "dust" (very small amounts after trading)
    LMin:=RoundTo(LTicker.Ticker.Product.BaseMinSize,-8);
    LSize:=LMin;
    LInv:=RoundTo(AInventory,-8);
    LPrice:=RoundTo(LTicker.Ticker.Price,-8);
    LFunds:=RoundTo(AFunds,-8);

    //if the inventory is less than the minimum amount, just show
    //a zero aquisition cost to allow for buys when there is "dust"
    if LInv < LMin then
      LAAC:=0
    else
      LAAC:=RoundTo(AAAC,-8);

    //below we are going to show how we can use the average cost
    //and our current inventory to setup a sell or a buy. in the
    //case the current ticker is greater than the cost of goods,
    //we can put up a sell order utilizing the side of the GDAX order.
    {%region Sell-Logic}
    if (LPrice > AAAC) and (LInv >= LSize) then
    begin
      LogInfo('ticker/aac/inventory look right for a sell order');
      LPrice:=LTicker.Ticker.Ask;

      //see buy else statement for comments on properties
      LGDAXOrder.Price:=LPrice;
      LGDAXOrder.Size:=LSize;
      LGDAXOrder.OrderType:=TOrderType.otLimit;
      LDetails:=TGDAXOrderDetailsImpl.Create(LGDAXOrder);
      LDetails.Order.Side:=osSell;

      //before placing the order, call down to our allow sell method
      //and exit silently if not
      if not DoAllowSell(LFunds,LInv,LAAC,LPrice,LReason) then
      begin
        LogInfo(Format('DoAllowSell returned false with [reason]:%s',[LReason]));
        Exit(True);
      end;

      LogInfo('attempting to place sell order');
      if not AManager.Place(
        LDetails,
        FID,
        Error
      ) then
        Exit;
    end
    {%endregion}
    //otherwise, we will just continue trying to buy
    {%region Buy-Logic}
    else
    begin
      LogInfo('ticker/aac/inventory look right for a buy order');

      //use the "bid" price to determine the current quickest likely buy in price
      //(if we were attempting to sell using the order side prop osSell, then
      //we would want to use the "ask" for the best price)
      LPrice:=LTicker.Ticker.Bid;

      //see if we have a multiplier specified, and enough funds to cover it
      if FMultiplier > 1 then
      begin
        if (LFunds / (LMin * LPrice)) >= FMultiplier then
          LSize:=LMin * FMultiplier
        else
          LSize:=LMin;
      end
      //set to the minimum size allowed for the product
      else
        LSize:=LMin;

      //here is a simple check to make sure we always buy lower than cost
      //in order to achieve a simple dollar cost averaging mechanism
      if (LPrice >= AAAC) and (LInv >= LMin) then
      begin
        LogInfo('conditions not right for a buy order');
        Exit(True);
      end;

      //set the order to a "limit" type which on GDAX currently has no fees
      LGDAXOrder.OrderType:=TOrderType.otLimit;
      LGDAXOrder.Price:=LPrice;
      LGDAXOrder.Size:=LSize;
      LGDAXOrder.Side:=osBuy;

      //before placing the order, call down to our allow buy method
      //and exit silently if not
      if not DoAllowBuy(LFunds,LInv,LAAC,LPrice,LReason) then
      begin
        LogInfo(Format('DoAllowBuy returned false with [reason]:%s',[LReason]));
        if not (MilliSecondsBetween(Now,FTime) >= FAccumulate) then
          Exit(True)
        else
        begin
          LogInfo('waited in channel long enough... going to accumulate for min size');
          LSize:=LMin;
          LGDAXOrder.Size:=LSize;
        end;
      end;

      //another simple check is to make sure we have enough funds to even place
      //the size order (again just soft exit unless an error is what we want)
      if LFunds < (LSize * LPrice) then
      begin
        LogInfo(
          Format(
            'not enough funds [available]:%f [required]:%f',
            [
              LFunds,
              LSize * LPrice
            ]
          )
        );
        Exit(True);
      end;

      //now create the details for the order manager to work with
      LDetails:=TGDAXOrderDetailsImpl.Create(LGDAXOrder);

      //attempt to place the order with the manager
      LogInfo('attempting to place buy order');
      if not AManager.Place(
        LDetails,
        FID,
        Error
      ) then
        Exit;
    end;

    //store the time we placed to avoid any mismatch we may have from
    //comparing local to UTC time (GDAX works with UTC)
    FTime:=Now;
  end;
  {%endregion}

  //lastly, since everything was performed successfully, exit as true
  Result:=True;
end;

function TSampleGDAXImpl.DoAllowBuy(const AFunds, AInventory, AAC,
  ATickerPrice: Extended; out Reason: String): Boolean;
begin
  Result:=True;
end;

function TSampleGDAXImpl.DoAllowSell(const AFunds, AInventory, AAC,
  ATickerPrice: Extended; out Reason: String): Boolean;
begin
  Result:=True;
end;

constructor TSampleGDAXImpl.Create;
begin
  inherited Create;

  //create an instance of TWindowStrategyImpl
  FWindow := TWindowStrategyImpl.Create;

  //accumulate min size if we wait in a channel for this long
  FAccumulate:=15 * 60 * 1000;
end;

destructor TSampleGDAXImpl.Destroy;
begin
  //don't forget to free your resources
  FWindow:=nil;
  inherited Destroy;
end;

end.

