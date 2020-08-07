unit delilah.strategy.bobber.gdax;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  delilah.types,
  delilah.strategy.gdax,
  delilah.order.gdax,
  delilah.strategy.bobber,
  delilah.ticker.gdax;

type

  { IGDAXBobberStrategy }
  (*
    GDAX specialized bobber strategy
  *)
  IGDAXBobberStrategy = interface(IBobberStrategy)
    ['{61BDDC45-C3A9-4632-B296-C52BD94F88CA}']

    //property methods
    function GetUseLimitBuy: Boolean;
    function GetUseLimitSell: Boolean;
    procedure SetUseLimitBuy(const AValue: Boolean);
    procedure SetUseLimitSell(const AValue: Boolean);

    //properties

    (*
      when set to 'True' buys will be performed using limit orders, otherwise
      a market order will be used
    *)
    property UseLimitBuy : Boolean read GetUseLimitBuy write SetUseLimitBuy;

    (*
      when set to 'True' sells will be performed using limit orders, otherwise
      a market order will be used
    *)
    property UseLimitSell : Boolean read GetUseLimitSell write SetUseLimitSell;
  end;

  { TGDAXBobberStrategyImpl }
  (*
    base implementation for IBobberStrategy for the gdax/cb pro exchange
  *)
  TGDAXBobberStrategyImpl = class(TStrategyGDAXImpl, IGDAXBobberStrategy)
  strict private
    FThresh,
    FAnchThresh,
    FPosSize,
    FAnchor,
    FFunds : Extended;
    FState : TBobberState;
    FMode : TBobberFundsMode;
    FExitRequest,
    FEnterRequest,
    FLimitBuy,
    FLimitSell: Boolean;
    FOrder : IGDAXOrderDetails;
    FOrderID : String;

    (*
      handles state for a new ticker when we are in position
    *)
    function FeedInPos(const ATicker : ITickerGDAX; const AManager : IOrderManager;
      const AFunds, AInventory, AAAC : Extended; out Error : String) : Boolean;

    (*
      handles state for a new ticker when we are out of position
    *)
    function FeedOutPos(const ATicker : ITickerGDAX; const AManager : IOrderManager;
      const AFunds, AInventory, AAAC : Extended; out Error : String) : Boolean;

    (*
      handles state for a new ticker when we are attempting to enter position
    *)
    function FeedEnteringPos(const ATicker : ITickerGDAX; const AManager : IOrderManager;
      const AFunds, AInventory, AAAC : Extended; out Error : String) : Boolean;

    (*
      handles state for a new ticker when we are attempting to exit position
    *)
    function FeedExitingPos(const ATicker : ITickerGDAX; const AManager : IOrderManager;
      const AFunds, AInventory, AAAC : Extended; out Error : String) : Boolean;

    (*
      checks if the current price would become the new anchor and can optionally
      set the anchor price
    *)
    function IsNewAnchor(const ATicker : ITickerGDAX; const ABuySide : Boolean;
      out IsUp : Boolean; const ASetAnchor : Boolean = False) : Boolean;

    (*
      attempts to close the current position and will adjust state appropriately
      will fail if an error occurs at the order manager and state will not be affected
    *)
    function ClosePosition(const ATicker : ITickerGDAX; const AManager : IOrderManager;
      const AFunds, AInventory, AAAC : Extended; out Error : String) : Boolean;

    (*
      attempts to open a new position and will adjust state appropriately
      will fail if an error occurs at the order manager and state will not be affected
    *)
    function OpenPosition(const ATicker : ITickerGDAX; const AManager : IOrderManager;
      const AFunds, AInventory, AAAC : Extended; out Error : String) : Boolean;

    (*
      avoid "too specific" errors by using adjusting by minimum product size
    *)
    procedure AdjustForMinSize(var Size : Extended; const ATicker : ITickerGDAX);
  protected
    function GetThresh: Extended;
    function GetAnchThresh: Extended;
    function GetPositionSize: Extended;
    procedure SetPositionSize(const AValue: Extended);
    function GetState: TBobberState;
    procedure SetAnchor(const AValue: Extended);
    procedure SetFunds(const AValue: Extended);
    procedure SetMode(const AValue: TBobberFundsMode);
    function GetMode: TBobberFundsMode;
    procedure SetThresh(const AValue: Extended);
    procedure SetAnchThresh(const AValue: Extended);
    function GetAnchor: Extended;
    function GetFunds: Extended;

    function GetUseLimitBuy: Boolean;
    function GetUseLimitSell: Boolean;
    procedure SetUseLimitBuy(const AValue: Boolean);
    procedure SetUseLimitSell(const AValue: Boolean);
  strict protected
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      const AFunds, AInventory, AAAC: Extended; out Error: String): Boolean; override;
  public
    property Threshold : Extended read GetThresh write SetThresh;
    property AdjustAnchorThreshold : Extended read GetAnchThresh write SetAnchThresh;
    property Anchor : Extended read GetAnchor write SetAnchor;
    property Funds : Extended read GetFunds write SetFunds;
    property FundsMode : TBobberFundsMode read GetMode write SetMode;
    property State : TBobberState read GetState;
    property PositionSize : Extended read GetPositionSize write SetPositionSize;

    property UseLimitBuy : Boolean read GetUseLimitBuy write SetUseLimitBuy;
    property UseLimitSell : Boolean read GetUseLimitSell write SetUseLimitSell;

    procedure ExitPosition;
    procedure EnterPosition;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  typinfo,
  gdax.api.types,
  gdax.api.orders,
  gdax.api.consts;

{ TGDAXBobberStrategyImpl }

function TGDAXBobberStrategyImpl.GetThresh: Extended;
begin
  Result := FThresh;
end;

function TGDAXBobberStrategyImpl.GetAnchThresh: Extended;
begin
  Result := FAnchThresh;
end;

function TGDAXBobberStrategyImpl.GetPositionSize: Extended;
begin
  Result := FPosSize;
end;

function TGDAXBobberStrategyImpl.GetState: TBobberState;
begin
  Result := FState;
end;

procedure TGDAXBobberStrategyImpl.SetAnchor(const AValue: Extended);
begin
  FAnchor := AValue;
end;

procedure TGDAXBobberStrategyImpl.SetFunds(const AValue: Extended);
begin
  FFunds := AValue;
end;

procedure TGDAXBobberStrategyImpl.SetMode(const AValue: TBobberFundsMode);
begin
  FMode := AValue;
end;

function TGDAXBobberStrategyImpl.GetMode: TBobberFundsMode;
begin
  Result := FMode;
end;

procedure TGDAXBobberStrategyImpl.SetThresh(const AValue: Extended);
begin
  FThresh := AValue;
end;

procedure TGDAXBobberStrategyImpl.SetAnchThresh(const AValue: Extended);
begin
  FAnchThresh := AValue;
end;

function TGDAXBobberStrategyImpl.GetAnchor: Extended;
begin
  Result := FAnchor;
end;

function TGDAXBobberStrategyImpl.GetFunds: Extended;
begin
  Result := FFunds;
end;

function TGDAXBobberStrategyImpl.GetUseLimitBuy: Boolean;
begin
  Result := FLimitBuy;
end;

function TGDAXBobberStrategyImpl.GetUseLimitSell: Boolean;
begin
  Result := FLimitSell;
end;

procedure TGDAXBobberStrategyImpl.SetUseLimitBuy(const AValue: Boolean);
begin
  FLimitBuy := AValue;
end;

procedure TGDAXBobberStrategyImpl.SetUseLimitSell(const AValue: Boolean);
begin
  FLimitSell := AValue;
end;

function TGDAXBobberStrategyImpl.DoFeed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LTicker : ITickerGDAX;
begin
  try
    Result := inherited DoFeed(ATicker, AManager, AFunds, AInventory, AAAC, Error);

    if not Result then
      Exit;

    //reset result
    Result := False;

    //cast to a gdax ticker for ask/bid
    LTicker := ATicker as ITickerGDAX;

    //handle redirecting inputs to proper state method
    case FState of
      bsInPos:
        Result := FeedInPos(LTicker, AManager, AFunds, AInventory, AAAC, Error);
      bsOutPos:
        Result := FeedOutPos(LTicker, AManager, AFunds, AInventory, AAAC, Error);
      bsEntering:
        Result := FeedEnteringPos(LTicker, AManager, AFunds, AInventory, AAAC, Error);
      bsExiting:
        Result := FeedExitingPos(LTicker, AManager, AFunds, AInventory, AAAC, Error);
      else
        Error := 'DoFeed::unhandled state';
    end;

    //now log out the details for the strategy
    LogInfo(Format('DoFeed::state:%s anchor:%f position:%f', [GetEnumName(TypeInfo(TBobberState), Ord(FState)), FAnchor, FPosSize]));
  except on E : Exception do
    Error := 'DoFeed::' + E.Message;
  end;
end;

function TGDAXBobberStrategyImpl.FeedInPos(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LIsUp: Boolean;
begin
  Result := False;

  //check to see if we "actually" are in a position
  if AInventory <= ATicker.Ticker.Product.BaseMinSize then
  begin
    //reset to out of position and exit
    FState := bsOutPos;
    FPosSize := 0;
    Exit(True);
  end;

  //check to see if we've found a new anchor price
  if IsNewAnchor(ATicker, False, LIsUp) then
  begin
    //as long as the new anchor is higher, we can just move on
    if LIsUp then
      Result := True
    //otherwise we need to try and close our position
    else
      Result := ClosePosition(ATicker, AManager, AFunds, AInventory, AAAC, Error);

    //as long as we are successful, set the new anchor
    if Result then
      IsNewAnchor(ATicker, False, LIsUp, True);
  end
  //if we don't have a new anchor, check to see if an exit request has been made
  else if FExitRequest then
  begin
    Result := ClosePosition(ATicker, AManager, AFunds, AInventory, AAAC, Error);

    //on success, close out the exit request since it has been made
    if Result then
      FExitRequest := False;
  end
  //nothing to do here
  else
    Result := True;
end;

function TGDAXBobberStrategyImpl.FeedOutPos(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LIsUp: Boolean;
begin
  Result := False;

  //check to see if we "actually" are out of position by seeing if
  //there is not enough to funds to purchase the minimum product
  if (AFunds / ATicker.Ticker.Ask) < ATicker.Ticker.Product.BaseMinSize then
  begin
    //now, if we don't have inventory, just bail since there's nothing to do
    if AInventory < ATicker.Ticker.Product.BaseMinSize then
      Exit(True);

    //reset to in position since we have no more funds to use
    FState := bsInPos;
    Exit(True);
  end;

  //check to see if we've found a new anchor price
  if IsNewAnchor(ATicker, True, LIsUp) then
  begin
    //as long as the new anchor is lower, we can just move on
    if not LIsUp then
      Result := True
    //otherwise we need to attempt to take a position
    else
      Result := OpenPosition(ATicker, AManager, AFunds, AInventory, AAAC, Error);

    //as long as we are successful, set the new anchor
    if Result then
      IsNewAnchor(ATicker, True, LIsUp, True);
  end
  //if we don't have a new anchor, check to see if an enter request has been made
  else if FEnterRequest then
  begin
    Result := OpenPosition(ATicker, AManager, AFunds, AInventory, AAAC, Error);

    //on success, close out the enter request since it has been made
    if Result then
      FEnterRequest := False;
  end
  //nothing to do here
  else
    Result := True;
end;

function TGDAXBobberStrategyImpl.FeedEnteringPos(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
const
  (*
    remaining size to purchase
  *)
  REMAINING_SIZE : Extended = 0;

var
  LStatus: TOrderManagerStatus;
  LDetails : IOrderDetails;

  procedure ClearOrderDetails;
  begin
    FOrder := nil;
    FOrderID := '';
    REMAINING_SIZE := 0;
  end;

  procedure PlaceReplacementOrder;
  var
    LReplacement : IGDAXOrderDetails;
    LSize : Extended;
    LID : String;
  begin
    //set local size to remaining, then adjust for quote
    LSize := REMAINING_SIZE;
    AdjustForMinSize(LSize, ATicker);

    //when the new size would be smaller than the minimum, we can
    //just exit and let the next feed finalize the position
    if LSize < ATicker.Ticker.Product.BaseMinSize then
    begin
      REMAINING_SIZE := 0;
      Exit;
    end;

    //adjust some fields on the order to place it again
    FOrder.Order.Size := LSize;
    FOrder.Order.FilledSized := 0;
    FOrder.Order.OrderStatus := stUnknown;
    FOrder.Order.ID := '';

    //to simplify things we'll just "chase" the bid until we can get a fill
    FOrder.Order.Price := ATicker.Ticker.Bid;
    LReplacement := TGDAXOrderDetailsImpl.Create(FOrder.Order);

    //if we can't place the order then bubble up the exception
    if not AManager.Place(LReplacement, LID, Error) then
      raise Exception.Create('PlaceReplacementOrder::' + Error);

    //success, so update the new order info
    FOrderID := LID;
    FOrder := LReplacement;
  end;

  procedure MoveInOrOutPosition;
  begin
    //otherwise, see if we have a position
    if FPosSize > 0 then
      FState := bsInPos
    //if all else failed above, we are "out of position"
    else
      FState := bsOutPos;

    ClearOrderDetails;
  end;

  (*
    when the order status reports back as cancelled
  *)
  procedure CancelledLogic;
  begin
    //decrement any partials and adjust position
    if FOrder.Order.FilledSized > 0 then
    begin
      FPosSize := FPosSize + FOrder.Order.FilledSized;
      REMAINING_SIZE := REMAINING_SIZE - FOrder.Order.FilledSized;
    end;

    //check to make sure we have enough funds to purchase the amount
    repeat
      //decrement the size until we find a suitable amount to purchase
      if (REMAINING_SIZE * ATicker.Ticker.Ask) > AFunds then
      begin
        REMAINING_SIZE := REMAINING_SIZE - ATicker.Ticker.Product.BaseMinSize;
        LogInfo(Format('FeedEnteringPos::CancelledLogic::size too high, adjusting remaining size, new [size]:%f', [REMAINING_SIZE]))
      end
      else
        break;
    until REMAINING_SIZE < ATicker.Ticker.Product.BaseMinSize;

    //min size check
    if REMAINING_SIZE < ATicker.Ticker.Product.BaseMinSize then
      REMAINING_SIZE := 0;

    //check if retrying
    if REMAINING_SIZE > 0 then
      PlaceReplacementOrder
    //otherwise move in or out of position
    else
      MoveInOrOutPosition;
  end;

  (*
    if the order status is completed
  *)
  procedure CompletedLogic;
  begin
    //need an order to continue, so mark state depending on pos size
    //also to note, we can't have replacement orders in this situation
    if not Assigned(FOrder) or (not Assigned(FOrder.Order)) then
    begin
      MoveInOrOutPosition;
      Exit;
    end;

    //update position
    FPosSize := FPosSize + FOrder.Order.FilledSized;

    //decrement the filled from the remaining size
    REMAINING_SIZE := REMAINING_SIZE - FOrder.Order.FilledSized;

    //if the remaining amount is less than the minimum size, discard it
    if REMAINING_SIZE < ATicker.Ticker.Product.BaseMinSize then
      REMAINING_SIZE := 0;

    //check to see if we have any remaining order amount, if so place a new order
    if REMAINING_SIZE > 0 then
    begin
      PlaceReplacementOrder;
      Exit;
    end;

    //move in or out of position
    MoveInOrOutPosition;
  end;

begin
  try
    Result := False;

    //don't be dumb
    if FPosSize < 0 then
      FPosSize := 0;

    //orders are removed from the manager on completion, so first check
    //to see if it exists
    if not AManager.Exists[FOrderID] then
    begin
      //reset when order details has been cleared
      if not Assigned(FOrder) then
        CompletedLogic
      //handle cancels
      else if FOrder.Order.OrderStatus = stCancelled then
        CancelledLogic
      //handle completed status
      else
       CompletedLogic;
    end
    //otherwise we need to handle either re-placing limit orders or waiting
    else
    begin
      LStatus := AManager.Status[FOrderID];

      //handle cancels by setting to out of position or in-position for partial
      if LStatus = omCanceled then
        CancelledLogic
      //check for completion
      else if LStatus = omCompleted then
        CompletedLogic
      //for limit orders that are active we handle trying to keep up with the price
      else if (LStatus = omActive) and (FOrder.Order.OrderType = otLimit) then
      begin
        //initialize the remaining size
        if REMAINING_SIZE <> FOrder.Size then
          REMAINING_SIZE := FOrder.Size;

        //if the bid hasn't changed then there's nothing to do
        if ATicker.Ticker.Bid = FOrder.Order.Price then
          Exit(True);

        //attempt to cancel
        if not AManager.Cancel(FOrderID, LDetails, Error) then
        begin
          Error := 'FeedEnteringPos::' + Error;
          Exit;
        end;
      end;
    end;

    //log some info
    WriteLn(Format('FeedEnteringPos::[remainingSize]:%f', [REMAINING_SIZE]));

    //if no conditions bailed early above move on for the next feed
    Result := True;
  except on E : Exception do
    Error := 'FeedEnteringPos::' + E.Message;
  end;
end;

function TGDAXBobberStrategyImpl.FeedExitingPos(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LStatus: TOrderManagerStatus;
  LDetails : IOrderDetails;

  procedure ClearOrderDetails;
  begin
    FOrder := nil;
    FOrderID := '';
    FPosSize := 0;
  end;

  procedure PlaceReplacementOrder;
  var
    LReplacement : IGDAXOrderDetails;
    LSize : Extended;
    LID : String;
  begin
    //in this case pos size is decrement for partials so we can use it directly
    LSize := FPosSize;
    AdjustForMinSize(LSize, ATicker);

    //when the new size would be smaller than the minimum, we can
    //just exit and let the next feed finalize the position
    if LSize < ATicker.Ticker.Product.BaseMinSize then
    begin
      FPosSize := 0;
      Exit;
    end;

    //adjust some fields on the order to place it again
    FOrder.Order.Size := LSize;
    FOrder.Order.FilledSized := 0;
    FOrder.Order.OrderStatus := stUnknown;
    FOrder.Order.ID := '';

    //to simplify things we'll just "chase" the Ask until we can get a fill
    FOrder.Order.Price := ATicker.Ticker.Ask;
    LReplacement := TGDAXOrderDetailsImpl.Create(FOrder.Order);

    //if we can't place the order then bubble up the exception
    if not AManager.Place(LReplacement, LID, Error) then
      raise Exception.Create('PlaceReplacementOrder::' + Error);

    //update new order info
    FOrderID := LID;
    FOrder := LReplacement;
  end;

  procedure MoveInOrOutPosition;
  begin
    //otherwise, see if we have a position
    if FPosSize > 0 then
      FState := bsInPos
    //if all else failed above, we are "out of position"
    else
      FState := bsOutPos;

    ClearOrderDetails;
  end;

  (*
    when the order status reports back as cancelled
  *)
  procedure CancelledLogic;
  begin
    //decrement any partials and adjust position
    if FOrder.Order.FilledSized > 0 then
      FPosSize := FPosSize - FOrder.Order.FilledSized;

    //now make sure position size isn't higher than inventory
    if FPosSize > AInventory then
      FPosSize := AInventory;

    //min size check
    if FPosSize < ATicker.Ticker.Product.BaseMinSize then
      FPosSize := 0;

    //check if retrying (still have inventory to sell)
    if FPosSize > 0 then
      PlaceReplacementOrder
    //otherwise move in or out of position
    else
      MoveInOrOutPosition;
  end;

  (*
    if the order status is completed
  *)
  procedure CompletedLogic;
  begin
    //need an order to continue, so mark state depending on pos size
    //also to note, we can't have replacement orders in this situation
    if not Assigned(FOrder) or (not Assigned(FOrder.Order)) then
    begin
      MoveInOrOutPosition;
      Exit;
    end;

    //update position
    FPosSize := FPosSize - FOrder.Order.FilledSized;

    //if the remaining amount is less than the minimum size, discard it
    if FPosSize < ATicker.Ticker.Product.BaseMinSize then
      FPosSize := 0;

    //check to see if we have any remaining order amount, if so place a new order
    if FPosSize > 0 then
    begin
      PlaceReplacementOrder;
      Exit;
    end;

    //move in or out of position
    MoveInOrOutPosition;
  end;

begin
  try
    Result := False;

    //simple check to make sure an external source hasn't sold the inventory
    if FPosSize > AInventory then
      FPosSize := AInventory;

    //orders are removed from the manager on completion, so first check
    //to see if it exists
    if not AManager.Exists[FOrderID] then
    begin
      //reset when order details has been cleared
      if not Assigned(FOrder) then
        CompletedLogic
      //handle cancels
      else if FOrder.Order.OrderStatus = stCancelled then
        CancelledLogic
      //handle completed status
      else
       CompletedLogic;
    end
    //otherwise we need to handle either re-placing limit orders or waiting
    else
    begin
      LStatus := AManager.Status[FOrderID];

      //handle cancels by setting to out of position or in-position for partial
      if LStatus = omCanceled then
        CancelledLogic
      //check for completion
      else if LStatus = omCompleted then
        CompletedLogic
      //for limit orders that are active we handle trying to keep up with the price
      else if (LStatus = omActive) and (FOrder.Order.OrderType = otLimit) then
      begin
        //when an order is placed at a larger amount than the recorded
        //position size, update the position so replacement orders will
        //accurately be executed
        if FOrder.Size <> FPosSize then
        begin
          //account for external strategies to adjust inventory
          if FOrder.Size < AInventory then
            FPosSize := FOrder.Size
          else
            FPosSize := AInventory;
        end;

        //if the ask hasn't changed then there's nothing to do
        if ATicker.Ticker.Ask = FOrder.Order.Price then
          Exit(True);

        //attempt to cancel
        if not AManager.Cancel(FOrderID, LDetails, Error) then
        begin
          Error := 'FeedExitingPos::' + Error;
          Exit;
        end;
      end;
    end;

    //if no conditions bailed early above move on for the next feed
    Result := True;
  except on E : Exception do
    Error := 'FeedExitingPos::' + E.Message;
  end;
end;

function TGDAXBobberStrategyImpl.IsNewAnchor(const ATicker: ITickerGDAX;
  const ABuySide: Boolean; out IsUp: Boolean; const ASetAnchor: Boolean): Boolean;
var
  LThresh : Extended;
begin
  Result := False;
  IsUp := False;

  //use anchor threshold first, but default to regular thresh if using a
  //a uniform mode
  if FAnchThresh > 0 then
    LThresh := FAnchThresh
  else
    LThresh := FThresh;

  if ABuySide then
  begin
    //check to see if ask price is higher than anchor
    IsUp := ATicker.Ticker.Ask > FAnchor;

    //base case
    if FAnchor <= 0 then
      Result := True
    //on an upwards move buy side needs to exceed the "open threshold"
    //find the percent difference and check if this is at or above the threshold
    else if IsUp and (Abs(1 - ATicker.Ticker.Ask / FAnchor) >= Abs(FThresh)) then
      Result := True
    //when we're going down, use the "adjust threshold"
    else if not IsUp and (Abs(1 - ATicker.Ticker.Ask / FAnchor) >= Abs(LThresh)) then
      Result := True;

    if ASetAnchor then
      FAnchor := ATicker.Ticker.Ask;
  end
  else
  begin
    //check to see if bid price is higher than anchor
    IsUp := ATicker.Ticker.Bid > FAnchor;

    //base case
    if FAnchor <= 0 then
      Result := True
    //when we're going up, use the "adjust threshold"
    else if IsUp and (Abs(1 - ATicker.Ticker.Bid / FAnchor) >= Abs(LThresh)) then
      Result := True
    //on an downards move sell side needs to exceed the "close threshold"
    //find the percent difference and check if this is at or above the threshold
    else if not IsUp and (Abs(1 - ATicker.Ticker.Bid / FAnchor) >= Abs(FThresh)) then
      Result := True;

    if ASetAnchor then
      FAnchor := ATicker.Ticker.Bid;
  end;
end;

function TGDAXBobberStrategyImpl.ClosePosition(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LBid, LMin, LSize: Extended;
  LOrder: IGDAXOrder;
  LError : String;

  (*
    fixed base determines sell amount in 'base' currency (ie.btc/usd usd would be base)
  *)
  function FixedBaseSize : Extended;
  begin
    //find the total value in base that we have
    Result := LBid * AInventory;

    //check to see if the value of position is greater than requested amount
    if Result >= FFunds then
      Result := FFunds;

    //now return the size
    Result := Result / LBid;
  end;

  (*
    fixed coin determines sell size in 'product' (ie. btc/usd btc would be coin)
  *)
  function FixedCoinSize : Extended;
  begin
    Result := AInventory;

    //as long as the total inventory is greater than what is set to maintain,
    //return the smaller of the two, otherwise return all of inventory
    if Result >= FFunds then
      Result := FFunds;
  end;

  (*
    percent available will determine sell size on a percent of current inventory
  *)
  function PercentAvailableSize : Extended;
  begin
    //set the result to a percentage current inv
    Result := AInventory * FFunds;

    //minimum inventory check
    if Result < LMin then
      Result := LMin;
  end;

  (*
    percent total calculates the account's 'total' value, and takes a percentage
    of this to sell
  *)
  function PercentTotalSize : Extended;
  var
    LCurrent : Extended;
  begin
    //in this case ffunds is a percent of total
    Result := ((AInventory * AAAC + AFunds) * FFunds) / LBid;

    //set to the current inventory
    LCurrent := AInventory;

    //when the current value is less than what is requested, set it to current
    if Result >= LCurrent then
      Result := LCurrent;
  end;

begin
  try
    Result := False;

    //position size is "actual" inventory, but we need to check if what we
    //have recorded isn't less than what is currently available
    if FPosSize > AInventory then
      FPosSize := AInventory;

    //check to see if our position is less than the minimum size, if so
    //we can skip straight to "out of position"
    if FPosSize < ATicker.Ticker.Product.BaseMinSize then
    begin
      FPosSize := 0;
      FState := bsOutPos;
      Exit(True);
    end;

    //initialize local vars
    LBid := ATicker.Ticker.Bid;
    LSize := 0;
    LMin := ATicker.Ticker.Product.BaseMinSize;

    //since we use market orders, we need to adjust for funds
    if not FLimitSell and (LMin < (ATicker.Ticker.Product.MinMarketFunds / LBid)) then
      LMin :=  ATicker.Ticker.Product.MinMarketFunds / LBid;

    //determine the size of the order dependant on the mode
    case FMode of
      bmFixedBase:
        LSize := FixedBaseSize;
      bmFixedCoin:
        LSize := FixedCoinSize;
      bmPercentAvailable:
        LSize := PercentAvailableSize;
      bmPercentTotal:
        LSize := PercentTotalSize;
      else
      begin
        Error := 'ClosePosition::unhandled funds mode';
        Exit;
      end
    end;

    //now make sure we respect the min size
    AdjustForMinSize(LSize, ATicker);

    //create and initialize an order
    LOrder := TGDAXOrderImpl.Create;
    LOrder.Product := ATicker.Ticker.Product;

    if FLimitSell then
      LOrder.OrderType := otLimit
    else
      LOrder.OrderType := otMarket;

    LOrder.Side := osSell;
    LOrder.Price := LBid;
    LOrder.Size := LSize;

    //record the details so we can monitor status
    FOrder := TGDAXOrderDetailsImpl.Create(LOrder);

    //try and submit the order
    if not AManager.Place(FOrder, FOrderID, LError) then
    begin
      Error := 'ClosePosition::' + LError;

      //if failure we don't update state
      Exit;
    end;

    //on success update the state to 'exiting' and return
    FState := bsExiting;
    Result := True;
  except on E : Exception do
    Error := 'ClosePosition::' + E.Message;
  end;
end;

function TGDAXBobberStrategyImpl.OpenPosition(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LAsk, LMin, LSize: Extended;
  LOrder: IGDAXOrder;
  LError : String;

  (*
    fixed base determines buy amount in 'base' currency (ie.btc/usd usd would be base)
  *)
  function FixedBaseSize : Extended;
  begin
    //init to the total size we can purchase
    Result := FFunds / LAsk;

    //if we don't have enough funds to cover the fixed amount, use the total funds
    if Result > (AFunds / LAsk) then
      Result := AFunds / LAsk;
  end;

  (*
    fixed coin determines sell size in 'product' (ie. btc/usd btc would be coin)
  *)
  function FixedCoinSize : Extended;
  begin
    Result := AInventory;

    //as long as the total inventory is greater than what is set to maintain,
    //return the smaller of the two, otherwise return all of inventory
    if Result > FFunds then
      Result := FFunds;
  end;

  (*
    percent available will determine sell size on a percent of current funds
  *)
  function PercentAvailableSize : Extended;
  begin
    //set the result to a percentage current inv
    Result := (AFunds * FFunds) / LAsk;

    //minimum inventory check
    if Result < LMin then
      Result := LMin;
  end;

  (*
    percent total calculates the account's 'total' value, and takes a percentage
    of this to purchase
  *)
  function PercentTotalSize : Extended;
  var
    LCurrent : Extended;
  begin
    //in this case ffunds is a percent of total
    Result := ((AInventory * AAAC + AFunds) * FFunds) / LAsk;

    //find the current total purchasable amount
    LCurrent := AFunds  / LAsk;

    //when the current value is less than what is requested, set it to current
    if Result >= LCurrent then
      Result := LCurrent;
  end;

begin
  try
    Result := False;

    //initialize local vars
    LAsk := ATicker.Ticker.Bid;
    LSize := 0;
    LMin := ATicker.Ticker.Product.BaseMinSize;

    //since we use market orders, we need to adjust for funds
    if not FLimitBuy and  (LMin < (ATicker.Ticker.Product.MinMarketFunds / LAsk)) then
      LMin :=  ATicker.Ticker.Product.MinMarketFunds / LAsk;

    //determine the size of the order dependant on the mode
    case FMode of
      bmFixedBase:
        LSize := FixedBaseSize;
      bmFixedCoin:
        LSize := FixedCoinSize;
      bmPercentAvailable:
        LSize := PercentAvailableSize;
      bmPercentTotal:
        LSize := PercentTotalSize;
      else
      begin
        Error := 'OpenPosition::unhandled funds mode';
        Exit;
      end
    end;

    //check minimum order amount
    if LSize < LMin then
      LSize := LMin;

    //now make sure we respect the min size
    AdjustForMinSize(LSize, ATicker);

    //now do a final check to see if we have enough funds to cover the buy
    if (LSize * LAsk) > AFunds then
      Exit(True);

    //create and initialize an order
    LOrder := TGDAXOrderImpl.Create;
    LOrder.Product := ATicker.Ticker.Product;

    if FLimitSell then
      LOrder.OrderType := otLimit
    else
      LOrder.OrderType := otMarket;

    LOrder.Side := osBuy;
    LOrder.Price := LAsk;
    LOrder.Size := LSize;

    //record the details so we can monitor status
    FOrder := TGDAXOrderDetailsImpl.Create(LOrder);

    //try and submit the order
    if not AManager.Place(FOrder, FOrderID, LError) then
    begin
      Error := 'OpenPosition::' + LError;

      //if failure we don't update state
      Exit;
    end;

    //on success update the state to 'entering' and return
    FState := bsEntering;
    Result := True;
  except on E : Exception do
    Error := 'OpenPosition::' + E.Message;
  end;
end;

procedure TGDAXBobberStrategyImpl.AdjustForMinSize(var Size: Extended;
  const ATicker: ITickerGDAX);
var
  LMin : Extended;
begin
  LMin := ATicker.Ticker.Product.BaseMinSize;

  Size := Trunc(Size / LMin) * LMin;

  if Size <= 0 then
    Size := 0;
end;

procedure TGDAXBobberStrategyImpl.SetPositionSize(const AValue: Extended);
begin
  FPosSize := AValue;

  if FPosSize < 0 then
    FPosSize := 0;

  if (FPosSize > 0) and (FState in [bsOutPos, bsEntering]) then
    FState := bsInPos;
end;

procedure TGDAXBobberStrategyImpl.ExitPosition;
begin
  if FState in [bsInPos, bsEntering] then
    FExitRequest := True;
end;

procedure TGDAXBobberStrategyImpl.EnterPosition;
begin
  if FState in [bsOutPos, bsExiting] then
    FEnterRequest := True;
end;

constructor TGDAXBobberStrategyImpl.Create;
begin
  inherited Create;
  FThresh := 0;
  FAnchThresh := 0;
  FPosSize := 0;
  FAnchor := -1;
  FFunds := 0;
  FState := bsOutPos;
  FMode := bmFixedBase;
  FExitRequest := False;
  FEnterRequest := False;
  FLimitBuy := True;
  FLimitSell := True;
end;

destructor TGDAXBobberStrategyImpl.Destroy;
begin
  inherited Destroy;
end;

end.

