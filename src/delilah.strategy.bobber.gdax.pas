unit delilah.strategy.bobber.gdax;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  delilah.types,
  delilah.strategy.gdax,
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
    FPosSize,
    FAnchor,
    FFunds : Extended;
    FState : TBobberState;
    FMode : TBobberFundsMode;
    FExitRequest,
    FEnterRequest,
    FLimitBuy,
    FLimitSell: Boolean;
    FOrder : IOrderDetails;

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
  protected
    function GetThresh: Extended;
    function GetPositionSize: Extended;
    function GetState: TBobberState;
    procedure SetAnchor(const AValue: Extended);
    procedure SetFunds(const AValue: Extended);
    procedure SetMode(const AValue: TBobberFundsMode);
    function GetMode: TBobberFundsMode;
    procedure SetThresh(const AValue: Extended);
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
    property Anchor : Extended read GetAnchor write SetAnchor;
    property Funds : Extended read GetFunds write SetFunds;
    property FundsMode : TBobberFundsMode read GetMode write SetMode;
    property State : TBobberState read GetState;
    property PositionSize : Extended read GetPositionSize;

    property UseLimitBuy : Boolean read GetUseLimitBuy write SetUseLimitBuy;
    property UseLimitSell : Boolean read GetUseLimitSell write SetUseLimitSell;

    procedure ExitPosition;
    procedure EnterPosition;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  delilah.order.gdax,
  gdax.api.types,
  gdax.api.orders,
  gdax.api.consts;

{ TGDAXBobberStrategyImpl }

function TGDAXBobberStrategyImpl.GetThresh: Extended;
begin
  Result := FThresh;
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
begin

end;

function TGDAXBobberStrategyImpl.FeedExitingPos(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
begin

end;

function TGDAXBobberStrategyImpl.IsNewAnchor(const ATicker: ITickerGDAX;
  const ABuySide: Boolean; out IsUp: Boolean; const ASetAnchor: Boolean): Boolean;
begin
  Result := False;
  IsUp := False;

  if ABuySide then
  begin
    //base case
    if FAnchor <= 0 then
      Result := True
    //find the percent difference and check if this is at or above the threshold
    else if Abs(1 - ATicker.Ticker.Ask / FAnchor) >= Abs(FThresh) then
      Result := True;

    //check to see if ask price is higher than anchor
    IsUp := ATicker.Ticker.Ask > FAnchor;

    if ASetAnchor then
      FAnchor := ATicker.Ticker.Ask;
  end
  else
  begin
    //base case
    if FAnchor <= 0 then
      Result := True
    //find the percent difference and check if this is at or above the threshold
    else if Abs(1 - ATicker.Ticker.Bid / FAnchor) >= Abs(FThresh) then
      Result := True;

    //check to see if bid price is higher than anchor
    IsUp := ATicker.Ticker.Bid > FAnchor;

    if ASetAnchor then
      FAnchor := ATicker.Ticker.Bid;
  end;
end;

function TGDAXBobberStrategyImpl.ClosePosition(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LBid, LMin: Extended;
  LOrder: IGDAXOrder;
begin
  try
    Result := False;

    //position size is "actual" inventory, but we need to check if what we
    //have recorded isn't less than what is currently available
    if FPosSize < AInventory then
      FPosSize := AInventory;

    //check to see if our position is less than the minimum size, if so
    //we can skip straight to "out of position"
    if FPosSize < ATicker.Ticker.Product.BaseMinSize then
    begin
      FPosSize := 0;
      FState := bsOutPos;
      Exit(True);
    end;

    //
    LBid := ATicker.Ticker.Bid;

    //since we use market orders, we need to adjust for funds
    if LMin < (ATicker.Ticker.Product.MinMarketFunds / LBid) then
      LMin :=  ATicker.Ticker.Product.MinMarketFunds / LBid;

    //create and initialize an order
    LOrder := TGDAXOrderImpl.Create;
    LOrder.Product := ATicker.Ticker.Product;
    LOrder.OrderType := otMarket; //for now to ensure we get the position always do market
    LOrder.Side := osBuy;
    LOrder.Price := LBid;
    LOrder.Size := Trunc(ASize / LBid / LMin) * LMin; //todo - figure out size based on funds mode

    if LOrder.Size < LMin then
      LOrder.Size := LMin;

    //record the details so we can monitor status
    FOrder := TGDAXOrderDetailsImpl.Create(LOrder);

    //submit the order
    //...

    //if failure we don't update state
    //...

    //success
    //Result := True;
  except on E : Exception do
    Error := 'ClosePosition::' + E.Message;
  end;
end;

function TGDAXBobberStrategyImpl.OpenPosition(const ATicker: ITickerGDAX;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
begin
  try
    Result := False;
    Error := 'OpenPosition::not impl';

  except on E : Exception do
    Error := 'OpenPosition::' + E.Message;
  end;
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

