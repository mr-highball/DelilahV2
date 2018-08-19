unit delilah.strategy.gdax.tiers;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.strategy.channels, delilah.types,
  delilah.strategy.gdax, fgl, delilah.strategy;

type

  (*
    enum showing all available order positions taken at a particular tier
  *)
  TPositionSize = (
    psSmall,
    psMid,
    psLarge,
    psAll
  );

  { ITierStrategyGDAX }
  (*
    strategy which sets various channels above and below each other (tiered)
    in order to open and close different sized positions
  *)
  ITierStrategyGDAX = interface(IStrategyGDAX)
    ['{C9AA33EF-DB73-4790-BD48-5AB5E27A4A81}']
    //property methods
    function GetChannel: IChannelStrategy;
    function GetLargePerc: Single;
    function GetMarketFee: Single;
    function GetMidPerc: Single;
    function GetOnlyLower: Boolean;
    function GetOnlyProfit: Boolean;
    function GetSmallPerc: Single;
    function GetUseMarketBuy: Boolean;
    function GetUseMarketSell: Boolean;
    procedure SetLargePerc(Const AValue: Single);
    procedure SetMarketFee(Const AValue: Single);
    procedure SetMarketSell(Const AValue: Boolean);
    procedure SetMidPerc(Const AValue: Single);
    procedure SetOnlyLower(Const AValue: Boolean);
    procedure SetOnlyProfit(Const AValue: Boolean);
    procedure SetSmallPerc(Const AValue: Single);
    procedure SetUseMarketBuy(Const AValue: Boolean);

    //properties
    (*
      specific details about all channels used can be accessed from this
      property, and changed if required
    *)
    property ChannelStrategy : IChannelStrategy read GetChannel;
    (*
      when true, sell orders can only be placed if it would result in profit
    *)
    property OnlyProfit : Boolean read GetOnlyProfit write SetOnlyProfit;
    (*
      when true, buy orders can only be placed if it would lower AAC
    *)
    property OnlyLowerAAC : Boolean read GetOnlyLower write SetOnlyLower;
    (*
      when true, market orders are made for buys instead of limit orders ensuring
      a quick entry price, but most likely incurring fees
    *)
    property UseMarketBuy : Boolean read GetUseMarketBuy write SetUseMarketBuy;
    (*
      when true, market orders are made for sells instead of limit orders ensuring
      a quick exit price, but most likely incurring fees
    *)
    property UseMarketSell : Boolean read GetUseMarketSell write SetMarketSell;
    (*
      the market fee associated with market orders. this setting is used
      in conjunction with OnlyProfit to determine if a sell can be made
    *)
    property MarketFee : Single read GetMarketFee write SetMarketFee;
    (*
      percentage of funds/inventory to use when a small tier position is detected
      either selling or buying
    *)
    property SmallTierPerc : Single read GetSmallPerc write SetSmallPerc;
    (*
      percentage of funds/inventory to use when a medium tier position is detected
      either selling or buying
    *)
    property MidTierPerc : Single read GetMidPerc write SetMidPerc;
    (*
      percentage of funds/inventory to use when a large tier position is detected
      either selling or buying
    *)
    property LargeTierPerc : Single read GetLargePerc write SetLargePerc;
  end;

  { TTierStrategyGDAXImpl }
  (*
    base implementation of a GDAX tiered stragegy
  *)
  TTierStrategyGDAXImpl = class(TStrategyGDAXImpl,ITierStrategyGDAX)
  strict private
    FChannel: IChannelStrategy;
    FOnlyLower,
    FOnlyProfit,
    FUseMarketBuy,
    FUseMarketSell: Boolean;
    FSmallPerc,
    FMidPerc,
    FMarketFee,
    FLargePerc: Single;
    FDontBuy,
    FSellItAllNow,
    FLargeBuy,
    FSmallBuy,
    FLargeSell,
    FMidSell,
    FSmallSell: Boolean;
    FIDS: TFPGList<String>;
    function GetChannel: IChannelStrategy;
    function GetLargePerc: Single;
    function GetMarketFee: Single;
    function GetMidPerc: Single;
    function GetOnlyLower: Boolean;
    function GetOnlyProfit: Boolean;
    function GetSmallPerc: Single;
    function GetUseMarketBuy: Boolean;
    function GetUseMarketSell: Boolean;
    procedure SetLargePerc(Const AValue: Single);
    procedure SetMarketFee(Const AValue: Single);
    procedure SetMarketSell(Const AValue: Boolean);
    procedure SetMidPerc(Const AValue: Single);
    procedure SetOnlyLower(Const AValue: Boolean);
    procedure SetOnlyProfit(Const AValue: Boolean);
    procedure SetSmallPerc(Const AValue: Single);
    procedure SetUseMarketBuy(Const AValue: Boolean);
    procedure InitChannel;
    procedure GTFOUp(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure GTFOLow(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure LargeBuyUp(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure LargeBuyLow(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure SmallBuyUp(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure SmallBuyLow(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure SmallSellUp(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure SmallSellLow(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure LargeSellUp(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure LargeSellLow(Const ASender:IChannel;Const ADirection:TChannelDirection);
  strict protected
    const
      GTFO = 'gtfo';
      LARGE_BUY = 'large-buy';
      SMALL_BUY = 'small-buy';
      SMALL_SELL = 'small-sell';
      LARGE_SELL = 'large-sell';
  strict protected
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      const AFunds, AInventory, AAAC: Extended; out Error: String): Boolean;override;
    (*
      returns true if we should take a particular position
    *)
    function GetPosition(Out Size:TPositionSize;Out Percentage:Single;Out Sell:Boolean):Boolean;
    (*
      after a call is made to GetPosition, call this method to report that
      we were successful placing to the order manager
    *)
    procedure PositionSuccess;
    (*
      clears managed positions by cancelling those not completed
    *)
    procedure ClearOldPositions(Const AManager:IOrderManager);
  public
    property ChannelStrategy : IChannelStrategy read GetChannel;
    property OnlyProfit : Boolean read GetOnlyProfit write SetOnlyProfit;
    property OnlyLowerAAC : Boolean read GetOnlyLower write SetOnlyLower;
    property UseMarketBuy : Boolean read GetUseMarketBuy write SetUseMarketBuy;
    property UseMarketSell : Boolean read GetUseMarketSell write SetMarketSell;
    property MarketFee : Single read GetMarketFee write SetMarketFee;
    property SmallTierPer : Single read GetSmallPerc write SetSmallPerc;
    property MidTierPerc : Single read GetMidPerc write SetMidPerc;
    property LargeTierPerc : Single read GetLargePerc write SetLargePerc;
    constructor Create(const AOnInfo, AOnError, AOnWarn: TStrategyLogEvent);override;
    destructor Destroy; override;
  end;

implementation
uses
  math,
  gdax.api.consts,
  gdax.api.types,
  gdax.api.orders,
  delilah.ticker.gdax,
  delilah.order.gdax;

{ TTierStrategyGDAXImpl }

function TTierStrategyGDAXImpl.GetChannel: IChannelStrategy;
begin
  Result:=FChannel;
end;

function TTierStrategyGDAXImpl.GetLargePerc: Single;
begin
  Result:=FLargePerc;
end;

function TTierStrategyGDAXImpl.GetMarketFee: Single;
begin
  Result:=FMarketFee;
end;

function TTierStrategyGDAXImpl.GetMidPerc: Single;
begin
  Result:=FMidPerc;
end;

function TTierStrategyGDAXImpl.GetOnlyLower: Boolean;
begin
  Result:=FOnlyLower;
end;

function TTierStrategyGDAXImpl.GetOnlyProfit: Boolean;
begin
  Result:=FOnlyProfit;
end;

function TTierStrategyGDAXImpl.GetSmallPerc: Single;
begin
  Result:=FSmallPerc;
end;

function TTierStrategyGDAXImpl.GetUseMarketBuy: Boolean;
begin
  Result:=FUseMarketBuy;
end;

function TTierStrategyGDAXImpl.GetUseMarketSell: Boolean;
begin
  Result:=FUseMarketSell;
end;

procedure TTierStrategyGDAXImpl.SetLargePerc(const AValue: Single);
begin
  if AValue<0 then
    FLargePerc:=0
  else
    FLargePerc:=AValue;
end;

procedure TTierStrategyGDAXImpl.SetMarketFee(const AValue: Single);
begin
  FMarketFee:=AValue;
end;

procedure TTierStrategyGDAXImpl.SetMarketSell(const AValue: Boolean);
begin
  FUseMarketSell:=AValue;
end;

procedure TTierStrategyGDAXImpl.SetMidPerc(const AValue: Single);
begin
  if AValue<0 then
    FMidPerc:=AValue
  else
    FMidPerc:=AValue;
end;

procedure TTierStrategyGDAXImpl.SetOnlyLower(const AValue: Boolean);
begin
  FOnlyLower:=AValue;
end;

procedure TTierStrategyGDAXImpl.SetOnlyProfit(const AValue: Boolean);
begin
  FOnlyProfit:=AValue;
end;

procedure TTierStrategyGDAXImpl.SetSmallPerc(const AValue: Single);
begin
  if AValue<0 then
    FSmallPerc:=AValue
  else
    FSmallPerc:=AValue;
end;

procedure TTierStrategyGDAXImpl.SetUseMarketBuy(const AValue: Boolean);
begin
  FUseMarketBuy:=AValue;
end;

procedure TTierStrategyGDAXImpl.InitChannel;
var
  LGTFO,
  LLargeBuy,
  LSmallBuy,
  LSmallSell,
  LLargeSell:IChannel;
begin
  //todo - these numbers are "magic" right now, should break them out
  //into a properties maybe

  LGTFO:=FChannel.Add(GTFO,-3.5,-4)[GTFO];
  LGTFO.OnLower:=GTFOLow;
  LGTFO.OnUpper:=GTFOUp;

  LLargeBuy:=FChannel.Add(LARGE_BUY,-2,-3)[LARGE_BUY];
  LLargeBuy.OnLower:=LargeBuyUp;
  LLargeBuy.OnUpper:=LargeBuyLow;

  LSmallBuy:=FChannel.Add(SMALL_BUY,0,-1)[SMALL_BUY];
  LSmallBuy.OnUpper:=SmallBuyUp;
  LSmallBuy.OnLower:=SmallBuyLow;

  LSmallSell:=FChannel.Add(SMALL_SELL,1,0)[SMALL_SELL];
  LSmallSell.OnUpper:=SmallSellUp;
  LSmallSell.OnLower:=SmallSellLow;

  LLargeSell:=FChannel.Add(LARGE_SELL,3,2)[LARGE_SELL];
  LLargeSell.OnUpper:=LargeSellUp;
  LLargeSell.OnLower:=LargeSellLow;
end;

procedure TTierStrategyGDAXImpl.GTFOUp(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('GTFOUp::direction is ' + IntToStr(Ord(ADirection)));
  //entering or exiting this channel from the upper bounds puts us in a
  //"watch" mode for GTFO by toggling the "dont buy" flag
  case ADirection of
   cdEnter: FDontBuy:=True;
   cdExit: FDontBuy:=False;
 end;
end;

procedure TTierStrategyGDAXImpl.GTFOLow(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('GTFOLow::direction is ' + IntToStr(Ord(ADirection)));
  //we need to gtfo...
  case ADirection of
    cdExit: FSellItAllNow:=True;
    cdEnter: FSellItAllNow:=False;
  end;
end;

procedure TTierStrategyGDAXImpl.LargeBuyUp(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('LargeBuyUp::direction is ' + IntToStr(Ord(ADirection)));
  //if we break the upper bounds, we're no longer requesting for a large
  //buy in
  case ADirection of
    cdExit: FLargeBuy:=False;
  end;
end;

procedure TTierStrategyGDAXImpl.LargeBuyLow(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('LargeBuyLow::direction is ' + IntToStr(Ord(ADirection)));
  //entering from the lower bound of this channel means it's a good oppurtunity
  //to buy in, while exiting, means hold off
  case ADirection of
    cdEnter: FLargeBuy:=True;
    cdExit: FLargeBuy:=False;
  end;
end;

procedure TTierStrategyGDAXImpl.SmallBuyUp(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('SmallBuyUp::direction is ' + IntToStr(Ord(ADirection)));
  //if we break the upper bounds, we're no longer requesting for a small
  //buy in
  case ADirection of
    cdExit: FSmallBuy:=False;
  end;
end;

procedure TTierStrategyGDAXImpl.SmallBuyLow(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('SmallBuyLow::direction is ' + IntToStr(Ord(ADirection)));
  //entering from the lower bound of this channel means it's a good oppurtunity
  //to buy in, while exiting, means hold off
  case ADirection of
    cdEnter: FSmallBuy:=True;
    cdExit: FSmallBuy:=False;
  end;
end;

procedure TTierStrategyGDAXImpl.SmallSellUp(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('SmallSellUp::direction is ' + IntToStr(Ord(ADirection)));
  //exiting the upper bounds means we no longer should sell small
  case ADirection of
    cdExit: FSmallSell:=False;
  end;
end;

procedure TTierStrategyGDAXImpl.SmallSellLow(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('SmallSellLow::direction is ' + IntToStr(Ord(ADirection)));
  //exiting the lower, means we no longer should sell, but entering
  //puts us in the sell mode
  case ADirection of
    cdEnter: FSmallSell:=True;
    cdExit: FSmallSell:=False;
  end;
end;

procedure TTierStrategyGDAXImpl.LargeSellUp(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('LargeSellUp::direction is ' + IntToStr(Ord(ADirection)));
  //large sell exiting puts us into to large sell mode, while
  //entering puts us back to mid sell
  case ADirection of
    cdExit: FLargeSell:=True;
    cdEnter: FMidSell:=True;
  end;
end;

procedure TTierStrategyGDAXImpl.LargeSellLow(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  LogInfo('LargeSellLow::direction is ' + IntToStr(Ord(ADirection)));
  //the lower channel controls mid-range sell oppurtunities
  case ADirection of
    cdEnter: FMidSell:=True;
    cdExit: FMidSell:=False;
  end;
end;

function TTierStrategyGDAXImpl.DoFeed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  I:Integer;
  LID:String;
  LTicker:ITickerGDAX;
  LSize:TPositionSize;
  LOrderSize,
  LMin:Extended;
  LPerc,
  LOrderSellTot,
  LOrderBuyTot:Single;
  LSell:Boolean;
  LGDAXOrder:IGDAXOrder;
  LDetails:IOrderDetails;
  LChannel:IChannel;
begin
  Result:=inherited DoFeed(ATicker, AManager, AFunds, AInventory, AAAC, Error);
  if not Result then
    Exit;
  Result:=False;
  try
    //feed the channel
    if not FChannel.Feed(ATicker,AManager,AFunds,AInventory,AAAC,Error) then
      Exit;

    if not FChannel.IsReady then
    begin
      LogInfo(
        Format(
          'window is not ready [size]:%d [collected]:%d',
          [
            FChannel.WindowSizeInMilli,
            FChannel.CollectedSizeInMilli
          ]
        )
      );
      Exit(True);
    end;

    //log the channels' state
    LogInfo('DoFeed::[StdDev]:' + FloatToStr(FChannel.StdDev));
    for I:=0 to Pred(FChannel.Count) do
    begin
      LChannel:=FChannel.ByIndex[I];
      LogInfo('DoFeed::channel ' + LChannel.Name + ' ' +
        '[anchor]-' + FloatToStr(LChannel.AnchorPrice) + ' ' +
        '[lower]-' + FloatToStr(LChannel.Lower) + ' ' +
        '[upper]-' + FloatToStr(LChannel.Upper)
      );
      LChannel:=nil;
    end;

    //cast to gdax ticker
    LTicker:=ATicker as ITickerGDAX;
    LMin:=LTicker.Ticker.Product.BaseMinSize;

    //initialize the order
    LGDAXOrder:=TGDAXOrderImpl.Create;
    LGDAXOrder.Product:=LTicker.Ticker.Product;

    //get whether or not we should make a position
    if GetPosition(LSize,LPerc,LSell) then
    begin
      //this will remove any old limit orders outstanding
      ClearOldPositions(AManager);

      //see if we need to place a sell
      if LSell then
      begin
        LogInfo('DoFeed::sell logic reached');
        //set the order size based off the percentage returned to us
        LOrderSize:=Abs(RoundTo(AInventory * LPerc,-8));

        if (LOrderSize < LMin) and (RoundTo(AInventory,-8) >= LMin) then
        begin
          LogInfo('DoFeed::SellMode::order too small, but we have at least min setting size of order to min');
          LOrderSize:=LMin;
        end;

        //check to see if we have enough inventory to perform a sell
        if (LOrderSize < LMin) or (LOrderSize > AInventory) then
        begin
          LogInfo(Format('DoFeed::SellMode::%s is lower than min size or no inventory',[FloatToStr(LOrderSize)]));
          LogInfo('DoFeed::SellMode::clearing signals');

          //although this is not a "success" we have no inventory, and should still should clear the signals
          //to allow for buys to be acted on since sells are prioritized
          PositionSuccess;
          Exit(True);
        end;

        //if we are in only profit mode, we need to some validation before
        //placing an order
        if FOnlyProfit then
        begin
          //figure out the total amount depending on limit/market
          if FUseMarketSell then
          begin
            //figure the amount of currency we would receive minus the fees
            LOrderSellTot:=(LOrderSize * LTicker.Ticker.Ask) - (FMarketFee * LOrderSize);
            LGDAXOrder.OrderType:=otMarket;
            LogInfo('DoFeed::SellMode::using market sell, with OnlyProfit, total sell amount would be ' + FloatToStr(LOrderSellTot));
          end
          else
          begin
            LOrderSellTot:=LOrderSize * LTicker.Ticker.Ask;
            LGDAXOrder.OrderType:=otLimit;
            LGDAXOrder.Price:=LTicker.Ticker.Ask;
            LogInfo('DoFeed::SellMode::using limit sell, with OnlyProfit, total sell amount would be ' + FloatToStr(LOrderSellTot));
          end;

          //if the cost to sell is less-than aquisition of size, we would be
          //losing money, and that is not allowed in this case
          if LOrderSellTot < (AAAC * LOrderSize) then
          begin
            LogInfo('DoFeed::SellMode::sell would result in loss, and OnlyProfit is on, exiting');
            Exit(True)
          end;
        end
        else
        begin
          if FUseMarketSell then
            LGDAXOrder.OrderType:=otMarket
          else
          begin
            LGDAXOrder.OrderType:=otLimit;
            LGDAXOrder.Price:=LTicker.Ticker.Ask;
          end;
        end;

        //set the order side to sell
        LGDAXOrder.Side:=osSell;
        LGDAXOrder.Size:=LOrderSize;
      end
      //otherwise we are seeing if we can open a buy position
      else
      begin
        LogInfo('DoFeed::buy logic');
        //see if we have enough funds to cover either a limit or market
        LOrderBuyTot:=Abs(RoundTo(AFunds * LPerc,-8));

        if LOrderBuyTot > AFunds then
        begin
          LogInfo(Format('DoFeed::BuyMode::would need %f funds, but only %f',[LOrderBuyTot,AFunds]));
          Exit(True);
        end;

        //set the order size based on the amount of funds we have
        //and how many units of min this will purchase
        LOrderSize:=Trunc(LOrderBuyTot / LTicker.Ticker.Bid / LMin) * LMin;

        //check to see the order size isn't too small
        if LOrderSize < LMin then
        begin
          LogInfo(Format('DoFeed::BuyMode::%s is lower than min size',[FloatToStr(LOrderSize)]));
          Exit(True);
        end;

        //simple check to see if bid is lower than aac when requested
        if FOnlyLower and (RoundTo(AInventory,-8) >= LMin) then
          if not (LTicker.Ticker.Bid < AAAC) then
          begin
            LogInfo(Format('DoFeed::BuyMode::ticker [bid]:%f is not lower than [aac]:%f',[LTicker.Ticker.Bid,AAAC]));
            Exit(True);
          end;

        //update order depending on type
        if FUseMarketBuy then
          LGDAXOrder.OrderType:=otMarket
        else
        begin
          LGDAXOrder.OrderType:=otLimit;
          LGDAXOrder.Price:=LTicker.Ticker.Bid;
        end;

        //set the order up for buying
        LGDAXOrder.Side:=osBuy;
        LGDAXOrder.Size:=LOrderSize;
      end;

      LogInfo(Format('DoFeed::[buyorder]:%s [limit]:%s [price]:%f [size]:%f',[BoolToStr(LGDAXOrder.Side=osBuy,True),BoolToStr(LGDAXOrder.OrderType=otLimit,True),LGDAXOrder.Price,LGDAXOrder.Size]));

      //call the manager to place the order
      LDetails:=TGDAXOrderDetailsImpl.Create(LGDAXOrder);
      if not AManager.Place(
        LDetails,
        LID,
        Error
      ) then
        Exit;

      //add limit order id's to a list so we can periodically check them
      //if we are switching positions
      if LGDAXOrder.OrderType=otLimit then
        FIDS.Add(LID);

      //if we were successful calling the order manager, report success
      PositionSuccess;
      Result:=True;
    end
    else
      Exit(True);
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TTierStrategyGDAXImpl.GetPosition(out Size: TPositionSize; out
  Percentage: Single; out Sell: Boolean): Boolean;
begin
  Result:=False;
  //prioritize an all sell above everything
  if FSellItAllNow then
  begin
    Sell:=True;
    Percentage:=1;
    Size:=psAll;
    Exit(True);
  end
  else
  begin
    //check for any buys weighted highest to lowest in priority
    if not FDontBuy then
    begin
      if FLargeBuy then
      begin
        Sell:=False;
        Percentage:=FLargePerc;
        Size:=psLarge;
        Exit(True);
      end
      else if FSmallBuy then
      begin
        Sell:=False;
        Percentage:=FSmallPerc;
        Size:=psSmall;
        Exit(True);
      end;
    end;

    //check for any sells weighted highest to lowest in priority
    if FLargeSell then
    begin
      Sell:=True;
      Percentage:=FLargePerc;
      Size:=psLarge;
      Exit(True);
    end
    else if FMidSell then
    begin
      Sell:=True;
      Percentage:=FMidPerc;
      Size:=psMid;
      Exit(True);
    end
    else if FSmallSell then
    begin
      Sell:=True;
      Percentage:=FSmallPerc;
      Size:=psSmall;
      Exit(True);
    end;
  end;
end;

procedure TTierStrategyGDAXImpl.PositionSuccess;
begin
  //init all signal flags to false
  FDontBuy:=False;
  FLargeBuy:=False;
  FSmallBuy:=False;
  FLargeSell:=False;
  FMidSell:=False;
  FSmallSell:=False;
end;

procedure TTierStrategyGDAXImpl.ClearOldPositions(Const AManager:IOrderManager);
var
  I:Integer;
  LDetails:IOrderDetails;
  LError:String;
begin
  //will iterate all managed positions and cancel those not completed
  for I:=0 to Pred(FIDS.Count) do
  begin
    if AManager.Exists[FIDS[I]] then
    begin
      if AManager.Status[FIDS[I]]<>omCompleted then
        if not AManager.Cancel(FIDS[I],LDetails,LError) then
        begin
          LogError('ClearOldPositions::' + LError);
          Exit;
        end;
    end;
  end;

  //if everything went well, clear the positions
  FIDS.Clear;
end;

constructor TTierStrategyGDAXImpl.Create(const AOnInfo, AOnError,
  AOnWarn: TStrategyLogEvent);
begin
  inherited Create(AOnInfo,AOnError,AOnWarn);
  FChannel:=TChannelStrategyImpl.Create(AOnInfo,AOnError,AOnWarn);
  FIDS:=TFPGList<String>.Create;
  FSmallPerc:=0.02;
  FMidPerc:=0.03;
  FLargePerc:=0.05;
  FMarketFee:=0.003;
  FUseMarketBuy:=False;
  FUseMarketSell:=False;
  InitChannel;
end;

destructor TTierStrategyGDAXImpl.Destroy;
begin
  FChannel:=nil;
  FIDS.Free;
  inherited Destroy;
end;

end.

