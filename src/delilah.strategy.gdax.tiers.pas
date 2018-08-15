unit delilah.strategy.gdax.tiers;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.strategy.channels, delilah.types,
  delilah.strategy.gdax;

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
    procedure SetLargePerc(Const AValue: Single);
    procedure SetMarketFee(Const AValue: Single);
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
    function GetChannel: IChannelStrategy;
    function GetLargePerc: Single;
    function GetMarketFee: Single;
    function GetMidPerc: Single;
    function GetOnlyLower: Boolean;
    function GetOnlyProfit: Boolean;
    function GetSmallPerce: Single;
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
  strict protected
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      const AFunds, AInventory, AAAC: Extended; out Error: String): Boolean;override;
  public
    property ChannelStrategy : IChannelStrategy read GetChannel;
    property OnlyProfit : Boolean read GetOnlyProfit write SetOnlyProfit;
    property OnlyLowerAAC : Boolean read GetOnlyLower write SetOnlyLower;
    property UseMarketBuy : Boolean read GetUseMarketBuy write SetUseMarketBuy;
    property UseMarketSell : Boolean read GetUseMarketSell write SetMarketSell;
    property MarketFee : Single read GetMarketFee write SetMarketFee;
    property SmallTierPer : Single read GetSmallPerce write SetSmallPerc;
    property MidTierPerc : Single read GetMidPerc write SetMidPerc;
    property LargeTierPerc : Single read GetLargePerc write SetLargePerc;
    constructor Create; override; overload;
    destructor Destroy; override;
  end;

implementation

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

function TTierStrategyGDAXImpl.GetSmallPerce: Single;
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
begin
  //todo - init the channel with all "channels" that we will be using, as
  //well as wiring up any events to those channels
end;

function TTierStrategyGDAXImpl.DoFeed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LTicker:ITickerGDAX;
begin
  Result:=inherited DoFeed(ATicker, AManager, AFunds, AInventory, AAAC, Error);
  if not Result then
    Exit;
  Result:=False;
  try
    //feed the channel
    if not FChannel.Feed(ATicker,AManager,AFunds,AInventory,AAAC,Error) then
      Exit;

    //cast to gdax ticker
    LTicker:=ATicker as ITickerGDAX;


  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TTierStrategyGDAXImpl.Create;
begin
  inherited Create;
  FChannel:=TChannelImpl.Create;
end;

destructor TTierStrategyGDAXImpl.Destroy;
begin
  FChannel:=nil;
  inherited Destroy;
end;

end.

