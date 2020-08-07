unit delilah.strategy.bobber;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  delilah.types,
  delilah.strategy;

type

  (*
    the funds mode controls how a IBobberStrategy utilizes the funds property
    to open/close positions
  *)
  TBobberFundsMode = (
    bmPercentTotal, //percent of total funds
    bmPercentAvailable, //percent of just what is available (inventory or funds)
    bmFixedBase, //fixed amount of funds in the base currency (ie. usd, btc, etc...)
    bmFixedCoin //fixed amount in the traded coin (opposite of bfFixedBase)
  );

  TBobberState = (
    bsOutPos, //out of position, and not trying to enter one
    bsInPos, //in position and not trying to exit one
    bsEntering, //out of position and trying to get into one
    bsExiting //in position and trying to exit
  );

  { IBobberStrategy }
  (*
    bobber strategy maintains a "floating" amount of funds that are affected
    by price movements

    general workings should go like...
    You start at a price point and specify the threshold (ie. 0.5%).
    Bobber has a current price point. When the price drops below threshold,
    if in position a sell will occur. Also, the price point is now moved down.
    The move will occur regardless if in position. If the price goes above the
    threshold and not in position, a buy will occur and the price is moved up.
    The price point can continue to drop or rise and a position (open/close)
    only occurs once.
  *)
  IBobberStrategy = interface(IStrategy)
    ['{79B585B8-046F-4756-AD78-B5A586526A39}']

    //property methods
    function GetThresh: Extended;
    function GetAnchThresh: Extended;
    function GetPositionSize: Extended;
    function GetState: TBobberState;
    procedure SetAnchor(const AValue: Extended);
    procedure SetAnchThresh(const AValue: Extended);
    procedure SetFunds(const AValue: Extended);
    procedure SetMode(const AValue: TBobberFundsMode);
    function GetMode: TBobberFundsMode;
    procedure SetPositionSize(const AValue: Extended);
    procedure SetThresh(const AValue: Extended);
    function GetAnchor: Extended;
    function GetFunds: Extended;

    //properties

    (*
      threshold is a percentage used to determine if a position
      should be made. when the price of a coin raises or lowers below
      this threshold (uses anchor price to determine this) these two events
      can occur
        1.) position made (open/close respective to higher/lower)
        2.) anchor price adjusted to the new price
    *)
    property Threshold : Extended read GetThresh write SetThresh;

    (*
      the adjustment threshold is used to move the anchor price in a
      conditional fashion. when out of position, the anchor is moved
      when the price lowers by the threshold (and continues to do so)
      but when in position, the anchor is moved on the price increasing
      above this threshold.
    *)
    property AdjustAnchorThreshold : Extended read GetAnchThresh write SetAnchThresh;

    (*
      current anchor price
    *)
    property Anchor : Extended read GetAnchor write SetAnchor;

    (*
      amount of funds to use for open/close positions
    *)
    property Funds : Extended read GetFunds write SetFunds;

    (*
      funds mode used to change behavior of open/close positions
    *)
    property FundsMode : TBobberFundsMode read GetMode write SetMode;

    (*
      the state of the strategy
    *)
    property State : TBobberState read GetState;

    (*
      actual size of inventory this strategy is holding
    *)
    property PositionSize : Extended read GetPositionSize write SetPositionSize;

    //methods

    (*
      when called will attempt to exit a position each time a ticker is fed
      until successful. once exiting, the anchor will be adjusted to the exit
      price, and the state will return to bsOutPos

      note:
        generally should not be called in favor of letting tickers fed to
        trigger positions, but may be useful when combining stragies
    *)
    procedure ExitPosition;

    (*
      when called will attempt to enter a position each time a ticker is fed
      until successful. once entering, the anchor will be adjusted to the enter
      price, and the state will return to bsInPos

      note:
        generally should not be called in favor of letting tickers fed to
        trigger positions, but may be useful when combining stragies
    *)
    procedure EnterPosition;
  end;

implementation

end.

