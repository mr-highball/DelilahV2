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
    psMed,
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
  end;
implementation

end.

