unit delilah.strategy.gdax.sample.extended;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, delilah.strategy.gdax.sample;

type

  { ISampleGDAXExt }
  (*
    demonstrating how to extend implemented strategies to further code reuse
  *)
  ISampleGDAXExt = interface(ISampleGDAX)
    ['{0BC943DB-FA2F-4C42-BF55-83C22C82123D}']
    //here we would add any extended properties from the sample strategy
  end;

  { TSampleGDAXExtImpl }

  TSampleGDAXExtImpl = class(TSampleGDAXImpl,ISampleGDAXExt)
  strict private
    //helper method to calculate an average of the window
    function GetAverage(Const ATickerPrice:Extended):Extended;
  strict protected
    (*
      below we need to override the virtual methods to alter the functionality
      of our parent sample strategy
    *)
    function DoAllowBuy(const AFunds, AInventory, AAC, ATickerPrice: Extended;Out Reason:String): Boolean; override;
    function DoAllowSell(const AFunds, AInventory, AAC, ATickerPrice: Extended;Out Reason:String): Boolean; override;
  public
  end;

implementation

{ TSampleGDAXExtImpl }

function TSampleGDAXExtImpl.GetAverage(const ATickerPrice: Extended): Extended;
begin
  Result:=(ATickerPrice + Window.HighestPrice + Window.LowestPrice) / 3;
end;

function TSampleGDAXExtImpl.DoAllowBuy(const AFunds, AInventory, AAC,
  ATickerPrice: Extended;Out Reason:String): Boolean;
var
  LAvg:Extended;
  LReason:String;
begin
  Result:=inherited DoAllowBuy(AFunds, AInventory, AAC, ATickerPrice, LReason);
  if not Result then
    Exit;
  Result:=False;

  //simple check to make sure there is somewhat of a "spread" between
  //buys, by looking at the percentage difference from AAC and ticker.
  //in this case, we are hard coding .5% but this could be a property
  if AAC > 0 then
    if not (Abs(1 - (ATickerPrice / AAC)) > 0.005) then
    begin
      Reason:=' condition not met [Abs(1 - (ATickerPrice / AAC)) > 0.005]';
      Exit;
    end;

  //this is a method utilizing the parent's window to only buy below
  //the current average
  LAvg:=GetAverage(ATickerPrice);
  LogInfo(Format('DoAllowSell [average]:%f',[LAvg]));
  if not (ATickerPrice < LAvg) then
  begin
    Reason:='ticker price is not less than average';
    Exit;
  end;

  Result:=True;
end;

function TSampleGDAXExtImpl.DoAllowSell(const AFunds, AInventory, AAC,
  ATickerPrice: Extended;Out Reason:String): Boolean;
var
  LAvg:Extended;
  LReason:String;
begin
  Result:=inherited DoAllowSell(AFunds, AInventory, AAC, ATickerPrice, LReason);
  if not Result then
    Exit;
  Result:=False;

  //this is a method utilizing the parent's window to only sell above
  //the current average
  LAvg:=GetAverage(ATickerPrice);
  LogInfo(Format('DoAllowSell [average]:%f',[LAvg]));
  if ATickerPrice > LAvg then
  begin
    Reason:='ticker price is not greater than average';
    Result:=True;
  end;
end;

end.

