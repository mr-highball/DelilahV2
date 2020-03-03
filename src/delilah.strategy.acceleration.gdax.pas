unit delilah.strategy.acceleration.gdax;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  delilah.types,
  delilah.strategy.acceleration;

type

  { TGDAXAccelerationStrategyImpl }
  (*
    gdax acceleration strategy implementation
  *)
  TGDAXAccelerationStrategyImpl = class(TAccelerationStrategyImpl)
  strict private
  strict protected
    function TakePosition(const ATicker: ITicker; const APosition: TAccelPosition;
      const ASize: Extended; out IsPosition: Boolean; out Reason: String): IOrderDetails; override;

    function ClosePosition(const ATicker: ITicker; const ASize: Extended;
      const ADetails: IOrderDetails; out IsClose: Boolean; out Reason: String): IOrderDetails; override;

    function GetMinOrderSize(const ATicker: ITicker): Extended; override;
  public
  end;

implementation
uses
  gdax.api.consts,
  delilah.ticker.gdax,
  delilah.order.gdax,
  gdax.api.types,
  gdax.api.orders;

{ TGDAXAccelerationStrategyImpl }

function TGDAXAccelerationStrategyImpl.TakePosition(const ATicker: ITicker;
  const APosition: TAccelPosition; const ASize: Extended; out
  IsPosition: Boolean; out Reason: String): IOrderDetails;
var
  LOrder : IGDAXOrder;
  LTicker: ITickerGDAX;
  LMin, LBid: Extended;
begin
  Result := nil;
  IsPosition := False;
  LMin := GetMinOrderSize(ATicker);

  //cast the ticker
  LTicker := ATicker as ITickerGDAX;
  LBid := LTicker.Ticker.Bid;

  //create and initialize an order
  LOrder := TGDAXOrderImpl.Create;
  LOrder.Product := LTicker.Ticker.Product;
  LOrder.OrderType := otMarket; //for now to ensure we get the position always do market
  LOrder.Side := osBuy;
  LOrder.Price := LTicker.Ticker.Bid;
  LOrder.Size := Trunc(ASize / LBid / LMin) * LMin;

  if LOrder.Size < LMin then
  begin
    Reason := 'order size is less than minimum';
    Exit;
  end;

  //success
  Result := TGDAXOrderDetailsImpl.Create(LOrder);
  IsPosition := True;
end;

function TGDAXAccelerationStrategyImpl.ClosePosition(const ATicker: ITicker;
  const ASize: Extended; const ADetails: IOrderDetails; out IsClose: Boolean;
  out Reason: String): IOrderDetails;
var
  LOrder : IGDAXOrder;
  LTicker: ITickerGDAX;
  LMin, LAsk: Extended;
begin
  Result := nil;
  IsClose := False;
  LMin := GetMinOrderSize(ATicker);

  //cast the ticker
  LTicker := ATicker as ITickerGDAX;
  LAsk := LTicker.Ticker.Ask;

  //create and initialize an order
  LOrder := TGDAXOrderImpl.Create;
  LOrder.Product := LTicker.Ticker.Product;
  LOrder.OrderType := otMarket; //for now to ensure we get the position always do market
  LOrder.Side := osSell;
  LOrder.Price := LAsk;
  LOrder.Size := Trunc(ASize / LMin) * LMin; //trunc dust

  if LOrder.Size < LMin then
  begin
    Reason := 'order size is less than minimum';
    Exit;
  end;

  //success
  Result := TGDAXOrderDetailsImpl.Create(LOrder);
  IsClose := True;
end;

function TGDAXAccelerationStrategyImpl.GetMinOrderSize(const ATicker: ITicker): Extended;
begin
  if not Assigned(ATicker) then
    Exit(inherited GetMinOrderSize(ATicker));

  Result := (ATicker as ITickerGDAX).Ticker.Product.BaseMinSize;
end;

end.

