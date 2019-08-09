unit delilah.order.gdax;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, delilah.order, gdax.api.types,
  gdax.api.consts, ledger;

type

  { IGDAXOrderDetails }

  IGDAXOrderDetails = interface(IOrderDetails)
    ['{91294778-1291-44C4-87B4-A25779C3CF19}']
    //property methods
    function GetOrder: IGDAXOrder;
    procedure SetOrder(Const AValue: IGDAXOrder);
    //properties
    property Order : IGDAXOrder read GetOrder write SetOrder;
  end;

  { TGDAXOrderDetailsImpl }
  (*
    order details adapter for a gdax order
  *)
  TGDAXOrderDetailsImpl = class(TOrderDetailsImpl,IGDAXOrderDetails)
  strict private
    FOrder: IGDAXOrder;
    function GetOrder: IGDAXOrder;
    procedure SetOrder(Const AValue: IGDAXOrder);
  strict protected
    function DoGetPrice: Extended; override;
    function DoGetSize: Extended; override;
    function DoGetType: TOrderDetailsType; override;
    function DoGetFees: Extended; override;
    procedure DoSetPrice(const AValue: Extended); override;
    procedure DoSetSize(const AValue: Extended); override;
    procedure DoSetType(const AValue: TOrderDetailsType); override;
    procedure DoSetFees(const AValue: Extended); override;
  public
    property Order : IGDAXOrder read GetOrder write SetOrder;
    constructor Create(Const AOrder:IGDAXOrder); overload;
    destructor Destroy; override;
  end;

implementation

{ TGDAXOrderDetailsImpl }

function TGDAXOrderDetailsImpl.GetOrder: IGDAXOrder;
begin
  Result:=FOrder;
end;

procedure TGDAXOrderDetailsImpl.SetOrder(const AValue: IGDAXOrder);
begin
  FOrder:=nil;
  FOrder:=AValue;
end;

function TGDAXOrderDetailsImpl.DoGetPrice: Extended;
begin
  if not Assigned(FOrder) then
    Exit(0);

  (*
    below we are using the ExecutedValue which is only present on orders
    after 2016, but is the cumulative (size * price), so we don't have to make
    a second call to the fills endpoint
  *)
  if (FOrder.OrderStatus = stSettled) or (FOrder.OrderStatus = stCancelled)
  then
  begin
    if FOrder.FilledSized > 0 then
      Result:=(FOrder.ExecutedValue / FOrder.FilledSized)
    else
      Result:=0;
  end
  else
    Result:=FOrder.Price;
end;

function TGDAXOrderDetailsImpl.DoGetSize: Extended;
begin
  if not Assigned(FOrder) then
    Exit(0);

  //if the order is done or cancelled, then report back the filled size
  if FOrder.OrderStatus in [stCancelled,stSettled] then
    Result:=FOrder.FilledSized
  else
    Result:=FOrder.Size;
end;

function TGDAXOrderDetailsImpl.DoGetType: TOrderDetailsType;
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX order not assigned in ' + Self.Classname);
  if FOrder.Side=osBuy then
    Result:=odBuy
  else
    Result:=odSell;
end;

function TGDAXOrderDetailsImpl.DoGetFees: Extended;
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX order not assigned in ' + Self.Classname);

  Result := Abs(FOrder.FillFees);
end;

procedure TGDAXOrderDetailsImpl.DoSetPrice(const AValue: Extended);
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX order not assigned in ' + Self.Classname);
  FOrder.Price:=AValue;
end;

procedure TGDAXOrderDetailsImpl.DoSetSize(const AValue: Extended);
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX order not assigned in ' + Self.Classname);
  FOrder.Size:=AValue;
end;

procedure TGDAXOrderDetailsImpl.DoSetType(const AValue: TOrderDetailsType);
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX order not assigned in ' + Self.Classname);
  if AValue=odBuy then
    FOrder.Side:=osBuy
  else
    FOrder.Side:=osSell;
end;

procedure TGDAXOrderDetailsImpl.DoSetFees(const AValue: Extended);
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX order not assigned in ' + Self.Classname);

  FOrder.FillFees := AValue;
end;


constructor TGDAXOrderDetailsImpl.Create(const AOrder: IGDAXOrder);
begin
  inherited Create;
  FOrder:=AOrder;
end;

destructor TGDAXOrderDetailsImpl.Destroy;
begin
  FOrder:=nil;
  inherited Destroy;
end;

end.

