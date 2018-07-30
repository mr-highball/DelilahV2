unit delilah.order.gdax;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, delilah.order, gdax.api.types, ledger;

type

  { TGDAXOrderDetails }
  (*
    order details adapter for a gdax order
  *)
  TGDAXOrderDetails = class(TOrderDetailsImpl)
  strict private
    FOrder: IGDAXOrder;
  strict protected
    function DoGetPrice: Extended; override;
    function DoGetSize: Extended; override;
    function DoGetType: TLedgerType; override;
    procedure DoSetPrice(const AValue: Extended); override;
    procedure DoSetSize(const AValue: Extended); override;
    procedure DoSetType(const AValue: TLedgerType); override;
    function LedgerTypeForOrder:TLedgerType;
  public
    constructor Create(Const AOrder:IGDAXOrder); override; overload;
    destructor Destroy; override;
  end;

implementation
uses
  gdax.api.consts;
{ TGDAXOrderDetails }

function TGDAXOrderDetails.DoGetPrice: Extended;
begin
  if not Assigned(FOrder) then
    Exit(0);
  Result:=FOrder.Price;
end;

function TGDAXOrderDetails.DoGetSize: Extended;
begin
  if not Assigned(FOrder) then
    Exit(0);
  Result:=FOrder.Size;
end;

function TGDAXOrderDetails.DoGetType: TLedgerType;
begin
  Result:=LedgerTypeForOrder;
end;

procedure TGDAXOrderDetails.DoSetPrice(const AValue: Extended);
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX order not assigned in ' + Self.Classname);
  FOrder.Price:=AValue;
end;

procedure TGDAXOrderDetails.DoSetSize(const AValue: Extended);
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX order not assigned in ' + Self.Classname);
  FOrder.Size:=AValue;
end;

procedure TGDAXOrderDetails.DoSetType(const AValue: TLedgerType);
begin
  raise Exception.Create('setting ledger type not supported in ' + Self.Classname);
end;

function TGDAXOrderDetails.LedgerTypeForOrder: TLedgerType;
begin
  if not Assigned(FOrder) then
    Exit(ltDebit);
  if FOrder.Side=osBuy then
    Exit(ltDebit)
  else if FOrder.Side=osSell then
    Exit(ltCredit)
  else
    Exit(ltDebit);
end;

constructor TGDAXOrderDetails.Create(const AOrder: IGDAXOrder);
begin
  inherited Create;
  FOrder:=AOrder;
end;

destructor TGDAXOrderDetails.Destroy;
begin
  FOrder:=nil;
  inherited Destroy;
end;

end.

