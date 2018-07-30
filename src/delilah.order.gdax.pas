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
    function DoGetType: TLedgerType; override;
    procedure DoSetPrice(const AValue: Extended); override;
    procedure DoSetSize(const AValue: Extended); override;
    procedure DoSetType(const AValue: TLedgerType); override;
    function LedgerTypeForOrder:TLedgerType;
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
  Result:=FOrder.Price;
end;

function TGDAXOrderDetailsImpl.DoGetSize: Extended;
begin
  if not Assigned(FOrder) then
    Exit(0);
  Result:=FOrder.Size;
end;

function TGDAXOrderDetailsImpl.DoGetType: TLedgerType;
begin
  Result:=LedgerTypeForOrder;
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

procedure TGDAXOrderDetailsImpl.DoSetType(const AValue: TLedgerType);
begin
  raise Exception.Create('setting ledger type not supported in ' + Self.Classname);
end;

function TGDAXOrderDetailsImpl.LedgerTypeForOrder: TLedgerType;
begin
  Result:=ltDebit;
  if not Assigned(FOrder) then
    Exit(ltDebit);
  if FOrder.Side=osBuy then
    Exit(ltDebit)
  else if FOrder.Side=osSell then
    Exit(ltCredit)
  else
    Exit(ltDebit);
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

