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
    function DoGetInvType: TLedgerType; override;
    procedure DoSetPrice(const AValue: Extended); override;
    procedure DoSetSize(const AValue: Extended); override;
    procedure DoSetType(const AValue: TLedgerType); override;
    procedure DoSetInvType(const AValue: TLedgerType); override;
    function LedgerTypeForOrder:TLedgerType;
    function InvLedgerTypeForOrder:TLedgerType;
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
  //price needs to add any fees associated
  Result:=FOrder.Price + Abs(FOrder.FillFees);
end;

function TGDAXOrderDetailsImpl.DoGetSize: Extended;
begin
  if not Assigned(FOrder) then
    Exit(0);
  //when an order is completed we have to differentiate between
  //requested size, and what was actually filled. here we choose
  //to report the filled size in this manner, but may change pending use
  if FOrder.OrderStatus in [stSettled,stDone] then
    Result:=FOrder.FilledSized
  else
    Result:=FOrder.Size;
end;

function TGDAXOrderDetailsImpl.DoGetType: TLedgerType;
begin
  Result:=LedgerTypeForOrder;
end;

function TGDAXOrderDetailsImpl.DoGetInvType: TLedgerType;
begin
  Result:=InvLedgerTypeForOrder;
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

procedure TGDAXOrderDetailsImpl.DoSetInvType(const AValue: TLedgerType);
begin
  raise Exception.Create('setting inventory ledger type not supported in ' + Self.Classname);
end;

function TGDAXOrderDetailsImpl.LedgerTypeForOrder: TLedgerType;
begin
  Result:=ltDebit;
  if not Assigned(FOrder) then
    Exit(ltDebit);
  //on a buy, we debit funds
  if FOrder.Side=osBuy then
    Exit(ltDebit)
  //on a sell, we credit funds
  else if FOrder.Side=osSell then
    Exit(ltCredit)
  else
    Exit(ltDebit);
end;

function TGDAXOrderDetailsImpl.InvLedgerTypeForOrder: TLedgerType;
begin
  Result:=ltDebit;
  if not Assigned(FOrder) then
    Exit(ltDebit);
  //on a buy we credit inventory
  if FOrder.Side=osBuy then
    Exit(ltCredit)
  //on a sell we debit inventory
  else if FOrder.Side=osSell then
    Exit(ltDebit)
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

