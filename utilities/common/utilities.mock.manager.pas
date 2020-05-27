unit utilities.mock.manager;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  delilah.types,
  gdax.api.consts,
  delilah.manager,
  delilah.manager.gdax;

type

  { IMockOrderManager }
  (*
    order manager for the GDAX crypto currency exchange
  *)
  IMockOrderManager = interface(IOrderManager)
    ['{0F58D00E-7D26-447B-B246-C70AE651670C}']
    //property methods
    function GetFee: Single;
    function GetSellPrice: Single;
    procedure SetBuyPrice(const AValue: Single);
    procedure SetFee(const AValue: Single);
    procedure SetSellPrice(const AValue: Single);
    function GetBuyPrice: Single;

    //properties
    property FeePercentage : Single read GetFee write SetFee;
    property BuyPrice : Single read GetBuyPrice write SetBuyPrice;
    property SellPrice : Single read GetSellPrice write SetSellPrice;
  end;

  { TMockOrderManagerImpl }
  (*
    base implementation of a mock order manager
  *)
  TMockOrderManagerImpl = class(TOrderManagerImpl, IMockOrderManager)
  strict private
    FBuyPrice: Single;
    FFee : Single;
    FSellPrice: Single;
    function GetBuyPrice: Single;
    function GetSellPrice: Single;
    procedure SetBuyPrice(const AValue: Single);
    procedure SetSellPrice(const AValue: Single);
  strict protected
    function GDAXDetailsValid(Const ADetails:IOrderDetails;Out Error:String):Boolean;
    function GDAXStatusToEngineStatus(Const AStatus:TOrderStatus):TOrderManagerStatus;

    function DoPlace(const ADetails: IOrderDetails;
      out Error: String): Boolean; override;
    function DoCancel(const ADetails: IOrderDetails;
      out Error: String): Boolean; override;
    function DoGetStatus(const ADetails: IOrderDetails): TOrderManagerStatus;override;
    function DoRefresh(out Error: String): Boolean; override;
  protected
    function GetFee: Single;
    procedure SetFee(const AValue: Single);
  public
    property FeePercentage : Single read GetFee write SetFee;
    property BuyPrice : Single read GetBuyPrice write SetBuyPrice;
    property SellPrice : Single read GetSellPrice write SetSellPrice;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  delilah.order.gdax,
  math,
  dateutils;

{ TMockOrderManagerImpl }

function TMockOrderManagerImpl.GetBuyPrice: Single;
begin
  Result := FBuyPrice;
end;

function TMockOrderManagerImpl.GetSellPrice: Single;
begin
  Result := FSellPrice;
end;

procedure TMockOrderManagerImpl.SetBuyPrice(const AValue: Single);
begin
  FBuyPrice := AValue;
end;

procedure TMockOrderManagerImpl.SetSellPrice(const AValue: Single);
begin
  FSellPrice := AValue;
end;

function TMockOrderManagerImpl.GDAXDetailsValid(const ADetails: IOrderDetails;
  out Error: String): Boolean;
var
  LDetails:IGDAXOrderDetails;
begin
  Result:=False;
  if not Assigned(ADetails) then
  begin
    Error:='order details nil or invalid';
    Exit;
  end;
  if not (ADetails is IGDAXOrderDetails) then
  begin
    Error:='order details is not IGDAXOrderDetails';
    Exit;
  end;
  LDetails:=ADetails as IGDAXOrderDetails;
  if not Assigned(LDetails.Order) then
  begin
    Error:='gdax not assigned in order details';
    Exit;
  end;
  Result:=True;
end;

function TMockOrderManagerImpl.GDAXStatusToEngineStatus(
  const AStatus: TOrderStatus): TOrderManagerStatus;
begin
  Result:=omCanceled;
  //map the gdax status as best we can to the engine statuses
  case AStatus of
    stActive,stPending,stOpen,stDone: Result:=omActive;
    stCancelled,stRejected,stUnknown: Result:=omCanceled;
    stSettled: Result:=omCompleted;
  end;
end;

function TMockOrderManagerImpl.DoPlace(const ADetails: IOrderDetails;
  out Error: String): Boolean;
var
  LDetails:IGDAXOrderDetails;
begin
  Result:=False;
  try
    //will check everything needed for continuing
    if not GDAXDetailsValid(ADetails,Error) then
      Exit;

    LDetails:=ADetails as IGDAXOrderDetails;

    //set the details to appear completely filled. we could do more complicated
    //things such as checking for limit orders, waiting until price hits, etc...
    //but this is the easiest thing to do in order to get a ballpark estimate
    LDetails.Order.FilledSized := LDetails.Order.Size;
    LDetails.Order.OrderStatus := stSettled;
    LDetails.Order.Settled := True;

    if ADetails.OrderType = odBuy then
    begin
      LDetails.Order.Price := FBuyPrice;
      LDetails.Order.ExecutedValue := FBuyPrice * LDetails.Order.FilledSized;
      LDetails.Order.FillFees := FFee * (LDetails.Order.FilledSized * FBuyPrice);
    end
    else
    begin
      LDetails.Order.Price := FSellPrice;
      LDetails.Order.ExecutedValue := FSellPrice * LDetails.Order.FilledSized;
      LDetails.Order.FillFees := FFee * (LDetails.Order.FilledSized * FSellPrice);
    end;

    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TMockOrderManagerImpl.DoCancel(const ADetails: IOrderDetails;
  out Error: String): Boolean;
begin
  //in this mock manager, all orders will fill immediately
  Result := False;
  Error := 'mock manager fills immediately';
end;

function TMockOrderManagerImpl.DoGetStatus(const ADetails: IOrderDetails): TOrderManagerStatus;
var
  LID: String;
begin
  Result := GDAXStatusToEngineStatus((ADetails as IGDAXOrderDetails).Order.OrderStatus);

  if Result = omCompleted then
  begin
    ID(ADetails, LID);
    DoOnStatus(ADetails, LID, omActive, omCompleted);
  end;
end;

function TMockOrderManagerImpl.DoRefresh(out Error: String): Boolean;
begin
  Result := True;
end;

function TMockOrderManagerImpl.GetFee: Single;
begin
  Result := FFee;
end;

procedure TMockOrderManagerImpl.SetFee(const AValue: Single);
begin
  FFee := AValue;
end;

constructor TMockOrderManagerImpl.Create;
begin
  inherited Create;
  FFee := 0;
end;

destructor TMockOrderManagerImpl.Destroy;
begin

  inherited Destroy;
end;

end.

