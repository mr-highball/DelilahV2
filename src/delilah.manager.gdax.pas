unit delilah.manager.gdax;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, delilah.manager, gdax.api.types,
  gdax.api.consts;

type

  { IGDAXOrderManager }
  (*
    order manager for the GDAX crypto currency exchange
  *)
  IGDAXOrderManager = interface(IOrderManager)
    ['{0F58D00E-7D26-447B-B246-C70AE651670C}']
    //property methods
    function GetAuth: IGDAXAuthenticator;
    procedure SetAuth(Const AValue: IGDAXAuthenticator);
    //properties
    property Authenticator : IGDAXAuthenticator read GetAuth write SetAuth;
  end;

  { TGDAXOrderManagerImpl }
  (*
    base implementation of a gdax order manager
  *)
  TGDAXOrderManagerImpl = class(TOrderManagerImpl,IGDAXOrderManager)
  strict private
    FAuth: IGDAXAuthenticator;
    function GetAuth: IGDAXAuthenticator;
    procedure SetAuth(Const AValue: IGDAXAuthenticator);
  strict protected
    function GDAXDetailsValid(Const ADetails:IOrderDetails;Out Error:String):Boolean;
    function GDAXStatusToEngineStatus(Const AStatus:TOrderStatus):TOrderManagerStatus;
    function DoPlace(const ADetails: IOrderDetails;
      out Error: String): Boolean; override;
    function DoCancel(const ADetails: IOrderDetails;
      out Error: String): Boolean; override;
    function DoGetStatus(const ADetails: IOrderDetails): TOrderManagerStatus;override;
  public
    property Authenticator : IGDAXAuthenticator read GetAuth write SetAuth;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  gdax.api.authenticator, delilah.order.gdax;

{ TGDAXOrderManagerImpl }

function TGDAXOrderManagerImpl.GetAuth: IGDAXAuthenticator;
begin
  Result:=FAuth;
end;

procedure TGDAXOrderManagerImpl.SetAuth(const AValue: IGDAXAuthenticator);
begin
  FAuth:=nil
  FAuth:=AValue;
end;

function TGDAXOrderManagerImpl.GDAXDetailsValid(const ADetails: IOrderDetails;
  out Error: String): Boolean;
var
  LDetails:IGDAXOrderDetails;
begin
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
end;

function TGDAXOrderManagerImpl.GDAXStatusToEngineStatus(
  const AStatus: TOrderStatus): TOrderManagerStatus;
begin
  Result:=omCanceled;
  //map the gdax status as best we can to the engine statuses
  case AStatus of
    stActive,stPending: Result:=omActive;
    stCancelled,stRejected,stUnknown: Result:=omCanceled;
    stSettled,stDone: Result:=omCompleted;
  end;
end;

function TGDAXOrderManagerImpl.DoPlace(const ADetails: IOrderDetails;
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
    //attempt to post the order assuming strategy has filled it out correctly
    if LDetails.Order.Post(LContent,Error) then
      Exit;
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TGDAXOrderManagerImpl.DoCancel(const ADetails: IOrderDetails;
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
    //attempt to post the order assuming strategy has filled it out correctly
    if LDetails.Order.Delete(LContent,Error) then
      Exit;
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TGDAXOrderManagerImpl.DoGetStatus(const ADetails: IOrderDetails): TOrderManagerStatus;
var
  LDetails:IGDAXOrderDetails;
  LOldStatus,
  LNewStatus:TOrderManagerStatus;
  LID:String;
begin
  Result:=False;
  try
    //will check everything needed for continuing
    if not GDAXDetailsValid(ADetails,Error) then
      Exit;
    LDetails:=ADetails as IGDAXOrderDetails;
    LOldStatus:=GDAXStatusToEngineStatus(LDetails.Order.OrderStatus);
    //attempt to post the order assuming strategy has filled it out correctly
    if LDetails.Order.Get(LContent,Error) then
      Exit;
    if not ID(ADetails,LID) then
    begin
      Error:='unable to fetch id for order details in ' + Self.Classname;
      Exit;
    end;
    LNewStatus:=GDAXStatusToEngineStatus(LDetails.Order.OrderStatus);
    //notify listeners since base class does not handle status changes when
    //fetching statuses (handles cancel, remove, place automatically)
    if LOldStatus<>LNewStatus then
      DoOnStatus(LDetails,LID,LOldStatus,LNewStatus);
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TGDAXOrderManagerImpl.Create;
begin
  inherited Create;
  FAuth:=TGDAXAuthenticatorImpl.Create;
end;

destructor TGDAXOrderManagerImpl.Destroy;
begin
  FAuth:=nil;
  inherited Destroy;
end;

end.

