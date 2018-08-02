unit delilah;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, ledger, ledger.standard;

type

  { TDelilahImpl }
  (*
    base implementation for IDelilah
  *)
  TDelilahImpl = class(TInterfacedObject,IDelilah)
  strict private
    FFunds: Extended;
    FCompound: Boolean;
    FFundsLedger: IExtendedLedger;
    FHoldsLedger: IExtendedLedger;
    FInvLedger: IExtendedLedger;
    FOnPlace: TOrderPlaceEvent;
    FOnRemove: TOrderRemoveEvent;
    FOnStatus: TOrderStatusEvent;
    FOrderManager: IOrderManager;
    FState: TEngineState;
    FStrategies: TStrategies;
    function GetCompound: Boolean;
    function GetFundsLedger: IExtendedLedger;
    function GetOnPlace: TOrderPlaceEvent;
    function GetOnRemove: TOrderRemoveEvent;
    function GetOnStatus: TOrderStatusEvent;
    function GetOrderManager: IOrderManager;
    function GetState: TEngineState;
    function GetStrategies: TStrategies;
    procedure SetCompound(AValue: Boolean);
    procedure SetFunds(Const AValue: Extended);
    procedure SetFundsLedger(Const AValue: IExtendedLedger);
    procedure SetHoldsLedger(Const AValue: IExtendedLedger);
    procedure SetOnPlace(Const AValue: TOrderPlaceEvent);
    procedure SetOnRemove(Const AValue: TOrderRemoveEvent);
    procedure SetOnStatus(Const AValue: TOrderStatusEvent);
    procedure SetOrderManager(Const AValue: IOrderManager);
    function GetAvailableFunds: Extended;
    function GetFunds: Extended;
    function GetHolds: Extended;
    function GetHoldsLedger: IExtendedLedger;
    function GetInventory: Extended;
    function GetInventoryLedger: IExtendedLedger;
  strict protected
    procedure SetState(Const AState:TEngineState);
    procedure QueueTicker(Const ATicker:ITicker);
    function DoStart(Out Error:String):Boolean;virtual;
    function DoStop(Out Error:String):Boolean;virtual;
    function DoFeed(Const ATicker:ITicker;Out Error:string):Boolean;virtual;
  public
    property OnPlace : TOrderPlaceEvent read GetOnPlace write SetOnPlace;
    property OnRemove : TOrderRemoveEvent read GetOnRemove write SetOnRemove;
    property OnStatus : TOrderStatusEvent read GetOnStatus write SetOnStatus;
    property Strategies : TStrategies read GetStrategies;
    property OrderManager : IOrderManager read GetOrderManager write SetOrderManager;
    property FundsLedger : IExtendedLedger read GetFundsLedger write SetFundsLedger;
    property HoldsLedger : IExtendedLedger read GetHoldsLedger write SetHoldsLedger;
    property InventoryLedger : IExtendedLedger read GetInventoryLedger write SetHoldsLedger;
    property Funds : Extended read GetFunds write SetFunds;
    property Compound : Boolean read GetCompound write SetCompound;
    property AvailableFunds : Extended read GetAvailableFunds;
    property Holds : Extended read GetHolds;
    property Inventory : Extended read GetInventory;
    property EngineState : TEngineState read GetState;
    function Feed(Const ATicker:ITicker;Out Error:string):Boolean;
    function Start(Out Error:String):Boolean;overload;
    function Start:Boolean;overload;
    function Stop(Out Error:String):Boolean;overload;
    function Stop:Boolean;overload;
    constructor Create;virtual;
    destructor Destroy; override;
  end;

implementation

{ TDelilahImpl }

function TDelilahImpl.GetCompound: Boolean;
begin
  Result:=FCompound;
end;

function TDelilahImpl.GetFundsLedger: IExtendedLedger;
begin
  Result:=FFundsLedger;
end;

function TDelilahImpl.GetOnPlace: TOrderPlaceEvent;
begin
  Result:=FOnPlace;
end;

function TDelilahImpl.GetOnRemove: TOrderRemoveEvent;
begin
  Result:=FOnRemove;
end;

function TDelilahImpl.GetOnStatus: TOrderStatusEvent;
begin
  Result:=FOnStatus;
end;

function TDelilahImpl.GetOrderManager: IOrderManager;
begin
  Result:=FOrderManager;
end;

function TDelilahImpl.GetState: TEngineState;
begin
  Result:=FState;
end;

function TDelilahImpl.GetStrategies: TStrategies;
begin
  Result:=FStrategies;
end;

procedure TDelilahImpl.SetCompound(AValue: Boolean);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  FCompound:=AValue;
end;

procedure TDelilahImpl.SetFunds(const AValue: Extended);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  FFunds:=AValue;
end;

procedure TDelilahImpl.SetFundsLedger(const AValue: IExtendedLedger);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  FFundsLedger:=nil;
  FFundsLedger:=AValue;
end;

procedure TDelilahImpl.SetHoldsLedger(const AValue: IExtendedLedger);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  FHoldsLedger:=nil;
  FHoldsLedger:=AValue;
end;

procedure TDelilahImpl.SetOnPlace(const AValue: TOrderPlaceEvent);
begin
  FOnPlace:=AValue;
end;

procedure TDelilahImpl.SetOnRemove(const AValue: TOrderRemoveEvent);
begin
  FOnRemove:=AValue;
end;

procedure TDelilahImpl.SetOnStatus(const AValue: TOrderStatusEvent);
begin
  FOnStatus:=AValue;
end;

procedure TDelilahImpl.SetOrderManager(const AValue: IOrderManager);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  FOrderManager:=AValue;
end;

function TDelilahImpl.GetAvailableFunds: Extended;
begin
  Result:=FFundsLedger.Balance - FHoldsLedger.Balance;
end;

function TDelilahImpl.GetFunds: Extended;
begin
  Result:=FFunds;
end;

function TDelilahImpl.GetHolds: Extended;
begin
  Result:=FHoldsLedger.Balance;
end;

function TDelilahImpl.GetHoldsLedger: IExtendedLedger;
begin
  Result:=FHoldsLedger;
end;

function TDelilahImpl.GetInventory: Extended;
begin
  Result:=FInvLedger.Balance;
end;

function TDelilahImpl.GetInventoryLedger: IExtendedLedger;
begin
  Result:=FInvLedger;
end;

procedure TDelilahImpl.SetState(const AState: TEngineState);
begin
  FState:=AState;
end;

procedure TDelilahImpl.QueueTicker(const ATicker: ITicker);
begin
  //todo - thread safe queue of a ticker. could either operate on an event system
  //or a polling mechanism in a separate thread
end;

function TDelilahImpl.DoStart(out Error: String): Boolean;
begin
  Result:=False;
  //todo
end;

function TDelilahImpl.DoStop(out Error: String): Boolean;
begin
  Result:=False;
  //todo
end;

function TDelilahImpl.DoFeed(const ATicker: ITicker; out Error: string): Boolean;
begin
  Result:=False;
  try
    QueueTicker(ATicker);
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TDelilahImpl.Feed(const ATicker: ITicker; out Error: string): Boolean;
begin
  Result:=DoFeed(ATicker,Error);
end;

function TDelilahImpl.Start(out Error: String): Boolean;
begin
  Result:=DoStart(Error);
end;

function TDelilahImpl.Start: Boolean;
var
  LError:String;
begin
  Result:=Start(LError);
end;

function TDelilahImpl.Stop(out Error: String): Boolean;
begin
  Result:=DoStop(Error);
end;

function TDelilahImpl.Stop: Boolean;
var
  LError:String;
begin
  Result:=Stop(LError);
end;

constructor TDelilahImpl.Create;
begin
  FFunds:=0;
  FCompound:=False;
  FFundsLedger:=NewExtendedLedger;
  FHoldsLedger:=NewExtendedLedger;
  FInvLedger:=NewExtendedLedger;
  FState:=TEngineState.esStopped;
  FStrategies:=TStrategies.Create;
end;

destructor TDelilahImpl.Destroy;
begin
  FStrategies.Free;
  FInVLedger:=nil;
  FHoldsLedger:=nil;
  FFundsLedger:=nil;
  inherited Destroy;
end;

end.

