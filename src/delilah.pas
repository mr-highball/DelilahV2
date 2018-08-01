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
    FFundsLedger: IDoubleLedger;
    FHoldsLedger: IDoubleLedger;
    FInvLedger: IDoubleLedger;
    FOnPlace: TOrderPlaceEvent;
    FOnRemove: TOrderRemoveEvent;
    FOnStatus: TOrderStatusEvent;
    FOrderManager: IOrderManager;
    FState: TEngineState;
    FStrategies: TStrategies;
    function GetCompound: Boolean;
    function GetFundsLedger: IDoubleLedger;
    function GetOnPlace: TOrderPlaceEvent;
    function GetOnRemove: TOrderRemoveEvent;
    function GetOnStatus: TOrderStatusEvent;
    function GetOrderManager: IOrderManager;
    function GetState: TEngineState;
    function GetStrategies: TStrategies;
    procedure SetCompound(AValue: Boolean);
    procedure SetFunds(Const AValue: Extended);
    procedure SetFundsLedger(Const AValue: IDoubleLedger);
    procedure SetHoldsLedger(Const AValue: IDoubleLedger);
    procedure SetOnPlace(Const AValue: TOrderPlaceEvent);
    procedure SetOnRemove(Const AValue: TOrderRemoveEvent);
    procedure SetOnStatus(Const AValue: TOrderStatusEvent);
    procedure SetOrderManager(Const AValue: IOrderManager);
    function GetAvailableFunds: Extended;
    function GetFunds: Extended;
    function GetHolds: Extended;
    function GetHoldsLedger: IDoubleLedger;
    function GetInventory: Extended;
    function GetInventoryLedger: IDoubleLedger;
  strict protected
    procedure SetState(Const AState:TEngineState);
    function DoStart(Out Error:String):Boolean;virtual;
    function DoStop(Out Error:String):Boolean;virtual;
    function DoFeed(Const ATicker:ITicker;Out Error:string):Boolean;virtual;
  public
    property OnPlace : TOrderPlaceEvent read GetOnPlace write SetOnPlace;
    property OnRemove : TOrderRemoveEvent read GetOnRemove write SetOnRemove;
    property OnStatus : TOrderStatusEvent read GetOnStatus write SetOnStatus;
    property Strategies : TStrategies read GetStrategies;
    property OrderManager : IOrderManager read GetOrderManager write SetOrderManager;
    property FundsLedger : IDoubleLedger read GetFundsLedger write SetFundsLedger;
    property HoldsLedger : IDoubleLedger read GetHoldsLedger write SetHoldsLedger;
    property InventoryLedger : IDoubleLedger read GetInventoryLedger write SetHoldsLedger;
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

end;

function TDelilahImpl.GetFundsLedger: IDoubleLedger;
begin

end;

function TDelilahImpl.GetOnPlace: TOrderPlaceEvent;
begin

end;

function TDelilahImpl.GetOnRemove: TOrderRemoveEvent;
begin

end;

function TDelilahImpl.GetOnStatus: TOrderStatusEvent;
begin

end;

function TDelilahImpl.GetOrderManager: IOrderManager;
begin

end;

function TDelilahImpl.GetState: TEngineState;
begin

end;

function TDelilahImpl.GetStrategies: TStrategies;
begin

end;

procedure TDelilahImpl.SetCompound(AValue: Boolean);
begin

end;

procedure TDelilahImpl.SetFunds(const AValue: Extended);
begin

end;

procedure TDelilahImpl.SetFundsLedger(const AValue: IDoubleLedger);
begin

end;

procedure TDelilahImpl.SetHoldsLedger(const AValue: IDoubleLedger);
begin

end;

procedure TDelilahImpl.SetOnPlace(const AValue: TOrderPlaceEvent);
begin

end;

procedure TDelilahImpl.SetOnRemove(const AValue: TOrderRemoveEvent);
begin

end;

procedure TDelilahImpl.SetOnStatus(const AValue: TOrderStatusEvent);
begin

end;

procedure TDelilahImpl.SetOrderManager(const AValue: IOrderManager);
begin

end;

function TDelilahImpl.GetAvailableFunds: Extended;
begin

end;

function TDelilahImpl.GetFunds: Extended;
begin

end;

function TDelilahImpl.GetHolds: Extended;
begin

end;

function TDelilahImpl.GetHoldsLedger: IDoubleLedger;
begin

end;

function TDelilahImpl.GetInventory: Extended;
begin

end;

function TDelilahImpl.GetInventoryLedger: IDoubleLedger;
begin

end;

procedure TDelilahImpl.SetState(const AState: TEngineState);
begin
  FState:=AState;
end;

function TDelilahImpl.DoStart(out Error: String): Boolean;
begin

end;

function TDelilahImpl.DoStop(out Error: String): Boolean;
begin

end;

function TDelilahImpl.DoFeed(const ATicker: ITicker; out Error: string): Boolean;
begin

end;

function TDelilahImpl.Feed(const ATicker: ITicker; out Error: string): Boolean;
begin

end;

function TDelilahImpl.Start(out Error: String): Boolean;
begin

end;

function TDelilahImpl.Start: Boolean;
begin

end;

function TDelilahImpl.Stop(out Error: String): Boolean;
begin

end;

function TDelilahImpl.Stop: Boolean;
begin

end;

constructor TDelilahImpl.Create;
begin
  FFunds:=0;
  FCompound:=False;
  FFundsLedger:=NewDoubleLedger;
  FHoldsLedger:=NewDoubleLedger;
  FInvLedger:=NewDoubleLedger;
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

