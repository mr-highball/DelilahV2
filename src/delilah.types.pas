unit delilah.types;

{$mode delphi}

interface

uses
  Classes, SysUtils, ledger, ledger.standard;

type

  { IOrderDetails }
  (*
    base interface for details about an order containing only minimum
    properties required for engine
  *)
  IOrderDetails = interface
    ['{2C57FC56-B252-4B05-9808-C33322970279}']
    //property methods
    function GetPrice: Extended;
    function GetSize: Extended;
    function GetType: TLedgerType;
    procedure SetPrice(Const AValue: Extended);
    procedure SetSize(Const AValue: Extended);
    procedure SetType(Const AValue: TLedgerType);
    //properties
    property Size : Extended read GetSize write SetSize;
    property Price : Extended read GetPrice write SetPrice;
    property LedgerType : TLedgerType read GetType write SetType;
  end;

  (*
    status of an order detail managed by the IOrderManager
  *)
  TOrderManagerStatus = (
    omActive,
    omCompleted,
    omPendingCancel,
    omCanceled
  );

  (*
    event triggered when the status is changed for a managed order details
  *)
  TOrderStatusEvent = procedure(Const ADetails:IOrderDetails;Const AID:String;
    Const AOldStatus,ANewStatus:TOrderManagerStatus;Var Allow:Boolean;
    Out ADisallowReason:String) of object;

  (*
    event triggered when a new order is placed
  *)
  TOrderPlaceEvent = procedure(Const ADetails:IOrderDetails;Const AID:String) of object;

  (*
    event triggered when a managed order details is removed
  *)
  TOrderRemoveEvent = procedure(Const ADetails:IOrderDetails;Const AID:String) of object;

  { IOrderManager }
  (*
    entry point for carrying out actions on an order
  *)
  IOrderManager = interface
    ['{80F693C5-77EA-43F4-BA5D-75C71F778312}']
    //property methods
    function GetCount: Cardinal;
    function GetExists(Const AID: String): Boolean;
    function GetOnPlace: TOrderPlaceEvent;
    function GetOnRemove: TOrderRemoveEvent;
    function GetOnStatus: TOrderStatusEvent;
    function GetStatus(Const AID: String): TOrderManagerStatus;
    procedure SetOnPlace(Const AValue: TOrderPlaceEvent);
    procedure SetOnRemove(Const AValue: TOrderRemoveEvent);
    procedure SetOnStatus(Const AValue: TOrderStatusEvent);
    //events
    property OnPlace : TOrderPlaceEvent read GetOnPlace write SetOnPlace;
    property OnRemove : TOrderRemoveEvent read GetOnRemove write SetOnRemove;
    property OnStatus : TOrderStatusEvent read GetOnStatus write SetOnStatus;
    //properties
    property Count:Cardinal read GetCount;
    property Status[Const AID:String] : TOrderManagerStatus read GetStatus;
    property Exists[Const AID:String] : Boolean read GetExists;
    //methods
    function Place(Const ADetails:IOrderDetails;
      Out ID:String;Out Error:String):Boolean;overload;
    function Place(Const ADetails:IOrderDetails;Out ID:String):Boolean;overload;
    function Cancel(Const AID:String;
      Out Details:IOrderDetails;Out Error:String):Boolean;overload;
    function Cancel(Const AID:String;
      Out Details:IOrderDetails):Boolean;overload;
    function Delete(Const AID:String;Out Error:String):Boolean;overload;
    function Delete(Const AID:String):Boolean;overload;
    function Details(Out Details:IOrderDetails;
      Out Error):Boolean;overload;
    function Details(Out Details:IOrderDetails):Boolean;overload;
  end;

  (*
    state of delilah engine
  *)
  TEngineState = (
    esStopped,
    esStarted
  );

  //todo - strategy, strategy list, allow mutliple strategies in delilah

  { IDelilah }
  (*
    IDelilah is the main trading engine
  *)
  IDelilah = interface
    ['{1374EC48-BFA4-45E7-AE90-B4576B6A121B}']
    //property methods
    function GetOrderManager: IOrderManager;
    function GetState: TEngineState;
    procedure SetFunds(Const AValue: Extended);
    procedure SetHoldsLedger(Const AValue: IDoubleLedger);
    procedure SetOrderManager(Const AValue: IOrderManager);
    function GetAvailableFunds: Extended;
    function GetFunds: Extended;
    function GetHolds: Extended;
    function GetHoldsLedger: IDoubleLedger;
    function GetInventory: Extended;
    function GetInventoryLedger: IDoubleLedger;
    //events
    //properties
    property OrderManager : IOrderManager read GetOrderManager write SetOrderManager;
    property HoldsLedger : IDoubleLedger read GetHoldsLedger write SetHoldsLedger;
    property InventoryLedger : IDoubleLedger read GetInventoryLedger write SetHoldsLedger;
    property Funds : Extended read GetFunds write SetFunds;
    property AvailableFunds : Extended read GetAvailableFunds;
    property Holds : Extended read GetHolds;
    property Inventory : Extended read GetInventory;
    property EngineState : TEngineState read GetState;
    //methods
  end;


implementation

end.

