unit delilah.types;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, ledger, ledger.standard;

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
    Const AOldStatus,ANewStatus:TOrderManagerStatus) of object;

  (*
    event triggered before an order is placed
  *)
  TBeforeOrderPlaceEvent = procedure(Const ADetails:IOrderDetails;
    Var Allow:Boolean;Out ADisallowReason) of object;

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
    function GetOnBeforePlace: TBeforeOrderPlaceEvent;
    function GetOnPlace: TOrderPlaceEvent;
    function GetOnRemove: TOrderRemoveEvent;
    function GetOnStatus: TOrderStatusEvent;
    function GetStatus(Const AID: String): TOrderManagerStatus;
    procedure SetOnBeforePlace(Const AValue: TBeforeOrderPlaceEvent);
    procedure SetOnPlace(Const AValue: TOrderPlaceEvent);
    procedure SetOnRemove(Const AValue: TOrderRemoveEvent);
    procedure SetOnStatus(Const AValue: TOrderStatusEvent);
    //events
    property OnBeforePlace : TBeforeOrderPlaceEvent read GetOnBeforePlace
      write SetOnBeforePlace;
    property OnPlace : TOrderPlaceEvent read GetOnPlace write SetOnPlace;
    property OnRemove : TOrderRemoveEvent read GetOnRemove write SetOnRemove;
    property OnStatus : TOrderStatusEvent read GetOnStatus write SetOnStatus;
    //properties
    property Count:Cardinal read GetCount;
    property Status[Const AID:String] : TOrderManagerStatus read GetStatus;
    property Exists[Const AID:String] : Boolean read GetExists;default;
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
    function Details(Const AID:String;Out Details:IOrderDetails;
      Out Error):Boolean;overload;
    function Details(Const AID:String;Out Details:IOrderDetails):Boolean;overload;
  end;

  { ITicker }
  (*
    a ticker tells the price at a current point in time
  *)
  ITicker = interface
    ['{52EC942B-4774-43F0-83EE-D0D301953103}']
    //property methods
    function GetPrice: Extended;
    function GetTime: TDateTime;
    procedure SetPrice(Const AValue: Extended);
    procedure SetTime(Const AValue: TDateTime);
    //properties
    property Price : Extended read GetPrice write SetPrice;
    property Time : TDateTime read GetTime write SetTime;
  end;

  { IStrategy }
  (*
    a strategy defines the "how" in trading and is activated by feeding ticker
    information and an order manager to operate with
  *)
  IStrategy = interface
    ['{7F3F7AE5-BD9A-45EE-B4E3-6F3E9DC52964}']
    function Feed(Const ATicker : ITicker;Const AManager:IOrderManager;
      Out Error:String):Boolean;
  end;

  TStrategies = TFPGList<IStrategy>;

  (*
    state of delilah engine
  *)
  TEngineState = (
    esStopped,
    esStarted
  );

  { IDelilah }
  (*
    IDelilah is the main trading engine
  *)
  IDelilah = interface
    ['{1374EC48-BFA4-45E7-AE90-B4576B6A121B}']
    function GetCompound: Boolean;
    //property methods
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

    //events
    property OnPlace : TOrderPlaceEvent read GetOnPlace write SetOnPlace;
    property OnRemove : TOrderRemoveEvent read GetOnRemove write SetOnRemove;
    property OnStatus : TOrderStatusEvent read GetOnStatus write SetOnStatus;

    //properties
    (*
      strategies define the "how" in trading and can be added via this property.
      a single strategy can be used, but this allows for "stacking" of strategies
      and will be fed tickers in FIFO order when the Feed method is called on
      the engine. add and remove when the engine is stopped
    *)
    property Strategies : TStrategies read GetStrategies;
    (*
      assign an order manager to allow the placing/removing of orders (ie. gdax manager)
    *)
    property OrderManager : IOrderManager read GetOrderManager write SetOrderManager;
    (*
      the funds ledger contains entries of completed orders
    *)
    property FundsLedger : IDoubleLedger read GetFundsLedger write SetFundsLedger;
    (*
      the holds ledger contains entries of order details that have not been
      completed and counts negatively towards AvailableFunds. once an order
      is completed, a credit entry will automatically be made to Holds and
      a corresponding Debit will be made to FundsLedger
    *)
    property HoldsLedger : IDoubleLedger read GetHoldsLedger write SetHoldsLedger;
    (*
      the inventory ledger contains entries of completed orders Size
    *)
    property InventoryLedger : IDoubleLedger read GetInventoryLedger write SetHoldsLedger;
    (*
      assign the available Funds that the engine has to work with. orders can
      only be placed if they would not overdraft this amount
    *)
    property Funds : Extended read GetFunds write SetFunds;
    (*
      when compound is true, the engine is allowed to re-invest profits earned,
      essentially, (TradeStack = Funds + Profit) otherwise Funds is a hard cap
    *)
    property Compound : Boolean read GetCompound write SetCompound;
    (*
      AvailableFunds = FundsLedger - HoldsLedger
    *)
    property AvailableFunds : Extended read GetAvailableFunds;
    (*
      quick access to the balance in the HoldsLedger
    *)
    property Holds : Extended read GetHolds;
    (*
      quick access to the balance in the InventoryLedger
    *)
    property Inventory : Extended read GetInventory;
    (*
      the current state of the engine
    *)
    property EngineState : TEngineState read GetState;

    //methods
    (*
      entry point for ticker information to the engine. calling will
      feed all strategies as long as the engine is running
    *)
    function Feed(Const ATicker:ITicker;Out Error:string):Boolean;
    (*
      starts the engine's processing loop
    *)
    function Start(Out Error:String):Boolean;overload;
    function Start:Boolean;overload;
    (*
      stops the engine's processing loop
    *)
    function Stop(Out Error:String):Boolean;overload;
    function Stop:Boolean;overload;
  end;


implementation

end.

