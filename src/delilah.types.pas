unit delilah.types;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, ledger, ledger.standard;

type

  TOrderDetailsType = (odBuy,odSell);

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
    function GetType: TOrderDetailsType;
    procedure SetPrice(Const AValue: Extended);
    procedure SetSize(Const AValue: Extended);
    procedure SetType(Const AValue: TOrderDetailsType);
    //properties
    property Size : Extended read GetSize write SetSize;
    property Price : Extended read GetPrice write SetPrice;
    property OrderType : TOrderDetailsType read GetType write SetType;
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
    (*
      event triggered during a request to place an order which allows listener
      to deny the request when necessary
    *)
    property OnBeforePlace : TBeforeOrderPlaceEvent read GetOnBeforePlace
      write SetOnBeforePlace;
    (*
      event triggered once an order is successfully placed
    *)
    property OnPlace : TOrderPlaceEvent read GetOnPlace write SetOnPlace;
    (*
      event triggered once an order is successfully removed
    *)
    property OnRemove : TOrderRemoveEvent read GetOnRemove write SetOnRemove;
    (*
      event triggered once an order changes from one status to another
    *)
    property OnStatus : TOrderStatusEvent read GetOnStatus write SetOnStatus;

    //properties
    (*
      the current count of order details this manager instance is storing
    *)
    property Count:Cardinal read GetCount;
    (*
      assuming an order id provided is valid, this property will fetch
      the current status of the order details. additionally, if this fetch
      results in a change of status, notifications will be made
    *)
    property Status[Const AID:String] : TOrderManagerStatus read GetStatus;
    (*
      does a specific id exist
    *)
    property Exists[Const AID:String] : Boolean read GetExists;default;

    //methods
    (*
      attempts to place an order provided all details. on success, proper
      events will be raised and a new identifier will be generated
    *)
    function Place(Const ADetails:IOrderDetails;
      Out ID:String;Out Error:String):Boolean;overload;
    function Place(Const ADetails:IOrderDetails;Out ID:String):Boolean;overload;
    (*
      will attempt to cancel an active order raising proper events if succesful
    *)
    function Cancel(Const AID:String;
      Out Details:IOrderDetails;Out Error:String):Boolean;overload;
    function Cancel(Const AID:String;
      Out Details:IOrderDetails):Boolean;overload;
    (*
      removes an order if possible provided an identifier
    *)
    function Delete(Const AID:String;Out Error:String):Boolean;overload;
    function Delete(Const AID:String):Boolean;overload;
    (*
      fetch order details provided an identifier
    *)
    function Details(Const AID:String;Out Details:IOrderDetails;
      Out Error:String):Boolean;overload;
    function Details(Const AID:String;Out Details:IOrderDetails):Boolean;overload;
    (*
      given an order details output the id, return success found
    *)
    function ID(Const ADetails:IOrderDetails;Out ID:String):Boolean;
    (*
      will attempt to refresh the status information for managed orders
    *)
    function Refresh(Out Error:String):Boolean;
    (*
      will attempt to remove all orders. orders unable to be removed will be
      "silently" be skipped over
    *)
    procedure Clear;
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

  (*
    list of tickers
  *)
  TTickers = TFPGInterfacedObjectList<ITicker>;

  { IStrategy }
  (*
    a strategy defines the "how" in trading and is activated by feeding ticker
    information and an order manager to operate with
  *)
  IStrategy = interface
    ['{7F3F7AE5-BD9A-45EE-B4E3-6F3E9DC52964}']
    function Feed(Const ATicker : ITicker;Const AManager:IOrderManager;
      Const AFunds,AInventory,AAAC:Extended;Out Error:String):Boolean;
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
    //property methods
    function GetAAC: Extended;
    function GetAvailableInventory: Extended;
    function GetCompound: Boolean;
    function GetFundsLedger: IExtendedLedger;
    function GetHoldsInventoryLedger: IExtendedLedger;
    function GetInventoryHolds: Extended;
    function GetOnPlace: TOrderPlaceEvent;
    function GetOnRemove: TOrderRemoveEvent;
    function GetOnStatus: TOrderStatusEvent;
    function GetOrderManager: IOrderManager;
    function GetState: TEngineState;
    function GetStrategies: TStrategies;
    procedure SetAAC(Const AValue: Extended);
    procedure SetCompound(Const AValue: Boolean);
    procedure SetFunds(Const AValue: Extended);
    procedure SetFundsLedger(Const AValue: IExtendedLedger);
    procedure SetHoldsInventoryLedger(Const AValue: IExtendedLedger);
    procedure SetHoldsLedger(Const AValue: IExtendedLedger);
    procedure SetInventoryLedger(Const AValue: IExtendedLedger);
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
    property FundsLedger : IExtendedLedger read GetFundsLedger write SetFundsLedger;
    (*
      the holds ledger contains entries of order details that have not been
      completed and counts negatively towards AvailableFunds. once an order
      is completed, a credit entry will automatically be made to Holds and
      a corresponding Debit will be made to FundsLedger
    *)
    property HoldsLedger : IExtendedLedger read GetHoldsLedger write SetHoldsLedger;
    (*
      the inventory ledger contains entries of completed orders Size
    *)
    property InventoryLedger : IExtendedLedger read GetInventoryLedger write SetInventoryLedger;
    (*
      like the holds ledger, however for inventory
    *)
    property HoldsInventoryLedger : IExtendedLedger read GetHoldsInventoryLedger write SetHoldsInventoryLedger;
    (*
      assign the available Funds that the engine has to work with. orders can
      only be placed if they would not overdraft this amount
    *)
    property Funds : Extended read GetFunds write SetFunds;
    (*
      AAC = average aquisition cost, basically the average cost it took
      to purchase all of the current inventory on hand
    *)
    property AAC : Extended read GetAAC write SetAAC;
    (*
      when compound is true, the engine is allowed to re-invest profits earned,
      essentially, (TradeStack = Funds + Profit) otherwise Funds is a hard cap
    *)
    property Compound : Boolean read GetCompound write SetCompound;
    (*
      AvailableFunds = FundsLedger + HoldsLedger, where HoldsLedger contains
      a negative balance
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
      AvailableInventory = (Inventory + InventoryHolds) where holds contains
      a negative balance
    *)
    property AvailableInventory : Extended read GetAvailableInventory;
    (*
      quick access to the balance in the inventory holds ledger
    *)
    property InventoryHolds : Extended read GetInventoryHolds;
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

