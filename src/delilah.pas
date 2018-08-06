unit delilah;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, ledger, ledger.standard, fgl;

type

  { TDelilahImpl }
  (*
    base implementation for IDelilah
  *)
  TDelilahImpl = class(TInterfacedObject,IDelilah)
  strict private
    type
      TLedgerSource = (lsHold,lsStd,lsInvHold,lsStdInv);

      { TLedgerPair }

      TLedgerPair = packed record
      public
        ID : String;
        Source : TLedgerSource;
        class operator Equal(Const A,B:TLedgerPair):Boolean;
      end;

      TLedgerPairList = TFPGList<TLedgerPair>;
      TOrderLedgerMap = TFPGMapObject<String,TLedgerPairList>;
  strict private
    FFunds: Extended;
    FCompound: Boolean;
    FFundsLedger: IExtendedLedger;
    FHoldsLedger: IExtendedLedger;
    FInvLedger: IExtendedLedger;
    FHoldsInvLedger: IExtendedLedger;
    FOnPlace: TOrderPlaceEvent;
    FOnRemove: TOrderRemoveEvent;
    FOnStatus: TOrderStatusEvent;
    FOrderManager: IOrderManager;
    FState: TEngineState;
    FStrategies: TStrategies;
    FOldPlace: TOrderPlaceEvent;
    FOldRemove: TOrderRemoveEvent;
    FOldStatus: TOrderStatusEvent;
    FOrderLedger: TOrderLedgerMap;
    function GetAvailableInventory: Extended;
    function GetInventoryHolds: Extended;
    procedure SetInventoryLedger(Const AValue: IExtendedLedger);
    function GetCompound: Boolean;
    function GetFundsLedger: IExtendedLedger;
    function GetHoldsInventoryLedger: IExtendedLedger;
    function GetOnPlace: TOrderPlaceEvent;
    function GetOnRemove: TOrderRemoveEvent;
    function GetOnStatus: TOrderStatusEvent;
    function GetOrderManager: IOrderManager;
    function GetState: TEngineState;
    function GetStrategies: TStrategies;
    procedure SetCompound(Const AValue: Boolean);
    procedure SetFunds(Const AValue: Extended);
    procedure SetFundsLedger(Const AValue: IExtendedLedger);
    procedure SetHoldsInventoryLedger(Const AValue: IExtendedLedger);
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
    procedure DoPlace(Const ADetails:IOrderDetails;Const AID:String);
    procedure DoRemove(Const ADetails:IOrderDetails;Const AID:String);
    procedure DoStatus(Const ADetails:IOrderDetails;Const AID:String;
      Const AOldStatus,ANewStatus:TOrderManagerStatus);
  strict protected
    procedure SetState(Const AState:TEngineState);
    (*
      stores a ledger id and order id pair in order to lookup the source ledger
    *)
    procedure StoreLedgerID(Const AOrderID,ALedgerID:String;Const ALedgerSource:TLedgerSource);
    (*
      given an order id and a source to balance, this method will automatically
      record an entry required to balance to zero
    *)
    procedure BalanceOrder(Const AOrderID:String;Const ALedgerSource:TLedgerSource);
    (*
      performs necessary steps to move an order from active to completed
    *)
    procedure CompleteOrder(Const ADetails:IOrderDetails;Const AID:String);
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
    property InventoryLedger : IExtendedLedger read GetInventoryLedger write SetInventoryLedger;
    property HoldsInventoryLedger : IExtendedLedger read GetHoldsInventoryLedger write SetHoldsInventoryLedger;
    property Funds : Extended read GetFunds write SetFunds;
    property Compound : Boolean read GetCompound write SetCompound;
    property AvailableFunds : Extended read GetAvailableFunds;
    property Holds : Extended read GetHolds;
    property Inventory : Extended read GetInventory;
    property AvailableInventory : Extended read GetAvailableInventory;
    property InventoryHolds : Extended read GetInventoryHolds;
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

{ TDelilahImpl.TLedgerPair }

class operator TDelilahImpl.TLedgerPair.Equal(const A, B: TLedgerPair): Boolean;
begin
  Result:=A.ID=B.ID;
end;

{ TDelilahImpl }

procedure TDelilahImpl.SetInventoryLedger(const AValue: IExtendedLedger);
begin
  FInvLedger:=AValue;
end;

function TDelilahImpl.GetAvailableInventory: Extended;
begin
  Result:=Inventory - InventoryHolds;
end;

function TDelilahImpl.GetInventoryHolds: Extended;
begin
  Result:=FHoldsInvLedger.Balance;
end;

function TDelilahImpl.GetCompound: Boolean;
begin
  Result:=FCompound;
end;

function TDelilahImpl.GetFundsLedger: IExtendedLedger;
begin
  Result:=FFundsLedger;
end;

function TDelilahImpl.GetHoldsInventoryLedger: IExtendedLedger;
begin
  Result:=FHoldsInvLedger;
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

procedure TDelilahImpl.SetCompound(const AValue: Boolean);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  FCompound:=AValue;
end;

procedure TDelilahImpl.SetFunds(const AValue: Extended);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  //balance out what we had for our old funds
  if FFunds<>0 then
    FFundsLedger.RecordEntry(
      FFunds,
      ltDebit
    );
  FFunds:=AValue;
  //record an entry for the new funds
  FFundsLedger.RecordEntry(
    FFunds,
    ltCredit
  );
end;

procedure TDelilahImpl.SetFundsLedger(const AValue: IExtendedLedger);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  FFundsLedger:=nil;
  FFundsLedger:=AValue;
end;

procedure TDelilahImpl.SetHoldsInventoryLedger(const AValue: IExtendedLedger);
begin
  if not (FState=esStopped) then
    raise Exception.Create('engine is running, stop first');
  FHoldsInvLedger:=nil;
  FHoldsInvLedger:=AValue;
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
  FOldStatus:=nil;
  FOldRemove:=nil;
  FOldStatus:=nil;
  FOrderManager:=nil;
  FOrderManager:=AValue;
  //don't "lose" the events if they were assigned
  if Assigned(FOrderManager) then
  begin
    FOldStatus:=FOrderManager.OnStatus;
    FOldRemove:=FOrderManager.OnRemove;
    FOldStatus:=FOrderManager.OnStatus;
  end;
  //now redirect the order manager events to the engine
  FOrderManager.OnStatus:=DoStatus;
  FOrderManager.OnRemove:=DoRemove;
  FOrderManager.OnStatus:=DoStatus;
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

procedure TDelilahImpl.DoPlace(const ADetails: IOrderDetails; const AID: String);
var
  LID:String;
  LStatus:TOrderManagerStatus;
begin
  LStatus:=FOrderManager.Status[AID];
  case LStatus of
    //active orders are considered to be on "hold" until they have been completed
    omActive:
      begin
        //record an entry into the holds ledger
        FHoldsLedger.RecordEntry(
          ADetails.Price * ADetails.Size,
          ADetails.LedgerType,
          LID
        );
        //now store the ledger id associated with this order id
        StoreLedgerID(AID,LID,lsHold);
        //record an entry for the holds inventory
        FHoldsInvLedger.RecordEntry(
          ADetails.Size,
          ADetails.InventoryLedgerType,
          LID
        );
        StoreLedgerID(AID,LID,lsInvHold)
      end;
    //when an order has been completed, we need to check that there is no
    //holds entry, and if so, balance it out with an opposite ledger type,
    //which complete order will do for us
    omCompleted: CompleteOrder(ADetails,AID);
  end;
end;

procedure TDelilahImpl.DoRemove(const ADetails: IOrderDetails; const AID: String);
var
  I:Integer;
begin
  //on removing, just balance any amounts that may be in hold
  BalanceOrder(AID,lsHold);
  BalanceOrder(AID,lsInvHold);
  //remove the lookup entry as well
  I:=FOrderLedger.IndexOf(AID);
  if I>=0 then
    FOrderLedger.Delete(I);
end;

procedure TDelilahImpl.DoStatus(const ADetails: IOrderDetails;
  const AID: String; const AOldStatus, ANewStatus: TOrderManagerStatus);
begin
  case ANewStatus of
    //when an order is marked as canceled we need to balance
    //any holds on the funds and inventory ledger
    omCanceled:
      begin
        BalanceOrder(AID,lsHold);
        BalanceOrder(AID,lsInvHold);
      end;
    //for completed orders, call down to complete order method
    omCompleted:
      begin
        CompleteOrder(ADetails,AID);
      end;
  end;
end;

procedure TDelilahImpl.SetState(const AState: TEngineState);
begin
  FState:=AState;
end;

procedure TDelilahImpl.StoreLedgerID(const AOrderID, ALedgerID: String;
  const ALedgerSource: TLedgerSource);
var
  I:Integer;
  LLedger:IExtendedLedger;
  LList:TLedgerPairList;
  LEntry:TLedgerPair;
begin
  if FOrderManager.Exists[AOrderID] then
  begin
    //depending on source, assign local ledger properly
    case ALedgerSource of
      lsHold: LLedger:=FHoldsLedger;
      lsStd: LLedger:=FFundsLedger;
      lsInvHold: LLedger:=FHoldsInvLedger;
      lsStdInv: LLedger:=FInvLedger;
    end;
    //check in map to see if we have the pair list for this order
    I:=FOrderLedger.IndexOf(AOrderID);
    if I<0 then
    begin
      LList:=TLedgerPairList.Create;
      //the entry below is used as a means of looking up a given order id
      //and find a corresponding ledger entry that was placed in one of our
      //ledgers (holds/inv/etc...)
      LEntry.ID:=ALedgerID;
      LEntry.Source:=ALedgerSource;
      //store the lookup entry
      LList.Add(LEntry);
    end;
  end;
end;

procedure TDelilahImpl.BalanceOrder(const AOrderID: String;
  const ALedgerSource: TLedgerSource);
var
  I:Integer;
  LList:TLedgerPairList;
  LIndexes:TArray<Integer>;
  LOwned:TLedgerPairList;
  LLedger:IExtendedLedger;
begin
  //check to see if we even have this order id recorded, and if not everything
  //should be properly balanced
  I:=FOrderLedger.IndexOf(AOrderID);
  if I<0 then
    Exit;
  LOwned:=FOrderLedger.Data[I];
  //if the owned list contains no pairs, we should also be balanced
  if LOwned.Count<1 then
    Exit;
  //create a local ledger list for adding all pairs for a given source
  LList:=TLedgerPairList.Create;
  try
    for I:=0 to Pred(LOwned.Count) do
    begin
      if LOwned[I].Source=ALedgerSource then
      begin
        LList.Add(LOwned[I]);
        //store the index for removal later
        SetLength(LIndexes,Succ(Length(LIndexes)));
        LIndexes[High(LIndexes)]:=I;
      end;
    end;
    //no matching ledger pairs for source, safe to exit
    if LList.Count<0 then
      Exit;
    //get a reference to the source ledger
    case ALedgerSource of
      lsHold: LLedger:=FHoldsLedger;
      lsStd: LLedger:=FFundsLedger;
      lsInvHold: LLedger:=FHoldsInvLedger;
      lsStdInv: LLedger:=FInvLedger;
    end;
    //for each record we need to add a balancing entry to the source
    for I:=0 to Pred(LList.Count) do
    begin
      //below makes the assumption our ledger wasn't flattened or tampered with
      //by an outside source...
      try
        //if we are credit entry, then we need to debit
        if LLedger[LList[I].ID].LedgerType=ltCredit then
          LLedger.RecordEntry(
            LLedger[LList[I].ID].Entry,
            ltDebit
          )
        //otherwise record an opposite debit
        else
          LLedger.RecordEntry(
            LLedger[LList[I].ID].Entry,
            ltCredit
          );
      finally
        //todo - currently just swallowing any exception which would occur, address
      end;
    end;
    //now for all of the indexes, remove from the lookup list
    for I:=0 to High(LIndexes) do
      LOwned.Delete(LIndexes[I]);
  finally
    LList.Free;
  end;
end;

procedure TDelilahImpl.CompleteOrder(const ADetails: IOrderDetails;
  const AID: String);
var
  LID:String;
begin
  //nothing to complete if we aren't managing this order
  if not FOrderManager.Exists[AID] then
    Exit;
  //balance the hold ledgers
  BalanceOrder(AID,lsHold);
  BalanceOrder(AID,lsInvHold);
  //record entries to funds ledger
  FFundsLedger.RecordEntry(
    ADetails.Price * ADetails.Size,
    ADetails.LedgerType,
    LID
  );
  StoreLedgerID(AID,LID,lsStd);
  //record entries to inventory ledger
  FFundsLedger.RecordEntry(
    ADetails.Size,
    ADetails.LedgerType,
    LID
  );
  StoreLedgerID(AID,LID,lsInvHold);
end;

function TDelilahImpl.DoStart(out Error: String): Boolean;
begin
  //base class operates in an evented way only, so no background process to stop
  Result:=True;
end;

function TDelilahImpl.DoStop(out Error: String): Boolean;
begin
  //base class operates in an evented way only, so no background process to stop
  Result:=True;
end;

function TDelilahImpl.DoFeed(const ATicker: ITicker; out Error: string): Boolean;
var
  I:Integer;
begin
  Result:=False;
  try
    //simply iterate strategies and attempt to feed, they are responsible
    //for the rest of the logic in our base class engine
    for I:=0 to Pred(FStrategies.Count) do
      if not FStrategies[I].Feed(ATIcker,FOrderManager,
        AvailableFunds,AvailableInventory,Error
      ) then
        Exit;
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
  Result:=False;
  try
    //already running
    if FState=esStarted then
      Exit(True);
    Result:=DoStart(Error);
    FState:=esStarted;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TDelilahImpl.Start: Boolean;
var
  LError:String;
begin
  Result:=Start(LError);
end;

function TDelilahImpl.Stop(out Error: String): Boolean;
begin
  Result:=False;
  try
    //already stopped
    if FState=esStopped then
      Exit(true);
    Result:=DoStop(Error);
    FState:=esStopped;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TDelilahImpl.Stop: Boolean;
var
  LError:String;
begin
  Result:=Stop(LError);
end;

constructor TDelilahImpl.Create;
begin
  FOrderLedger:=TOrderLedgerMap.Create(true);
  FFunds:=0;
  FCompound:=False;
  FFundsLedger:=NewExtendedLedger;
  FHoldsLedger:=NewExtendedLedger;
  FHoldsInvLedger:=NewExtendedLedger;
  FInvLedger:=NewExtendedLedger;
  FState:=TEngineState.esStopped;
  FStrategies:=TStrategies.Create;
  FOldPlace:=nil;
  FOldRemove:=nil;
  FOldStatus:=nil;
end;

destructor TDelilahImpl.Destroy;
begin
  FStrategies.Free;
  FInVLedger:=nil;
  FHoldsLedger:=nil;
  FHoldsInvLedger:=nil;
  FFundsLedger:=nil;
  FOrderLedger.Free;
  inherited Destroy;
end;

end.
