unit delilah;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, ledger, ledger.standard, fgl;

type

  TEngineLogEvent = procedure(Const AMessage:String) of object;
  
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
        class operator GreaterThan(A,B: TLedgerPair):Boolean;
        class operator LessThan(A,B: TLedgerPair):Boolean;
      end;

      TLedgerPairList = TFPGList<TLedgerPair>;
      TOrderLedgerMap = TFPGMapObject<String,TLedgerPairList>;
    const
      LOG_PREFIX = 'Engine::';
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
    FAAC: Extended;
    FOnInfo: TEngineLogEvent;
    FOnError: TEngineLogEvent;
    FOnWarn: TEngineLogEvent;
    function GetAAC: Extended;
    function GetAvailableInventory: Extended;
    function GetInventoryHolds: Extended;
    procedure SetAAC(Const AValue: Extended);
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
    procedure DoBeforePlace(Const ADetails:IOrderDetails;
      Var Allow:Boolean;Out ADisallowReason:String);
    procedure DoPlace(Const ADetails:IOrderDetails;Const AID:String);
    procedure DoRemove(Const ADetails:IOrderDetails;Const AID:String);
    procedure DoStatus(Const ADetails:IOrderDetails;Const AID:String;
      Const AOldStatus,ANewStatus:TOrderManagerStatus);
  strict protected
    //logging methods
    procedure LogInfo(Const AMessage:String);
    procedure LogError(Const AMessage:String);
    procedure LogWarning(Const AMessage:String);
    
  
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
    property AAC : Extended read GetAAC write SetAAC;
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
    constructor Create;virtual; overload;
    constructor Create(Const AOnInfo,AOnError,AOnWarn:TEngineLogEvent);virtual; overload;
    destructor Destroy; override;
  end;

implementation
uses
  math;

{ TDelilahImpl.TLedgerPair }

class operator TDelilahImpl.TLedgerPair.Equal(const A, B: TLedgerPair): Boolean;
begin
  Result:=A.ID=B.ID;
end;

class operator TDelilahImpl.TLedgerPair.GreaterThan(A, B: TLedgerPair): Boolean;
begin
  Result:=Ord(A.Source)>Ord(B.Source);
end;

class operator TDelilahImpl.TLedgerPair.LessThan(A, B: TLedgerPair): Boolean;
begin
  Result:=Ord(A.Source)<Ord(B.Source);
end;

{ TDelilahImpl }

procedure TDelilahImpl.LogInfo(const AMessage: String);
begin
  if Assigned(FOnInfo) then
    FOnInfo(Self.Classname + '::' + AMessage);
end;

procedure TDelilahImpl.LogError(const AMessage: String);
begin
  if Assigned(FOnError) then
    FOnError(Self.Classname + '::' + AMessage);
end;

procedure TDelilahImpl.LogWarning(const AMessage: String);
begin
  if Assigned(FOnWarn) then
    FOnWarn(Self.Classname + '::' + AMessage);
end;

procedure TDelilahImpl.SetInventoryLedger(const AValue: IExtendedLedger);
begin
  FInvLedger:=nil;
  //reset aquisition cost since our inventory ledger is being reset
  FAAC:=0;
  FInvLedger:=AValue;
end;

function TDelilahImpl.GetAvailableInventory: Extended;
begin
  //use addition here, because holds are going to be a series of credits (negative)
  if InventoryHolds>0 then
    Result:=Inventory
  else
    Result:=Inventory + InventoryHolds;
end;

function TDelilahImpl.GetAAC: Extended;
begin
  Result:=FAAC;
end;

function TDelilahImpl.GetInventoryHolds: Extended;
begin
  Result:=FHoldsInvLedger.Balance
end;

procedure TDelilahImpl.SetAAC(const AValue: Extended);
begin
  FAAC:=AValue;
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
  //if the funds is changing, re-balance the ledger
  if (FFunds<>AValue) then
    FFundsLedger.RecordEntry(
      FFundsLedger.Balance,
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
  FOldPlace:=nil;
  FOldRemove:=nil;
  FOldStatus:=nil;
  FOrderManager:=nil;
  FOrderManager:=AValue;
  //don't "lose" the events if they were assigned
  if Assigned(FOrderManager) then
  begin
    FOldPlace:=FOrderManager.OnPlace;
    FOldRemove:=FOrderManager.OnRemove;
    FOldStatus:=FOrderManager.OnStatus;
  end;
  //now redirect the order manager events to the engine
  FOrderManager.OnPlace:=DoPlace;
  FOrderManager.OnRemove:=DoRemove;
  FOrderManager.OnStatus:=DoStatus;
  FOrderManager.OnBeforePlace:=DoBeforePlace;
end;

function TDelilahImpl.GetAvailableFunds: Extended;
begin
  Result:=0;
  //holds ledger should not be added if positive (ie. in the event a sell is on
  //the book but being held)
  if Holds>0 then
    Result:=FFundsLedger.Balance
  //use addition here, because holds are going to be a series of credits (negative)
  else
    Result:=FFundsLedger.Balance + Holds
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
  Result:=FInvLedger.Balance
end;

function TDelilahImpl.GetInventoryLedger: IExtendedLedger;
begin
  Result:=FInvLedger;
end;

procedure TDelilahImpl.DoBeforePlace(const ADetails: IOrderDetails;
  var Allow: Boolean; out ADisallowReason:String);
var
  LAmt:Extended;
begin
  Allow:=False;

  //do not allow orders unless the engine is started
  if EngineState <> esStarted then
  begin
    ADisallowReason := 'engine not started';
    Exit;
  end;

  //if the order total cost would exceed either funds or attempt
  //to sell more inventory than we have, then this would be an invalid order
  if ADetails.OrderType=odBuy then
  begin
    //price may or may not be filled out, but size cannot be zero
    LAmt:=ADetails.Size;

    //can't make a purchase for no inventory
    if LAmt <= 0 then
    begin
      ADisallowReason:=Format(
        '[size]:%s is negative or zero, which is not allowed for a buy',
        [FloatToStr(LAmt)]
      );
      Exit;
    end;

    //now set the amount equal to the total cost of the order
    LAmt:=ADetails.Price * ADetails.Size;

    if RoundTo(LAmt,-8) > RoundTo(AvailableFunds,-8) then
    begin
      ADisallowReason:=Format(
        '[cost]:%s would exceed available funds of %s',
        [FloatToStr(LAmt),FloatToStr(AvailableFunds)]
      );
      Exit;
    end;
  end
  else
  begin
    //just checking against the size of the details
    LAmt:=ADetails.Size;

    //can't sell less than or equal to inventory
    if LAmt <= 0 then
    begin
      ADisallowReason:=Format(
        '[size]:%s is negative or zero, which is not allowed for a sell',
        [FloatToStr(LAmt)]
      );
      Exit;
    end;

    //basic check against available inventory
    if RoundTo(LAmt,-8) > RoundTo(AvailableInventory,-8) then
    begin
      ADisallowReason:=Format(
        '%s would exceed available funds of %s',
        [FloatToStr(LAmt),FloatToStr(AvailableInventory)]
      );
      Exit;
    end;
  end;

  //as long as everything above checked out, we will allow the transaction
  //to be performed
  Allow:=True;
end;

procedure TDelilahImpl.DoPlace(const ADetails: IOrderDetails; const AID: String);
const
  PREFIX = LOG_PREFIX + 'DoPlace::';
var
  LID:String;
  LStatus:TOrderManagerStatus;
  LBal,
  LEntry:Extended;
begin
  LogInfo(PREFIX + 'starting');
  LBal:=0;

  //record an entry into the holds ledger
  if ADetails.OrderType = odBuy then
  begin
    LEntry := ADetails.Price * ADetails.Size;
    LBal := FHoldsLedger.RecordEntry(
      LEntry,
      ltDebit,
      LID
    ).Balance;

    LogInfo(PREFIX + 'HoldsLedger::buy detected recording debit, [entry]-' + FloatToStr(LEntry) + ' new balance [balance]-' + FloatToStr(LBal));
  end
  else
  begin
    LEntry := ADetails.Price * ADetails.Size;
    LBal:=FHoldsLedger.RecordEntry(
      LEntry,
      ltCredit,
      LID
    ).Balance;

    LogInfo(PREFIX + 'HoldsLedger::sell detected recording credit, [entry]-' + FloatToStr(LEntry) + ' new balance [balance]-' + FloatToStr(LBal));
  end;

  //now store the ledger id associated with this order id
  StoreLedgerID(AID, LID, lsHold);

  //record an entry for the holds inventory
  if ADetails.OrderType = odBuy then
  begin
    LEntry := ADetails.Size;
    LBal:=FHoldsInvLedger.RecordEntry(
      ADetails.Size,
      ltCredit,
      LID
    ).Balance;

    LogInfo(PREFIX + 'InvHoldsLedger::buy detected recording credit, [entry]-' + FloatToStr(LEntry) + ' new balance [balance]-' + FloatToStr(LBal));
  end
  else
  begin
    LEntry := ADetails.Size;
    LBal := FHoldsInvLedger.RecordEntry(
      ADetails.Size,
      ltDebit,
      LID
    ).Balance;

    LogInfo(PREFIX + 'InvHoldsLedger::sell detected recording debit, [entry]-' + FloatToStr(LEntry) + ' new balance [balance]-' + FloatToStr(LBal));
  end;

  //store to ledger
  StoreLedgerID(AID, LID, lsInvHold);

  //raise "old" events if any
  if Assigned(FOldPlace) then
    FOldPlace(ADetails,AID);

  if Assigned(FOnPlace) then
    FOnPlace(ADetails,AID);

  LogInfo(PREFIX + 'finished');
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
  if Assigned(FOldRemove) then
    FOldRemove(ADetails,AID);
  if Assigned(FOnRemove) then
    FOnRemove(ADetails,AID);
end;

procedure TDelilahImpl.DoStatus(const ADetails: IOrderDetails;
  const AID: String; const AOldStatus, ANewStatus: TOrderManagerStatus);
const
  PREFIX = LOG_PREFIX + 'DoStatus::';
begin
  LogInfo(PREFIX + 'starting status handler');
  
  case ANewStatus of    
    //when an order is marked as canceled/completed call down to complete order
    omCanceled,omCompleted:
      begin
        LogInfo(PREFIX + 'status changed, starting CompleteOrder for [ID]-' + AID);
        CompleteOrder(ADetails,AID);
      end;
    else
      LogInfo(PREFIX + 'status changed, but not canceled or completed [Old]-' + OrderManagerStatusToString(AOldStatus) + ' [New]-' + OrderManagerStatusToString(ANewStatus));
  end;
  if Assigned(FOldStatus) then
    FOldStatus(ADetails,AID,AOldStatus,ANewStatus);
  if Assigned(FOnStatus) then
    FOnStatus(ADetails,AID,AOldStatus,ANewStatus);
    
  LogInfo(PREFIX + 'finished status handler');
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
      FOrderLedger.Add(AOrderID,LList);
    end
    else
    begin
      LList:=FOrderLedger.Data[I];
      LEntry.ID:=ALedgerID;
      LEntry.Source:=ALedgerSource;
      LList.Add(LEntry);
    end;
  end;
end;

procedure TDelilahImpl.BalanceOrder(const AOrderID: String;
  const ALedgerSource: TLedgerSource);
const 
  PREFIX = LOG_PREFIX + 'BalanceOrder::';
var
  I:Integer;
  LIndexes:TArray<Integer>;
  LPair:TLedgerPair;
  LList:TLedgerPairList;
  LOwned:TLedgerPairList;
  LLedger:IExtendedLedger;
  LType:TLedgerType;
  LBal:Extended;
begin
  LogInfo(PREFIX + 'starting');  
  LBal:=0;
  
  //check to see if we even have this order id recorded, and if not everything
  //should be properly balanced
  I := FOrderLedger.IndexOf(AOrderID);
  
  if I < 0 then
  begin
    LogWarning('could not find [ID]-' + AOrderID + ' in order ledger');
    Exit;
  end;
  
  //get the owned list
  LOwned := FOrderLedger.Data[I];
  
  //if the owned list contains no pairs, we should also be balanced
  if LOwned.Count < 1 then
  begin
    LogInfo('owned count is zero (no pairs)');
    Exit;
  end;
  
  //create a local ledger list for adding all pairs for a given source
  LList := TLedgerPairList.Create;
  try
    try
      LogInfo('iterating owned');
      for I:=0 to Pred(LOwned.Count) do
      begin
        LPair:=LOwned[I];
        
        if LPair.Source = ALedgerSource then
        begin
          LList.Add(LPair);
          
          //store the index for removal later
          SetLength(LIndexes,Succ(Length(LIndexes)));
          LIndexes[High(LIndexes)]:=I;
        end;
      end;
      
      //no matching ledger pairs for source, safe to exit
      if LList.Count < 0 then
      begin
        LogWarning('no mathching ledger pairs');
        Exit;
      end;
      
      //get a reference to the source ledger
      LogInfo('finding ledger source');
      case ALedgerSource of
        lsHold: 
          begin            
            LLedger := FHoldsLedger;
            LogInfo('[LedgerSource]-holds ledger');
          end;
        lsStd: 
          begin
            LLedger := FFundsLedger;
            LogInfo('[LedgerSource]-funds ledger');
          end;
        lsInvHold: 
          begin
            LLedger := FHoldsInvLedger;
            LogInfo('[LedgerSource]-holds inventory ledger');
          end;
        lsStdInv: 
          begin 
            LLedger := FInvLedger;
            LogInfo('[LedgerSource]-inventory ledger');
          end;
      end;
           
      //for each record we need to add a balancing entry to the source
      LogInfo('Balancing::starting');
      
      for I := 0 to Pred(LList.Count) do
      begin
        //below makes the assumption our ledger wasn't flattened or tampered with
        //by an outside source...
        try
          LPair := LList[I];
          
          //if we are credit entry, then we need to debit
          if LLedger[LPair.ID].LedgerType = ltCredit then
          begin
            LBal:=LLedger.RecordEntry(
              LLedger[LPair.ID].Entry,
              ltDebit
            ).Balance;
            
            LogInfo(PREFIX + 'Balancing::record debit, [entry]-' + FloatToStr(LLedger[LPair.ID].Entry) + ' new balance [balance]-' + FloatToStr(LBal));
          end
          //otherwise record a credit
          else
          begin
            LBal := LLedger.RecordEntry(
              LLedger[LPair.ID].Entry,
              ltCredit
            ).Balance;
            
            LogInfo(PREFIX + 'Balancing::record credit, [entry]-' + FloatToStr(LLedger[LPair.ID].Entry) + ' new balance [balance]-' + FloatToStr(LBal));
          end;
        except on E : Exception do
          LogError(PREFIX + 'Balancing::' + E.Message);
        end;
      end;
      
      LogInfo('Balancing::finished');
      
      //now for all of the indexes, remove from the lookup list
      LogInfo('about to remove from owned [count]-' + IntToStr(Length(LIndexes)));
      
      for I:=0 to High(LIndexes) do
        LOwned.Delete(LIndexes[I]);
      
      LogInfo(PREFIX + 'finished');
    except on E : Exception do
      LogError(PREFIX + E.Message);
    end;    
  finally
    LList.Free;
  end;
end;

procedure TDelilahImpl.CompleteOrder(const ADetails: IOrderDetails;
  const AID: String);
const
  PREFIX = LOG_PREFIX + 'CompleteOrder::';
var
  LID:String;
  LOldInv:Extended;
  LBal:Extended;
begin
  LogInfo(PREFIX + 'starting for [ID]-' + AID);
  LBal:=0;
  
  //nothing to complete if we aren't managing this order
  if not FOrderManager.Exists[AID] then
  begin
    LogWarning(PREFIX + 'ID not found in order manager');
    Exit;
  end;

  //balance the hold ledgers
  BalanceOrder(AID,lsHold);
  BalanceOrder(AID,lsInvHold);

  if ADetails.Size > 0 then
  begin
    //record entries to funds ledger
    if ADetails.OrderType = odBuy then
    begin
      LBal:=FFundsLedger.RecordEntry(
        ADetails.Price * ADetails.Size + ADetails.Fees,
        ltDebit,
        LID
      ).Balance;

      LogInfo(PREFIX + 'FundsLedger::buy side, new balance [balance]-' + FloatToStr(LBal));
    end
    else
    begin
      LBal:=FFundsLedger.RecordEntry(
        ADetails.Price * ADetails.Size - ADetails.Fees,
        ltCredit,
        LID
      ).Balance;

      LogInfo(PREFIX + 'FundsLedger::sell side, new balance [balance]-' + FloatToStr(LBal));
    end;
    
    //store the id
    StoreLedgerID(AID, LID, lsStd);
    LOldInv:=Inventory;
    
    LogInfo('[OldInventory]-' + FloatToStr(LOldInv));

    //record entries to inventory ledger
    if ADetails.OrderType = odBuy then
    begin
      LBal:=FInvLedger.RecordEntry(
        ADetails.Size,
        ltCredit,
        LID
      ).Balance;

      LogInfo(PREFIX + 'InvLedger::buy side, new balance [balance]-' + FloatToStr(LBal));
    end
    else
    begin
      LBal:=FInvLedger.RecordEntry(
        ADetails.Size,
        ltDebit,
        LID
      ).Balance;
      LogInfo(PREFIX + 'InvLedger::sell side, new balance [balance]-' + FloatToStr(LBal));
    end;
    StoreLedgerID(AID,LID,lsStdInv);

    //now update the average aquisition cost for our inventory
    if Inventory <= 0 then
      FAAC:=0
    (*
      only update the aquistion cost if the balance has increased
      and the price * size would be non-zero
    *)
    else if (LOldInv < Inventory) and (ADetails.Price > 0) then
    begin
      LogInfo('AACUpdate::[OldAAC]-' + FloatToStr(FAAC));
      
      //update the aac for buy orders by adding fees (the actual cost)
      if ADetails.OrderType = odBuy then
        FAAC := (FAAC * LOldInv + (ADetails.Price * ADetails.Size + ADetails.Fees)) / Inventory
      //sell orders we need to subtract the fees
      else
        FAAC := (FAAC * LOldInv + (ADetails.Price * ADetails.Size  - ADetails.Fees)) / Inventory;

      LogInfo('AACUpdate::[AAC]-' + FloatToStr(FAAC));
    end;
  end
  else
    LogWarning(PREFIX + 'size is zero or lower [size]-' + FloatToStr(ADetails.Size));

  //lastly remove this order from the manager since we no longer need it tracked
  FOrderManager.Delete(AID);
  
  LogInfo(PREFIX + 'finished for [ID]-' + AID);
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
    //before calling down to the strategies we need to refresh our manager
    //only if we are started
    if (EngineState = esStarted) and (not FOrderManager.Refresh(Error)) then
      Exit;

    //simply iterate strategies and attempt to feed, they are responsible
    //for the rest of the logic in our base class engine
    for I:=0 to Pred(FStrategies.Count) do
    begin
      //after refreshing, feed the next strategy ticker information
      if not FStrategies[I].Feed(
        ATicker,
        FOrderManager,
        AvailableFunds,
        AvailableInventory,
        FAAC,
        Error
      ) then
        Exit;
    end;

    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TDelilahImpl.Feed(const ATicker: ITicker; out Error: string): Boolean;
begin
  Result := DoFeed(ATicker,Error);
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
var
  I: Integer;
begin
  Result:=False;
  try
    //already stopped
    if FState=esStopped then
      Exit(true);
    Result:=DoStop(Error);

    //clear strategy data
    for I := 0 to Pred(FStrategies.Count) do
      FStrategies[I].Clear;

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
  FAAC:=0;
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
  FOnInfo:=nil;
  FOnError:=nil;
  FOnWarn:=nil;
end;

constructor TDelilahImpl.Create(const AOnInfo, AOnError,
  AOnWarn: TEngineLogEvent);
begin
  Create;
  FOnInfo:=AOnInfo;
  FOnError:=AOnError;
  FOnWarn:=AOnWarn;
end;

destructor TDelilahImpl.Destroy;
begin
  FStrategies.Free;
  FInVLedger:=nil;
  FHoldsLedger:=nil;
  FHoldsInvLedger:=nil;
  FFundsLedger:=nil;
  FOnInfo:=nil;
  FOnError:=nil;
  FOnWarn:=nil;
  FOrderLedger.Free;
  inherited Destroy;
end;

end.

