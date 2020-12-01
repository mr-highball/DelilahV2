unit delilah.manager;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, gdax.api.types, fgl;

type

  TManagerLogEvent = procedure(Const AMessage:String) of object;

  { TOrderManagerImpl }
  (*
    base implementation of IOrderManager with virtual accessors
  *)
  TOrderManagerImpl = class(TInterfacedObject,IOrderManager)
  public
    type
      TOrderDetails = TFPGMapInterfacedObjectData<String,IOrderDetails>;
  strict private
    FOnInfo,
    FOnError,
    FOnWarn : TManagerLogEvent;
    FOnBeforePlace: TBeforeOrderPlaceEvent;
    FOnPlace: TOrderPlaceEvent;
    FOnRemove: TOrderRemoveEvent;
    FOnStatus: TOrderStatusEvent;
    FOrders: TOrderDetails;
  protected
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
  strict protected
    property Orders : TOrderDetails read FOrders;
    //children override these
    function DoGetStatus(
      Const ADetails: IOrderDetails): TOrderManagerStatus;virtual;abstract;
    function DoPlace(Const ADetails:IOrderDetails;
      Out Error:String):Boolean;virtual;abstract;
    function DoCancel(Const ADetails:IOrderDetails;
      Out Error:String):Boolean;virtual;abstract;

    //event helper methods
    procedure DoOnBeforePlace(Const ADetails:IOrderDetails;
      Var Allow:Boolean;Out ADisallowReason:String);
    procedure DoOnPlace(Const ADetails:IOrderDetails;Const AID:String);
    procedure DoOnRemove(Const ADetails:IOrderDetails;Const AID:String);
    procedure DoOnStatus(Const ADetails:IOrderDetails;Const AID:String;
      Const AOldStatus,ANewStatus:TOrderManagerStatus);
    function DoRefresh(Out Error:String):Boolean;virtual;abstract;

    //logging methods
    procedure LogInfo(Const AMessage:String);
    procedure LogError(Const AMessage:String);
    procedure LogWarning(Const AMessage:String);
  public
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
      Out Error:String):Boolean;overload;
    function Details(Const AID:String;Out Details:IOrderDetails):Boolean;overload;
    function ID(Const ADetails:IOrderDetails;Out ID:String):Boolean;
    function Refresh(Out Error:String):Boolean;
    procedure Clear;
    constructor Create;virtual;overload;
    constructor Create(Const AOnInfo,AOnError,AOnWarn:TManagerLogEvent);virtual;overload;
    destructor Destroy; override;
  end;

implementation

{ TOrderManagerImpl }

function TOrderManagerImpl.GetCount: Cardinal;
begin
  Result:=FOrders.Count;
end;

function TOrderManagerImpl.GetExists(const AID: String): Boolean;
begin
  if not FOrders.Sorted then
    FOrders.Sort;
  Result:=FOrders.IndexOf(AID)>=0;
end;

function TOrderManagerImpl.GetOnBeforePlace: TBeforeOrderPlaceEvent;
begin
  Result:=FOnBeforePlace;
end;

function TOrderManagerImpl.GetOnPlace: TOrderPlaceEvent;
begin
  Result:=FOnPlace;
end;

function TOrderManagerImpl.GetOnRemove: TOrderRemoveEvent;
begin
  Result:=FOnRemove;
end;

function TOrderManagerImpl.GetOnStatus: TOrderStatusEvent;
begin
  Result:=FOnStatus;
end;

function TOrderManagerImpl.GetStatus(const AID: String): TOrderManagerStatus;
var
  LDetails:IOrderDetails;
  LError:String;
begin
  Result:=omCanceled;

  //simple exist check first
  if not Exists[AID] then
    Exit(omCanceled);

  if not Details(AID,LDetails,LError) then
    raise Exception.Create(LError);

  //call to child for status
  Result:=DoGetStatus(LDetails);
end;

procedure TOrderManagerImpl.SetOnBeforePlace(
  const AValue: TBeforeOrderPlaceEvent);
begin
  FOnBeforePlace:=AValue;
end;

procedure TOrderManagerImpl.SetOnPlace(const AValue: TOrderPlaceEvent);
begin
  FOnPlace:=AValue;
end;

procedure TOrderManagerImpl.SetOnRemove(const AValue: TOrderRemoveEvent);
begin
  FOnRemove:=AValue;
end;

procedure TOrderManagerImpl.SetOnStatus(const AValue: TOrderStatusEvent);
begin
  FOnStatus:=AValue;
end;

procedure TOrderManagerImpl.DoOnBeforePlace(const ADetails: IOrderDetails;
  var Allow: Boolean; out ADisallowReason:String);
begin
  if Assigned(FOnBeforePlace) then
    FOnBeforePlace(ADetails,Allow,ADisallowReason)
end;

procedure TOrderManagerImpl.DoOnPlace(const ADetails: IOrderDetails;
  const AID: String);
begin
  if Assigned(FOnPlace) then
    FOnPlace(ADetails,AID);
end;

procedure TOrderManagerImpl.DoOnRemove(const ADetails: IOrderDetails;
  const AID: String);
begin
  if Assigned(FOnRemove) then
    FOnRemove(ADetails,AID);
end;

procedure TOrderManagerImpl.DoOnStatus(const ADetails: IOrderDetails;
  const AID: String; const AOldStatus, ANewStatus: TOrderManagerStatus);
begin
  if Assigned(FOnStatus) then
    FOnStatus(ADetails,AID,AOldStatus,ANewStatus);
end;

procedure TOrderManagerImpl.LogInfo(const AMessage: String);
begin
  if Assigned(FOnInfo) then
    FOnInfo(Self.Classname + '::' + AMessage);
end;

procedure TOrderManagerImpl.LogError(const AMessage: String);
begin
  if Assigned(FOnError) then
    FOnError(Self.Classname + '::' + AMessage);
end;

procedure TOrderManagerImpl.LogWarning(const AMessage: String);
begin
  if Assigned(FOnWarn) then
    FOnWarn(Self.Classname + '::' + AMessage);
end;

function TOrderManagerImpl.Place(const ADetails: IOrderDetails; out ID: String;
  out Error: String): Boolean;
var
  LAllow:Boolean;
begin
  Result:=False;
  try
    //make sure the details is valid
    if not Assigned(ADetails) then
      raise Exception.Create('details is unassigned');

    //notify on before place and see if we can continue
    LAllow:=True;
    DoOnBeforePlace(ADetails,LAllow,Error);

    if not LAllow then
      Exit;

    //call down to children to see if placing the order was successful
    if not DoPlace(ADetails,Error) then
    begin
      LogError('Place::an error occurrred [error]:' + Error);
      Exit;
    end;

    //generate a guid and store the reference
    ID:=TGuid.NewGuid.ToString();
    FOrders.Add(ID,ADetails);

    //notify order place
    DoOnPlace(ADetails,ID);

    //request status after a successful order place
    Status[ID];

    //success
    Result:=True;
    LogInfo('Place::Success');
  except on E:Exception do
  begin
    LogError('Place::' + E.Message);
    Error:=E.Message;
  end
  end;
end;

function TOrderManagerImpl.Place(const ADetails: IOrderDetails; out ID: String): Boolean;
var
  LError:String;
begin
  Result:=Place(ADetails,ID,LError);
end;

function TOrderManagerImpl.Cancel(const AID: String; out
  Details: IOrderDetails; out Error: String): Boolean;
var
  LOldStatus:TOrderManagerStatus;
begin
  Result:=False;
  try
    if not Self.Details(AID,Details,Error) then
      Exit;

    LOldStatus:=DoGetStatus(Details);

    //can only perform a cancel request on active orders
    if LOldStatus <> omActive then
    begin
      Error:='status of order id: ' + AID + ' is not omActive';
      Exit;
    end;

    //call to child for cancel
    if not DoCancel(Details,Error) then
      Exit;

    //success
    Result:=True;
  except on E:Exception do
  begin
    LogError('Cancel::' + E.Message);
    Error:=E.Message;
  end
  end;
end;

function TOrderManagerImpl.Cancel(const AID: String; out Details: IOrderDetails): Boolean;
var
  LError:String;
begin
  Result:=Cancel(AID,Details,LError);
end;

function TOrderManagerImpl.Delete(const AID: String; out Error: String): Boolean;
var
  I:Integer;
  LDetails:IOrderDetails;
begin
  Result:=False;
  try
    //check to see if order exists, if not return true
    if not Exists[AID] then
      Exit(True);
    //remove the order from the map
    if not FOrders.Sorted then
      FOrders.Sort;
    I:=FOrders.IndexOf(AID);
    LDetails:=FOrders.Data[I];
    FOrders.Delete(I);
    //notify remove
    DoOnRemove(LDetails,AID);
    //success
    Result:=True;
  except on E:Exception do
  begin
    LogError('Delete::' + E.Message);
    Error:=E.Message;
  end
  end;
end;

function TOrderManagerImpl.Delete(const AID: String): Boolean;
var
  LError:String;
begin
  Result:=Self.Delete(AID,LError);
end;

function TOrderManagerImpl.Details(const AID: String;
  out Details: IOrderDetails; out Error:String): Boolean;
begin
  Result:=False;
  try
    //see if exists
    if not Exists[AID] then
    begin
      Error:=AID + ' does not exist when calling Details';
      Exit;
    end;
    Details:=FOrders.Data[FOrders.IndexOf(AID)];
    //success
    Result:=True;
  except on E:Exception do
  begin
    LogError('Details::' + E.Message);
    Error:=E.Message;
  end
  end;
end;

function TOrderManagerImpl.Details(const AID: String;
  out Details: IOrderDetails): Boolean;
var
  LError:String;
begin
  Result:=Self.Details(AID,Details,LError);
end;

function TOrderManagerImpl.ID(const ADetails: IOrderDetails; out ID: String): Boolean;
var
  I:Integer;
begin
  Result:=False;
  I:=FOrders.IndexOfData(ADetails);
  if I<0 then
    Exit;
  ID:=FOrders.Keys[I];
  Result:=True;
end;

function TOrderManagerImpl.Refresh(out Error: String): Boolean;
begin
  Result:=False;
  try
    Result:=DoRefresh(Error);
  except on E:Exception do
    Error:=E.Message;
  end;
end;

procedure TOrderManagerImpl.Clear;
var
  I:Integer;
  LIDS:TArray<String>;
begin
  SetLength(LIDS,FOrders.Count);
  //get all of the ids
  for I:=0 to Pred(FOrders.Count) do
    LIDS[I]:=FOrders.Keys[I];
  //silently fail if delete fails
  for I:=0 to High(LIDS) do
    Delete(LIDS[I]);
end;

constructor TOrderManagerImpl.Create;
begin
  FOrders:=TOrderDetails.Create;
end;

constructor TOrderManagerImpl.Create(const AOnInfo, AOnError,
  AOnWarn: TManagerLogEvent);
begin
  Create;
  FOnInfo:=AOnInfo;
  FOnError:=AOnError;
  FOnWarn:=AOnWarn;
end;

destructor TOrderManagerImpl.Destroy;
begin
  FOrders.Free;
  inherited Destroy;
end;

end.

