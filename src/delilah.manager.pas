unit delilah.manager;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, gdax.api.types, fgl;

type

  { TOrderManagerImpl }
  (*
    base implementation of IOrderManager with virtual accessors
  *)
  TOrderManagerImpl = class(TInterfacedObject,IOrderManager)
  public
    type
      TOrderDetails = TFPGMapInterfacedObjectData<String,IOrderDetails>;
  strict private
    FOnBeforePlace: TBeforeOrderPlaceEvent;
    FOnPlace: TOrderPlaceEvent;
    FOnRemove: TOrderRemoveEvent;
    FOnStatus: TOrderStatusEvent;
    FOrders: TOrderDetails;
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
    function DoGetStatus(Const AID: String): TOrderManagerStatus;virtual;abstract;
    procedure DoSetOnBeforePlace(Const AValue: TBeforeOrderPlaceEvent);virtual;abstract;
    procedure DoSetOnPlace(Const AValue: TOrderPlaceEvent);virtual;abstract;
    procedure DoSetOnRemove(Const AValue: TOrderRemoveEvent);virtual;abstract;
    procedure DoSetOnStatus(Const AValue: TOrderStatusEvent);virtual;abstract;
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
      Out Error):Boolean;overload;
    function Details(Const AID:String;Out Details:IOrderDetails):Boolean;overload;
    constructor Create;virtual;
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
begin
  //simple exist check first
  if not Exists[AID] then
    Exit(omCanceled);
  //call to child for status
  Result:=DoGetStatus(AID);
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

function TOrderManagerImpl.Place(const ADetails: IOrderDetails; out ID: String;
  out Error: String): Boolean;
begin
  Result:=False;
  //make sure the details is valid
  //...
  //notify on before place and see if we can continue
  //...
  //generate a guid and store the reference
  //...
  //call down to children to see if placing the order was successful
  //...
  //notify order place
  //...
end;

function TOrderManagerImpl.Place(const ADetails: IOrderDetails; out ID: String): Boolean;
var
  LError:String;
begin
  Result:=Place(ADetails,ID,LError);
end;

function TOrderManagerImpl.Cancel(const AID: String; out
  Details: IOrderDetails; out Error: String): Boolean;
begin
  Result:=False;
  //check for valid id
  //...
  //fetch the details
  //...
  //call to child for cancel
  //...
  //on success notify status change
  //...
end;

function TOrderManagerImpl.Cancel(const AID: String; out Details: IOrderDetails): Boolean;
var
  LError:String;
begin
  Result:=Cancel(AID,Details,LError);
end;

function TOrderManagerImpl.Delete(const AID: String; out Error: String): Boolean;
begin
  Result:=False;
  //check to see if order exists, if not return true
  //...
  //remove the order from the map
  //...
  //notify remove
  //...
end;

function TOrderManagerImpl.Delete(const AID: String): Boolean;
var
  LError:String;
begin
  Result:=Self.Delete(AID,LError);
end;

function TOrderManagerImpl.Details(Const AID:String;out Details: IOrderDetails;
  out Error): Boolean;
begin
  Result:=False;
  //see if exists
  //...
  //if so delete
  //...
end;

function TOrderManagerImpl.Details(Const AID:String;out Details: IOrderDetails): Boolean;
var
  LError:String;
begin
  Result:=Self.Details(AID,Details,LError);
end;

constructor TOrderManagerImpl.Create;
begin
  FOrders:=TOrderDetails.Create;
end;

destructor TOrderManagerImpl.Destroy;
begin
  FOrders.Free;
  inherited Destroy;
end;

end.

