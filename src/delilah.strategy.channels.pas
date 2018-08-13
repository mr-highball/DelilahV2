unit delilah.strategy.channels;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.strategy, delilah.types, delilah.strategy.window,
  fgl;

type

  //forward
  IChannelStrategy = interface;
  IChannel = interface;

  TChannelDirection = (
    cdEnter,
    cdExit
  );

  TChannelEvent = procedure(Const ASender:IChannel;
    Const ADirection:TChannelDirection) of object;

  { IChannel }
  (*
    an abstraction for a defined price range based on an anchor price
    and deviations for an upper and lower limit
  *)
  IChannel = interface
    ['{7E2B54FC-43AE-48B6-B12B-584959816EA6}']
    //property methods
    function GetAnchor: Single;
    function GetLower: Single;
    function GetLowerStd: Single;
    function GetName: String;
    function GetOnLower: TChannelEvent;
    function GetOnUpper: TChannelEvent;
    function GetParent: IChannelStrategy;
    function GetStd: Single;
    function GetUpper: Single;
    function GetUpperStd: Single;
    procedure SetAnchor(Const AValue: Single);
    procedure SetLowerStd(Const AValue: Single);
    procedure SetName(Const AValue: String);
    procedure SetOnLower(Const AValue: TChannelEvent);
    procedure SetOnUpper(Const AValue: TChannelEvent);
    procedure SetParent(AValue: IChannelStrategy);
    procedure SetStd(Const AValue: Single);
    procedure SetUpperStd(Const AValue: Single);

    //events
    property OnLower : TChannelEvent read GetOnLower write SetOnLower;
    property OnUpper : TChannelEvent read GetOnUpper write SetOnUpper;

    //properties
    (*
      optional name of the channel
    *)
    property Name : String read GetName write SetName;
    (*
      upper bound of the channel in standard deviations
    *)
    property UpperStdDev : Single read GetUpperStd write SetUpperStd;
    (*
      lower bound of the channel in standard deviations
    *)
    property LowerStdDev : Single read GetLowerStd write SetLowerStd;
    (*
      current price to base calculations off of
    *)
    property AnchorPrice : Single read GetAnchor write SetAnchor;
    (*
      current standard deviation used in calculations
    *)
    property StdDev : Single read GetStd write SetStd;
    (*
      hard price of upper channel
    *)
    property Upper : Single read GetUpper;
    (*
      hard price of lower channel
    *)
    property Lower : Single read GetLower;
    (*
      parent strategy for the channel
    *)
    property Parent : IChannelStrategy read GetParent write SetParent;

    //methods
    (*
      quick method for updating properties
    *)
    function Update(Const AStd,AAnchor,AUpperStd,ALowerStd : Single):IChannel;overload;
    function Update(Const AStd,AAnchor : Single):IChannel;overload;
    (*
      method called to see if price has entered or exited this channel
    *)
    function CheckPrice(Const APrice : Single):IChannel;
    (*
      unsets parent and returns self
    *)
    function Orphan:IChannel;
  end;

  (*
    list of channels
  *)
  TChannels = TFPGInterfacedObjectList<IChannel>;

  (*
    a map of channels where the name is the key
  *)
  TChannelMap = TFPGMapInterfacedObjectData<String,IChannel>;

  { IChannelStrategy }
  (*
    IChannelStraegy uses information provided from its window to setup
    "channels" which can have certain actions taken on entering and leaving
  *)
  IChannelStrategy = interface(IWindowStrategy)
    ['{427C0FB3-0941-4C26-B8F1-E9C0980C2E93}']
    //property methods
    function GetByIndex(const I: Cardinal): IChannel;
    function GetByName(const AName: String): IChannel;
    function GetCount: Cardinal;

    //properties
    property ByName[Const AName:String] : IChannel read GetByName;default;
    property ByIndex[Const I:Cardinal] : IChannel read GetByIndex;
    property Count : Cardinal read GetCount;

    //methods
    function Add(Const AName:String;Const AUpperStdDev,ALowerStdDev:Single):IChannelStrategy;
    function IndexOf(Const AName:String;Out Index:Cardinal):IChannelStrategy;
    function Remove(Const AName:String):IChannelStrategy;overload;
    function Remove(Const AIndex:Cardinal):IChannelStrategy;overload;
  end;

  { TChannelImpl }
  (*
    base implementation for a channel
  *)
  TChannelImpl = class(TInterfacedObject,IChannel)
  strict private
    FName: String;
    FOnUpper,
    FOnLower: TChannelEvent;
    FAnchor,
    FLowerStd,
    FUpperStd,
    FLastPrice,
    FStd: Single;
    FParent: IChannelStrategy;
    function GetAnchor: Single;
    function GetLower: Single;
    function GetLowerStd: Single;
    function GetName: String;
    function GetOnLower: TChannelEvent;
    function GetOnUpper: TChannelEvent;
    function GetParent: IChannelStrategy;
    function GetStd: Single;
    function GetUpper: Single;
    function GetUpperStd: Single;
    procedure SetAnchor(Const AValue: Single);
    procedure SetLowerStd(Const AValue: Single);
    procedure SetName(Const AValue: String);
    procedure SetOnLower(Const AValue: TChannelEvent);
    procedure SetOnUpper(Const AValue: TChannelEvent);
    procedure SetParent(AValue: IChannelStrategy);
    procedure SetStd(Const AValue: Single);
    procedure SetUpperStd(Const AValue: Single);
    procedure DoOnUpper(Const ASender:IChannel;Const ADirection:TChannelDirection);
    procedure DoOnLower(Const ASender:IChannel;Const ADirection:TChannelDirection);
  strict protected
    procedure DoBeforeCheckPrice(Const APrice:Single);virtual;
    procedure DoAfterCheckPrice(Const APrice:Single;Const ATriggers:Boolean;
      Const ADirection:TChannelDirection);virtual;
  public
    //events
    property OnLower : TChannelEvent read GetOnLower write SetOnLower;
    property OnUpper : TChannelEvent read GetOnUpper write SetOnUpper;
  public
    //properties
    property Name : String read GetName write SetName;
    property UpperStdDev : Single read GetUpperStd write SetUpperStd;
    property LowerStdDev : Single read GetLowerStd write SetLowerStd;
    property AnchorPrice : Single read GetAnchor write SetAnchor;
    property StdDev : Single read GetStd write SetStd;
    property Upper : Single read GetUpper;
    property Lower : Single read GetLower;
    property Parent : IChannelStrategy read GetParent write SetParent;

    //methods
    function Update(Const AStd,AAnchor,AUpperStd,ALowerStd : Single):IChannel;overload;
    function Update(Const AStd,AAnchor : Single):IChannel;overload;
    function CheckPrice(Const APrice : Single):IChannel;
    function Orphan:IChannel;
  end;

  (*
    meta class for channel implementations
  *)
  TChannelImplClass = class of TChannelImpl;

implementation

{ TChannelImpl }

function TChannelImpl.GetAnchor: Single;
begin
  Result:=FAnchor;
end;

function TChannelImpl.GetLower: Single;
begin
  Result:=FAnchor - (FLowerStd * FStd);
end;

function TChannelImpl.GetLowerStd: Single;
begin
  Result:=FLowerStd;
end;

function TChannelImpl.GetName: String;
begin
  Result:=FName;
end;

function TChannelImpl.GetOnLower: TChannelEvent;
begin
  Result:=FOnLower;
end;

function TChannelImpl.GetOnUpper: TChannelEvent;
begin
  Result:=FOnUpper;
end;

function TChannelImpl.GetParent: IChannelStrategy;
begin
  Result:=FParent;
end;

function TChannelImpl.GetStd: Single;
begin
  Result:=FStd;
end;

function TChannelImpl.GetUpper: Single;
begin
  Result:=FAnchor + (FUpperStd * FStd);
end;

function TChannelImpl.GetUpperStd: Single;
begin
  Result:=FUpperStd;
end;

procedure TChannelImpl.SetAnchor(const AValue: Single);
begin
  FAnchor:=AValue;
end;

procedure TChannelImpl.SetLowerStd(const AValue: Single);
begin
  FLowerStd:=AValue;
end;

procedure TChannelImpl.SetName(const AValue: String);
begin
  if Assigned(FParent) then
    raise Exception.Create('assigned to parent, cannot change name');
  FName:=AValue;
end;

procedure TChannelImpl.SetOnLower(const AValue: TChannelEvent);
begin
  FOnLower:=AValue;
end;

procedure TChannelImpl.SetOnUpper(const AValue: TChannelEvent);
begin
  FOnUpper:=AValue;
end;

procedure TChannelImpl.SetParent(AValue: IChannelStrategy);
begin
  if Assigned(FParent) then
    FParent.Remove(FName);
  FParent:=nil;
  FParent:=AValue;
end;

procedure TChannelImpl.SetStd(const AValue: Single);
begin
  FStd:=AValue;
end;

procedure TChannelImpl.SetUpperStd(const AValue: Single);
begin
  FUpperStd:=AValue;
end;

procedure TChannelImpl.DoOnUpper(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  if Assigned(FOnUpper) then
    FOnUpper(ASender,ADirection);
end;

procedure TChannelImpl.DoOnLower(const ASender: IChannel;
  const ADirection: TChannelDirection);
begin
  if Assigned(FOnLower) then
    FOnLower(ASender,ADirection);
end;

procedure TChannelImpl.DoBeforeCheckPrice(const APrice: Single);
begin
  //nothing in base
end;

procedure TChannelImpl.DoAfterCheckPrice(const APrice: Single;
  const ATriggers: Boolean; const ADirection: TChannelDirection);
begin
  //nothing in base
end;

function TChannelImpl.Update(const AStd, AAnchor, AUpperStd, ALowerStd: Single):IChannel;
begin
  Result:=nil;
  StdDev:=AStd;
  AnchorPrice:=AAnchor;
  UpperStdDev:=AUpperStd;
  LowerStdDev:=ALowerStd;
  Result:=Self as IChannel;
end;

function TChannelImpl.Update(const AStd, AAnchor: Single):IChannel;
begin
  Result:=Update(AStd,AAnchor,UpperStdDev,LowerStdDev);
end;

function TChannelImpl.CheckPrice(const APrice: Single):IChannel;
begin
  //init result
  Result:=Self as IChannel;

  //on the first check price, this will be zero, so go ahead and assign
  if FLastPrice <= 0 then
    FLastPrice:=APrice;

  //call to virtual "before" method
  DoBeforeCheckPrice(APrice);

  //check for price entering via upper channel
  if (FLastPrice > Upper) and (APrice < Upper) then
    DoOnUpper(Result,cdEnter)
  //otherwise check for an exit
  else if (FLastPrice < Upper) and (APrice > Upper) then
    DoOnUpper(Result,cdExit);

  //check for price entering via lower channel (reverse from upper)
  if (FLastPrice < Lower) and (APrice > Lower) then
    DoOnLower(Result,cdEnter)
  //otherwise check for an exit
  else if (FLastPrice > Lower) and (APrice < Lower) then
    DoOnLower(Result,cdExit);

  //call to virtual "after" method
  DoAfterCheckPrice(APrice,False,cdEnter);

  //set the last price to this price
  FLastPrice:=APrice;
end;

function TChannelImpl.Orphan: IChannel;
begin
  Parent:=nil;
  Result:=Self as IChannel;
end;

end.

