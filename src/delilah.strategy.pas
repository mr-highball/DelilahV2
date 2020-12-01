unit delilah.strategy;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types;

type

  TStrategyLogEvent = procedure(Const AMessage:String) of object;

  { TStrategyImpl }
  (*
    base implementation of IStrategy
  *)
  TStrategyImpl = class(TInterfacedObject,IStrategy)
  strict private
    FOnInfo,
    FOnError,
    FOnWarn : TStrategyLogEvent;
  protected
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      Const AFunds,AInventory,AAAC:Extended;Out Error: String):Boolean;virtual;abstract;
    procedure LogInfo(Const AMessage:String);
    procedure LogError(Const AMessage:String);
    procedure LogWarning(Const AMessage:String);
    procedure DoClear;virtual;
  public
    function Feed(Const ATicker : ITicker;Const AManager:IOrderManager;
      Const AFunds,AInventory,AAAC:Extended;Out Error:String):Boolean;
    procedure Clear;
    constructor Create;virtual;overload;
    constructor Create(Const AOnInfo,AOnError,AOnWarn:TStrategyLogEvent);virtual;overload;
  end;

implementation

{ TStrategyImpl }

procedure TStrategyImpl.LogInfo(const AMessage: String);
begin
  if Assigned(FOnInfo) then
    FOnInfo(Self.Classname + '::' + AMessage);
end;

procedure TStrategyImpl.LogError(const AMessage: String);
begin
  if Assigned(FOnError) then
    FOnError(Self.Classname + '::' + AMessage);
end;

procedure TStrategyImpl.LogWarning(const AMessage: String);
begin
  if Assigned(FOnWarn) then
    FOnWarn(Self.Classname + '::' + AMessage);
end;

procedure TStrategyImpl.DoClear;
begin
  //nothing in base
end;

function TStrategyImpl.Feed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
begin
  LogInfo(
    Format(
      'about to feed strategy - [ticker]:%s [funds]:%s [inventory]:%s [aac]:%s',
      [
        FloatToStr(ATicker.Price),
        FloatToStr(AFunds),
        FloatToStr(AInventory),
        FloatToStr(AAAC)
      ]
    )
  );
  Result:=DoFeed(ATicker,AManager,AFunds,AInventory,AAAC,Error);
  if not Result then
    LogError(Error);
end;

procedure TStrategyImpl.Clear;
begin
  DoClear;
end;

constructor TStrategyImpl.Create;
begin
  FOnError:=nil;
  FOnInfo:=nil;
  FOnWarn:=nil;
end;

constructor TStrategyImpl.Create(const AOnInfo, AOnError,
  AOnWarn: TStrategyLogEvent);
begin
  Create;
  FOnInfo:=AOnInfo;
  FOnError:=AOnError;
  FOnWarn:=AOnWarn;
end;

end.

