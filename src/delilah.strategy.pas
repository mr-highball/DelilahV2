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
  strict protected
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      Const AFunds,AInventory,AAAC:Extended;Out Error: String):Boolean;virtual;abstract;
    procedure LogInfo(Const AMessage:String);
    procedure LogError(Const AMessage:String);
    procedure LogWarning(Const AMessage:String);
  public
    function Feed(Const ATicker : ITicker;Const AManager:IOrderManager;
      Const AFunds,AInventory,AAAC:Extended;Out Error:String):Boolean;
    constructor Create;virtual;overload;
    constructor Create(Const AOnInfo,AOnError,AOnWarn:TStrategyLogEvent);
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

function TStrategyImpl.Feed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
begin
  LogInfo(
    Format(
      'about to feed strategy - [ticker]:%f [funds]:%f [inventory]:%f [aac]:%f',
      [
        ATicker.Price,
        AFunds,
        AInventory,
        AAAC
      ]
    )
  );
  Result:=DoFeed(ATicker,AManager,AFunds,AInventory,AAAC,Error);
  if not Result then
    LogError(Error);
end;

constructor TStrategyImpl.Create;
begin
  //nothing
end;

constructor TStrategyImpl.Create(const AOnInfo, AOnError,
  AOnWarn: TStrategyLogEvent);
begin
  FOnInfo:=AOnInfo;
  FOnError:=AOnError;
  FOnWarn:=AOnWarn;
  create;
end;

end.

