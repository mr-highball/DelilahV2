unit delilah.strategy;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types;

type

  { TStrategyImpl }
  (*
    base implementation of IStrategy
  *)
  TStrategyImpl = class(TInterfacedObject,IStrategy)
  strict private
  strict protected
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      Const AFunds,AInventory:Extended;Out Error: String):Boolean;virtual;abstract;
  public
    function Feed(Const ATicker : ITicker;Const AManager:IOrderManager;
      Const AFunds,AInventory:Extended;Out Error:String):Boolean;
    constructor Create;virtual;overload;
  end;

implementation

{ TStrategyImpl }

function TStrategyImpl.Feed(const ATicker: ITicker;
  const AManager: IOrderManager; Const AFunds,AInventory:Extended;
  out Error: String): Boolean;
begin
  Result:=DoFeed(ATicker,AManager,AFunds,AInventory,Error);
end;

constructor TStrategyImpl.Create;
begin
  //nothing
end;

end.

