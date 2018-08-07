unit delilah.strategy.gdax;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, gdax.api.types, gdax.api.consts,
  delilah.strategy;

type

  { IStrategyGDAX }

  IStrategyGDAX = interface(IStrategy)
    ['{05FA98EC-1EC2-4FDF-A23E-9B4536E5C7B4}']
  end;

  { TStrategyGDAXImpl }

  TStrategyGDAXImpl = class(TStrategyImpl,IStrategyGDAX)
  strict private
  strict protected
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      Const AFunds,AInventory,AAAC:Extended;Out Error: String): Boolean; override;
  public
  end;

implementation
uses
  delilah.ticker.gdax;

{ TStrategyGDAXImpl }

function TStrategyGDAXImpl.DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
  Const AFunds,AInventory,AAAC:Extended;Out Error: String): Boolean;
begin
  Result:=False;
  //make sure we actually have a valid ticker refernce
  if not Assigned(ATicker) then
  begin
    Error:='ticker is unassigned';
    Exit;
  end;
  //check for supported ticker types
  if not (ATicker is ITickerGDAX) then
  begin
    Error:='only ITickerGDAX is supported';
    Exit;
  end;
  Result:=True;
end;

end.

