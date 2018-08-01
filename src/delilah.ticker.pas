unit delilah.ticker;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types;

type

  { TTickerImpl }
  (*
    base ticker implementation with virtual accessors
  *)
  TTickerImpl = class(TInterfacedObject,ITicker)
  strict private
    function GetPrice: Extended;
    function GetTime: TDateTime;
    procedure SetPrice(Const AValue: Extended);
    procedure SetTime(Const AValue: TDateTime);
  strict protected
    function DoGetPrice: Extended;virtual;abstract;
    function DoGetTime: TDateTime;virtual;abstract;
    procedure DoSetPrice(Const AValue: Extended);virtual;abstract;
    procedure DoSetTime(Const AValue: TDateTime);virtual;abstract;
  public
    property Price : Extended read GetPrice write SetPrice;
    property Time : TDateTime read GetTime write SetTime;
    constructor Create;virtual;overload;
  end;

implementation

{ TTickerImpl }

function TTickerImpl.GetPrice: Extended;
begin
  Result:=DoGetPrice;
end;

function TTickerImpl.GetTime: TDateTime;
begin
  Result:=DoGetTime;
end;

procedure TTickerImpl.SetPrice(const AValue: Extended);
begin
  DoSetPrice(AValue);
end;

procedure TTickerImpl.SetTime(const AValue: TDateTime);
begin
  DoSetTime(AValue);
end;

constructor TTickerImpl.Create;
begin
  //nothing
end;

end.

