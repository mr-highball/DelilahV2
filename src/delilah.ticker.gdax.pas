unit delilah.ticker.gdax;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, delilah.ticker, gdax.api.types,
  gdax.api.consts;

type

  { ITickerGDAX }
  (*
    an adapter for a IGDAXTicker
  *)
  ITickerGDAX = interface(ITicker)
    ['{D06C9B75-812E-49A0-B34A-30E229D1450F}']
    function GetTicker: IGDAXTicker;
    procedure SetTicker(Const AValue: IGDAXTicker);
    //properties
    property Ticker : IGDAXTicker read GetTicker write SetTicker;
  end;

  { TGDAXTickerImpl }
  (*
    implementation for the ITickerGDAX adapter
  *)
  TGDAXTickerImpl = class(TTickerImpl,ITickerGDAX)
  strict private
    FTicker: IGDAXTicker;
    function GetTicker: IGDAXTicker;
    procedure SetTicker(Const AValue: IGDAXTicker);
  strict protected
    function DoGetPrice: Extended; override;
    function DoGetTime: TDateTime; override;
    procedure DoSetPrice(const AValue: Extended); override;
    procedure DoSetTime(const AValue: TDateTime); override;
  public
    property Ticker : IGDAXTicker read GetTicker write SetTicker;
    constructor Create(Const ATicker:IGDAXTicker);override;
    destructor Destroy; override;
  end;

implementation

{ TGDAXTickerImpl }

function TGDAXTickerImpl.GetTicker: IGDAXTicker;
begin
  Result:=FTicker;
end;

procedure TGDAXTickerImpl.SetTicker(const AValue: IGDAXTicker);
begin
  FTicker:=nil;
  FTicker:=AValue;
end;

function TGDAXTickerImpl.DoGetPrice: Extended;
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX ticker not assigned in ' + Self.Classname);
  Result:=FTicker.Price;
end;

function TGDAXTickerImpl.DoGetTime: TDateTime;
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX ticker not assigned in ' + Self.Classname);
  Result:=FTicker.Time;
end;

procedure TGDAXTickerImpl.DoSetPrice(const AValue: Extended);
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX ticker not assigned in ' + Self.Classname);
  FTicker.Price:=AValue;
end;

procedure TGDAXTickerImpl.DoSetTime(const AValue: TDateTime);
begin
  if not Assigned(FOrder) then
    raise Exception.Create('GDAX ticker not assigned in ' + Self.Classname);
  FTicker.Time:=AValue;
end;

constructor TGDAXTickerImpl.Create(const ATicker: IGDAXTicker);
begin
  inherited Create;
  FTicker:=ATicker;
end;

destructor TGDAXTickerImpl.Destroy;
begin
  FTicker:=nil;
  inherited Destroy;
end;

end.

