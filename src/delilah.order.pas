unit delilah.order;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, ledger;

type

  { TOrderDetailsImpl }
  (*
    base implementation of IOrderDetails with virtual accessors
  *)
  TOrderDetailsImpl = class(TInterfacedObject,IOrderDetails)
  strict private
    function GetPrice: Extended;
    function GetSize: Extended;
    function GetType: TLedgerType;
    procedure SetPrice(Const AValue: Extended);
    procedure SetSize(Const AValue: Extended);
    procedure SetType(Const AValue: TLedgerType);
  strict protected
    function DoGetPrice: Extended;virtual;abstract;
    function DoGetSize: Extended;virtual;abstract;
    function DoGetType: TLedgerType;virtual;abstract;
    procedure DoSetPrice(Const AValue: Extended);virtual;abstract;
    procedure DoSetSize(Const AValue: Extended);virtual;abstract;
    procedure DoSetType(Const AValue: TLedgerType);virtual;abstract;
  public
    property Size : Extended read GetSize write SetSize;
    property Price : Extended read GetPrice write SetPrice;
    property LedgerType : TLedgerType read GetType write SetType;
    constructor Create;virtual;overload;
  end;

implementation

{ TOrderDetailsImpl }

function TOrderDetailsImpl.GetPrice: Extended;
begin
  Result:=DoGetPrice;
end;

function TOrderDetailsImpl.GetSize: Extended;
begin
  Result:=DoGetSize;
end;

function TOrderDetailsImpl.GetType: TLedgerType;
begin
  Result:=DoGetType;
end;

procedure TOrderDetailsImpl.SetPrice(const AValue: Extended);
begin
  DoSetPrice(AValue);
end;

procedure TOrderDetailsImpl.SetSize(const AValue: Extended);
begin
  DoSetSize(AValue);
end;

procedure TOrderDetailsImpl.SetType(const AValue: TLedgerType);
begin
  DoSetType(AValue);
end;

constructor TOrderDetailsImpl.Create;
begin
  //nothing in base
end;

end.

