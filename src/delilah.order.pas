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
    function GetFees: Extended;
    function GetPrice: Extended;
    function GetSize: Extended;
    function GetType: TOrderDetailsType;
    procedure SetPrice(Const AValue: Extended);
    procedure SetSize(Const AValue: Extended);
    procedure SetType(Const AValue: TOrderDetailsType);
    procedure SetFees(Const AValue: Extended);
  strict protected
    function DoGetPrice: Extended;virtual;abstract;
    function DoGetFees: Extended;virtual;abstract;
    function DoGetSize: Extended;virtual;abstract;
    function DoGetType: TOrderDetailsType;virtual;abstract;
    procedure DoSetPrice(Const AValue: Extended);virtual;abstract;
    procedure DoSetFees(Const AValue: Extended);virtual;abstract;
    procedure DoSetSize(Const AValue: Extended);virtual;abstract;
    procedure DoSetType(Const AValue: TOrderDetailsType);virtual;abstract;
  public
    property Size : Extended read GetSize write SetSize;
    property Price : Extended read GetPrice write SetPrice;
    property Fees : Extended read GetFees write SetFees;
    property OrderType : TOrderDetailsType read GetType write SetType;
    constructor Create;virtual;overload;
  end;

implementation

{ TOrderDetailsImpl }

function TOrderDetailsImpl.GetFees: Extended;
begin
  Result := DoGetFees;
end;

function TOrderDetailsImpl.GetPrice: Extended;
begin
  Result:=DoGetPrice;
end;

function TOrderDetailsImpl.GetSize: Extended;
begin
  Result:=DoGetSize;
end;

function TOrderDetailsImpl.GetType: TOrderDetailsType;
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

procedure TOrderDetailsImpl.SetType(const AValue: TOrderDetailsType);
begin
  DoSetType(AValue);
end;

procedure TOrderDetailsImpl.SetFees(const AValue: Extended);
begin
  DoSetFees(AValue)
end;

constructor TOrderDetailsImpl.Create;
begin
  //nothing in base
end;

end.

