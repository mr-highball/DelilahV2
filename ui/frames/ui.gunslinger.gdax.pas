unit ui.gunslinger.gdax;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  delilah.types,
  ui.Gunslinger;

type

  { TGDAXGunslinger }

  TGDAXGunslinger = class(TGunslinger)
  private
  strict protected
    function DoGetBuyFrame: TFrame; override;
    function DoGetSellFrame: TFrame; override;
    procedure DoPlaceBuy(const AManager: IOrderManager; const AFrame : TFrame); override;
    procedure DoPlaceSell(const AManager: IOrderManager; const AFrame : TFrame); override;
  public

  end;

implementation
uses
  ui.gunslinger.gdax.order;

{$R *.lfm}

{ TGDAXGunslinger }

function TGDAXGunslinger.DoGetBuyFrame: TFrame;
begin
  Result := TGDAXSlingerOrder.Create(nil);
end;

function TGDAXGunslinger.DoGetSellFrame: TFrame;
begin
  Result := TGDAXSlingerOrder.Create(nil);
end;

procedure TGDAXGunslinger.DoPlaceBuy(const AManager: IOrderManager;
  const AFrame : TFrame);
var
  LFrame : TGDAXSlingerOrder;
begin
  LFrame := TGDAXSlingerOrder(AFrame);
end;

procedure TGDAXGunslinger.DoPlaceSell(const AManager: IOrderManager;
  const AFrame : TFrame);
var
  LFrame : TGDAXSlingerOrder;
begin
  LFrame := TGDAXSlingerOrder(AFrame);
end;

end.

