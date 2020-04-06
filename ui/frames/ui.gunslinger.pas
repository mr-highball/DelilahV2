unit ui.Gunslinger;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls, StdCtrls, PairSplitter, ExtCtrls,
  delilah.types;

type

  { TGunslinger }

  TGunslinger = class(TFrame)
    btn_sell: TButton;
    btn_buy: TButton;
    grp_sell: TGroupBox;
    grp_buy: TGroupBox;
    img_hat: TImage;
    img_hat1: TImage;
    pnl_info: TPanel;
    scroll_buy: TScrollBox;
    scroll_sell: TScrollBox;
    split_main: TSplitter;
    procedure btn_buyClick(Sender: TObject);
    procedure btn_sellClick(Sender: TObject);
  strict private
    FManager : IOrderManager;
    FBuyFrame,
    FSellFrame : TFrame;
    function GetManager: IOrderManager;
    procedure SetManager(const AValue: IOrderManager);
  strict protected
    (*
      children override to provide controls
    *)
    function DoGetBuyFrame : TFrame; virtual; abstract;

    (*
      children override to provide controls
    *)
    function DoGetSellFrame : TFrame; virtual; abstract;

    (*
      children override to place a buy order with the manager
    *)
    procedure DoPlaceBuy(const AManager : IOrderManager; const AFrame : TFrame); virtual; abstract;

    (*
      children override to place a buy order with the manager
    *)
    procedure DoPlaceSell(const AManager : IOrderManager; const AFrame : TFrame); virtual; abstract;
  public
    property OrderManager : IOrderManager read GetManager write SetManager;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TGunslinger }

procedure TGunslinger.btn_buyClick(Sender: TObject);
begin
  DoPlaceBuy(FManager, FBuyFrame);
end;

procedure TGunslinger.btn_sellClick(Sender: TObject);
begin
  DoPlaceSell(FManager, FSellFrame);
end;

function TGunslinger.GetManager: IOrderManager;
begin
  Result := FManager;
end;

procedure TGunslinger.SetManager(const AValue: IOrderManager);
begin
  FManager := AValue;
end;

constructor TGunslinger.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FManager := nil;
  FBuyFrame := DoGetBuyFrame;
  FBuyFrame.Parent := scroll_buy;
  FBuyFrame.Align := TAlign.alClient;

  FSellFrame := DoGetSellFrame;
  FSellFrame.Parent := scroll_sell;
  FSellFrame.Align := TAlign.alClient;
end;

destructor TGunslinger.Destroy;
begin
  FManager := nil;
  FBuyFrame.Free;
  FSellFrame.Free;
  inherited Destroy;
end;

end.

