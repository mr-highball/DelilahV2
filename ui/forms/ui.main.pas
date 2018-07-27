unit ui.main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries,
  Forms, Controls, Graphics, Dialogs, JSONPropStorage, ExtCtrls, ComCtrls,
  StdCtrls, ui.ignition, ui.authenticator, ui.usercontrol.multiline,
  ui.usercontrol.products, ui.usercontrol;

type

  { TMain }

  TMain = class(TForm)
    chart_source: TListChartSource;
    chart_ticker: TChart;
    chart_tickerLineSeries1: TLineSeries;
    ignition_main: TIgnition;
    icons: TImageList;
    json_main: TJSONPropStorage;
    memo_licenses: TMemo;
    multi_log: TMultiLine;
    pctrl_main: TPageControl;
    scroll_strategy: TScrollBox;
    ts_product: TTabSheet;
    ts_about: TTabSheet;
    ts_chart: TTabSheet;
    ts_log: TTabSheet;
    ts_strategy: TTabSheet;
    ts_auth: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure json_mainRestoringProperties(Sender: TObject);
    procedure json_mainSavingProperties(Sender: TObject);
    procedure pctrl_mainChange(Sender: TObject);
  private
    FAuth : TAuthenticator;
    FProducts : TProducts;
    FProductInit : Boolean;
    procedure InitControls;
  public

  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

procedure TMain.json_mainRestoringProperties(Sender: TObject);
begin
  //todo
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  InitControls;
end;


procedure TMain.json_mainSavingProperties(Sender: TObject);
begin
  //todo
end;

procedure TMain.pctrl_mainChange(Sender: TObject);
begin
  if pctrl_main.ActivePage=ts_product then
    if not FProductInit then
    begin
      MessageDlg(
        'Products',
        'please wait while we fetch products from GDAX...',
        TMsgDlgType.mtInformation,
        [TMsgDlgBtn.mbOK],
        ''
      );
      FProducts.ProductFrame.Init;
      FProductInit:=True;
    end;
end;

procedure TMain.InitControls;
begin
  //logger
  multi_log.Options:=multi_log.Options - [ucAuthor];
  multi_log.Title:='Strategy Logger';
  multi_log.Description:='logging for the strategy';
  //authenticator
  FAuth:=TAuthenticator.Create(Self);
  FAuth.Parent:=ts_auth;
  FAuth.AnchorHorizontalCenterTo(ts_auth);
  FAuth.AnchorVerticalCenterTo(ts_auth);
  //products
  FProducts:=TProducts.Create(Self);
  FProducts.Parent:=ts_product;
  FProducts.Align:=TAlign.alClient;
  FProducts.ProductFrame.Authenticator:=FAuth.Authenticator;
  FProductInit:=False;
  //main tab
  pctrl_main.ActivePage:=ts_auth;
end;


end.

