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
  private
    FAuth : TAuthenticator;
    FProducts : TProducts;
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
  //main tab
  pctrl_main.ActivePage:=ts_auth;
end;


end.

