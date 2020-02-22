unit main;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Buttons,
  fgl,
  gdax.api.ticker,
  gdax.api.types,
  delilah.ticker,
  mock.manager,
  delilah.types,
  delilah;

type

  { TTickerLoader }
  //todo - this was stupid to do, api already has an exposed load method
  //       so whenever cleaning this up comes up around, just remove this type
  TTickerLoader = class(TGDAXTickerImpl)
  private
    FJSON : String;
    function GetJSON: String;
  public
    property JSON : String read GetJSON;
    procedure Load(const AJSON : String);
  end;

  TTickerList = TFPGInterfacedObjectList<TTickerLoader>;

  { TTickerParser }
  (*
    simple utility form to load ticker save files output by SimpleBot
    into spreadsheet readable formats
  *)
  TTickerParser = class(TForm)
    btn_load: TButton;
    btn_open_picker_csv: TSpeedButton;
    btn_save_csv: TButton;
    btn_save_simulate: TButton;
    edit_funds: TEdit;
    edit_directory: TEdit;
    edit_directory_csv: TEdit;
    edit_fee_perc: TEdit;
    pctrl_main: TPageControl;
    pnl_ctrls: TPanel;
    btn_open_picker: TSpeedButton;
    dialog_directory: TSelectDirectoryDialog;
    pnl_ctrls1: TPanel;
    pnl_ctrls_simulate: TPanel;
    progress_simulate: TProgressBar;
    Simulate: TTabSheet;
    ts_csv: TTabSheet;
    ts_load: TTabSheet;
    procedure btn_loadClick(Sender: TObject);
    procedure btn_open_pickerClick(Sender: TObject);
    procedure btn_open_picker_csvClick(Sender: TObject);
    procedure btn_save_csvClick(Sender: TObject);
    procedure btn_save_simulateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    FTickers : TTickerList;

    function GetStrategy : IStrategy;
  strict protected
    procedure SaveCSV;
    procedure SimulateStrategy;

    procedure PickFiles;
    procedure LoadFiles(const AFiles : TStrings);
  public
  end;

var
  TickerParser: TTickerParser;

implementation
uses
  FileUtil,
  delilah.ticker.gdax,
  delilah.strategy.gdax.sample,
  gdax.api.products;

{$R *.lfm}

{ TTickerParser }

procedure TTickerParser.btn_open_pickerClick(Sender: TObject);
begin
  if dialog_directory.Execute then
  begin
    edit_directory.Text := dialog_directory.FileName;
    edit_directory_csv.Text := edit_directory.Text + '\output.csv';
  end;
end;

procedure TTickerParser.btn_open_picker_csvClick(Sender: TObject);
begin
  if dialog_directory.Execute then
    edit_directory_csv.Text := dialog_directory.FileName;
end;

procedure TTickerParser.btn_save_csvClick(Sender: TObject);
begin
  SaveCSV;
end;

procedure TTickerParser.btn_save_simulateClick(Sender: TObject);
begin
  SimulateStrategy;
end;

procedure TTickerParser.FormCreate(Sender: TObject);
begin
  FTickers := TTickerList.Create;
  pctrl_main.ActivePage := ts_load;
end;

procedure TTickerParser.FormDestroy(Sender: TObject);
begin
  FTickers.Free;
end;

function TTickerParser.GetStrategy: IStrategy;

  function GetSample : IStrategy;
  var
    LSetup : ISampleGDAX;
  begin
    LSetup := TSampleGDAXImpl.Create;
    LSetup.WindowSizeInMilli := 5 * 60 * 1000; //5 minute window

    Result := LSetup;
  end;

begin
  Result := GetSample;
end;

procedure TTickerParser.SaveCSV;
var
  LOutput : TStringList;
  I: Integer;
begin
  LOutput := TStringList.Create;
  try
    //add headers
    LOutput.Add(
      'price' + ',' +
      'ask' + ',' +
      'bid' + ',' +
      'size' + ',' +
      'volume' + ','+
      'time'
    );

    //output csv rows
    for I := 0 to Pred(FTickers.Count) do
      LOutput.Add(
        FloatToStr(FTickers[I].Price) + ',' +
        FloatToStr(FTickers[I].Ask) + ',' +
        FloatToStr(FTickers[I].Bid) + ',' +
        FloatToStr(FTickers[I].Size) + ',' +
        FloatToStr(FTickers[I].Volume) + ',' +
        FloatToStr(FTickers[I].Time)
      );

    LOutput.SaveToFile(edit_directory_csv.Text);
    ShowMessage('saved to ' + edit_directory_csv.Text);
  finally
    LOutput.Free;
  end;
end;

procedure TTickerParser.SimulateStrategy;
var
  LEngine: IDelilah;
  LStrategy : IStrategy;
  LManager : IMockOrderManager;
  LStep: Int64;
  I: Integer;
  LTicker : ITickerGDAX;
  LError: string;
  LLoader: TTickerLoader;
  LProduct : IGDAXProduct;
begin
  //create the product that the tickers represent
  //todo - make this configurable or just load from api
  LProduct := TGDAXProductImpl.Create
  LProduct.BaseCurrency := 'USD';
  LProduct.BaseMaxSize := 100000;
  LProduct.BaseMinSize := 0.0001;
  LProduct.ID := 'BTC-USD';
  LProduct.QuoteIncrement := LProduct.BaseMinSize;
  LProduct.LoadFromJSON();

  //create and setup engine
  LEngine := TDelilahImpl.Create;
  LEngine.Funds := StrToIntDef(edit_funds.Text, 1000);

  //create and setup order manager
  LManager := TMockOrderManagerImpl.Create;
  LManager.FeePercentage := StrToFloatDef(edit_fee_perc.Text, 0) / 100;
  LEngine.OrderManager := LManager;

  //get the strategy to use for simulation and add to the engine
  LStrategy := GetStrategy;
  LEngine.Strategies.Add(LStrategy);

  progress_simulate.Position := 0;
  LStep := Trunc(FTickers.Count / 20);
  for I := 0 to Pred(FTickers.Count) do
  begin
    //feed with the current ticker by copying first
    LLoader := TTickerLoader.Create;
    LLoader.LoadFromJSON(FTickers[I].JSON, LError);
    LTicker := TGDAXTickerImpl.Create(LLoader);
    LTicker.Ticker.Product := LProduct;

    LEngine.Feed(LTicker, LError);

    if (I > 0) and (I mod LStep = 0) then
    begin
      progress_simulate.StepBy(5);
      Application.ProcessMessages;
    end;

    if I mod 200 = 0 then
      Application.ProcessMessages;
  end;
end;

procedure TTickerParser.PickFiles;
var
  LSearch : TListFileSearcher;
  LFiles: TStringList;
begin
  //now load all the files from the folder and do some work
  LFiles := TStringList.Create;
  LSearch := TListFileSearcher.Create(LFiles);
  try
    LSearch.Search(edit_directory.Text);
    LoadFiles(LFiles);
  finally
    LFiles.Free;
    LSearch.Free;
  end;
end;

procedure TTickerParser.btn_loadClick(Sender: TObject);
begin
  PickFiles;
end;

procedure TTickerParser.LoadFiles(const AFiles: TStrings);
var
  LFile: TStringList;
  LTicker: TTickerLoader;
  I, J: Integer;
begin
  FTickers.Clear;

  LFile := TStringList.Create;
  try
    //iterate files and load the contents
    for I := 0 to Pred(AFiles.Count) do
    begin
      if not FileExists(AFiles[I]) then
        Continue;

      LFile.LoadFromFile(AFiles[I]);

      //tickers are stored one line at a time in a json object
      for J := 0 to Pred(LFile.Count) do
      begin
        if (LFile[J].Length < 1) or (LFile[J][1] <> '{') then
          Continue;

        //load the ticker
        LTicker := TTickerLoader.Create;
        FTickers.Add(LTicker);
        LTicker.Load(LFile[J]);
      end;
    end;
  finally
    LFile.Free;
  end;
end;

{ TTickerLoader }

function TTickerLoader.GetJSON: String;
begin
  Result := FJSON;
end;

procedure TTickerLoader.Load(const AJSON: String);
var
  LError: string;
begin
  if not DoLoadFromJSON(AJSON, LError) then
    Raise Exception.Create(LError);

  FJSON := AJSON;
end;

end.

