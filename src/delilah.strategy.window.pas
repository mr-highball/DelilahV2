unit delilah.strategy.window;

{$mode delphi}

interface

uses
  Classes, SysUtils, delilah.types, delilah.strategy;

type

  { IWindowStrategy }
  (*
    IWindowStrategy's specialize in working on a set of tickers in some
    defined "window" of time
  *)
  IWindowStrategy = interface(IStrategy)
    ['{535CAC6E-19BA-4C2D-975B-EA3C737C6A75}']
    //property methods
    function GetCleanPerc: Single;
    function GetCleanThresh: Single;
    function GetCollected: Cardinal;
    function GetIsReady: Boolean;
    function GetWindowSize: Cardinal;
    procedure SetCleanPerc(Const AValue: Single);
    procedure SetCleanThresh(Const AValue: Single);
    procedure SetWindowSize(Const AValue: Cardinal);
    function GetTickers: TTickers;

    //properties
    (*
      the desired time from start of ticker collection to the last tick
    *)
    property WindowSizeInMilli : Cardinal read GetWindowSize write SetWindowSize;
    property CollectedSizeInMilli : Cardinal read GetCollected;
    (*
      all tickers collected within the lifetime of being fed
    *)
    property Tickers : TTickers read GetTickers;
    (*
      the percentage of the window size to "cleanup" once a threshold is
      reached
    *)
    property CleanupPercentage : Single read GetCleanPerc write SetCleanPerc;
    (*
      threshold to trigger a "cleanup" of old tickers based in percentage
      of the window size. (ie. for triggering a cleanup once window size is exceeded
      by 10%, the threshold would be set to 1.10, since we only cleanup after IsReady is true)
    *)
    property CleanupThreshold : Single read GetCleanThresh write SetCleanThresh;
    (*
      returns true when the length of time from the first ticker stored
      and the last ticker stored is at or above the window size
    *)
    property IsReady : Boolean read GetIsReady;
  end;

  { TWindowStrategyImpl }

  TWindowStrategyImpl = class(TStrategyImpl,IWindowStrategy)
  strict private
    FTickers: TTickers;
    FCleanPerc: Single;
    FCleanThresh: Single;
    FSize: Cardinal;
    function GetCleanPerc: Single;
    function GetCleanThresh: Single;
    function GetCollected: Cardinal;
    function GetIsReady: Boolean;
    function GetWindowSize: Cardinal;
    procedure SetCleanPerc(Const AValue: Single);
    procedure SetCleanThresh(Const AValue: Single);
    procedure SetWindowSize(Const AValue: Cardinal);
    function GetTickers: TTickers;
  strict protected
    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      const AFunds, AInventory: Extended; out Error: String): Boolean; override;
  public
    property WindowSizeInMilli : Cardinal read GetWindowSize write SetWindowSize;
    property CollectedSizeInMilli : Cardinal read GetCollected;
    property Tickers : TTickers read GetTickers;
    property CleanupPercentage : Single read GetCleanPerc write SetCleanPerc;
    property CleanupThreshold : Single read GetCleanThresh write SetCleanThresh;
    property IsReady : Boolean read GetIsReady;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  DateUtil;

{ TWindowStrategyImpl }

function TWindowStrategyImpl.GetCleanPerc: Single;
begin
  Result:=FCleanPerc;
end;

function TWindowStrategyImpl.GetCleanThresh: Single;
begin
  Result:=FCleanThresh;
end;

function TWindowStrategyImpl.GetCollected: Cardinal;
begin
  if FSize<2 then
    Exit(0);
  Result:=Abs(MilliSecondsBetween(
    FTickers[0].Time,
    FTickers[Pred(FTickers.Count)].Time
  ));
end;

function TWindowStrategyImpl.GetIsReady: Boolean;
begin
  //if we've captured at least window size we are ready
  Result:=CollectedSizeInMilli>=FSize;
end;

function TWindowStrategyImpl.GetWindowSize: Cardinal;
begin
  Result:=FSize;
end;

procedure TWindowStrategyImpl.SetCleanPerc(const AValue: Single);
begin
  FCleanPerc:=AValue;
end;

procedure TWindowStrategyImpl.SetCleanThresh(const AValue: Single);
begin
  FCleanThresh:=AValue;
end;

procedure TWindowStrategyImpl.SetWindowSize(const AValue: Cardinal);
begin
  FSize:=AValue;
end;

function TWindowStrategyImpl.GetTickers: TTickers;
begin
  Result:=FTickers;
end;

function TWindowStrategyImpl.DoFeed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory: Extended; out
  Error: String): Boolean;
var
  LMilli:Cardinal;
  I,J:Integer;
begin
  Result:=False;
  try
    //only check cleanup if we are ready
    if not IsReady then
      Exit(True);
    //below we need to attend to cleaning up tickers to keep the window thresholds
    LMilli:=FCleanThresh * FSize;
    if CollectedSizeInMilli>=LMilli then
    begin
      //now set milli to the amount we need to satisfy the cleanup
      LMilli:=FCleanPerc * FSize;
      J:=-1;
      for I:=1 to Pred(FTickers.Count) do
      begin
        if Abs(MilliSecondsBetween(
          FTickers[0].Time,
          FTicker[I].Time))>=LMilli
        then
        begin
          J:=I;
          Break;
        end;
      end;
      //up to J, delete the first recorded tick
      for I:=0 to J do
        FTickers.Delete(0);
    end;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TWindowStrategyImpl.Create;
begin
  inherited Create;
  FTickers:=TTickers.Create;
  //default to cleaning 10% once we reach 110% past the window size
  FCleanPerc:=0.10;
  FCleanThresh:=1.10
end;

destructor TWindowStrategyImpl.Destroy;
begin
  FTickers.Free;;
  inherited Destroy;
end;

end.

