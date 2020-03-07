unit delilah.strategy.acceleration;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  delilah.types,
  delilah.strategy.window;

type

  (*
    the current state of this strategy
  *)
  TAccelPosition = (
    apNone,
    apRisky,
    apFull
  );

  { IAccelerationStrategy }
  (*
    the acceleration strategy uses a window of time to determine a
    "lagging" and "leading" indicator. positions are made when
    indicators "cross" either upwards or downwards
  *)
  IAccelerationStrategy = interface(IWindowStrategy)
    ['{42E20828-B673-4B7A-82DF-3AFA08BFE4C4}']

    //property methods
    function GetLeadEnd: Single;
    function GetLeadStart: Single;
    function GetPosition: TAccelPosition;
    function GetPosPercent: Single;
    function GetRiskyPerc: Single;
    function GetThresh: Single;
    function GetThreshDown: Single;
    procedure SetLeadEnd(const AValue: Single);
    procedure SetLeadStart(const AValue: Single);
    procedure SetPosPercent(const AValue: Single);
    procedure SetRiskyPerc(const AValue: Single);
    procedure SetThresh(const AValue: Single);
    procedure SetThreshDown(const AValue: Single);
    function GetCurLag: Single;
    function GetCurLead: Single;

    //properties

    (*
      the lead starting percent sets the beginning of the leading indicator
      and corresponds to a position inside of the window
    *)
    property LeadStartPercent : Single read GetLeadStart write SetLeadStart;

    (*
      the end starting percent sets the ending of the leading indicator
      and corresponds to a position inside of the window. This can't exceed
      1.0 (100%) and can't be prior to the starting percent
    *)
    property LeadEndPercent : Single read GetLeadEnd write SetLeadEnd;

    (*
      when a LONG position is determined, then this percentage is used
      to make an order out of the total available funds. The criteria for
      a position are trending UP lag indicator, and an upwards cross from
      the lead indicator
    *)
    property PositionPercent : Single read GetPosPercent write SetPosPercent;

    (*
      when a (risky) LONG position is determined, then this percentage is used
      to make an order out of the total available funds. The criteria for
      a position are trending DOWN lag indicator, and an upwards cross from
      the lead indicator. 0% can be specified to avoid making this position
    *)
    property RiskyPositionPercent : Single read GetRiskyPerc write SetRiskyPerc;

    (*
      the "buffer" around the lagging indicator that should be used to determine
      if the leading indicator has "crossed" upwards (buy)
    *)
    property CrossThresholdPercent : Single read GetThresh write SetThresh;

    (*
      the "buffer" around the lagging indicator that should be used to determine
      if the leading indicator has "crossed" downwards (sell)
    *)
    property CrossDownThresholdPercent : Single read GetThreshDown write SetThreshDown;

    (*
      the current "state" this strategy is in
    *)
    property Position : TAccelPosition read GetPosition;

    (*
      the current lagging acceleration average
    *)
    property CurLagAccel : Single read GetCurLag;

    (*
      the current leading acceleration average
    *)
    property CurLeadAccel : Single read GetCurLead;
  end;

  { TAccelerationStrategyImpl }
  (*
    base implementation for an acceleration based strategy.
    the only two methods required for children to override are
    - TakePosition()
    - ClosePosition()

    recommended to override in case minimums vary per exchange / ticker
    - GetMinOrderSize()
  *)
  TAccelerationStrategyImpl = class(TWindowStrategyImpl, IAccelerationStrategy)
  strict private
    FLeadEnd,
    FLeadStart,
    FPosPercent,
    FRiskyPerc,
    FThresh,
    FThreshDown,
    FLag,
    FLead: Single;
    FPosSize : Extended;
    FPosition : TAccelPosition;
    FPositionDetails : IOrderDetails;
  protected
    function GetCurLag: Single;
    function GetCurLead: Single;
    function GetPosition: TAccelPosition;
    function GetLeadEnd: Single;
    function GetLeadStart: Single;
    function GetPosPercent: Single;
    function GetRiskyPerc: Single;
    function GetThresh: Single;
    procedure SetLeadEnd(const AValue: Single);
    procedure SetLeadStart(const AValue: Single);
    procedure SetPosPercent(const AValue: Single);
    procedure SetRiskyPerc(const AValue: Single);
    procedure SetThresh(const AValue: Single);
    function GetThreshDown: Single;
    procedure SetThreshDOwn(const AValue: Single);
  strict protected
    (*
      returns the minimum order size that can be taken. children can
      override this as it's used in DoFeed to determine if we are in position
    *)
    function GetMinOrderSize(const ATicker : ITicker) : Extended; virtual;

    (*
      children override this method to return an order details that will
      open a position of a given size.

      @ATicker:
        - the current ticker object provided to this strategy
      @APosition:
        - the state this position is
      @ASize:
        - the size that the position will be in "funds spent"
      @IsPosition:
        - child needs to set this to true if the position should be executed
      @Reason:
        - child defined reason used if the position is declined
    *)
    function TakePosition(const ATicker : ITicker;
      const APosition : TAccelPosition; const ASize : Extended;
      out IsPosition : Boolean; out Reason : String) : IOrderDetails;virtual; abstract;

    (*
      children override this method to return an order details that will
      close a position of a given size of product

      @ATicker:
        - the current ticker object provided to this strategy
      @ASize:
        - the size that the position will be in product to sell
      @ADetails:
        - the original order details that were executed to open the position
      @IsClose:
        - child needs to set this to true if the we should close our position
      @Reason:
        - child defined reason used if the position is declined
    *)
    function ClosePosition(const ATicker : ITicker; const ASize : Extended;
      const ADetails : IOrderDetails;
      out IsClose : Boolean; out Reason : String) : IOrderDetails; virtual; abstract;

    function DoFeed(const ATicker: ITicker; const AManager: IOrderManager;
      const AFunds, AInventory, AAAC: Extended; out Error: String): Boolean; override;
  public
    property LeadStartPercent : Single read GetLeadStart write SetLeadStart;
    property LeadEndPercent : Single read GetLeadEnd write SetLeadEnd;
    property PositionPercent : Single read GetPosPercent write SetPosPercent;
    property RiskyPositionPercent : Single read GetRiskyPerc write SetRiskyPerc;
    property CrossThresholdPercent : Single read GetThresh write SetThresh;
    property CrossDownThresholdPercent : Single read GetThreshDown write SetThreshDown;
    property Position : TAccelPosition read GetPosition;
    property CurLagAccel : Single read GetCurLag;
    property CurLeadAccel : Single read GetCurLead;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  Math,
  delilah.strategy.utils,
  delilah.ticker.gdax;

{ TAccelerationStrategyImpl }

function TAccelerationStrategyImpl.GetThreshDown: Single;
begin
  Result := FThreshDown;
end;

procedure TAccelerationStrategyImpl.SetThreshDOwn(const AValue: Single);
begin
  FThreshDown := AValue;
end;

function TAccelerationStrategyImpl.GetMinOrderSize(const ATicker: ITicker): Extended;
begin
  //base just returns a small number and doesn't use the ticker
  Result := 0.00000001;
end;

function TAccelerationStrategyImpl.DoFeed(const ATicker: ITicker;
  const AManager: IOrderManager; const AFunds, AInventory, AAAC: Extended; out
  Error: String): Boolean;
var
  LTicker: ITickerGDAX;
  LMin: Extended;
  LLagging, LLeading: Single;
  LInPosition: Boolean;
  LPos : TAccelPosition;
  LFunds : Extended;
  LDetails: IOrderDetails;
  LID: String;
  LLeadStart, LLeadEnd: Int64;

  (*
    helper to determine if we should take a position
  *)
  function GetTakePosition(const ALeadAccel, ALagAccel : Single;
    out Position : TAccelPosition; out Funds : Extended) : Boolean;
  const
    MIN_LAG = 0.00001;
  begin
    Result := False;
    Position := apNone;
    Funds := 0;

    //avoid outliers approaching very close to zero
    if (ALagAccel <> 0)
      and (Abs(ALeadAccel) > MIN_LAG) //check to make sure we have a higher lead
      and (Abs(ALagAccel) < MIN_LAG) //outlier check for small "dust"
    then
    begin
      LogInfo('acceleration outlier detecting, bypassing position check');
      Exit;
    end;

    //check for main position
    if (ALagAccel > 0) and (ALeadAccel > 0) then
    begin
      Funds := FPosPercent * AFunds;

      if Funds <= 0 then
        Exit(False);

      if Funds > AFunds then
        Funds := AFunds;

      Result := ALeadAccel >= (ALagAccel * (1 + FThresh));
      Position := apFull;
    end
    //check for risky position
    else if FRiskyPerc > 0 then
    begin
      Funds := FRiskyPerc * AFunds;

      if Funds < 0 then
        Exit(False);

      if Funds > AFunds then
        Funds := AFunds;

      Result := ALeadAccel >= (ALagAccel * (1 - FThresh));
      Position := apRisky;
    end;
  end;

  (*
    helper to determine if we should close the position
  *)
  function GetClosePosition(const ALeadAccel, ALagAccel : Single) : Boolean;
  const
    MIN_LAG = 0.00001;
  begin
    Result := False;

    if FPosition = apNone then
      Exit;

    //avoid outliers approaching very close to zero
    if (ALagAccel <> 0)
      and (Abs(ALeadAccel) > MIN_LAG) //check to make sure we have a higher lead
      and (Abs(ALagAccel) < MIN_LAG) //outlier check for small "dust"
    then
    begin
      LogInfo('acceleration outlier detecting, bypassing position check');
      Exit;
    end;

    if ALagAccel > 0 then
      Result := ALeadAccel <= (ALagAccel * (1 - FThreshDown))
    else
      Result := ALeadAccel <= (ALagAccel * (1 + FThreshDown));
  end;

  procedure ClearPosition;
  begin
    FPosSize := 0;
    FPositionDetails := nil;
    FPosition := apNone;
  end;

begin
  try
    //call down to base to feed the window
    Result := inherited DoFeed(ATicker, AManager, AFunds, AInventory, AAAC, Error);

    //bail if this didn't work for some reason
    if not Result then
      Exit;

    //if the window isn't ready gracefully exit this method
    if not IsReady then
    begin
      LogInfo(Format('window is not ready [size]:%d [collected]:%d', [WindowSizeInMilli, CollectedSizeInMilli]));
      Exit(True);
    end;

    LMin := GetMinOrderSize(ATicker);

    //calculate the lagging acceleration for the window
    LLagging := GetAverageAcceleratePerTick(Tickers) * Tickers.Count;

    //calculate the leading indicator (need to provide explicit start / end markers)
    LLeadStart := Abs(Trunc(Tickers.Count * FLeadStart));
    LLeadEnd := Abs(Trunc(Tickers.Count * FLeadEnd));
    LLeading := GetAverageAcceleratePerTick(
      Tickers,
      LLeadStart,
      LLeadEnd
    ) * Succ(LLeadEnd - LLeadStart);

    //update internal (could probably just use internal vars here)
    FLag := LLagging;
    FLead := LLeading;

    //determine if we are in a position
    LInPosition := (FPosition in [apFull, apRisky]) and (FPosSize >= LMin);

    if LInPosition then
    begin
      if AInventory < LMin then
      begin
        LInPosition := False;
        ClearPosition;
      end;
    end
    else
      ClearPosition;

    //log the state for users
    LogInfo(Format('acceleration indicators [in-position]:%s [lagging]:%f [leading]:%f', [BoolToStr(LInPosition, True), LLagging, LLeading]));

    //if we aren't then check if we need to open one
    if (not LInPosition) or (FPosition = apRisky) then
    begin
      //check if we should take a position
      if GetTakePosition(LLeading, LLagging, LPos, LFunds) then
      begin
        //if we already have a risky position, don't take another one
        if (FPosition = apRisky) and (LPos = apRisky) then
        begin
          LogInfo('risky position detected, but already in position');
          Exit(True);
        end;

        LDetails := TakePosition(ATicker, LPos, LFunds, LInPosition, Error);

        //if the call to take is declined, then warn and exit gracefully
        if not LInPosition then
        begin
          LogWarning(Format('TakePosition declined for [reason]:%s', [Error]));
          Exit(True);
        end;

        //try to place the order
        if not AManager.Place(LDetails, LID, Error) then
          Exit;

        //check to include risky position size to the new position
        if FPosition = apRisky then
          FPosSize := FPosSize + LDetails.Size
        //otherwise good to just set the size
        else
          FPosSize := LDetails.Size;

        //update internals to taking position
        FPosition := LPos;
        FPositionDetails := LDetails;

        LogInfo(Format('position taken new position [size]:%f', [FPosSize]));
        Exit(True);
      end;
    end;

    //otherwise we need to see if we need to close
    if LInPosition and GetClosePosition(LLeading, LLagging)
    then
    begin
      LFunds := FPosSize;

      //bounds checking for inventory (can't sell more than we have)
      //also making sure we don't leave "dust"
      if (LFunds > AInventory) or ((AInventory - LFunds) < LMin) then
        LFunds := AInventory;

      if LFunds < LMin then
      begin
        ClearPosition;
        LogInfo('in-position but inventory less than min, closing without order');
        Exit(True);
      end;

      //get the details for closing this position
      LDetails := ClosePosition(
        ATicker,
        LFunds,
        FPositionDetails,
        LInPosition,
        Error
      );

      //if the call to take is declined, then warn and exit gracefully
      if not LInPosition then
      begin
        LogWarning(Format('ClosePosition declined for [reason]:%s', [Error]));
        Exit(True);
      end;

      //try to place the order
      if not AManager.Place(LDetails, LID, Error) then
        Exit;

      //reset internals, position closed
      ClearPosition;

      LogInfo('position closed');
    end;

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

constructor TAccelerationStrategyImpl.Create;
begin
  inherited Create;
  FLag := 0;
  FLead := 0;
  FPosSize := 0;
  FPositionDetails := nil;
  FLeadEnd := 1.0;
  FLeadStart := 0.5;
  FPosPercent := 1.0;
  FRiskyPerc := 0.10;
  FThresh := 0.0025;
  FThreshDown := 0.0025;
end;

destructor TAccelerationStrategyImpl.Destroy;
begin
  FPositionDetails := nil;
  inherited Destroy;
end;

function TAccelerationStrategyImpl.GetCurLag: Single;
begin
  Result := FLag
end;

function TAccelerationStrategyImpl.GetCurLead: Single;
begin
  Result := FLead;
end;

function TAccelerationStrategyImpl.GetPosition: TAccelPosition;
begin
  Result := FPosition;
end;

function TAccelerationStrategyImpl.GetLeadEnd: Single;
begin
  Result := FLeadEnd;
end;

function TAccelerationStrategyImpl.GetLeadStart: Single;
begin
  Result := FLeadStart;
end;

function TAccelerationStrategyImpl.GetPosPercent: Single;
begin
  Result := FPosPercent;
end;

function TAccelerationStrategyImpl.GetRiskyPerc: Single;
begin
  Result := FRiskyPerc;
end;

function TAccelerationStrategyImpl.GetThresh: Single;
begin
  Result := FThresh;
end;

procedure TAccelerationStrategyImpl.SetLeadEnd(const AValue: Single);
begin
  FLeadEnd := AValue;

  if FLeadEnd < 0 then
    FLeadEnd := 0;

  if FLeadEnd > 1 then
    FLeadEnd := 1;
end;

procedure TAccelerationStrategyImpl.SetLeadStart(const AValue: Single);
begin
  FLeadStart := AValue;

  if FLeadStart < 0 then
    FLeadStart := 0;

  if FLeadStart > 1 then
    FLeadStart := 1;
end;

procedure TAccelerationStrategyImpl.SetPosPercent(const AValue: Single);
begin
  FPosPercent := AValue;
end;

procedure TAccelerationStrategyImpl.SetRiskyPerc(const AValue: Single);
begin
  FRiskyPerc := AValue;
end;

procedure TAccelerationStrategyImpl.SetThresh(const AValue: Single);
begin
  FThresh := AValue;
end;

end.

