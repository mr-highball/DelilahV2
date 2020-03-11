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
    function GetMaxDecel: Extended;
    function GetPosition: TAccelPosition;
    function GetPosPercent: Single;
    function GetRiskyPerc: Single;
    function GetThresh: Single;
    function GetThreshDown: Single;
    function GetUseDyn: Boolean;
    procedure SetLeadEnd(const AValue: Single);
    procedure SetLeadStart(const AValue: Single);
    procedure SetPosPercent(const AValue: Single);
    procedure SetRiskyPerc(const AValue: Single);
    procedure SetThresh(const AValue: Single);
    procedure SetThreshDown(const AValue: Single);
    function GetCurLag: Single;
    function GetCurLead: Single;
    function GetAccelStd: Extended;
    function GetMaxAccel: Extended;
    function GetAvgAccel: Extended;
    function GetAvgDecel: Extended;
    function GetAvgMaxAccel: Extended;
    function GetAvgMaxDecel: Extended;
    function GetDecelStd: Extended;
    procedure SetUseDyn(const AValue: Boolean);

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

      (CurLeadAccel - CurLagAccel) / CurLagAccel
    *)
    property CrossThresholdPercent : Single read GetThresh write SetThresh;

    (*
      the "buffer" around the lagging indicator that should be used to determine
      if the leading indicator has "crossed" downwards (sell)

      (CurLeadAccel - CurLagAccel) / CurLagAccel
    *)
    property CrossDownThresholdPercent : Single read GetThreshDown write SetThreshDown;

    (*
      the current "state" this strategy is in
    *)
    property Position : TAccelPosition read GetPosition;

    (*
      the current lagging acceleration for the entire window
      averageAccelerationPerTick * TickCount
    *)
    property CurLagAccel : Single read GetCurLag;

    (*
      the current leading acceleration for the leading window
      the window is defined as LeadStart -> LeadEnd
      averageAccelerationPerTick * TickCount
    *)
    property CurLeadAccel : Single read GetCurLead;

    (*
      the average of window acceleration differences. might be confusing,
      here's some info:

      once the window is "Ready", for every tick that reports
      a positive acceleration difference (CurLeadAccel - CurLagAccel) / CurLagAccel
      that number will be recorded. The mean of these recordings is
      the "AverageAcceleration". It's important to note that this metric
      is the "opposite" of "AverageDeceleration"
    *)
    property AverageAcceleration : Extended read GetAvgAccel;

    (*
      for every new MaxAccleration, this metric will be averaged with
      the previous "high"
    *)
    property AverageMaxAcceleration : Extended read GetAvgMaxAccel;

    (*
      records the maximum acceleration difference seen from the beggining
      of feeding ticks to this strategy
    *)
    property MaxAcceleration : Extended read GetMaxAccel;

    (*
      the standard deviation of the "AverageAcceleration" recordings
    *)
    property AccelerationStdDev : Extended read GetAccelStd;

    (*
      the "average of averages" of decelerations

      once the window is "Ready", for every tick that reports
      a negative acceleration (deceleration) difference (CurLeadAccel - CurLagAccel)
      that number will be recorded. The mean of these recordings is
      the "AverageDeceleration". It's important to note that this metric
      is the "opposite" of "AverageAcceleration"
    *)
    property AverageDeceleration : Extended read GetAvgDecel;

    (*
      for every new MaxDeceleration, this metric will be averaged with
      the previous "high"
    *)
    property AverageMaxDeceleration : Extended read GetAvgMaxDecel;

    (*
      records the maximum acceleration difference seen from the beggining
      of feeding ticks to this strategy
    *)
    property MaxDeceleration : Extended read GetMaxDecel;

    (*
      the standard deviation of the "AverageDeceleration" recordings
    *)
    property DecelerationStdDev : Extended read GetDecelStd;

    (*
      attempts to optimize cross thresholds to make better entries / exits.
      user specified cross percents are used as minimums in this case
      **experimental**
    *)
    property UseDynamicPositions : Boolean read GetUseDyn write SetUseDyn;
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
    FMaxAccel,
    FAvgMaxAccel,
    FAvgAccel,
    FMaxDecel,
    FAvgMaxDecel,
    FAvgDecel,
    FStdAccel,
    FStdDecel: Extended;
    FAccels,
    FDecels,
    FLaggings: TArray<Extended>;
    FUseDyn : Boolean;
  protected
    function GetAccelStd: Extended;
    function GetAMaxAccel: Extended;
    function GetAvgAccel: Extended;
    function GetAvgDecel: Extended;
    function GetAvgMaxAccel: Extended;
    function GetAvgMaxDecel: Extended;
    function GetDecelStd: Extended;
    function GetMaxAccel: Extended;
    function GetMaxDecel: Extended;
    function GetUseDyn: Boolean;
    procedure SetUseDyn(const AValue: Boolean);
    procedure UpdateExtremaAccel(const ALeading, ALagging : Single;
      out IsOutlier : Boolean);
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
    function CalcAccelDiffPerc(const ALead, ALag : Single;
      out IsAccel : Boolean) : Single;

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
    property AverageAcceleration : Extended read GetAvgAccel;
    property AverageMaxAcceleration : Extended read GetAvgMaxAccel;
    property MaxAcceleration : Extended read GetAMaxAccel;
    property AccelerationStdDev : Extended read GetAccelStd;
    property AverageDeceleration : Extended read GetAvgDecel;
    property AverageMaxDeceleration : Extended read GetAvgMaxDecel;
    property MaxDeceleration : Extended read GetMaxDecel;
    property DecelerationStdDev : Extended read GetDecelStd;
    property UseDynamicPositions : Boolean read GetUseDyn write SetUseDyn;

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

function TAccelerationStrategyImpl.CalcAccelDiffPerc(const ALead, ALag: Single;
  out IsAccel: Boolean): Single;
begin
  Result := 0;

  if ALag = 0 then
    Exit;

  IsAccel := ALead > ALag;
  Result := (ALead - ALag) / Abs(ALag);
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
  LInPosition, LIsOutlier: Boolean;
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
  var
    LDiff,
    LThresh: Single;
    LIsAccel : Boolean;
  begin
    Result := False;
    LIsAccel := False;
    Position := apNone;
    Funds := 0;

    //calculate the percent difference between leading & lagging
    LDiff := CalcAccelDiffPerc(ALeadAccel, ALagAccel, LIsAccel);

    if LDiff = 0 then
      Exit;

    //see if we are using dynamic thresholds
    if FUseDyn then
    begin
      //set here to easily debug
      LThresh := FAvgAccel + (FStdDecel * 0.10);

      //negate when user specified to do so
      if FThresh < 0 then
        LThresh := -LThresh;

      //set back to the user specified threshold if it's stricter
      if (FThresh > 0) and (LThresh < FThresh) then
        LThresh := FThresh
      else if (FThresh < 0) and (LThresh > FThresh) then
        LThresh := FThresh;
    end
    //not using dynamic, set to user specified
    else
      LThresh := FThresh;

    //check for main position
    if ALagAccel > 0 then
    begin
      //find the amount of funds to spend for a full position
      Funds := FPosPercent * AFunds;

      if Funds <= 0 then
        Exit(False);

      if Funds > AFunds then
        Funds := AFunds;

      //take position if positive diff percent, and is greater than the threshold
      //or if threshold is negative handle differently
      Result :=
        ((LThresh > 0) and (LDiff >= LThresh))
        or
        ((LThresh < 0) and (LDiff <= LThresh));
      Position := apFull;
    end
    //check for risky position
    else if FRiskyPerc > 0 then
    begin
      //find amount of funds to spend for a risky position
      Funds := FRiskyPerc * AFunds;

      if Funds < 0 then
        Exit(False);

      if Funds > AFunds then
        Funds := AFunds;

      //close position if positive diff percent, and is greater than the threshold
      Result :=
        ((LThresh > 0) and (LDiff >= LThresh))
        or
        ((LThresh < 0) and (LDiff <= LThresh));
      Position := apRisky;
    end;
  end;

  (*
    helper to determine if we should close the position
  *)
  function GetClosePosition(const ALeadAccel, ALagAccel : Single) : Boolean;
  var
    LDiff : Single;
    LIsAccel : Boolean;
    LThresh : Single;
  begin
    Result := False;
    LIsAccel := False;

    if FPosition = apNone then
      Exit;

    //calculate the percent difference between leading & lagging
    LDiff := CalcAccelDiffPerc(ALeadAccel, ALagAccel, LIsAccel);

    if LDiff = 0 then
      Exit;

    //see if we are using dynamic thresholds
    if FUseDyn then
    begin
      //set here to easily debug
      LThresh := abs(FAvgDecel) + abs(FStdDecel) * 0.10;

      //negate when user specified to do so
      if FThreshDown < 0 then
        LThresh := -LThresh;

      //set back to the user specified threshold if it's stricter
      if not LIsAccel and (FThreshDown > 0) and (LThresh < FThreshDown) then
        LThresh := FThreshdown
      else if not LIsAccel and (FThreshDown < 0) and (LThresh > FThreshDown) then
        LThresh := FThreshDown
      else if LIsAccel and (FThreshDown < 0) and (LThresh < FThreshDown) then
        LThresh := FThreshDown;
    end
    //not using dynamic, set to user specified
    else
      LThresh := FThreshDown;


    //close position if negative diff percent, and is greater than the threshold
    //or if we negative threshold set, and the diff exceeds it
    Result :=
      (not LIsAccel and (LThresh > 0) and (abs(LDiff) >= LThresh))
      or
      (not LIsAccel and (LThresh < 0) and (LDiff <= LThresh))
      or
      (LIsAccel and (LThresh < 0) and (LDiff >= abs(LThresh))); //take profit case
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

    //using min, clear position if we have "dust"
    if (FPosSize > 0)
      and (AInventory < LMin)
      and (LMin - AInventory > (LMin - LMin * 0.999))
    then
    begin
      LogInfo('dust detected, clearing position');
      ClearPosition;
    end;

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

    //updates metrics (avg, max, outliers, etc...)
    UpdateExtremaAccel(LLeading, LLagging, LIsOutlier);

    if LIsOutlier then
    begin
      LogInfo(Format('outlier lagging detected, skipping [lagging]:%f', [LLagging]));
      Exit(True);
    end;

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
          LogWarning(Format('TakePosition::declined for [reason]:%s', [Error]));
          Exit(True);
        end;

        if (LDetails.Size * ATicker.Price) > AFunds then
        begin
          LogWarning('TakePosition::order details size greater than current funds');
          Exit;
        end;

        if (LDetails.Size <= 0) then
        begin
          LogWarning('TakePosition::zero sized order, skipping place');
          Exit;
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
  FUseDyn := False;
  SetLength(FAccels, 0);
  SetLength(FDecels, 0);
  FAvgMaxAccel := 0;
  FMaxAccel := 0;
  FMaxDecel := 0;
  FAvgMaxDecel := 0;
  FAvgAccel := 0;
  FAvgDecel := 0;
  FStdAccel := 0;
  FStdDecel := 0;
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

procedure TAccelerationStrategyImpl.UpdateExtremaAccel(const ALeading, ALagging : Single;
  out IsOutlier : Boolean);

  procedure WriteLog;
  begin
    LogInfo(Format('acceleration readout [avgAccel]:%f [maxAccel]:%f [stdAccel]:%f [avgDecel]:%f [maxDecel]:%f [stdDecel]:%f', [FAvgMaxAccel, FMaxAccel, FStdAccel, FAvgMaxDecel, FMaxDecel, FStdDecel]));
  end;

const
  OUTLIER_BOUND = 0.15;
var
  LAccelDiffPerc: Single;
  I: Int64;
  LIsAccel : Boolean;
  LLagAvg,
  LLagStd : Extended;
begin
  IsOutlier := False;
  LLagAvg := 0;
  LLagStd := 0;

  //see if we have an outlier lagging so our numbers aren't hosed by close
  //to zero bullshit
  if Length(FLaggings) > 1 then
  begin
    meanandstddev(FLaggings, LLagAvg, LLagStd);

    if (ALagging >= 0 - (LLagStd * OUTLIER_BOUND))
      and (ALagging <= 0 + (LLagStd * OUTLIER_BOUND))
    then
    begin
      IsOutlier := True;
      Exit;
    end;

    SetLength(FLaggings, Succ(Length(FLaggings)));
    FLaggings[High(FLaggings)] := ALagging;

    //bounds check like below to keep along the lines of ticker count
    if Length(FLaggings) > (Tickers.Count * 1.10) then
    begin
      I := Trunc(Length(FLaggings) * 0.10);
      Move(FLaggings[I], FLaggings[0], SizeOf(FLaggings[I]) * (Length(FLaggings) - I));
      SetLength(FLaggings, Length(FLaggings) - Succ(I));
    end;
  end
  else
  begin
    SetLength(FLaggings, Succ(Length(FLaggings)));
    FLaggings[High(FLaggings)] := ALagging;
  end;

  //calculate the difference percentage between leading and lagging
  LAccelDiffPerc := CalcAccelDiffPerc(ALeading, ALagging, LIsAccel);

  //ignore zero accelerations
  if (LAccelDiffPerc <> 0) then
  begin
    //acceleration
    if LIsAccel then
    begin
      //keep to the size of tickers
      if Length(FAccels) > (Tickers.Count * 1.10) then
      begin
        I := Trunc(Length(FAccels) * 0.10);
        Move(FAccels[I], FAccels[0], SizeOf(FAccels[I]) * (Length(FAccels) - I));
        SetLength(FAccels, Length(FAccels) - Succ(I));

        //re-balance max
        FMaxAccel := maxvalue(FAccels);
        FAvgMaxAccel := FMaxAccel;
      end;

      SetLength(FAccels, Succ(Length(FAccels)));
      FAccels[High(FAccels)] := LAccelDiffPerc;

      //get the average / std
      meanandstddev(FAccels, FAvgAccel, FStdAccel);

      if LAccelDiffPerc > FMaxAccel then
      begin
        FMaxAccel := LAccelDiffPerc;

        if FAvgMaxAccel <> 0 then
          FAvgMaxAccel := (FAvgMaxAccel + LAccelDiffPerc) / 2
        else
          FAvgMaxAccel := FLead / FLag;
      end;
    end
    //deceleration
    else
    begin
      //keep to the size of tickers
      if Length(FDecels) > (Tickers.Count * 1.10) then
      begin
        I := Trunc(Length(FDecels) * 0.10);
        Move(FDecels[I], FDecels[0], SizeOf(FDecels[I]) * (Length(FDecels) - I));
        SetLength(FDecels, Length(FDecels) - Succ(I));

        //re-balance max
        FMaxDecel := maxvalue(FDecels);
        FAvgMaxDecel := FMaxDecel;
      end;

      SetLength(FDecels, Succ(Length(FDecels)));
      FDecels[High(FDecels)] := LAccelDiffPerc;

      //get the average / std
      meanandstddev(FDecels, FAvgDecel, FStdDecel);

      if LAccelDiffPerc < FMaxDecel then
      begin
        FMaxDecel := LAccelDiffPerc;

        if FAvgMaxDecel <> 0 then
          FAvgMaxDecel := (FAvgMaxDecel + LAccelDiffPerc) / 2
        else
          FAvgMaxDecel := LAccelDiffPerc;
      end;
    end;

    WriteLog;
  end;
end;

function TAccelerationStrategyImpl.GetAccelStd: Extended;
begin
  Result := FStdAccel;
end;

function TAccelerationStrategyImpl.GetAMaxAccel: Extended;
begin
  Result := FMaxAccel;
end;

function TAccelerationStrategyImpl.GetAvgAccel: Extended;
begin
  Result := FAvgAccel;
end;

function TAccelerationStrategyImpl.GetAvgDecel: Extended;
begin
  Result := FAvgDecel;
end;

function TAccelerationStrategyImpl.GetAvgMaxAccel: Extended;
begin
  Result := FAvgMaxAccel;
end;

function TAccelerationStrategyImpl.GetAvgMaxDecel: Extended;
begin
  Result := FAvgMaxDecel;
end;

function TAccelerationStrategyImpl.GetDecelStd: Extended;
begin
  Result := FStdDecel;
end;

function TAccelerationStrategyImpl.GetMaxAccel: Extended;
begin
  Result := FMaxAccel;
end;

function TAccelerationStrategyImpl.GetMaxDecel: Extended;
begin
  Result := FMaxDecel;
end;

function TAccelerationStrategyImpl.GetUseDyn: Boolean;
begin
  Result := FUseDyn;
end;

procedure TAccelerationStrategyImpl.SetUseDyn(const AValue: Boolean);
begin
  FUseDyn := AValue;
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

