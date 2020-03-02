unit delilah.strategy.utils;

{$mode delphi}

interface

uses
  Classes,
  SysUtils, fgl,
  delilah.types;

(*
  provided a list of tickers, will calculate the average acceleration
  per tick (factors in difference of time between tick) for a given
  range

  @ATickers:
    - tickers to iterate over
  @AStart:
    - starting index (0-based)
    - bounds checking performed, when out will use 0
  @AEnd:
    - ending index (0-based)
    - bounds checking performed, when out will use High(ATickers)
*)
function GetAverageAcceleratePerTick(const ATickers : TTickers;
  const AStart : Integer = -1; const AEnd : Integer = -1) : Single; inline;

(*
  returns the average time in milliseconds between ticks for a given range
*)
function GetAverageTimePerTick(const ATickers : TTickers;
  const AStart : Integer = -1; const AEnd : Integer = -1) : Integer; inline;

implementation
uses
  dateutils,
  Math;

function GetAverageAcceleratePerTick(const ATickers : TTickers; const AStart,
  AEnd: Integer): Single;
var
  LTimePerTick,
  LStart,
  LEnd, I: Integer;
begin
  Result := 0;

  //need the average time per tick to calculate the acceleration
  LTimePerTick := GetAverageTimePerTick(ATickers, AStart, AEnd);

  if LTimePerTick <= 0 then
    Exit;

  //bounds checking on markers
  if AStart < 0 then
    LStart := 0;

  if (AEnd < 0) or (AEnd >= ATickers.Count) then
    LEnd := Pred(ATickers.Count);

  //aggregate the accelerations
  Result := (ATickers[1].Price - ATickers[0].Price) / LTimePerTick;

  for I := Succ(LStart) to LEnd do
    Result := Result + ((ATickers[I].Price - ATickers[Pred(I)].Price) / LTimePerTick);

  //return the average
  Result := Result / ATickers.Count;
end;

function GetAverageTimePerTick(const ATickers: TTickers; const AStart,
  AEnd: Integer): Integer;
var
  LStart,
  LEnd, I: Integer;
begin
  Result := 0;

  //need at least 2 tickers to have a diff between them
  if ATickers.Count <= 2 then
    Exit;

  //bounds checking on markers
  if AStart < 0 then
    LStart := 0;

  if (AEnd < 0) or (AEnd >= ATickers.Count) then
    LEnd := Pred(ATickers.Count);

  //use result as an aggregate of all time diffs
  Result := MilliSecondsBetween(ATickers[1].Time, ATickers[0].Time);

  for I := Succ(LStart) to LEnd do
    Inc(Result, MilliSecondsBetween(ATickers[I].Time, ATickers[Pred(I)].Time));

  Result := Result div ATickers.Count;
end;

end.

