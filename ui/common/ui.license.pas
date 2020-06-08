unit ui.license;

{$mode delphi}

interface

(*
  to have a small layer of security we'll take full key/secret and output
  the partial hash
*)
procedure HashCreds(const AApiKey, ASecret : String);

(*
  either challenge succeeds or this terminates the application
*)
procedure ValidateOrKill();

(*
  checks if the license has expired
  a call to HashCreds still needs to be performed even if no call to kill is
*)
function IsLicenseExpired : Boolean;

implementation
uses
  SysUtils,
  Math,
  HlpIHash,
  HlpHashFactory;

const
  LICENSE_KEY = ''; //variable length substring of key
  LICENSE_SECRET = ''; //variable length substring of secret
  LICENSE_EXPIRE = ''; //dd/mm/yyyy

  LICENSE_ERROR = 7056; //license has expired in windows world

var
  PUZZLER : array[0 .. 60] of String;
  PUZZLER_IX,
  PUZZLER_IX_DATE,
  PUZZLER_IX_LICENSE,
  LICENSE_LENGTH: Integer;

procedure HashCreds(const AApiKey, ASecret: String);

  procedure Puzzle;
  var
    I: Integer;
  begin
    PUZZLER_IX := RandomRange(Low(PUZZLER), High(PUZZLER));

    //get an index for the license info
    repeat
      PUZZLER_IX_LICENSE := RandomRange(Low(PUZZLER), High(PUZZLER));
    until PUZZLER_IX_LICENSE <> PUZZLER_IX;

    //get an index for the expiration date
    repeat
      PUZZLER_IX_DATE := RandomRange(Low(PUZZLER), High(PUZZLER));
    until (PUZZLER_IX_DATE <> PUZZLER_IX) and (PUZZLER_IX_DATE <> PUZZLER_IX_LICENSE);

    //obfuuuscattionnnn
    for I := 0 to HIGH(PUZZLER) do
      if (I <> PUZZLER_IX) and (I <> PUZZLER_IX_LICENSE) and (I <> PUZZLER_IX_LICENSE) then
        PUZZLER[I] := IntToHex(RandomRange(1000, 9999), 4);
  end;

  procedure StorePuzzle;
  begin
    PUZZLER[PUZZLER_IX_LICENSE] := LICENSE_KEY + LICENSE_SECRET;
    PUZZLER[PUZZLER_IX_DATE] := LICENSE_EXPIRE;
  end;

begin
  try
    Puzzle;
    StorePuzzle;

    //hash the inputs
    PUZZLER[PUZZLER_IX] := THashFactory
      .THash32
      .CreateBernstein()
      .ComputeString(AApiKey.Substring(0, LICENSE_LENGTH) + ASecret.Substring(0, LICENSE_LENGTH), TEncoding.Default)
      .ToString();
  except
    Halt(LICENSE_ERROR);
  end;
end;

procedure ValidateOrKill();
begin
  try
    if IsLicenseExpired then
      Halt(LICENSE_ERROR);

    //hash the key info
    if THashFactory
      .THash32
      .CreateBernstein()
      .ComputeString(PUZZLER[PUZZLER_IX_LICENSE], TEncoding.Default)
      .ToString() <> PUZZLER[PUZZLER_IX]
    then
      Halt(LICENSE_ERROR);
  except
    Halt;
  end;
end;

function IsLicenseExpired: Boolean;
begin
  //simple date check
  Result := (PUZZLER[PUZZLER_IX_DATE] <> '') and (StrToDate(PUZZLER[PUZZLER_IX_DATE]) <= Now);
end;

initialization
  LICENSE_LENGTH := LICENSE_KEY.Length;
end.

