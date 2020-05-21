unit ui.usercontrol.slider;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ui.usercontrol,
  ui.usercontrol.slider.control;

type

  { TSlider }

  TSlider = class(TUserControl)
  strict private
    function GetMax: Integer;
    function GetMaxDescr: String;
    function GetMin: Integer;
    function GetMinDescr: String;
    function GetValue: Integer;
    procedure SetMax(const AValue: Integer);
    procedure SetMaxDescr(const AValue: String);
    procedure SetMin(const AValue: Integer);
    procedure SetMinDescr(const AValue: String);
    procedure SetValue(const AValue: Integer);
  protected
    procedure DoInitControls; override;
  public
    property MinValue : Integer read GetMin write SetMin;
    property MaxValue : Integer read GetMax write SetMax;
    property Value : Integer read GetValue write SetValue;
    property MinDescr : String read GetMinDescr write SetMinDescr;
    property MaxDescr : String read GetMaxDescr write SetMaxDescr;

    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TSlider }

function TSlider.GetMax: Integer;
begin
  Result := TSliderControl(Control).trackbar.Max;
end;

function TSlider.GetMaxDescr: String;
begin
  if not Assigned(Control) then
    Exit;

  Result := TSliderControl(Control).lbl_right_descr.Caption;
end;

function TSlider.GetMin: Integer;
begin
  if not Assigned(Control) then
    Exit;

  Result := TSliderControl(Control).trackbar.Min;
end;

function TSlider.GetMinDescr: String;
begin
  if not Assigned(Control) then
    Exit;

  Result := TSliderControl(Control).lbl_left_descr.Caption;
end;

function TSlider.GetValue: Integer;
begin
  if not Assigned(Control) then
    Exit;

  Result := TSliderControl(Control).trackbar.Position;
end;

procedure TSlider.SetMax(const AValue: Integer);
begin
  if not Assigned(Control) then
    Exit;

  TSliderControl(Control).trackbar.Max := AValue;
end;

procedure TSlider.SetMaxDescr(const AValue: String);
begin
  if not Assigned(Control) then
    Exit;

  TSliderControl(Control).lbl_right_descr.Caption := AValue;
  TSliderControl(Control).lbl_right_descr.Visible := Trim(AValue) <> '';
end;

procedure TSlider.SetMin(const AValue: Integer);
begin
  if not Assigned(Control) then
    Exit;

  TSliderControl(Control).trackbar.Min := AValue;
end;

procedure TSlider.SetMinDescr(const AValue: String);
begin
  TSliderControl(Control).lbl_left_descr.Caption := AValue;
  TSliderControl(Control).lbl_left_descr.Visible := Trim(AValue) <> '';
end;

procedure TSlider.SetValue(const AValue: Integer);
begin
  TSliderControl(Control).trackbar.Position := AValue;
end;

procedure TSlider.DoInitControls;
begin
  inherited DoInitControls;
  MinValue := 0;
  MaxValue := 100;
end;

constructor TSlider.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Control := TSliderControl.Create(nil);
end;

end.

