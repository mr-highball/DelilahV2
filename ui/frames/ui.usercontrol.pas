unit ui.usercontrol;

{$mode delphi}

interface

uses
  Classes, windows, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  (*
    defines what is shown for this user control
  *)
  TUserControlOption = (
    ucTitle,
    ucAuthor,
    ucDescr,
    ucControl
  );

  TUserControlOptions = set of TUserControlOption;

  TUserControl = class;
  TUserChangeEvent = procedure (const AControl : TUserControl) of object;

  { TUserControl }
  (*
    base class for a user defined control type with options to determine
    what is shown
  *)
  TUserControl = class(TFrame)
    lbl_title: TLabel;
    lbl_descr: TLabel;
    lbl_author: TLabel;
    pnl_control: TPanel;
  strict private
    class var FInstance : Integer;
  strict private
    FControl: TControl;
    FOnChange: TUserChangeEvent;
    FOptions: TUserControlOptions;
    FHeightPerc,
    FWidthPerc : Single;
    function GetAuthor: String;
    function GetControl: TControl;
    function GetControlHeight: Single;
    function GetControlWidth: Single;
    function GetDescr: String;
    function GetTitle: String;
    procedure SetAuthor(AValue: String);
    procedure SetControl(AValue: TControl);
    procedure SetControlHeight(AValue: Single);
    procedure SetControlWidth(AValue: Single);
    procedure SetDescr(AValue: String);
    procedure SetOptions(AValue: TUserControlOptions);
    procedure SetTitle(AValue: String);
    procedure ResizeControl(Sender : TObject);
  protected
    procedure Loaded; override;
    procedure DoInitControls;virtual;
    procedure DoUserChange;
  public
    property OnUserChange : TUserChangeEvent read FOnChange write FOnChange;
    property Title : String read GetTitle write SetTitle;
    property Author : String read GetAuthor write SetAuthor;
    property Description : String read GetDescr write SetDescr;
    property Control : TControl read GetControl write SetControl;
    property ControlWidthPercent : Single read GetControlWidth write SetControlWidth;
    property ControlHeightPercent : Single read GetControlHeight write SetControlHeight;
    property Options : TUserControlOptions read FOptions write SetOptions;
    procedure InitControls;
    constructor Create(TheOwner: TComponent); override;
  end;

  TUserControlClass = class of TUserControl;

implementation

{$R *.lfm}

{ TUserControl }

function TUserControl.GetAuthor: String;
begin
  Result:=lbl_author.Caption;
end;

function TUserControl.GetControl: TControl;
begin
  Result:=FControl;
end;

function TUserControl.GetControlHeight: Single;
begin
  Result := FHeightPerc;
end;

function TUserControl.GetControlWidth: Single;
begin
  Result := FWidthPerc;
end;

function TUserControl.GetDescr: String;
begin
  Result:=lbl_descr.Caption;
end;

function TUserControl.GetTitle: String;
begin
  Result:=lbl_title.Caption;
end;

procedure TUserControl.SetAuthor(AValue: String);
begin
  lbl_author.Caption := AValue;
end;

procedure TUserControl.SetControl(AValue: TControl);
begin
  if Assigned(FControl) then
  begin
    FControl.Parent:=nil;
    FControl.Free;
  end;

  FControl:=AValue;

  if Assigned(AValue) then
  begin
    FControl.Parent := pnl_control;
    FControl.Visible := True;
  end;
end;

procedure TUserControl.SetControlHeight(AValue: Single);
begin
  FHeightPerc := AValue;
end;

procedure TUserControl.SetControlWidth(AValue: Single);
begin
  FWidthPerc := AValue;
end;

procedure TUserControl.SetDescr(AValue: String);
begin
  lbl_descr.Caption := AValue;
end;

procedure TUserControl.SetOptions(AValue: TUserControlOptions);
begin
  if FOptions = AValue then
    Exit;

  FOptions := AValue;
  lbl_title.Visible:=False;
  lbl_descr.Visible:=False;
  lbl_author.Visible:=False;
  pnl_control.Visible:=False;

  if FOptions=[] then
    Exit;

  if ucTitle in FOptions then
    lbl_title.Visible:=True;

  if ucDescr in FOptions then
    lbl_descr.Visible:=True;

  if ucAuthor in FOptions then
    lbl_author.Visible:=True;

  if ucControl in FOptions then
  begin
    pnl_control.Visible:=True;

    if Assigned(FControl) then
    begin
      FControl.Parent := pnl_control;
      FControl.Visible := True;
    end;
  end;
end;

procedure TUserControl.SetTitle(AValue: String);
begin
  lbl_title.Caption := AValue;
end;

procedure TUserControl.ResizeControl(Sender: TObject);
begin
  if Assigned(Control) and (FWidthPerc > 0) then
    Control.Width := Round(pnl_control.Width * FWidthPerc);

  if Assigned(Control) and (FHeightPerc > 0) then
    Control.Height := Round(pnl_control.Height * FHeightPerc);
end;

procedure TUserControl.Loaded;
begin
  inherited Loaded;
  InitControls;
end;

procedure TUserControl.DoInitControls;
begin
  Title := '';
  Author := '';
  Description := '';

  if Assigned(FControl) and not (pnl_control.ContainsControl(FControl)) then
  begin
    FControl.Parent:=pnl_control;
    FControl.Visible:=True;
  end;

  //align to the center of the control panel
  if Assigned(FControl) then
  begin
    FControl.AnchorHorizontalCenterTo(pnl_control);
    FControl.AnchorVerticalCenterTo(pnl_control);
  end;

  //default to showing everything
  Options := [ucTitle, ucDescr, ucAuthor, ucControl];
end;

procedure TUserControl.DoUserChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TUserControl.InitControls;
begin
  DoInitControls;
end;

constructor TUserControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  if FInstance < 1 then
    FInstance := 1;

  Name := 'UserControl' + IntToStr(FInstance);
  Inc(FInstance);
  FHeightPerc := 0;
  FWidthPerc := 0;
  pnl_control.AddHandlerOnResize(ResizeControl);
end;

end.

