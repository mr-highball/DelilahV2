unit ui.usercontrol;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

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
    FControl: TControl;
    FOptions: TUserControlOptions;
    function GetAuthor: String;
    function GetControl: TControl;
    function GetDescr: String;
    function GetTitle: String;
    procedure SetAuthor(AValue: String);
    procedure SetControl(AValue: TControl);
    procedure SetDescr(AValue: String);
    procedure SetOptions(AValue: TUserControlOptions);
    procedure SetTitle(AValue: String);
  protected
    procedure Loaded; override;
    procedure DoInitControls;virtual;
  public
    property Title : String read GetTitle write SetTitle;
    property Author : String read GetAuthor write SetAuthor;
    property Description : String read GetDescr write SetDescr;
    property Control : TControl read GetControl write SetControl;
    property Options : TUserControlOptions read FOptions write SetOptions;
    procedure InitControls;
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
  lbl_author.Caption:=AValue;
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
    FControl.Parent:=pnl_control;
    FControl.Visible:=True;
  end;
end;

procedure TUserControl.SetDescr(AValue: String);
begin
  lbl_descr.Caption:=AValue;
end;

procedure TUserControl.SetOptions(AValue: TUserControlOptions);
begin
  if FOptions=AValue then
    Exit;
  FOptions:=AValue;
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
    pnl_control.Visible:=True;
end;

procedure TUserControl.SetTitle(AValue: String);
begin
  lbl_title.Caption:=AValue;
end;

procedure TUserControl.Loaded;
begin
  inherited Loaded;
  InitControls;
end;

procedure TUserControl.DoInitControls;
begin
  Title:='';
  Author:='';
  Description:='';
  if Assigned(FControl) and not (pnl_control.ContainsControl(FControl)) then
  begin
    FControl.Parent:=pnl_control;
    FControl.Visible:=True;
  end;
  //default to showing nothing (allow children to define)
  Options:=[];
end;

procedure TUserControl.InitControls;
begin
  DoInitControls;
end;

end.

