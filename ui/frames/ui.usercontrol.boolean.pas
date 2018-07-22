unit ui.usercontrol.boolean;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  ui.usercontrol, StdCtrls;

type

  { TBool }

  TBool = class(TUserControl)
  private
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
  protected
    procedure DoInitControls; override;
  public
    property Checked : Boolean read GetChecked write SetChecked;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TBool }

function TBool.GetChecked: Boolean;
begin
  Result:=TCheckBox(Control).Checked;
end;

procedure TBool.SetChecked(AValue: Boolean);
begin
  TCheckBox(Control).Checked:=AValue;
end;

procedure TBool.DoInitControls;
begin
  inherited DoInitControls;
  Control.Visible:=True;
  TCheckBox(Control).AutoSize:=True;
  TCheckBox(Control).Caption:='Enabled';
  Control.AnchorVerticalCenterTo(pnl_control);
  Control.AnchorHorizontalCenterTo(pnl_control);
  Options:=[ucTitle,ucDescr,ucAuthor,ucControl];
end;

constructor TBool.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Control:=TCheckBox.Create(nil);
end;

end.

