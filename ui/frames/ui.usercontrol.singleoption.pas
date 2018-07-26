unit ui.usercontrol.singleoption;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  ui.usercontrol;

type

  { TSingleOption }

  TSingleOption = class(TUserControl)
  private
    function GetOptions: TStrings;
  protected
    procedure DoInitControls; override;
  public
    property Options : TStrings read GetOptions;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  StdCtrls;

{$R *.lfm}

{ TSingleOption }

function TSingleOption.GetOptions: TStrings;
begin
  Result:=TComboBox(Control).Items;
end;

procedure TSingleOption.DoInitControls;
begin
  inherited DoInitControls;
  Control.Visible:=True;
  Control.AnchorSide[akLeft].Side:=asrLeft;
  Control.AnchorSide[akLeft].Control:=pnl_control;
  Control.AnchorSide[akRight].Side:=asrRight;
  Control.AnchorSide[akRight].Control:=pnl_control;
  Control.Anchors:=Control.Anchors + [akLeft,akRight];
  Options:=[ucTitle,ucDescr,ucAuthor,ucControl];
end;

constructor TSingleOption.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Control:=TComboBox.Create(nil);
end;

end.

