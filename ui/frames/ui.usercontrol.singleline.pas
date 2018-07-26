unit ui.usercontrol.singleline;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  ui.usercontrol;

type

  { TSingleLine }

  TSingleLine = class(TUserControl)
  private
    function GetText: String;
    procedure SetText(AValue: String);
  protected
    procedure DoInitControls; override;
  public
    property Text : String read GetText write SetText;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  StdCtrls;

{$R *.lfm}

{ TSingleLine }

function TSingleLine.GetText: String;
begin
  Result:=TEdit(Control).Text;
end;

procedure TSingleLine.SetText(AValue: String);
begin
  TEdit(Control).Text:=AValue;
end;

procedure TSingleLine.DoInitControls;
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

constructor TSingleLine.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Control:=TEdit.Create(nil);
end;

end.

