unit ui.usercontrol.singleline;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
    ui.usercontrol;

type

  { TUserControl1 }

  TUserControl1 = class(TUserControl)
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

{ TUserControl1 }

function TUserControl1.GetText: String;
begin
  Result:=TEdit(Control).Text;
end;

procedure TUserControl1.SetText(AValue: String);
begin
  TEdit(Control).Text:=AValue;
end;

procedure TUserControl1.DoInitControls;
begin
  inherited DoInitControls;
  Control.Visible:=True;
  Control.AnchorSide[akLeft].Side:=asrLeft;
  Control.AnchorSide[akLeft].Control:=pnl_control;
  Control.AnchorSide[akRight].Side:=asrLeft;
  Control.AnchorSide[akRight].Control:=pnl_control;
  Options:=[ucTitle,ucDescr,ucAuthor,ucControl];
end;

constructor TUserControl1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Control:=TEdit.Create(nil);
end;

end.

