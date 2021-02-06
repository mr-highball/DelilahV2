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
    procedure EditChange(Sender: TObject);
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

procedure TSingleLine.EditChange(Sender: TObject);
begin
  DoUserChange;
end;

function TSingleLine.GetText: String;
begin
  Result:=TEdit(Control).Text;
end;

procedure TSingleLine.SetText(AValue: String);
begin
  TEdit(Control).Text := AValue;
end;

procedure TSingleLine.DoInitControls;
begin
  inherited;
  Text := '';
end;

constructor TSingleLine.Create(TheOwner: TComponent);
begin
  Control := TEdit.Create(nil);
  TEdit(Control).OnChange := EditChange;
  inherited Create(TheOwner);
end;

end.

