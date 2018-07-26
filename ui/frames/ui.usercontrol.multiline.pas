unit ui.usercontrol.multiline;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  ui.usercontrol, StdCtrls;

type

  { TMultiLine }

  TMultiLine = class(TUserControl)
  private
    function GetLines: TStrings;
    function GetText: String;
    procedure SetText(AValue: String);
  protected
    procedure DoInitControls; override;
  public
    property Lines : TStrings read GetLines;
    property Text : String read GetText write SetText;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TMultiLine }

function TMultiLine.GetLines: TStrings;
begin
  Result:=TMemo(Control).Lines;
end;

function TMultiLine.GetText: String;
begin
  Result:=TMemo(Control).Text;
end;

procedure TMultiLine.SetText(AValue: String);
begin
  TMemo(Control).Text:=AValue;
end;

procedure TMultiLine.DoInitControls;
begin
  inherited DoInitControls;
  Control.Align:=TAlign.alClient;
  TMemo(Control).ScrollBars:=ssAutoBoth;
  TMemo(Control).WordWrap:=False;
  Options:=[ucTitle,ucDescr,ucAuthor,ucControl];
end;

constructor TMultiLine.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //parent handles memory
  Control:=TMemo.Create(nil);
end;

end.

