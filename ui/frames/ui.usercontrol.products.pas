unit ui.usercontrol.products;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  ui.usercontrol, ui.product;

type

  { TProducts }

  TProducts = class(TUserControl)
  private
    function GetFrame: TProductFrame;
  protected
    procedure DoInitControls; override;
  public
    property ProductFrame : TProductFrame read GetFrame;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TProducts }

function TProducts.GetFrame: TProductFrame;
begin
  Result:=Control as TProductFrame;
end;

procedure TProducts.DoInitControls;
begin
  inherited DoInitControls;
  Title:='Product Selector';
  Description:='product choice screen with other related settings';
  Options:=[ucTitle,ucDescr,ucControl];
end;

constructor TProducts.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Control:=TProductFrame.Create(nil);
end;

end.

