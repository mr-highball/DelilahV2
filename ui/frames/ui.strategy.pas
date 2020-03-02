unit ui.strategy;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  delilah.types,
  fgl;

type
  //forward
  TConfigureStrategy = class;
  TOnSaveStrategy = procedure(const ASender : TConfigureStrategy; const AName : String;
    const AStrategy : IStrategy) of object;

  TNotifyList = TFPGList<TNotifyEvent>;

  { TConfigureStrategy }

  TConfigureStrategy = class(TFrame)
    btn_save: TButton;
    edit_name: TEdit;
    pnl_name: TPanel;
    pnl_ctrls: TPanel;
    scroll_controls: TScrollBox;
    procedure btn_saveClick(Sender: TObject);
  private
    FOnSave: TOnSaveStrategy;
    FStrategy : IStrategy;
    FSubs : TNotifyList;
    function GetStrategy: IStrategy;
    procedure SetStrategy(const AValue: IStrategy);
  protected
    procedure DoReload(const AStrategy : IStrategy); virtual; abstract;
  public
    (*
      event triggered when the save button is clicked
    *)
    property OnSave : TOnSaveStrategy read FOnSave write FOnSave;
    property Strategy : IStrategy read GetStrategy write SetStrategy;

    procedure AddBeforeSaveSubscriber(const ACallback : TNotifyEvent);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TConfigureStrategyClass = class of TConfigureStrategy;

implementation
uses
  Dialogs;

{$R *.lfm}

{ TConfigureStrategy }

procedure TConfigureStrategy.btn_saveClick(Sender: TObject);
var
  I : Integer;
begin
  if edit_name.Text = '' then
  begin
    ShowMessage('Please input a name first.');
    Exit;
  end;

  for I := 0 to Pred(FSubs.Count) do
    try
      if Assigned(FSubs[I]) then
        FSubs[I](Self);
    finally
    end;

  if Assigned(FOnSave) then
  begin
    btn_save.Enabled := False;
    FOnSave(Self, edit_name.Text, FStrategy);
  end;

  if Assigned(Parent) then
    GetParentForm(Self).ModalResult := mrOK;
end;

function TConfigureStrategy.GetStrategy: IStrategy;
begin
  Result := FStrategy;
end;

procedure TConfigureStrategy.SetStrategy(const AValue: IStrategy);
begin
  FStrategy := AValue;
  DoReload(FStrategy);
end;

procedure TConfigureStrategy.AddBeforeSaveSubscriber(
  const ACallback: TNotifyEvent);
begin
  FSubs.Add(ACallback);
end;

constructor TConfigureStrategy.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FSubs := TNotifyList.Create;
  FOnSave := nil;
  FStrategy := nil;
end;

destructor TConfigureStrategy.Destroy;
begin
  FSubs.Free;
  FStrategy := nil;
  inherited Destroy;
end;

end.

