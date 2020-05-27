unit utilities.filebrowser;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  Dialogs;

type

  { TDirectoryBrowser }
  (*
    browse for a directory and pulls a list of all files within
  *)
  TDirectoryBrowser = class(TFrame)
    btn_browse: TButton;
    btn_load: TButton;
    list_files: TCheckListBox;
    edit_directory: TEdit;
    pnl_controls: TPanel;
    directory_dialog: TSelectDirectoryDialog;
    procedure btn_browseClick(Sender: TObject);
    procedure btn_loadClick(Sender: TObject);
  private
    FLoaded : Boolean;
    function GetFileNames: TStrings;
    function GetIsLoaded: Boolean;

  public
    property IsLoaded : Boolean read GetIsLoaded;
    property FileNames : TStrings read GetFileNames;

    procedure AddCheckedFiles(const AFiles : TStrings);
    procedure LoadFiles(const ADirectory : String = '');

    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
  FileUtil;

{$R *.lfm}

{ TDirectoryBrowser }

procedure TDirectoryBrowser.btn_loadClick(Sender: TObject);
begin
  LoadFiles;
end;

procedure TDirectoryBrowser.btn_browseClick(Sender: TObject);
begin
  if directory_dialog.Execute then
  begin
    edit_directory.Text := directory_dialog.FileName;
    LoadFiles;
  end;
end;

function TDirectoryBrowser.GetFileNames: TStrings;
begin
  Result := list_files.Items;
end;

function TDirectoryBrowser.GetIsLoaded: Boolean;
begin
  Result := FLoaded;
end;

procedure TDirectoryBrowser.AddCheckedFiles(const AFiles: TStrings);
var
  I: Integer;
begin
  for I := 0 to Pred(list_files.Count) do
    if list_files.Checked[I] then
      AFiles.Add(list_files.Items[I]);
end;

procedure TDirectoryBrowser.LoadFiles(const ADirectory: String);
var
  LDir : String;
  LSearch: TListFileSearcher;
  I: Integer;
begin
  list_files.Clear;
  FLoaded := False;

  if ADirectory <> '' then
    LDir := ADirectory
  else
    LDir := edit_directory.Text;

  LSearch := TListFileSearcher.Create(list_files.Items);
  try
    LSearch.Search(LDir, '', False);

    for I := 0 to Pred(list_files.Count) do
      list_files.Checked[I] := True;
  finally
    LSearch.Free;
  end;
end;

constructor TDirectoryBrowser.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLoaded := False;
end;

end.

