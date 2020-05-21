unit ui.usercontrol.slider.control;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls;

type

  { TSliderControl }

  TSliderControl = class(TFrame)
    lbl_left_descr: TLabel;
    lbl_right_descr: TLabel;
    trackbar: TTrackBar;
  public

  end;

implementation

{$R *.lfm}

end.

