unit FullImagePreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TForm2 = class(TForm)
    ScrollBox1: TScrollBox;
    ImagePreview: TImage;
    procedure FormCreate(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
end;

end.
