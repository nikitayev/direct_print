program DirectPrint;

uses
  Forms,
  printpreview in 'printpreview.pas' {Form1},
  PrinterUtils in 'PrinterUtils.pas',
  FullImagePreview in 'FullImagePreview.pas' {Form2},
  U_PaperDimensions in 'U_PaperDimensions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
