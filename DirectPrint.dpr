program DirectPrint;

uses
  Forms,
  printpreview in 'printpreview.pas' {Form1},
  PrinterUtils in 'PrinterUtils.pas',
  U_PaperDimensions in 'U_PaperDimensions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
