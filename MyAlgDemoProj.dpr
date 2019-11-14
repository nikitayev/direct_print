program MyAlgDemoProj;

uses
  Forms,
  MyAlgDemo in 'MyAlgDemo.pas' {Form1},
  PrinterUtils in 'PrinterUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
