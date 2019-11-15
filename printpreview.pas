unit printpreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, U_PaperDimensions;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    LeftMarginEdit: TEdit;
    TopMarginEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    RightMarginEdit: TEdit;
    Label5: TLabel;
    BottomMarginEdit: TEdit;
    ApplyMarginsButton: TButton;
    OrientationRGroup: TRadioGroup;
    Label6: TLabel;
    ZoomEdit: TEdit;
    ZoomUpDown: TUpDown;
    btPrint: TButton;
    ScrollBox1: TScrollBox;
    ImagePreview: TImage;
    DPIEdit: TEdit;
    UpDown1: TUpDown;
    Label7: TLabel;
    procedure LeftMarginEditKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure InitPrintSettings;
    procedure btPrintClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ZoomEditChange(Sender: TObject);
    procedure DPIEditChange(Sender: TObject);
  private
    { Private declarations }
    PreviewText: string;
    outputarea: TRect; {print area in 1/1000 inches}
    pagewidth, pageheight: double; {printer page dimension in inch}
    printerResX, printerResY: integer; {printer resolution in dots/inch}
    minmarginX, minmarginY: double; {nonprintable margin in inch}
    WmfPage: TMetafile;
    procedure InitPrintingCanvas(aMonitorDPI, aZOOM: integer);
    procedure DrawDemoImage1();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses printers;

{$R *.DFM}

function Cm2Pixel(const CmValue: double; printerResX: integer): double;
begin
  result := CmValue * InchPerCm * printerResX;
end;

function TextHeightScreen2Printer(Height: integer; printerResY: integer): integer;
begin
  // соответствие системе СИ: http://verstka.otrok.ru/law/punkt2.html
  result := round(Height * Cm2Pixel(1, printerResY) * 0.0351);
end;

procedure TForm1.LeftMarginEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #9, FormatSettings.DecimalSeparator]) then
    Key := #0;
end;

procedure TForm1.ZoomEditChange(Sender: TObject);
begin
  InitPrintingCanvas(StrToIntDef(DPIEdit.Text, 102), StrToIntDef(ZoomEdit.Text, 100));
end;

procedure TForm1.btPrintClick(Sender: TObject);
begin
  with Printer do
  begin
    BeginDoc;
    Margins.Left := 0;
    Margins.Top  := 0;
    Margins.Right := 0;
    Margins.Bottom := 0;
    Printer.Canvas.Draw(0, 0, WmfPage);
    EndDoc;
  end;
end;

procedure TForm1.DPIEditChange(Sender: TObject);
begin
  InitPrintingCanvas(StrToIntDef(DPIEdit.Text, 102), StrToIntDef(ZoomEdit.Text, 100));
end;

procedure TForm1.DrawDemoImage1;
var
  coeff: double;
  outputareaprinter: TRect;
  WmfCanvas: TMetafileCanvas;
begin
  coeff := printerResX * 0.001;
  outputareaprinter := Rect(round(outputarea.Left * coeff),
    round(outputarea.Top * coeff),
    round(outputarea.Right * coeff),
    round(outputarea.Bottom * coeff));

  WmfCanvas := TMetafileCanvas.Create(WmfPage, 0);

    {specify font height in cm}
  WmfCanvas.Font.Name := 'Times New Roman';
  WmfCanvas.Font.Height := TextHeightScreen2Printer(26, printerResY);
    {paint page white}
  WmfCanvas.Brush.Color := RGB(255, 235, 235);
  WmfCanvas.Brush.Style := bsSolid;
  WmfCanvas.FillRect(Rect(0, 0, Printer.PageWidth, Printer.PageHeight));
    {draw the text}
  DrawText(WmfCanvas.handle, pchar(PreviewText), Length(PreviewText),
    outputareaprinter, DT_WORDBREAK or DT_LEFT);
    {Draw thin gray lines to mark borders}
  WmfCanvas.Pen.Color := clGray;
  WmfCanvas.Pen.Style := psSolid;
  WmfCanvas.Pen.Width := 10;

  with WmfCanvas do
  begin
    MoveTo(round(outputareaprinter.left - Cm2Pixel(1, printerResX)), outputareaprinter.top);
    LineTo(round(outputareaprinter.right + Cm2Pixel(1, printerResX)), outputareaprinter.top);
    MoveTo(round(outputareaprinter.left - Cm2Pixel(1, printerResX)), outputareaprinter.bottom);
    LineTo(round(outputareaprinter.right + Cm2Pixel(1, printerResX)), outputareaprinter.bottom);
    MoveTo(outputareaprinter.left, round(outputareaprinter.top - Cm2Pixel(1, printerResX)));
    LineTo(outputareaprinter.left, round(outputareaprinter.bottom + Cm2Pixel(1, printerResX)));
    MoveTo(outputareaprinter.right, round(outputareaprinter.top - Cm2Pixel(1, printerResX)));
    LineTo(outputareaprinter.right, round(outputareaprinter.bottom + Cm2Pixel(1, printerResX)));

    MoveTo(round(outputareaprinter.left + Cm2Pixel(1, printerResX)), outputareaprinter.top +
      round(Cm2Pixel(1, printerResX)));
    LineTo(round(outputareaprinter.left + Cm2Pixel(2, printerResX)), outputareaprinter.top +
      round(Cm2Pixel(1, printerResX)));
    MoveTo(round(outputareaprinter.left + Cm2Pixel(1, printerResX)), outputareaprinter.top +
      round(Cm2Pixel(2, printerResX)));
    LineTo(round(outputareaprinter.left + Cm2Pixel(4, printerResX)), outputareaprinter.top +
      round(Cm2Pixel(2, printerResX)));
    MoveTo(round(outputareaprinter.left + Cm2Pixel(1, printerResX)), outputareaprinter.top +
      round(Cm2Pixel(3, printerResX)));
    LineTo(round(outputareaprinter.left + Cm2Pixel(8, printerResX)), outputareaprinter.top +
      round(Cm2Pixel(3, printerResX)));
  end;
  WmfCanvas.Free;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(WmfPage);
end;

procedure TForm1.FormCreate(Sender: TObject);

  procedure loadpreviewtext;
  var
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      //sl.Loadfromfile(Extractfilepath(application.exename) + 'printpreview.pas');
      sl.Add('/*/*/*/*0123456789--*-*-*-*');
      sl.Add('/*/*/*/*0123456789--*-*-*-*');
      sl.Add('/*/*/*/*0123456789--*-*-*-*');
      sl.Add('/*/*/*/*0123456789--*-*-*-*');
      sl.Add('/*/*/*/*0123456789--*-*-*-*');
      sl.Add('/*/*/*/*0123456789--*-*-*-*');
      PreviewText := sl.Text;
    finally
      sl.free
    end;
  end;

begin
  DoubleBuffered := true;
  {Initialize the margin edits with a margin of 0.75 inch}
  TopMarginEdit.Text := FormatFloat('0.00', 3);
  LeftMarginEdit.Text := FormatFloat('0.00', 2);
  BottomMarginEdit.Text := FormatFloat('0.00', 1.5);
  RightMarginEdit.Text := FormatFloat('0.00', 2);
  {Initialize the orientation radio group}
  if Printer.Orientation = poPortrait then
    OrientationRGroup.ItemIndex := 0
  else
    OrientationRGroup.ItemIndex := 1;
  {load test text for display}
  LoadPreviewtext;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  InitPrintingCanvas(StrToIntDef(DPIEdit.Text, 102), StrToIntDef(ZoomEdit.Text, 100));
end;

procedure TForm1.InitPrintingCanvas(aMonitorDPI, aZOOM: integer);
var
  ScreenRect: TRect;
  zZOOMCoeff: double;
begin
  InitPrintSettings;

  FreeAndNil(WmfPage);
  WmfPage := TMetafile.Create;
  WmfPage.SetSize(Printer.PageWidth, Printer.PageHeight);

  DrawDemoImage1;

  zZOOMCoeff := aZOOM / 100;
  ScreenRect := Rect(0, 0, round(pagewidth * aMonitorDPI * zZOOMCoeff), round(pageheight * aMonitorDPI * zZOOMCoeff));
  ImagePreview.SetBounds(0, 0, ScreenRect.Right, ScreenRect.Bottom);
  ImagePreview.Picture.Bitmap.SetSize(ScreenRect.Right, ScreenRect.Bottom);
  ImagePreview.Picture.Bitmap.Canvas.StretchDraw(ScreenRect, WmfPage);
end;

procedure TForm1.InitPrintSettings;

  function GetMargin(S: string; inX: boolean): double;
  begin
    Result := StrToFloat(S);
    if InX then
    begin
      if Result < minmarginX then
        Result := minmarginX;
    end
    else
    if Result < minmarginY then
      Result := minmarginY;
  end;

begin
  if OrientationRGroup.ItemIndex = 0 then
    Printer.Orientation := poPortrait
  else
    Printer.Orientation := poLandscape;

    //Получить разрешение
  printerResX := GetDeviceCaps(printer.handle, LOGPIXELSX);
  printerResY := GetDeviceCaps(printer.handle, LOGPIXELSY);
    //pagewidth := (GetDeviceCaps(printer.handle, PHYSICALWIDTH)) / printerResX;
    //pageheight := GetDeviceCaps(printer.handle, PHYSICALHEIGHT) / printerResY;
    //Получение размера листа в мм
  pagewidth := InchPerMM * GetDeviceCaps(printer.handle, HORZSIZE);
  pageheight := InchPerMM * GetDeviceCaps(printer.handle, VERTSIZE);

  minmarginX := GetDeviceCaps(printer.handle, PHYSICALOFFSETX) / printerResX;
  minmarginY := GetDeviceCaps(printer.handle, PHYSICALOFFSETY) / printerResY;
  outputarea.Left := Round(InchPerCm * GetMargin(LeftMarginEdit.Text, true) * 1000);
  outputarea.Top := Round(InchPerCm * GetMargin(TopMarginEdit.Text, false) * 1000);
  outputarea.Right := Round((pagewidth - InchPerCm * GetMargin(RightMarginEdit.Text, true)) *
    1000);
  outputarea.Bottom := Round((pageheight - InchPerCm * GetMargin(BottomMarginEdit.Text, false)) * 1000);
end;

end.
