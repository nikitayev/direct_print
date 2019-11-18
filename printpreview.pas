unit printpreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, U_PaperDimensions, WinSpool;

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
    Label8: TLabel;
    Label9: TLabel;
    edPaperWidth: TEdit;
    Label10: TLabel;
    edPaperHeight: TEdit;
    procedure LeftMarginEditKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure InitPrintSettings;
    procedure btPrintClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ApplyMarginsButtonClick(Sender: TObject);
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
    procedure DrawDemoImage2();
    procedure DrawDemoImage3();
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

// http://delphimaster.net/view/2-1327473823/all
procedure setPageSize1(Width, Height: smallint);
var
  HPrinter: THandle;
  InfoSize, BytesNeeded: cardinal;
  DevMode: PDeviceMode;
  PI2: PPrinterInfo2;
  PrinterDefaults: TPrinterDefaults;
begin
  with PrinterDefaults do
  begin
    DesiredAccess := PRINTER_ACCESS_USE;
    pDatatype := nil;
    PDEVMODE  := nil;
  end;
  if OpenPrinter(PWideChar(Printer.Printers[Printer.PrinterIndex]), HPrinter, @PrinterDefaults) then
  begin
    try
      SetLastError(0);
      if not GetPrinter(HPrinter, 2, nil, 0, @BytesNeeded) then
      begin
        PI2 := AllocMem(BytesNeeded);
        try
          InfoSize := SizeOf(TPrinterInfo2);
          if GetPrinter(HPrinter, 2, PI2, BytesNeeded, @BytesNeeded) then
          begin
            DevMode := PI2.PDEVMODE;
            DevMode.dmFields := DevMode.dmFields or DM_PAPERSIZE or DM_PAPERWIDTH or DM_PAPERLENGTH;
            DevMode.dmPaperSize := DMPAPER_USER;
            DevMode.dmPaperWidth := Width;
            DevMode.dmPaperLength := Height;
            PI2.pSecurityDescriptor := nil;
            if DocumentProperties(0, HPrinter, PWideChar(Printer.Printers[Printer.PrinterIndex]),
              PI2.PDEVMODE^, PI2.PDEVMODE^,
              DM_IN_BUFFER or DM_OUT_BUFFER) = IDOK then
            begin
              WinSpool.setPrinter(HPrinter, 2, PI2, 0);
            end;
          end;
        finally
          FreeMem(PI2, BytesNeeded);
        end;
      end;
    finally
      ClosePrinter(HPrinter);
    end;
  end;
end;

// http://www.scalabium.com/faq/dct0020.htm
// https://www.youtube.com/watch?v=gX0Zb_VEwNc
procedure SetPrinterSettings(FPrinter: TPrinter; aPaperWidthMM, aPaperHeightMM: integer);
var
  FDevice: pchar;
  FDriver: pchar;
  FPort: pchar;
  DeviceMode: THandle;
  DevMode: PDeviceMode;
begin
  DeviceMode := 0;
  DevMode := nil;
  FDevice := StrAlloc(255);
  FDriver := StrAlloc(255);
  FPort := StrAlloc(255);
  try
    //to get a current printer settings
    FPrinter.GetPrinter(FDevice, FDriver, FPort, DeviceMode);
    //lock a printer device
    DevMode := GlobalLock(DeviceMode);

    //set a paper size as A4-Transverse
    DevMode^.dmFields := DevMode^.dmFields or DM_PAPERSIZE or DM_PAPERLENGTH or DM_PAPERWIDTH or
      DM_PANNINGWIDTH or DM_PANNINGHEIGHT;
    DevMode^.dmPaperSize := DMPAPER_USER;
    DevMode^.dmPaperWidth := aPaperWidthMM * 10;
    DevMode^.dmPaperLength := aPaperHeightMM * 10;
    DevMode^.dmPanningWidth := 0;
    DevMode^.dmPanningHeight := 0;

    {
    //set a paper source as Tractor bin
      DevMode^.dmFields := DevMode^.dmFields or DM_DEFAULTSOURCE;
      DevMode^.dmDefaultSource := DMBIN_TRACTOR;
    }

    //set a Landscape orientation
    {
    begin
      DevMode^.dmFields := DevMode^.dmFields or DM_ORIENTATION;
      DevMode^.dmOrientation := DMORIENT_LANDSCAPE;
    end;
    }

    //set a printer settings
    FPrinter.SetPrinter(FDevice, FDriver, FPort, DeviceMode);

    //unlock a device
    GlobalUnlock(DeviceMode);
  finally
    StrDispose(FDevice);
    StrDispose(FDriver);
    StrDispose(FPort);
  end;
end;

procedure TForm1.LeftMarginEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #9, FormatSettings.DecimalSeparator]) then
    Key := #0;
end;

procedure TForm1.ApplyMarginsButtonClick(Sender: TObject);
begin
  SetPrinterSettings(Printer, StrToIntDef(edPaperWidth.Text, 210), StrToIntDef(edPaperHeight.Text, 297));
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

procedure TForm1.DrawDemoImage2;
var
  coeff: double;
  outputareaprinter: TRect;
  WmfCanvas: TMetafileCanvas;
  x, y:  integer;
  countx, county: integer;
  zCMX, zCMY: integer;
begin
  coeff := printerResX * 0.001;
  outputareaprinter := Rect(round(outputarea.Left * coeff),
    round(outputarea.Top * coeff),
    round(outputarea.Right * coeff),
    round(outputarea.Bottom * coeff));

  WmfCanvas := TMetafileCanvas.Create(WmfPage, 0);

    {specify font height in cm}
  WmfCanvas.Font.Name := 'Times New Roman';
  WmfCanvas.Font.Height := TextHeightScreen2Printer(10, printerResY);
    {paint page white}
  WmfCanvas.Brush.Color := RGB(255, 255, 255);
  WmfCanvas.Brush.Style := bsSolid;
  WmfCanvas.FillRect(Rect(0, 0, Printer.PageWidth, Printer.PageHeight));

  WmfCanvas.Pen.Color := clGray;
  WmfCanvas.Pen.Style := psSolid;
  WmfCanvas.Pen.Width := 10;

  zCMX := round(Cm2Pixel(2, printerResX));
  zCMY := round(Cm2Pixel(1, printerResY));
  countx := round(Printer.PageWidth / zCMX);
  county := round(Printer.PageHeight / zCMY);

  for x := 0 to countx - 1 do
    for y := 0 to county - 1 do
    begin
     {draw the text}
      WmfCanvas.TextOut(round(x * zCMX + zCMX * 0.1), round(y * zCMY + zCMY * 0.1), Format('%d, %d', [x, y]));
      with WmfCanvas do
      begin
        MoveTo(zCMX * x, zCMY * (y + 1));
        LineTo(zCMX * (x + 1), zCMY * (y + 1));
        MoveTo(zCMX * (x + 1), zCMY * y);
        LineTo(zCMX * (x + 1), zCMY * (y + 1));
      end;
    end;
  WmfCanvas.Free;
end;

procedure TForm1.DrawDemoImage3;
var
  coeff: double;
  outputareaprinter: TRect;
  WmfCanvas: TMetafileCanvas;
  x, y:  integer;
  countx, county: integer;
  zCMX, zCMY: integer;
begin
  coeff := printerResX * 0.001;
  outputareaprinter := Rect(round(outputarea.Left * coeff),
    round(outputarea.Top * coeff),
    round(outputarea.Right * coeff),
    round(outputarea.Bottom * coeff));

  WmfCanvas := TMetafileCanvas.Create(WmfPage, 0);

    {specify font height in cm}
  WmfCanvas.Font.Name := 'Times New Roman';
  WmfCanvas.Font.Height := TextHeightScreen2Printer(10, printerResY);
    {paint page white}
  WmfCanvas.Brush.Color := RGB(255, 255, 255);
  WmfCanvas.Brush.Style := bsSolid;
  WmfCanvas.FillRect(Rect(0, 0, Printer.PageWidth, Printer.PageHeight));

  WmfCanvas.Pen.Color := clGray;
  WmfCanvas.Pen.Style := psSolid;
  WmfCanvas.Pen.Width := 10;

  zCMX := round(Cm2Pixel(1, printerResX));
  zCMY := round(Cm2Pixel(0.5, printerResY));
  countx := round(Printer.PageWidth / zCMX);
  county := round(Printer.PageHeight / zCMY);

  for x := 0 to countx - 1 do
    for y := 0 to county - 1 do
    begin
     {draw the text}
      WmfCanvas.TextOut(round(x * zCMX + zCMX * 0.1), round(y * zCMY + zCMY * 0.1), Format('%d, %d', [x, y]));
      with WmfCanvas do
      begin
        MoveTo(zCMX * x, zCMY * (y + 1));
        LineTo(zCMX * (x + 1), zCMY * (y + 1));
        MoveTo(zCMX * (x + 1), zCMY * y);
        LineTo(zCMX * (x + 1), zCMY * (y + 1));
      end;
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
  //SetPrinterSettings(Printer);
  //setPageSize1(5, 1);
  {load test text for display}
  LoadPreviewtext;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SetPrinterSettings(Printer, StrToIntDef(edPaperWidth.Text, 210), StrToIntDef(edPaperHeight.Text, 297));
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

  //DrawDemoImage1;
  DrawDemoImage2;

  zZOOMCoeff := aZOOM / 100;
  ScreenRect := Rect(0, 0, round(pagewidth * aMonitorDPI * zZOOMCoeff), round(pageheight * aMonitorDPI * zZOOMCoeff));
  edPaperWidth.Text := IntToStr(round(pagewidth * mmPerInch));
  edPaperHeight.Text := IntToStr(round(pageheight * mmPerInch));

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
