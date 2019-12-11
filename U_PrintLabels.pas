unit U_PrintLabels;

interface

uses Windows, SysUtils, Graphics, Types, Printers, Classes, Math;

procedure DrawLabelsImage(aLabels: TStrings; aPageWidthMM: integer = 210; aPageHeightMM: integer = 297;
  aLabelsInRow: integer = 4; aLabelsInColumn: integer = 10);

implementation

const
  GPagePrecission = 10000;

const
  CmPerInch = 2.54;
  mmPerInch = 25.4;
  InchPerCm = 1.0 / CmPerInch;
  InchPerMM = InchPerCm / 10;

var
  outputarea: TRect; {print area in 1/1000 inches}
  pagewidth, pageheight: double; {printer page dimension in inch}
  printerResX, printerResY: integer; {printer resolution in dots/inch}
  minmarginX, minmarginY: double; {nonprintable margin in inch}

function Cm2Pixel(const CmValue: double; printerResX: integer): double;
begin
  result := CmValue * InchPerCm * printerResX;
end;

function TextHeightScreen2Printer(Height: integer; printerResY: integer): integer;
begin
  // соответствие системе СИ: http://verstka.otrok.ru/law/punkt2.html
  result := round(Height * Cm2Pixel(1, printerResY) * 0.0351);
end;

procedure InitPrintSettings;

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
  Printer.Orientation := poPortrait;

  //Получить разрешение
  printerResX := GetDeviceCaps(printer.handle, LOGPIXELSX);
  printerResY := GetDeviceCaps(printer.handle, LOGPIXELSY);

  //Получение размера листа в мм
  pagewidth := InchPerMM * GetDeviceCaps(printer.handle, HORZSIZE);
  pageheight := InchPerMM * GetDeviceCaps(printer.handle, VERTSIZE);

  minmarginX := GetDeviceCaps(printer.handle, PHYSICALOFFSETX) / printerResX;
  minmarginY := GetDeviceCaps(printer.handle, PHYSICALOFFSETY) / printerResY;
  outputarea.Left := 0;
  outputarea.Top := 0;
  outputarea.Right := Round(pagewidth * GPagePrecission);
  outputarea.Bottom := Round(pageheight * GPagePrecission);
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

procedure DrawLabelsImage(aLabels: TStrings; aPageWidthMM: integer = 210; aPageHeightMM: integer = 297;
  aLabelsInRow: integer = 4; aLabelsInColumn: integer = 10);
var
  coeff: double;
  outputareaprinter: TRect;
  WmfCanvas: TMetafileCanvas;
  i: integer;
  zcoeffx, zcoeffy: double;
  WmfPage: TMetafile;
  dx, dy: Integer;
  x, y: Integer;

  function mm2pixelX(const Xmm: double): integer;
  begin
    result := round(zcoeffx * Xmm);
  end;

  function mm2pixelY(const Ymm: double): integer;
  begin
    result := round(zcoeffy * Ymm);
  end;

begin
  SetPrinterSettings(Printer, aPageWidthMM, aPageHeightMM);
  InitPrintSettings;

  WmfPage := TMetafile.Create;
  WmfPage.SetSize(Printer.PageWidth, Printer.PageHeight);

  try
    coeff := printerResX * (1.0 / GPagePrecission);
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
    WmfCanvas.FillRect(Rect(0, 0, Printer.PageWidth - 1, Printer.PageHeight - 1));

    WmfCanvas.Pen.Color := clBlack;
    WmfCanvas.Pen.Style := psSolid;
    WmfCanvas.Pen.Width := 1;

    zcoeffx := Cm2Pixel(1, printerResX) / 10;
    zcoeffy := Cm2Pixel(1, printerResY) / 10;

    dx := round(mm2pixelX(aPageWidthMM) / aLabelsInRow);
    dy := round(mm2pixelY(aPageHeightMM) / aLabelsInColumn);

    for i := 0 to min(aLabelsInRow*aLabelsInColumn, aLabels.Count) - 1 do
    begin
      x := i mod aLabelsInRow;
      y := i div aLabelsInRow;
      WmfCanvas.Rectangle(Rect(x*dx, y*dy, (x + 1) * dx, (y+1)*dy));
      WmfCanvas.TextOut(x*dx + mm2pixelX(10), y*dy + mm2pixelY(10), aLabels[i]);
    end;
    WmfCanvas.Free;


    // печать
    with Printer do
    begin
      BeginDoc;
      Printer.Canvas.Draw(0, 0, WmfPage);
      EndDoc;
    end;
  finally
    FreeAndNil(WmfPage);
  end;
end;

end.
