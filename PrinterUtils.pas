{
  Основное правило - всё в сантиметрах
}
unit PrinterUtils;

interface

uses windows, sysutils, graphics, printers, classes, forms,
  Math, Contnrs, U_PaperDimensions;

const
  SI_Pt2Cm = 0.0351; // http://verstka.otrok.ru/law/punkt2.html

type
  TFloatRect = record
    Left, Top, Right, Bottom: double;
  end;

  // http://ru.wikipedia.org/wiki/%D0%A4%D0%BE%D1%80%D0%BC%D0%B0%D1%82_%D0%B1%D1%83%D0%BC%D0%B0%D0%B3%D0%B8
type
  TWmfPage = class(TMetafile)
  private
    FCanvas: TMetafileCanvas;
    FTopMarginCm: double;
    FLeftMarginCm: double;
    FRightMarginCm: double;
    FBottomMarginCm: double;
    FprinterResX: integer;
    FprinterResY: integer;
    FPageWidthInch: double;
    FPageHeightInch: double;
    Foutputarea: TFloatRect;
    FminmarginX: double;
    FminmarginY: double;
    FOrientation: TPrinterOrientation;
    // максимальная позиция по ширине в см при отображении объектов
    FMaxXPositionCM: double;
    // максимальная позиция по высоте в см при отображении объектов
    FMaxYPositionCM: double;
    FWmfOutputRect: TRect;
    FWmfPageSizeInPixels: TRect;
    // максимальная позиция по высоте в см
    procedure SetBottomMarginCm(const Value: double);
    procedure SetCanvas(const Value: TMetafileCanvas);
    procedure SetLeftMarginCm(const Value: double);
    procedure SetRightMarginCm(const Value: double);
    procedure SetTopMarginCm(const Value: double);
    procedure SetminmarginX(const Value: double);
    procedure SetminmarginY(const Value: double);
    procedure Setoutputarea(const Value: TFloatRect);
    procedure Setpageheight(const Value: double);
    procedure Setpagewidth(const Value: double);
    procedure SetprinterResX(const Value: integer);
    procedure SetprinterResY(const Value: integer);
    procedure SetOrientation(const Value: TPrinterOrientation);

    procedure UpdatePageSettings;
  public
    constructor Create; overload;
    constructor Create(
      const LeftMarginCm, TopMarginCm, RightMarginCm, BottomMarginCm: double); overload;
    procedure Preview(Scale: double; DestCanvas: TCanvas);
    procedure GetPreviewDimensions(Scale: double; out Width, Height: integer);
    // самостоятельная печать страницы на текущий принтер
    procedure Print;
    // только отрисовка на канву принтера
    procedure DrawToPrinter;
    // утилита конвертации сантиметров в пиксели
    function Cm2Pixel(const CmValue: double): integer;
    // утилита определения точки с отступом
    function GetPixel(const CmValueX, CmValueY: double): TPoint;
    // утилита пересчёта прямоугольника с отступом
    function GetRectCm2Pixels(const rect: TFloatRect): TRect;
    // утилита преобразования размера экранного шрифта в размеры стандарта СИ (пиксели)
    function TextHeightScreen2PrinterPixel(Height: integer): integer;
    // утилита преобразования размера экранного шрифта в размеры стандарта СИ (сантиметры)
    function TextHeightScreen2PrinterCm(Height: integer): double;
    procedure InitPrintSettings(Orientation: TPrinterOrientation);
    // получить максимальные координаты текста и графики, для последующей разбивки
    function GetMaxDimensionsInCm: TFloatRect;
    destructor Destroy; override;

    // утилиты отрисовки - в сантиметрах с отступом
    procedure Rectangle(const rect: TFloatRect);
    procedure TextOut(const rect: TFloatRect; const S: string);
    procedure FrameRect(const Rect: TFloatRect);

    property Canvas: TMetafileCanvas read FCanvas;
    property Orientation: TPrinterOrientation read FOrientation write SetOrientation;
    property WmfOutputRect: TRect read FWmfOutputRect;
    property WmfPageSizeInPixels: TRect read FWmfPageSizeInPixels;
    property LeftMarginCm: double read FLeftMarginCm write SetLeftMarginCm;
    property TopMarginCm: double read FTopMarginCm write SetTopMarginCm;
    property RightMarginCm: double read FRightMarginCm write SetRightMarginCm;
    property BottomMarginCm: double read FBottomMarginCm write SetBottomMarginCm;
    property outputarea: TFloatRect read Foutputarea write Setoutputarea; {print area in pixels}
    property PageWidthInch: double read FPageWidthInch write Setpagewidth; {printer page dimension in inch}
    property PageHeightInch: double read FPageHeightInch write Setpageheight;
 {printer page dimension in inch}
    property printerResX: integer read FprinterResX write SetprinterResX; {printer resolution in dots/inch}
    property printerResY: integer read FprinterResY write SetprinterResY; {printer resolution in dots/inch}
    property minmarginX: double read FminmarginX write SetminmarginX; {nonprintable margin in inch}
    property minmarginY: double read FminmarginY write SetminmarginY; {nonprintable margin in inch}

  end;

  TWMFPageList = class(TObjectList)
  private
    function GetPages(Index: integer): TWmfPage;
    procedure SetPages(Index: integer; const Value: TWmfPage);
  public
    // 1) предполагается, что выходов за правую границу не будет
    constructor Create(BigPage: TWmfPage);
    property Pages[Index: integer]: TWmfPage read GetPages write SetPages;
  end;

// создание очередной страницы
function MakeNewPage: TWmfPage;
// создание канвы. Освобождать канву ПЕРЕД печатью
function GetPageCanvas(WmfPage: TWmfPage): TMetafileCanvas;
function FloatRect(Left, Top, Right, Bottom: double): TFloatRect;
function GetPixelCountFromInchSize(const Inches: double; PixelPerInch: integer): integer;
function GetPixelCountFromCmSize(const Centimeters: double; PixelPerInch: integer): integer;
procedure GetPaperSizes(PaperSize: TPageSizes; PixelPerInchX, PixelPerInchY: integer;
  out WidthInch, HeightInch: double; out PixelWidth, PixelHeight: double);
procedure SetPaperSize(const Value: TPageSizes);

implementation

function FloatRect(Left, Top, Right, Bottom: double): TFloatRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

// создание очередной страницы
function MakeNewPage: TWmfPage;
begin
  Result := TWmfPage.Create;
end;
// создание канвы. Освобождать канву ПЕРЕД печатью
function GetPageCanvas(WmfPage: TWmfPage): TMetafileCanvas;
begin
  Result := WmfPage.Canvas;
end;

function GetPixelCountFromInchSize(const Inches: double; PixelPerInch: integer): integer;
begin
  result := round(Inches * PixelPerInch);
end;

function GetPixelCountFromCmSize(const Centimeters: double; PixelPerInch: integer): integer;
begin
  result := GetPixelCountFromInchSize(Centimeters * CmPerInch, PixelPerInch);
end;

procedure SetPaperSize(const Value: TPageSizes);
var
  ADevice, ADriver, APort: array [0..255] of char;
  DeviceHandle: THandle;
  DevMode: PDeviceMode;
begin

  {взято из Пачеко, т.1}
  Printer.GetPrinter(ADevice, ADriver, APort, DeviceHandle);
  if DeviceHandle = 0 then
  begin
    Printer.PrinterIndex := Printer.PrinterIndex;
    Printer.GetPrinter(ADevice, ADriver, APort, DeviceHandle);
  end;
  if DeviceHandle = 0 then
    raise Exception.Create('Error setting printer caps!')
  else
    DevMode := GlobalLock(DeviceHandle);

  with DevMode^ do
  begin
    dmFields := dmFields or DM_PAPERSIZE;
    DevMode^.dmPaperSize := Dword(Value)
  end;

  Printer.SetPrinter(ADevice, ADriver, APort, DeviceHandle);

  if not DeviceHandle = 0 then
    GlobalUnlock(DeviceHandle);

  {AdjustImageSize;}
end;

function GetPaperSize: DWORD;
var
  ADevice, ADriver, APort: array [0..255] of char;
  DeviceHandle: THandle;
  DevMode: PDeviceMode;
begin
  Printer.GetPrinter(ADevice, ADriver, APort, DeviceHandle);
  if DeviceHandle = 0 then
  begin
    Printer.PrinterIndex := Printer.PrinterIndex;
    Printer.GetPrinter(ADevice, ADriver, APort, DeviceHandle);
  end;
  if DeviceHandle = 0 then
    raise Exception.Create('Error setting printer caps!')
  else
    DevMode := GlobalLock(DeviceHandle);

  with DevMode^ do
    Result := DevMode^.dmPaperSize;

  if not DeviceHandle = 0 then
    GlobalUnlock(DeviceHandle);
end;

procedure GetPaperSizes(PaperSize: TPageSizes; PixelPerInchX, PixelPerInchY: integer;
  out WidthInch, HeightInch: double; out PixelWidth, PixelHeight: double);
  procedure SetSizeInMM(out Width, Height: double; const WidthCm, HeightCm: double);
  begin
    Width := WidthCm * CmPerInch * 0.1;
    Height := HeightCm * CmPerInch * 0.1;
  end;

var
  zPaperDimension: TPaperSize;
begin
  zPaperDimension := PaperDimensions[PaperSize];
  SetSizeInMM(WidthInch, HeightInch, zPaperDimension.MMWidth, zPaperDimension.MMHeight);

  PixelWidth := PixelPerInchX * WidthInch;
  PixelHeight := PixelPerInchY * HeightInch;
end;


{ TWmfPage }

function TWmfPage.Cm2Pixel(const CmValue: double): integer;
begin
  result := round(CmValue * InchPerCm * printerResX);
end;

constructor TWmfPage.Create(const LeftMarginCm, TopMarginCm, RightMarginCm, BottomMarginCm: double);
begin
  FLeftMarginCm := LeftMarginCm;
  FTopMarginCm := TopMarginCm;
  FRightMarginCm := RightMarginCm;
  FBottomMarginCm := BottomMarginCm;
  FOrientation := Printer.Orientation;
  Create;
end;

constructor TWmfPage.Create;
begin
  inherited Create;
  FMaxXPositionCM := 0;
  FMaxYPositionCM := 0;
  InitPrintSettings(printer.Orientation);
  SetSize(Printer.PageWidth, Printer.PageHeight);
  FCanvas := TMetafileCanvas.Create(Self, 0);
  UpdatePageSettings;
end;

destructor TWmfPage.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TWmfPage.DrawToPrinter;
begin
  with Printer do
  begin
    if FCanvas <> nil then
      FreeAndNil(FCanvas);
    Printer.Canvas.Draw(0, 0, Self);
  end;
end;

procedure TWmfPage.FrameRect(const Rect: TFloatRect);
var
  RectDest: TRect;
begin
  if FCanvas = nil then
    FCanvas := TMetafileCanvas.Create(Self, 0);

  FMaxXPositionCM := max(FMaxXPositionCM, max(rect.Right, rect.Left));
  FMaxYPositionCM := max(FMaxYPositionCM, max(rect.Bottom, rect.Top));

  RectDest := GetRectCm2Pixels(rect);
  FCanvas.FrameRect(RectDest);
end;

function TWmfPage.GetMaxDimensionsInCm: TFloatRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := FMaxXPositionCM;
  Result.Bottom := FMaxYPositionCM;
end;

function TWmfPage.GetPixel(const CmValueX, CmValueY: double): TPoint;
begin
  Result.X := Round((CmValueX + FLeftMarginCm) * InchPerCm * printerResX);
  Result.Y := Round((CmValueY + FTopMarginCm) * InchPerCm * printerResY);
end;

procedure TWmfPage.GetPreviewDimensions(Scale: double; out Width, Height: integer);
begin
  Width := round(Scale * PageWidthInch * Screen.PixelsPerInch);
  Height := round(Scale * PageHeightInch * Screen.PixelsPerInch);
end;

function TWmfPage.GetRectCm2Pixels(const rect: TFloatRect): TRect;
begin
  Result.Left := Round((rect.Left + FLeftMarginCm) * InchPerCm * printerResX);
  Result.Top := Round((rect.Top + FTopMarginCm) * InchPerCm * printerResY);
  Result.Right := Round((rect.Right + FLeftMarginCm) * InchPerCm * printerResX);
  Result.Bottom := Round((rect.Bottom + FTopMarginCm) * InchPerCm * printerResY);
end;

procedure TWmfPage.InitPrintSettings(Orientation: TPrinterOrientation);
  function GetMargin(Margin: double; inX: boolean): double;
  begin
    Result := Margin;
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
  printer.Orientation := Orientation;
  printerResX := GetDeviceCaps(printer.handle, LOGPIXELSX);
  printerResY := GetDeviceCaps(printer.handle, LOGPIXELSY);
  PageWidthInch := GetDeviceCaps(printer.handle, PHYSICALWIDTH) / printerResX;
  PageHeightInch := GetDeviceCaps(printer.handle, PHYSICALHEIGHT) / printerResY;
  minmarginX := GetDeviceCaps(printer.handle, PHYSICALOFFSETX) / printerResX;
  minmarginY := GetDeviceCaps(printer.handle, PHYSICALOFFSETY) / printerResY;
  outputarea := FloatRect(
    GetMargin(InchPerCm * FLeftMarginCm, true),
    GetMargin(InchPerCm * FTopMarginCm, false),
    PageWidthInch - GetMargin(InchPerCm * FRightMarginCm, true),
    PageHeightInch - GetMargin(InchPerCm * FBottomMarginCm, false));
end;

procedure TWmfPage.Preview(Scale: double; DestCanvas: TCanvas);
var
  ScreenRect: TRect;
begin
  // make preview
  ScreenRect := Rect(0, 0, round(PageWidthInch * Screen.PixelsPerInch),
    round(PageHeightInch * Screen.PixelsPerInch));
  if (FCanvas <> nil) then  
    FreeAndNil(FCanvas);
  DestCanvas.StretchDraw(ScreenRect, Self);
  FCanvas := TMetafileCanvas.Create(Self, 0);
end;

procedure TWmfPage.Print;
begin
  with Printer do
  begin
    BeginDoc;
    DrawToPrinter;
    EndDoc;
  end;
end;

procedure TWmfPage.Rectangle(const rect: TFloatRect);
begin
  if FCanvas = nil then
    FCanvas := TMetafileCanvas.Create(Self, 0);

  FMaxXPositionCM := max(FMaxXPositionCM, max(rect.Right, rect.Left));
  FMaxYPositionCM := max(FMaxYPositionCM, max(rect.Bottom, rect.Top));

  FCanvas.Rectangle(GetRectCm2Pixels(rect));
end;

procedure TWmfPage.SetBottomMarginCm(const Value: double);
begin
  FBottomMarginCm := Value;
end;

procedure TWmfPage.SetCanvas(const Value: TMetafileCanvas);
begin
  FCanvas := Value;
end;

procedure TWmfPage.SetLeftMarginCm(const Value: double);
begin
  FLeftMarginCm := Value;
end;

procedure TWmfPage.SetminmarginX(const Value: double);
begin
  FminmarginX := Value;
end;

procedure TWmfPage.SetminmarginY(const Value: double);
begin
  FminmarginY := Value;
end;

procedure TWmfPage.SetOrientation(const Value: TPrinterOrientation);
begin
  FOrientation := Value;
end;

procedure TWmfPage.Setoutputarea(const Value: TFloatRect);
begin
  Foutputarea := Value;
end;

procedure TWmfPage.Setpageheight(const Value: double);
begin
  FPageHeightInch := Value;
end;

procedure TWmfPage.Setpagewidth(const Value: double);
begin
  FPageWidthInch := Value;
end;

procedure TWmfPage.SetprinterResX(const Value: integer);
begin
  FprinterResX := Value;
end;

procedure TWmfPage.SetprinterResY(const Value: integer);
begin
  FprinterResY := Value;
end;

procedure TWmfPage.SetRightMarginCm(const Value: double);
begin
  FRightMarginCm := Value;
end;

procedure TWmfPage.SetTopMarginCm(const Value: double);
begin
  FTopMarginCm := Value;
end;

function TWmfPage.TextHeightScreen2PrinterCm(Height: integer): double;
begin
  // соответствие системе СИ: http://verstka.otrok.ru/law/punkt2.html
  result := Height * 0.0351;
end;

function TWmfPage.TextHeightScreen2PrinterPixel(Height: integer): integer;
begin
  // соответствие системе СИ: http://verstka.otrok.ru/law/punkt2.html
  result := round(TextHeightScreen2PrinterCm(Height) * Cm2Pixel(1));
end;

procedure TWmfPage.TextOut(const rect: TFloatRect; const S: string);
var
  RectDest: TRect;
begin
  if FCanvas = nil then
    FCanvas := TMetafileCanvas.Create(Self, 0);

  FMaxXPositionCM := max(FMaxXPositionCM, max(rect.Right, rect.Left));
  FMaxYPositionCM := max(FMaxYPositionCM, max(rect.Bottom, rect.Top));

  RectDest := GetRectCm2Pixels(rect);
  FCanvas.TextRect(RectDest, RectDest.Left, RectDest.Top, S);
end;

procedure TWmfPage.UpdatePageSettings;
begin
  FPageWidthInch := printer.PageWidth / FprinterResX;
  FPageHeightInch := printer.PageHeight / FprinterResY;

  FWmfPageSizeInPixels := Rect(0, 0, printer.PageWidth, printer.PageHeight);

  FWmfOutputRect := Rect(Cm2Pixel(FLeftMarginCm), Cm2Pixel(FTopMarginCm),
    printer.PageWidth - Cm2Pixel(FRightMarginCm),
    printer.PageHeight - Cm2Pixel(FBottomMarginCm));

end;

{ TWMFPageList }

constructor TWMFPageList.Create(BigPage: TWmfPage);
var
  zFrame: integer;
  zFramesCount: integer;
  zPageDimCM: TFloatRect;
  zPage: TWMFPage;
  zRect: TRect;
begin
  inherited Create(true);
  // определим размер оригинальной площади вывода (может быть больше листа)
  zPageDimCM := BigPage.GetMaxDimensionsInCm;
  zFramesCount := Trunc(1 + zPageDimCM.Bottom * InchPerCm / BigPage.PageHeightInch);
  // прямоугольник текущего формата листа с отступами от края
  zRect := BigPage.WmfOutputRect;
 
  for zFrame := 0 to zFramesCount - 1 do
  begin
    zPage := TWMFPage.Create(BigPage.LeftMarginCm,
      BigPage.TopMarginCm, BigPage.RightMarginCm, BigPage.BottomMarginCm);

    zPage.Canvas.Brush.Style:=bsSolid;
    zPage.Canvas.Pen.Mode:=pmCopy;
    zPage.Canvas.Pen.Style:=psSolid;
    zPage.Canvas.CopyMode := cmSrcCopy;
    
    zPage.Canvas.Draw(0, -printer.PageHeight * zFrame, BigPage);

    inherited Add(zPage);
  end;
end;

function TWMFPageList.GetPages(Index: integer): TWmfPage;
begin
  Result := TWmfPage(inherited Items[Index]);
end;

procedure TWMFPageList.SetPages(Index: integer; const Value: TWmfPage);
begin
  inherited Items[Index] := Value;
end;

end.
