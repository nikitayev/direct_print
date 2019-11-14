unit printpreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, U_PaperDimensions;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PreviewPaintbox: TPaintBox;
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
    procedure ZoomUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure OrientationRGroupClick(Sender: TObject);
    procedure LeftMarginEditKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure PreviewPaintboxPaint(Sender: TObject);
    procedure ApplyMarginsButtonClick(Sender: TObject);
    procedure btPrintClick(Sender: TObject);
  Private
    { Private declarations }
    PreviewText: String;
    outputarea:  TRect; {print area in 1/1000 inches}
    pagewidth, pageheight: Double; {printer page dimension in inch}
    printerResX, printerResY: Integer; {printer resolution in dots/inch}
    minmarginX, minmarginY: Double; {nonprintable margin in inch}
  Public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses printers, FullImagePreview;

{$R *.DFM}

procedure TForm1.LeftMarginEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Not (Key In ['0'..'9', #9, DecimalSeparator]) then
    Key := #0;
end;

procedure TForm1.OrientationRGroupClick(Sender: TObject);
begin
  PreviewPaintbox.Invalidate;
end;

procedure TForm1.btPrintClick(Sender: TObject);
var
  coeff: Double;
  outputareaprinter: TRect;
  WmfPage: TMetafile;
  WmfCanvas: TMetafileCanvas;
  ScreenRect:TRect;

  function Cm2Pixel(CmValue: Double): Double;
  begin
    result := CmValue * InchInCm * printerResX;
  end;

  function TextHeightScreen2Printer(Height: Integer): Integer;
  begin
    // соответствие системе СИ: http://verstka.otrok.ru/law/punkt2.html
    result := round(Height*Cm2Pixel(1)*0.0351);
  end;

begin
  PreviewPaintbox.OnPaint := Nil;
  coeff := printerResX * 0.001;
  outputareaprinter := Rect(round(outputarea.Left * coeff),
    round(outputarea.Top * coeff),
    round(outputarea.Right * coeff),
    round(outputarea.Bottom * coeff));

  WmfPage := TMetafile.Create;
  WmfPage.SetSize(Printer.PageWidth, Printer.PageHeight);
  WmfCanvas := TMetafileCanvas.Create(WmfPage, 0);
  try

    {specify font height in cm}
    WmfCanvas.Font.Name := 'Times New Roman';
    WmfCanvas.Font.Height := TextHeightScreen2Printer(28);
    {paint page white}
    WmfCanvas.Brush.Color := RGB(255, 235, 235);
    WmfCanvas.Brush.Style := bsSolid;
    WmfCanvas.FillRect(Rect(0, 0, Printer.PageWidth, Printer.PageHeight));
    {draw the text}
    DrawText(WmfCanvas.handle, Pchar(PreviewText), Length(PreviewText),
      outputareaprinter, DT_WORDBREAK Or DT_LEFT);
    {Draw thin gray lines to mark borders}
    WmfCanvas.Pen.Color := clGray;
    WmfCanvas.Pen.Style := psSolid;
    WmfCanvas.Pen.Width := 10;

    with WmfCanvas do
    begin
      MoveTo(round(outputareaprinter.left - Cm2Pixel(1)), outputareaprinter.top);
      LineTo(round(outputareaprinter.right + Cm2Pixel(1)), outputareaprinter.top);
      MoveTo(round(outputareaprinter.left - Cm2Pixel(1)), outputareaprinter.bottom);
      LineTo(round(outputareaprinter.right + Cm2Pixel(1)), outputareaprinter.bottom);
      MoveTo(outputareaprinter.left, round(outputareaprinter.top - Cm2Pixel(1)));
      LineTo(outputareaprinter.left, round(outputareaprinter.bottom + Cm2Pixel(1)));
      MoveTo(outputareaprinter.right, round(outputareaprinter.top - Cm2Pixel(1)));
      LineTo(outputareaprinter.right, round(outputareaprinter.bottom + Cm2Pixel(1)));
    end;
    WmfCanvas.Free;

    // make preview
    ScreenRect:=Rect(0,0,round(pagewidth*Screen.PixelsPerInch),round(pageheight*Screen.PixelsPerInch));
    Form2.ImagePreview.SetBounds(0, 0, ScreenRect.Right, ScreenRect.Bottom);
    Form2.ImagePreview.Picture.Bitmap.SetSize(ScreenRect.Right, ScreenRect.Bottom);
    Form2.ImagePreview.Picture.Bitmap.Canvas.StretchDraw(ScreenRect, WmfPage);

    with Printer do
    begin
      BeginDoc;
      Printer.Canvas.Draw(0, 0, WmfPage);
      EndDoc;
    end;

    Form2.Show;

  finally
    PreviewPaintbox.OnPaint := PreviewPaintboxPaint;
    WmfPage.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  S: String;

  procedure loadpreviewtext;
  var
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      sl.Loadfromfile(Extractfilepath(application.exename) + 'printpreview.pas');
      PreviewText := sl.Text;
    finally
      sl.free
    end;
  end;

begin
  DoubleBuffered := True;
  {Initialize the margin edits with a margin of 0.75 inch}
  S := FormatFloat('0.00', 0.75);
  LeftMarginEdit.Text := S;
  TopMarginEdit.Text := S;
  RightMarginEdit.Text := S;
  BottomMarginEdit.Text := S;
  {Initialize the orientation radio group}
  if Printer.Orientation = poPortrait then
    OrientationRGroup.ItemIndex := 0
  else
    OrientationRGroup.ItemIndex := 1;
  {load test text for display}
  LoadPreviewtext;
end;

procedure TForm1.PreviewPaintboxPaint(Sender: TObject);
var
  scale: Double; {conversion factor, pixels per 1/1000 inch}

  procedure InitPrintSettings;
    function GetMargin(S: String; inX: Boolean): Double;
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
    printerResX := GetDeviceCaps(printer.handle, LOGPIXELSX);
    printerResY := GetDeviceCaps(printer.handle, LOGPIXELSY);
    pagewidth := GetDeviceCaps(printer.handle, PHYSICALWIDTH) / printerResX;
    pageheight := GetDeviceCaps(printer.handle, PHYSICALHEIGHT) / printerResY;
    minmarginX := GetDeviceCaps(printer.handle, PHYSICALOFFSETX) / printerResX;
    minmarginY := GetDeviceCaps(printer.handle, PHYSICALOFFSETY) / printerResY;
    outputarea.Left := Round(GetMargin(LeftMarginEdit.Text, True) * 1000);
    outputarea.Top := Round(GetMargin(TopMarginEdit.Text, False) * 1000);
    outputarea.Right := Round((pagewidth - GetMargin(RightMarginEdit.Text, True)) *
      1000);
    outputarea.Bottom := Round((pageheight - GetMargin(BottomMarginEdit.Text, False)) * 1000);
  end;

  procedure ScaleCanvas(Canvas: TCanvas; widthavail, heightavail: Integer);
  var
    needpixelswidth, needpixelsheight: Integer;
    {dimensions of preview at current zoom factor in pixels}
    orgpixels: TPoint;
    {origin of preview in pixels}
  begin
    {set up a coordinate system for the canvas that uses 1/1000 inch as unit,
    honors the zoom factor and maintains the MM_TEXT orientation of the
    coordinate axis (origin in top left corner, positive Y axis points down}
    scale := Screen.PixelsPerInch / 1000;
    {Apply zoom factor}
    scale := scale * StrToInt(Zoomedit.text) / 100;
    {figure out size of preview}
    needpixelswidth := Round(pagewidth * 1000 * scale);
    needpixelsheight := Round(pageheight * 1000 * scale);
    if needpixelswidth >= widthavail then
      orgpixels.X := 0
    else
      orgpixels.X := (widthavail - needpixelswidth) Div 2;
    if needpixelsheight >= heightavail then
      orgpixels.Y := 0
    else
      orgpixels.Y := (heightavail - needpixelsheight) Div 2;
    {change mapping mode to MM_ISOTROPIC}
    SetMapMode(canvas.handle, MM_ISOTROPIC);
    {move viewport origin to orgpixels}
    SetViewportOrgEx(canvas.handle, orgpixels.x, orgpixels.y, Nil);
    {scale the window}
    SetViewportExtEx(canvas.handle, Round(1000 * scale), Round(1000 * scale), Nil);
    SetWindowExtEx(canvas.handle, 1000, 1000, Nil);
  end;

begin
  if OrientationRGroup.ItemIndex = 0 then
    Printer.Orientation := poPortrait
  else
    Printer.Orientation := poLandscape;
  InitPrintsettings;
  with Sender As TPaintBox do
  begin
    ScaleCanvas(Canvas, ClientWidth, ClientHeight);
    Canvas.Font.Name := 'Times New Roman';
    {specify font height in 1/1000 inch}
    Canvas.Font.Height := Round((font.height * (10.6/12)) / font.pixelsperinch * 1000);
    {paint page white}
    Canvas.Brush.Color := clWindow;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect(0, 0, Round(pagewidth * 1000), Round(pageheight * 1000)));
    {draw the text}
    DrawText(canvas.handle, Pchar(PreviewText), Length(PreviewText),
      outputarea, DT_WORDBREAK Or DT_LEFT);
    {Draw thin gray lines to mark borders}
    Canvas.Pen.Color := clGray;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 10;
    with Canvas do
    begin
      MoveTo(outputarea.left - 100, outputarea.top);
      LineTo(outputarea.right + 100, outputarea.top);
      MoveTo(outputarea.left - 100, outputarea.bottom);
      LineTo(outputarea.right + 100, outputarea.bottom);
      MoveTo(outputarea.left, outputarea.top - 100);
      LineTo(outputarea.left, outputarea.bottom + 100);
      MoveTo(outputarea.right, outputarea.top - 100);
      LineTo(outputarea.right, outputarea.bottom + 100);
    end;
  end;
end;

procedure TForm1.ZoomUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  PreviewPaintbox.Invalidate;
end;

procedure TForm1.ApplyMarginsButtonClick(Sender: TObject);
begin
  PreviewPaintbox.Invalidate;
end;

end.