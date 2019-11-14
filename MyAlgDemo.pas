unit MyAlgDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, PrinterUtils, StdCtrls, JvBaseDlg, JvPageSetup, printers;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    PreviewImage: TImage;
    Button1: TButton;
    JvPageSetupDialog1: TJvPageSetupDialog;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    PreviewText: TStringList;
  public
    { Public declarations }
    Page: TWmfPage;
    procedure RefreshData;
  end;

var
  Form1: TForm1;

implementation

uses Types;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Page.Print;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if JvPageSetupDialog1.Execute then
  begin
    RefreshData;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: integer;
  PageList: TWMFPageList;
begin
  PageList := TWMFPageList.Create(Page);
  try
    Printer.BeginDoc;
    for I := 0 to PageList.Count - 2 do
    begin
      PageList.Pages[i].DrawToPrinter;
      Printer.NewPage;
    end;
    PageList.Pages[PageList.Count - 1].DrawToPrinter;

  finally
    Printer.EndDoc;
    FreeAndNil(PageList);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Page.Preview(1, PreviewImage.Picture.Bitmap.Canvas);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Page.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RefreshData;
end;

procedure TForm1.RefreshData;
var
  i: integer;
  rect: TFloatRect;
  PPWidth, PPHeight: integer;

  procedure loadpreviewtext;
  begin
    PreviewText := TStringList.Create;
    PreviewText.Loadfromfile(Extractfilepath(application.exename) + 'printpreview.pas');
  end;

begin
  loadpreviewtext;
  if (Page <> nil) then
    FreeAndNil(Page);
  Page := TWmfPage.Create;
  Page.LeftMarginCm := 0;
  Page.RightMarginCm := 0;
  Page.TopMarginCm := 0;
  Page.BottomMarginCm := 0;
  Page.InitPrintSettings(printer.Orientation);
  rect := FloatRect(0, 0, 10, 0.5);
  Page.Canvas.Font.Name := 'Times New Roman';
  Page.Canvas.Font.Color := clBlack;
  Page.Canvas.Font.Height := Page.TextHeightScreen2PrinterPixel(12);
  Page.Canvas.Brush.Style := bsSolid;
  Page.Canvas.Pen.Mode := pmCopy;
  Page.Canvas.Pen.Style := psSolid;
  Page.Canvas.Pen.Color := clBlack;
  for I := 0 to PreviewText.Count - 1 do
  begin
    Page.Canvas.Brush.Color := RGB(180 + random(75), 180 + random(75), 180 + random(75));
    Page.Rectangle(rect);
    Page.TextOut(rect, PreviewText[i]);
    Page.Canvas.Brush.Color := clBlack;
    Page.FrameRect(rect);
    rect.Top := rect.Top + 0.5;
    rect.Bottom := rect.Bottom + 0.5;
  end;
  Page.GetPreviewDimensions(1, PPWidth, PPHeight);
  PreviewImage.SetBounds(0, 0, PPWidth, PPHeight);
  PreviewImage.Picture.Bitmap.SetSize(PPWidth, PPHeight);
  Page.Preview(1, PreviewImage.Picture.Bitmap.Canvas);
end;

end.
