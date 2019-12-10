unit PrintLib;

interface

uses Printers, DBGrids, ExtCtrls, Windows, Dialogs,
  Classes, Math, Graphics;

type

  TTabJustify = (tjLeft, tjRight, tjCenter);

  TTab = class
  public
    TabColumnIndex: integer;
    TabPosition: integer;
    TabJustify: TTabJustify;
    TabWidth: integer;
    TabMargin: double;
    TabLines: byte;
    TabShade: byte;
    constructor Create(aColumnIndex, aPosition: integer; aJustify: TTabJustify; aWidth: integer; aMargin: double;
      aLines, aShade: byte);
  end;

  // Para cada formul�rio em que se utilizar o sistema de impress�o gen�rico,
  // chamar esse procedimento para imprimir
procedure PrintSelected(aTitle: string; aDBGrid: TDBGrid; aLogoImage: TImage);

// Imprimi os registros selecionados ou o atual
procedure PrintSelectedRecords;

function CalculateColumnWidth(aCanvas: TCanvas; aCol: TColumn): integer;

function GetTabList(aPrinter: TPrinter): TList;

// Rotinas Comuns
function CalculateFieldNameWidth(aCanvas: TCanvas): integer;
procedure PrintPageNum;
procedure NewLine;
procedure PrintHeader;
procedure PrintDetail(aLineIndex: integer);
procedure PrintFooter;

// Para o leiaute de m�ltiplos registros
procedure PrintTabs(aList: TList; IsHeader: Boolean); overload;
// Para o leiaute de registro �nico
procedure PrintTabs(aList: TList; IsHeader: Boolean; ColumnIndex: integer); overload;

implementation

uses Forms, SysUtils;

var
  DBGrid: TDBGrid;
  LogoImg: TImage;
  Title: string;
  PrintDialog: TPrintDialog;
  MyPrinter: TPrinter;
  TheOwner: TComponent;
  TabList: TList;
  LeftMargin, RightMargin, TopMargin, BottomMargin: integer;
  PrinterRes_X, PrinterRes_Y: integer;
  CurrentX, CurrentY: integer;
  DetailTextHeight: integer;
  MultiRecords: Boolean;

const
  DetailFontSize = 9;
  FieldTitleCaption = 'Campo';
  FieldValueTitleCaption = 'Valor';

constructor TTab.Create(aColumnIndex, aPosition: integer; aJustify: TTabJustify; aWidth: integer; aMargin: double;
  aLines: byte; aShade: byte);
begin
  TabColumnIndex := aColumnIndex;
  TabPosition := aPosition;
  TabJustify := aJustify;
  TabWidth := aWidth;
  TabMargin := aMargin;
  TabLines := aLines;
  TabShade := aShade;
end;

function GetTabList(aPrinter: TPrinter): TList;
var
  theList, innerList: TList;
  I: integer;
  aCol: TColumn;
  x, aColWidth, aColSpace: integer;
  aTab: TTab;
begin
  theList := TList.Create;
  innerList := TList.Create;
  aColSpace := aPrinter.Canvas.TextWidth('_');
  if MultiRecords then
  begin
    x := LeftMargin;
    aPrinter.Canvas.Font.Size := DetailFontSize;
    aPrinter.Canvas.Font.Style := aPrinter.Canvas.Font.Style + [fsBold];
    aPrinter.Canvas.Font.Style := aPrinter.Canvas.Font.Style - [fsBold];
    for I := 0 to DBGrid.Columns.Count - 1 do
    begin
      aCol := DBGrid.Columns[I];
      if aCol.Visible then
      begin
        aColWidth := CalculateColumnWidth(aPrinter.Canvas, aCol);
        if ((x + aColWidth) > RightMargin) then
        begin
          theList.Add(innerList);
          innerList := TList.Create;
          x := LeftMargin + (5 * aColSpace);
          aTab := TTab.Create(I, x, tjLeft, aColWidth, 0, 0, 0);
          innerList.Add(aTab);
        end
        else
        begin
          aTab := TTab.Create(I, x, tjLeft, aColWidth, 0, 0, 0);
          innerList.Add(aTab);
        end;
        x := x + aColWidth + aColSpace;
        if I = (DBGrid.Columns.Count - 1) then
          theList.Add(innerList);
      end;
    end;
  end
  else
  begin
    // Campo
    aColWidth := CalculateFieldNameWidth(aPrinter.Canvas);
    x := LeftMargin;
    aTab := TTab.Create(0, x, tjRight, aColWidth, 0, 0, 0);
    innerList.Add(aTab);
    // Valor
    x := x + aColWidth + aColSpace;
    aColWidth := RightMargin - LeftMargin - aColWidth - aColSpace;
    aTab := TTab.Create(1, x, tjLeft, aColWidth, 0, 0, 0);
    innerList.Add(aTab);
    theList.Add(innerList);
  end;
  Result := theList;
end;

procedure PrintSelected(aTitle: string; aDBGrid: TDBGrid; aLogoImage: TImage);
var
  msg: string;
begin
  Title := aTitle;
  DBGrid := aDBGrid;
  TheOwner := aDBGrid.Owner;
  LogoImg := aLogoImage;
  if DBGrid.SelectedRows.Count > 0 then
  begin
    msg := 'Imprimir ' + IntToStr(DBGrid.SelectedRows.Count) + ' registro(s) selecionado(s) ?';
    if Application.MessageBox(pchar(msg), 'Aviso ao Usu�rio', MB_YESNO + MB_ICONQUESTION) = ID_YES then
    begin
      // Desabilita atualiza��es no DBGrid
      DBGrid.DataSource.DataSet.DisableControls;
      DBGrid.Enabled := False;
      try
        // Dialogo para escolher impressora, etc.
        PrintDialog := TPrintDialog.Create(TheOwner);

        if DBGrid.SelectedRows.Count > 1 then
          Printer.Orientation := poLandscape
        else
          Printer.Orientation := poPortrait;

        // Se o usu�rio confirmou a impressora selecionada imprime...
        if PrintDialog.Execute then
        begin
          // Define a impressora selecionada
          MyPrinter := Printer;

          // Resolu��o da impressora (pontos por polegada)
          PrinterRes_X := GetDeviceCaps(MyPrinter.Handle, LOGPIXELSX);
          PrinterRes_Y := GetDeviceCaps(MyPrinter.Handle, LOGPIXELSY);

          // Define as margens
          LeftMargin := GetDeviceCaps(MyPrinter.Handle, PHYSICALOFFSETX);
          TopMargin := GetDeviceCaps(MyPrinter.Handle, PHYSICALOFFSETY);
          RightMargin := MyPrinter.PageWidth - LeftMargin;
          BottomMargin := MyPrinter.PageHeight - TopMargin;

          // Leiaute gen�rico para n registros;
          // Para um �nico registro, outro leiaute para aproveitar a
          // �rea �til da p�gina
          MultiRecords := (DBGrid.SelectedRows.Count > 1);
          // Imprime os registros selcionados ou o atual
          PrintSelectedRecords;
        end;
      finally
        // Reabilita atualiza��es no DBGrid
        DBGrid.Enabled := True;
        DBGrid.DataSource.DataSet.EnableControls;
      end;
    end;
  end;
end;

procedure PrintTabs(aList: TList; IsHeader: Boolean);
var
  K, x: integer;
  aTab: TTab;
  aTabCol: TColumn;
begin
  MyPrinter.Canvas.Font.Size := DetailFontSize;
  if IsHeader then
    MyPrinter.Canvas.Font.Style := MyPrinter.Canvas.Font.Style + [fsBold];
  for K := 0 to aList.Count - 1 do
  begin
    aTab := aList[K];
    aTabCol := DBGrid.Columns[aTab.TabColumnIndex];
    x := aTab.TabPosition;
    if IsHeader then
      MyPrinter.Canvas.TextOut(x, CurrentY, aTabCol.Title.Caption)
    else // detail -> get field value
      MyPrinter.Canvas.TextOut(x, CurrentY, DBGrid.DataSource.DataSet.FieldByName(aTabCol.FieldName).AsString);
  end;
  if IsHeader then
    MyPrinter.Canvas.Font.Style := MyPrinter.Canvas.Font.Style - [fsBold];
end;

function CalculatePosition(aTab: TTab; aTextToPrint: string): integer;
begin
  Result := LeftMargin;
  if aTab.TabJustify = tjLeft then
    Result := aTab.TabPosition
  else if aTab.TabJustify = tjRight then
    Result := aTab.TabPosition + (aTab.TabWidth - MyPrinter.Canvas.TextWidth(aTextToPrint))
  else if aTab.TabJustify = tjCenter then
    Result := aTab.TabPosition + ((aTab.TabWidth - MyPrinter.Canvas.TextWidth(aTextToPrint)) div 2);
end;

procedure PrintTabs(aList: TList; IsHeader: Boolean; ColumnIndex: integer);
var
  x: integer;
  aTab: TTab;
  aTabCol: TColumn;
  aTextToPrint: string;
begin
  if aList.Count <> 2 then
    Exit;
  MyPrinter.Canvas.Font.Size := DetailFontSize;
  if IsHeader then
    MyPrinter.Canvas.Font.Style := MyPrinter.Canvas.Font.Style + [fsBold];

  // Campo
  aTab := aList[0];
  if IsHeader then
    aTextToPrint := FieldTitleCaption
  else
  begin
    aTabCol := DBGrid.Columns[ColumnIndex];
    aTextToPrint := aTabCol.Title.Caption;
  end;
  x := CalculatePosition(aTab, aTextToPrint);
  MyPrinter.Canvas.TextOut(x, CurrentY, aTextToPrint);

  // Valor
  aTab := aList[1];
  if IsHeader then
    aTextToPrint := FieldValueTitleCaption
  else
  begin
    aTabCol := DBGrid.Columns[ColumnIndex];
    aTextToPrint := DBGrid.DataSource.DataSet.FieldByName(aTabCol.FieldName).AsString;
  end;
  x := CalculatePosition(aTab, aTextToPrint);
  MyPrinter.Canvas.TextOut(x, CurrentY, aTextToPrint);

  if IsHeader then
    MyPrinter.Canvas.Font.Style := MyPrinter.Canvas.Font.Style - [fsBold];
end;

procedure PrintHeader;
var
  x1, y1, x2, y2, aFontSize: integer;
  destWidth, destHeight, I: integer;
  aTab: TTab;
  aTList: TList;
begin
  // Ajusta o tamano da imagem � resolu��o da impressora
  destWidth := LogoImg.Picture.Width * PrinterRes_X div Screen.PixelsPerInch;
  destHeight := LogoImg.Picture.Height * PrinterRes_Y div Screen.PixelsPerInch;

  // Calcula o tamanho da fonte (a ter�a parte do tamanho da imagem do logo)
  aFontSize := Ceil(LogoImg.Picture.Height * 0.3);
  MyPrinter.Canvas.Font.Size := aFontSize;

  // Imprime o t�tulo
  x1 := LeftMargin;
  y1 := TopMargin + (destHeight div 2) - (MyPrinter.Canvas.TextHeight(Title) div 2);
  MyPrinter.Canvas.TextOut(x1, y1, Title);

  // Imprime o logotipo
  x1 := RightMargin - destWidth;
  y1 := TopMargin;
  x2 := x1 + destWidth;
  y2 := y1 + destHeight;
  MyPrinter.Canvas.StretchDraw(Rect(x1, y1, x2, y2), LogoImg.Picture.Bitmap);

  // Tra�a uma linha horizontal
  x1 := LeftMargin;
  y1 := TopMargin + destHeight + 1;
  x2 := RightMargin;
  y2 := y1 + 1;
  MyPrinter.Canvas.Rectangle(x1, y1, x2, y2);

  // Atualiza as coordenadas do cursor
  CurrentX := LeftMargin;
  CurrentY := y2 + 1;

  // Imprime os cabe�alhos das colunas
  for I := 0 to TabList.Count - 1 do
  begin
    aTList := TabList[I];
    if MultiRecords then
      PrintTabs(aTList, True)
    else
      PrintTabs(aTList, True, 0);
    NewLine;
    // Tra�a uma linha horizontal abaixo dos cabe�alhos das colunas
    aTab := (aTList[0]);
    x1 := aTab.TabPosition;
    y1 := CurrentY - 2;
    aTab := (aTList[aTList.Count - 1]);
    x2 := aTab.TabPosition + aTab.TabWidth;
    y2 := y1 + 1;
    MyPrinter.Canvas.Rectangle(x1, y1, x2, y2);
  end;
  // Mant�m uma dist�ncia entre os cabe�alhos e as linhas de detalhe
  CurrentY := CurrentY + (DetailTextHeight div 2);
end;

procedure NewLine;
begin
  CurrentY := CurrentY + DetailTextHeight;
  if (CurrentY >= BottomMargin - (DetailTextHeight * TabList.Count)) then
  begin
    PrintPageNum;
    MyPrinter.NewPage;
    PrintHeader;
  end;
end;

procedure PrintDetail(aLineIndex: integer);
var
  x1, y1, x2, y2, I: integer;
  PreviousBrushColor: TColor;
  aTabList: TList;
  aTab: TTab;
begin
  MyPrinter.Canvas.Font.Size := DetailFontSize;
  PreviousBrushColor := MyPrinter.Canvas.Brush.Color;
  if (aLineIndex mod 2) = 0 then
    MyPrinter.Canvas.Brush.Color := clSilver
  else
    MyPrinter.Canvas.Brush.Color := clWhite;

  for I := 0 to TabList.Count - 1 do
  begin
    aTabList := TabList[I];
    // Alterna a cor do fundo
    aTab := aTabList[0];
    x1 := aTab.TabPosition;
    y1 := CurrentY;
    aTab := aTabList[aTabList.Count - 1];
    x2 := aTab.TabPosition + aTab.TabWidth;
    y2 := y1 + DetailTextHeight;
    MyPrinter.Canvas.FillRect(Rect(x1, y1, x2, y2));
    // Imprime o texto
    if MultiRecords then
      PrintTabs(aTabList, False)
    else
      PrintTabs(aTabList, False, aLineIndex);
    // Proxima linha (exceto a �ltima)
    if I < TabList.Count - 1 then
      NewLine;
  end;
  MyPrinter.Canvas.Brush.Color := PreviousBrushColor;
end;

procedure PrintFooter;
var
  TextToPrint: string;
  x1, y1, x2, y2: integer;
begin
  if MultiRecords then
  begin
    TextToPrint := 'Total de Registros: ' + IntToStr(DBGrid.SelectedRows.Count);
    // Verifica se h� espa�o para imprimir na p�gina corrente
    if (CurrentY + (MyPrinter.Canvas.TextHeight(TextToPrint) * 2 + 4) >= BottomMargin) then
    begin
      PrintPageNum;
      MyPrinter.NewPage;
      PrintHeader;
    end;
    // Tra�a uma linha horizontal
    x1 := LeftMargin;
    y1 := CurrentY;
    x2 := RightMargin;
    y2 := y1 + 1;
    MyPrinter.Canvas.Rectangle(x1, y1, x2, y2);

    MyPrinter.Canvas.Font.Style := MyPrinter.Canvas.Font.Style + [fsBold];
    MyPrinter.Canvas.TextOut(RightMargin - MyPrinter.Canvas.TextWidth(TextToPrint), y2 + 1, TextToPrint);
    MyPrinter.Canvas.Font.Style := MyPrinter.Canvas.Font.Style - [fsBold];
  end;
  MyPrinter.Canvas.Font.Size := DetailFontSize;
  MyPrinter.Canvas.Brush.Color := clWhite;
  TextToPrint := 'Impresso em ' + DateTimeToStr(Now);
  PrintPageNum;
  y1 := BottomMargin - MyPrinter.Canvas.TextHeight(TextToPrint);
  MyPrinter.Canvas.TextOut(LeftMargin, y1, TextToPrint);
end;

procedure PrintSelectedRecords;
var
  I: integer;
begin

  MyPrinter.Canvas.Font.Size := DetailFontSize;

  // Calcula as tabula��es
  TabList := GetTabList(MyPrinter);

  // Calcula a altura da linha de detalhe
  DetailTextHeight := MyPrinter.Canvas.TextHeight('I');
  DetailTextHeight := Ceil(DetailTextHeight * 1.05);

  // Inicia o servi�o de impress�o
  MyPrinter.BeginDoc;
  try
    // Cabe�alho
    PrintHeader;
    // Detalhes
    if MultiRecords then
    begin
      // Todas as linhas selecionadas
      for I := 0 to DBGrid.SelectedRows.Count - 1 do
      begin
        DBGrid.DataSource.DataSet.GotoBookmark(Pointer(DBGrid.SelectedRows.Items[I]));
        PrintDetail(I);
        NewLine;
      end;
    end
    else
    begin
      // Todos os campos do registro corrente
      for I := 0 to DBGrid.Columns.Count - 1 do
      begin
        PrintDetail(I);
        NewLine;
      end;
    end;
    // Final do Relat�rio
    PrintFooter;
    // Finaliza o servi�o de impress�o
    MyPrinter.EndDoc;
  except
    // Qualquer erro aborta a impress�o
    MyPrinter.Abort;
    raise ;
  end;
end;

function CalculateColumnWidth(aCanvas: TCanvas; aCol: TColumn): integer;
var
  columnNameSize, dataSize, thisSize: integer;
  I: integer;
begin
  // Bold para calcular pelo maior tamanho entre o cabe�alho e o detalhe
  aCanvas.Font.Style := aCanvas.Font.Style + [fsBold];
  columnNameSize := aCanvas.TextWidth(aCol.Title.Caption);
  dataSize := 0;
  for I := 0 to DBGrid.SelectedRows.Count - 1 do
  begin
    DBGrid.DataSource.DataSet.GotoBookmark(Pointer(DBGrid.SelectedRows.Items[I]));
    thisSize := aCanvas.TextWidth(Trim(DBGrid.DataSource.DataSet.FieldByName(aCol.FieldName).AsString));
    if thisSize > dataSize then
      dataSize := thisSize;
  end;
  if dataSize > columnNameSize then
    Result := dataSize
  else
    Result := columnNameSize;
  aCanvas.Font.Style := aCanvas.Font.Style - [fsBold];
end;

function CalculateFieldNameWidth(aCanvas: TCanvas): integer;
var
  thisFieldNameSize, maxFieldNameSize: integer;
  I: integer;
  aCol: TColumn;
begin
  maxFieldNameSize := aCanvas.TextWidth(FieldTitleCaption); ;
  for I := 0 to DBGrid.Columns.Count - 1 do
  begin
    aCol := DBGrid.Columns[I];
    if aCol.Visible then
    begin
      thisFieldNameSize := aCanvas.TextWidth(aCol.Title.Caption);
      if thisFieldNameSize > maxFieldNameSize then
      begin
        maxFieldNameSize := thisFieldNameSize;
      end;
    end;
  end;
  Result := maxFieldNameSize;
end;

procedure PrintPageNum;
var
  x1, y1, x2, y2: integer;
  TextToPrint: string;
begin
  MyPrinter.Canvas.Font.Size := DetailFontSize;
  MyPrinter.Canvas.Brush.Color := clWhite;
  TextToPrint := 'P�g. ' + IntToStr(MyPrinter.PageNumber);
  x1 := LeftMargin;
  y1 := BottomMargin - MyPrinter.Canvas.TextHeight(TextToPrint) - 1;
  x2 := RightMargin;
  y2 := y1 + 1;
  MyPrinter.Canvas.Rectangle(x1, y1, x2, y2);
  x1 := RightMargin - MyPrinter.Canvas.TextWidth(TextToPrint);
  y1 := BottomMargin - MyPrinter.Canvas.TextHeight(TextToPrint);
  MyPrinter.Canvas.TextOut(x1, y1, TextToPrint);
end;

end.
