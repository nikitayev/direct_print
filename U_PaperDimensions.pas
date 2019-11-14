unit U_PaperDimensions;

interface

uses windows;

const
  CmPerInch = 2.54;
  mmPerInch = 25.4;
  InchPerCm = 1.0 / CmPerInch;
  InchPerMM = InchPerCm / 10;

type

  TPageSizes = (DMPAPER_UNKNOWN, DMPAPER_LETTER, DMPAPER_LETTERSMALL,
    DMPAPER_TABLOID, DMPAPER_LEDGER, DMPAPER_LEGAL,
    DMPAPER_STATEMENT, DMPAPER_EXECUTIVE,
    DMPAPER_A3, DMPAPER_A4, DMPAPER_A4SMALL, DMPAPER_A5,
    DMPAPER_B4, DMPAPER_B5, DMPAPER_FOLIO, DMPAPER_QUARTO,
    DMPAPER_10X14, DMPAPER_11X17, DMPAPER_NOTE,
    DMPAPER_A2 = 66,
    DMPAPER_A3_ROTATED = 76,
    DMPAPER_A4_ROTATED = 77,
    DMPAPER_A5_ROTATED = 78
    );

  TPaperSize = record
    MMWidth, MMHeight: double;
  end;

  TPaperDimensions = array [TPageSizes] of TPaperSize;

var
  PaperDimensions: TPaperDimensions =
    (
    (MMWidth: 10; MMHeight: 10), // DMPAPER_UNKNOWN     0
    (MMWidth: 8.12 * mmPerInch; MMHeight: 11 * mmPerInch), // DMPAPER_LETTER      1
    (MMWidth: 8.12 * mmPerInch; MMHeight: 11 * mmPerInch), // DMPAPER_LETTERSMALL 2
    (MMWidth: 11 * mmPerInch; MMHeight: 17 * mmPerInch), // DMPAPER_TABLOID     3
    (MMWidth: 17 * mmPerInch; MMHeight: 11 * mmPerInch), // DMPAPER_LEDGER      4
    (MMWidth: 8.12 * mmPerInch; MMHeight: 14 * mmPerInch), // DMPAPER_LEGAL       5
    (MMWidth: 5.12 * mmPerInch; MMHeight: 8.12 * mmPerInch), // DMPAPER_STATEMENT   6
    (MMWidth: 7.14 * mmPerInch; MMHeight: 10.12 * mmPerInch), // DMPAPER_EXECUTIVE   7
    (MMWidth: 297; MMHeight: 420), // DMPAPER_A3          8
    (MMWidth: 210; MMHeight: 297), // DMPAPER_A4          9
    (MMWidth: 210; MMHeight: 297), // DMPAPER_A4SMALL     10
    (MMWidth: 148; MMHeight: 210), // DMPAPER_A5          11
    (MMWidth: 250; MMHeight: 354), // DMPAPER_B4          12
    (MMWidth: 182; MMHeight: 257), // DMPAPER_B5          13
    (MMWidth: 8.12 * mmPerInch; MMHeight: 13 * mmPerInch), // DMPAPER_FOLIO       14
    (MMWidth: 215; MMHeight: 275), // DMPAPER_QUARTO      15
    (MMWidth: 10 * mmPerInch; MMHeight: 14 * mmPerInch), // DMPAPER_10X14       16
    (MMWidth: 11 * mmPerInch; MMHeight: 17 * mmPerInch), // DMPAPER_11X17       17
    (MMWidth: 8.12 * mmPerInch; MMHeight: 11 * mmPerInch), // DMPAPER_NOTE        18
    (MMWidth: 3.78 * mmPerInch; MMHeight: 8.78 * mmPerInch), // Envelope #9 3 78 x 8 78
    (MMWidth: 4.18 * mmPerInch; MMHeight: 9.12 * mmPerInch), // Envelope #10 4 18 x 9 12
    (MMWidth: 4.12 * mmPerInch; MMHeight: 10.38 * mmPerInch), // Envelope #11 4 12 x 10 38
    (MMWidth: 4.276 * mmPerInch; MMHeight: 11 * mmPerInch), // Envelope #12 4 \276 x 11
    (MMWidth: 5 * mmPerInch; MMHeight: 11.12 * mmPerInch), // Envelope #14 5 x 11 12
    (MMWidth: 10; MMHeight: 10), // C size sheet
    (MMWidth: 10; MMHeight: 10), // D size sheet
    (MMWidth: 10; MMHeight: 10), // E size sheet
    (MMWidth: 110; MMHeight: 220), // Envelope DL 110 x 220mm
    (MMWidth: 162; MMHeight: 229), // Envelope C5 162 x 229 mm
    (MMWidth: 324; MMHeight: 458), // Envelope C3  324 x 458 mm
    (MMWidth: 229; MMHeight: 324), // Envelope C4  229 x 324 mm
    (MMWidth: 114; MMHeight: 162), // Envelope C6  114 x 162 mm
    (MMWidth: 114; MMHeight: 229), // Envelope C65 114 x 229 mm
    (MMWidth: 250; MMHeight: 353), // Envelope B4  250 x 353 mm
    (MMWidth: 176; MMHeight: 250), // Envelope B5  176 x 250 mm
    (MMWidth: 176; MMHeight: 125), // Envelope B6  176 x 125 mm
    (MMWidth: 110; MMHeight: 230), // Envelope 110 x 230 mm
    (MMWidth: 3.875 * mmPerInch; MMHeight: 7.5 * mmPerInch), // Envelope Monarch 3.875 x 7.5 in
    (MMWidth: 3.58 * mmPerInch; MMHeight: 6.12 * mmPerInch), // 6 34 Envelope 3 58 x 6 12 in
    (MMWidth: 14.78 * mmPerInch; MMHeight: 11 * mmPerInch), // US Std Fanfold 14 78 x 11 in
    (MMWidth: 8.12 * mmPerInch; MMHeight: 12 * mmPerInch), // German Std Fanfold 8 12 x 12 in
    (MMWidth: 8.12 * mmPerInch; MMHeight: 13 * mmPerInch), // German Legal Fanfold 8 12 x 13 in
    (MMWidth: 250; MMHeight: 353), // B4 (ISO) 250 x 353 mm
    (MMWidth: 100; MMHeight: 148), // Japanese Postcard 100 x 148 mm
    (MMWidth: 9 * mmPerInch; MMHeight: 11 * mmPerInch), // 9 x 11 in
    (MMWidth: 10 * mmPerInch; MMHeight: 11 * mmPerInch), // 10 x 11 in
    (MMWidth: 15 * mmPerInch; MMHeight: 11 * mmPerInch), // 15 x 11 in
    (MMWidth: 220; MMHeight: 220), // Envelope Invite 220 x 220 mm
    (MMWidth: 10; MMHeight: 10), // RESERVED--DO NOT USE
    (MMWidth: 10; MMHeight: 10), // RESERVED--DO NOT USE
    (MMWidth: 9.275 * mmPerInch; MMHeight: 12 * mmPerInch), // Letter Extra 9 \275 x 12 in
    (MMWidth: 9.275 * mmPerInch; MMHeight: 15 * mmPerInch), // Legal Extra 9 \275 x 15 in
    (MMWidth: 11.69 * mmPerInch; MMHeight: 18 * mmPerInch), // Tabloid Extra 11.69 x 18 in
    (MMWidth: 9.27 * mmPerInch; MMHeight: 12.69 * mmPerInch), // A4 Extra 9.27 x 12.69 in
    (MMWidth: 8.275 * mmPerInch; MMHeight: 11 * mmPerInch), // Letter Transverse 8 \275 x 11 in
    (MMWidth: 210; MMHeight: 297), // A4 Transverse 210 x 297 mm
    (MMWidth: 9.275 * mmPerInch; MMHeight: 12 * mmPerInch), // Letter Extra Transverse 9\275 x 12 in
    (MMWidth: 227; MMHeight: 356), // SuperASuperAA4 227 x 356 mm
    (MMWidth: 305; MMHeight: 487), // SuperBSuperBA3 305 x 487 mm
    (MMWidth: 8.5 * mmPerInch; MMHeight: 12.69 * mmPerInch), // Letter Plus 8.5 x 12.69 in
    (MMWidth: 210; MMHeight: 330), // A4 Plus 210 x 330 mm
    (MMWidth: 148; MMHeight: 210), // A5 Transverse 148 x 210 mm
    (MMWidth: 182; MMHeight: 257), // B5 (JIS) Transverse 182 x 257 mm
    (MMWidth: 322; MMHeight: 445), // A3 Extra 322 x 445 mm
    (MMWidth: 174; MMHeight: 235), // A5 Extra 174 x 235 mm
    (MMWidth: 201; MMHeight: 276), // B5 (ISO) Extra 201 x 276 mm
    (MMWidth: 420; MMHeight: 594), // A2 420 x 594 mm
    (MMWidth: 297; MMHeight: 420), // A3 Transverse 297 x 420 mm
    (MMWidth: 322; MMHeight: 445), // A3 Extra Transverse 322 x 445 mm
    (MMWidth: 200; MMHeight: 148), // Japanese Double Postcard 200 x 148 mm
    (MMWidth: 105; MMHeight: 148), // A6 105 x 148 mm
    (MMWidth: 10; MMHeight: 10), // Japanese Envelope Kaku #2
    (MMWidth: 10; MMHeight: 10), // Japanese Envelope Kaku #3
    (MMWidth: 10; MMHeight: 10), // Japanese Envelope Chou #3
    (MMWidth: 10; MMHeight: 10), // Japanese Envelope Chou #4
    (MMWidth: 11 * mmPerInch; MMHeight: 8.5 * mmPerInch), // Letter Rotated 11 x 8 1/2 11 in
    (MMWidth: 420; MMHeight: 297), // A3 Rotated 420 x 297 mm
    (MMWidth: 297; MMHeight: 210), // A4 Rotated 297 x 210 mm
    (MMWidth: 210; MMHeight: 148)  // A5 Rotated 210 x 148 mm
    );

implementation

end.                                                                                            
