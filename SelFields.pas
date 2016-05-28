unit SelFields;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls;

type
  TfrmSFdlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    cb1: TCheckBox;
    cb2: TCheckBox;
    cb3: TCheckBox;
    cb4: TCheckBox;
    cb5: TCheckBox;
    cb6: TCheckBox;
    cb7: TCheckBox;
    cb8: TCheckBox;
    cb9: TCheckBox;
    cb10: TCheckBox;
    cb11: TCheckBox;
    cb12: TCheckBox;
    cb13: TCheckBox;
    cb15: TCheckBox;
    cb14: TCheckBox;
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSFdlg: TfrmSFdlg;


implementation
uses UNIT_LR7;
var
 CBA: array[1..pfqty] of TCheckBox;
 IsZHitu: boolean; //is ZavodyHalf in this unit

{$R *.DFM}

procedure TfrmSFdlg.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if key = VK_ESCAPE then begin hide; exit; end;
end;

procedure TfrmSFdlg.FormCreate(Sender: TObject);
var i: integer;
begin
CBA[1] := cb1;  CBA[2] := cb2;  CBA[3] := cb3;
CBA[4] := cb4;  CBA[5] := cb5;  CBA[6] := cb6;
CBA[7] := cb7;  CBA[8] := cb8;  CBA[9] := cb9;
CBA[10] := cb10;CBA[11] := cb11;CBA[12] := cb12;
CBA[13] := cb13;CBA[14] := cb14;CBA[15] := cb15;
end;

procedure ShowCBCaptions(IsZavody: boolean);
label PribLabel;
var i: integer;
begin
if not IsZavody then goto PribLabel;
for i := 1 to zfqty do begin
  CBA[i].Caption := FNZ[i];
  CBA[i].Hint    := FNZ[i];
 end;
for i := zfqty + 1 to pfqty do
 CBA[i].Visible := False;
exit;
PribLabel:
for i := 1 to pfqty do begin
  CBA[i].Caption := FNP[i];
  CBA[i].Hint    := FNP[i];
 end;
for i := zfqty + 1 to pfqty do
 CBA[i].Visible := True;
end;

procedure FillCB(IsZavody: boolean);
var w,i,fqty: word;
begin
for i := 1 to pfqty do CBA[i].Checked := false;
if IsZavody then begin
 w := wrz;
 fqty := zfqty;
end
else begin
 w := wrp;
 fqty := pfqty;
end;
for i := 1 to fqty do
 if w >= round( exp( (fqty - i) * ln(2) ) ) then begin
  w := w - round(exp( (fqty - i) * ln(2) ));
  CBA[i].Checked := true;
 end;
end;

procedure TfrmSFdlg.FormShow(Sender: TObject);
begin
IsZHitu := IsZavodyHalf;
ShowCBCaptions(IsZHitu);
FillCB(IsZHitu);
//i:=1;
end;


procedure TfrmSFdlg.CancelBtnClick(Sender: TObject);
begin
hide;
end;

procedure TfrmSFdlg.OKBtnClick(Sender: TObject);
var w,i,fqty: word;
begin
w := 0;
if IsZHitu then begin
 fqty := zfqty;
end
else begin
 fqty := pfqty;
end;
for i := 1 to fqty do
 if CBA[i].Checked then begin
  w := w + round(exp( (fqty - i) * ln(2) ));
 end;
if IsZHitu then begin
 wrz := w;
end
else begin
 wrp := w;
end;
hide;
end;

end.
