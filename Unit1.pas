unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, Grids, UNIT_LR7, StdCtrls, Menus, ComCtrls, ExtCtrls, Clipbrd;

type
  TForm1 = class(TForm)
    SG1: TStringGrid;
    SG2: TStringGrid;
    MM1: TMainMenu;
    mnuView: TMenuItem;
    mnuSG1: TMenuItem;
    mnuSG2: TMenuItem;
    SB1: TStatusBar;
    Panel1: TPanel;
    Panel11: TPanel;
    SGZinfo: TStringGrid;
    LBZ: TListBox;
    btnFindZ: TButton;
    N2: TMenuItem;
    mnuSaveZ: TMenuItem;
    mnuShowMain: TMenuItem;
    PopupZ: TPopupMenu;
    mnuShowAllZ: TMenuItem;
    mnuDelZ: TMenuItem;
    mnuAddZ: TMenuItem;
    mnuRestart: TMenuItem;
    mnuShowFilesTime: TMenuItem;
    N1: TMenuItem;
    mnuSearchAll: TMenuItem;
    Panel12: TPanel;
    SGPinfo: TStringGrid;
    LBP: TListBox;
    btnFindP: TButton;
    mnuSaveP: TMenuItem;
    PopupP: TPopupMenu;
    mnuShowAllP: TMenuItem;
    mnuDelP: TMenuItem;
    mnuAddP: TMenuItem;
    mnuExpandZ: TMenuItem;
    mnuExpandP: TMenuItem;
    mnuSetDafaultProportion: TMenuItem;
    PopupSG: TPopupMenu;
    mnuEquatemfr: TMenuItem;
    mnuRestartZ: TMenuItem;
    mnuRestartP: TMenuItem;
    mnuSaveAll: TMenuItem;
    mnuDiscardPribor: TMenuItem;
    extractZ: TMenuItem;
    PrintDialog1: TPrintDialog;
    extractP: TMenuItem;
    mnuOnlyChem: TMenuItem;
    mnuSelectFields: TMenuItem;
    mnuAutoSearch: TMenuItem;
    mnuCopyP2new: TMenuItem;
    EditFZ: TComboBox;
    EditFP: TComboBox;
    mnuMisc: TMenuItem;
    mnuTownInfo: TMenuItem;
    mnuShowDesc: TMenuItem;
    mnuCopyWebDescLink: TMenuItem;
    mnuWholeWordSearch: TMenuItem;
    mnuZInfo: TMenuItem;
    mnuInterRec: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mnuSG1Click(Sender: TObject);
    procedure mnuSG2Click(Sender: TObject);
    procedure btnFindZClick(Sender: TObject);
    procedure Panel11MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel11MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel11MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBZClick(Sender: TObject);
    procedure mnuShowMainClick(Sender: TObject);
    procedure mnuSaveZClick(Sender: TObject);
    procedure SGZinfoSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure mnuShowAllZClick(Sender: TObject);
    procedure mnuDelZClick(Sender: TObject);
    procedure mnuAddZClick(Sender: TObject);
    procedure mnuRestartClick(Sender: TObject);
    procedure mnuShowFilesTimeClick(Sender: TObject);
    procedure mnuSearchAllClick(Sender: TObject);
    procedure btnFindPClick(Sender: TObject);
    procedure Panel12MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel12MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel12MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBPClick(Sender: TObject);
    procedure mnuSavePClick(Sender: TObject);
    procedure SGPinfoSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure mnuShowAllPClick(Sender: TObject);
    procedure mnuDelPClick(Sender: TObject);
    procedure mnuAddPClick(Sender: TObject);
    procedure mnuSetAnotherHalfClick(Sender: TObject);
    procedure EditFZEnter(Sender: TObject);
    procedure EditFPEnter(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGPinfoEnter(Sender: TObject);
    procedure SGZinfoEnter(Sender: TObject);
    procedure SGPinfoExit(Sender: TObject);
    procedure SGZinfoGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
    procedure SGZinfoExit(Sender: TObject);
    procedure SGPinfoGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
    procedure mnuSetDafaultProportionClick(Sender: TObject);
    procedure mnuExpandZClick(Sender: TObject);
    procedure mnuExpandPClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SGPinfoDblClick(Sender: TObject);
    procedure SGPinfoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mnuEquatemfrClick(Sender: TObject);
    procedure SGZinfoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mnuRestartZClick(Sender: TObject);
    procedure mnuRestartPClick(Sender: TObject);
    procedure mnuSaveAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuDiscardPriborClick(Sender: TObject);
    procedure extractZClick(Sender: TObject);
    procedure extractPClick(Sender: TObject);
    procedure mnuOnlyChemClick(Sender: TObject);
    procedure mnuSelectFieldsClick(Sender: TObject);
    procedure mnuAutoSearchClick(Sender: TObject);
    procedure EditFZChange(Sender: TObject);
    procedure EditFPChange(Sender: TObject);
    procedure mnuCopyP2newClick(Sender: TObject);
    procedure SGZinfoDblClick(Sender: TObject);
    procedure mnuTownInfoClick(Sender: TObject);
    procedure mnuShowDescClick(Sender: TObject);
    procedure mnuCopyWebDescLinkClick(Sender: TObject);
    procedure SGZinfoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGZinfoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGPinfoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGPinfoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MM1Change(Sender: TObject; Source: TMenuItem;
      Rebuild: Boolean);
    procedure mnuWholeWordSearchClick(Sender: TObject);
    procedure mnuZInfoClick(Sender: TObject);
    procedure mnuInterRecClick(Sender: TObject);
    procedure N3Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
 appname = 'Foundation 3. ������ � ���������.';
var
  Form1: TForm1;

procedure FillSGZinfoHead;
procedure FillSGPinfoHead;
procedure FillZavodStringGrid;
procedure FillStringGridRowAsZavod(i: integer);
procedure FillPriborStringGrid;
procedure FillStringGridRowAsPribor(i: integer);
procedure HideAll;
procedure SetSizes;
procedure SetRowHeights;
procedure SGShowHint(rectype:integer; X, Y: Integer);

var
 InterResizing: boolean;
 SGPentered,SGZentered : boolean;
 initp11h, initp12h : integer; //initial heights of panels

implementation
uses SelFields;


var
 strprnFile: string;

{$R *.DFM}

procedure FillSGZinfoHead;
var i: integer;
begin
for i := 1 to zfqty do
with form1.sgzinfo, ZA[0] do
 cells[0,i-1] := FNZ[i];
end;
procedure FillSGPinfoHead;
var i: integer;
begin
for i := 1 to pfqty do
with form1.sgpinfo, PA[0] do 
 cells[0,i-1] := FNP[i];
end;

procedure SGMM (Shift: TShiftState; numb, X, Y: Integer);
var SG: TStringGrid; ind: integer;
begin
ind := 6;
if numb = 1 then sg := form1.sgzinfo;
if numb = 2 then sg := form1.sgpinfo;
if ( X > sg.ColWidths[0] - ind ) and ( X < sg.ColWidths[0] + ind )
 then sg.Cursor := crSizeWE
 else sg.Cursor := crArrow;
if SGInterResizing and (X > ind) and (X < Form1.Width - ind )then
 begin sg.ColWidths[0] := X;
       sg.ColWidths[1] := sg.width - sg.GridLineWidth - sg.ColWidths[0];
 end;
end;
procedure SGMD (Button: TMouseButton; Shift: TShiftState; numb, X, Y: Integer);
var SG: TStringGrid; ind: integer;
begin
ind := 6;
if numb = 1 then sg := form1.sgzinfo;
if numb = 2 then sg := form1.sgpinfo;
if ( X > sg.ColWidths[0] - ind ) and ( X < sg.ColWidths[0] + ind )
 then  SGInterResizing := true;
end;
procedure SGMU(Button: TMouseButton;  Shift: TShiftState; numb, X, Y: Integer);
var SG: TStringGrid;
begin
if numb = 1 then sg := form1.sgzinfo;
if numb = 2 then sg := form1.sgpinfo;
if SGInterResizing = true then SGInterResizing := False;
end;

function ForceForegroundWindow(hWnd: HWND): BOOL; stdcall;
const
 SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
 SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
 OsVerInfo: TOSVersionInfo;
 Win32MajorVersion: Integer;
 Win32MinorVersion: Integer;
 Win32Platform: Integer;
 ForegroundThreadID: DWORD;
 ThisThreadID: DWORD;
 Timeout: DWORD;
begin
 OsVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
 GetVersionEx(osVerInfo);
 Win32MajorVersion := OsVerInfo.dwMajorVersion;
 Win32MinorVersion := OsVerInfo.dwMinorVersion;
 Win32Platform := OsVerInfo.dwPlatformId;
 if IsIconic(hWnd) then ShowWindow(hWnd, SW_RESTORE);
 if GetForegroundWindow = hWnd then Result := True
 else
 begin
   if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
     ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and ((Win32MajorVersion > 4)
       or ((Win32MajorVersion = 4) and (Win32MinorVersion > 0)))) then
   begin
     Result := False;
     ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
     ThisThreadID := GetWindowThreadPRocessId(hWnd, nil);
     if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
     begin
       BringWindowToTop(hWnd);
       SetForegroundWindow(hWnd);
       AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
       Result := (GetForegroundWindow = hWnd);
     end;
     if not Result then
     begin
       SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @Timeout, 0);
       SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
         SPIF_SENDCHANGE);
       BringWindowToTop(hWnd);
       SetForegroundWindow(hWnd);
       SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(Timeout),
                                                              SPIF_SENDCHANGE);
     end;
   end
   else
   begin
     BringWindowToTop(hWnd);
     SetForegroundWindow(hWnd);
   end;
   Result := (GetForegroundWindow = hWnd);
 end;
end; // End of function ForceForegroundWindow

procedure TForm1.FormCreate(Sender: TObject);
var //Mutex: cardinal;
//Wd: HWnd;
//   TheWindowHandle : THandle;
bb : bool;

begin
application.Title := '������ ��������� Foundation 3....';
//Mutex:=CreateMutex(nil,true,'MyProga');
//if (Mutex = null) then application.Terminate;
//if FindWindow(pchar(appname), nil) <> 0 then  ShowMessage('Window Found!');
if FindWindow(nil, appname ) <> 0
 then begin
  ShowMessage('���������� Foundation 3 ��� ��������!');
  bb := ForceForegroundWindow(FindWindow(nil, appname ));
  application.Terminate;
  exit;
  end;
// else begin ShowMessage('Window Not Found!'); application.Terminate; end;
//if FindWindowEx(appname,null,null,null) <> 0 then  ShowMessage('Window Found!');

//   TheWindowHandle := FindAWindow(' ', '');
//   if TheWindowHandle = 0 then
//     ShowMessage('Window Not Found!') else
//     application.Terminate;
Form1.Caption := appname;
application.Title := appname;
SetSizes;
SetRowHeights;
Restart(3);
//FillZavodStringGrid;
//FillPriborStringGrid;
initp11h := panel11.Height;
initp12h := panel12.Height;
end;

procedure SetSizes;

procedure SetSubSizes(a:integer);
const ind = 10;
var
 LB: TListBox;
 SG: TStringGrid;
 Ed: TComboBox;
 Btn: TButton;
 Pan: TPanel;
 i,k,d: integer;
begin
if (a < 1) or (a > 2) then exit;
with form1 do
 if a = 1 then begin
      LB := lbz;
      SG := sgzinfo;
      Ed := Editfz;
      Btn:= btnfindz;
      Pan:= Panel11;
 end
 else begin
      LB := lbp;
      SG := sgpinfo;
      Ed := Editfp;
      Btn:= btnfindp;
      Pan:= Panel12;
 end;
a := pan.Width div 20 * 7;
LB.Width := a;
LB.Height := pan.Height - btn.height - ed.Height + 4;
btn.Width := a;
Ed.width := a;
sg.Width := form1.width - ind - LB.Width;
sg.ColWidths[1]:=sg.Width-sg.ColWidths[0] - 4;
end;

const ind = 6;
begin
with form1 do begin
 panel11.Height := (panel1.Height div (zfqty + pfqty)) * zfqty + ind-2;
 panel12.Height := panel1.Height - panel11.Height-2;
 SetSubSizes(1);
 SetSubSizes(2);
end;//with
end;

procedure SetRowHeights;
label L;
var
 i,k,d,a: integer;
 SG: TStringGrid;
begin
k := 0;
L:
if k = 0 then SG := Form1.SGZinfo else SG := Form1.SGPinfo;
//if k = 0 then d := zfqty-4 else d := pfqty-3;
d := zfqty + pfqty;
a := form1.Panel1.Height div d -2;
if a > 30 then a := 30;
for i := 1 to SG.RowCount do SG.RowHeights[i-1] := a;
if k = 0 then begin k := 1; goto L end;
end;

procedure FillZavodStringGrid;
var i: integer;
begin
for i := 0 to nz do begin
 FillStringGridRowAsZavod(i);
 Form1.SG1.Cells[0,i] := inttostr(i);
end;
Form1.SG1.FixedRows := 1;
end;


procedure FillPriborStringGrid;
var i: integer;
begin
for i := 0 to np do begin
 FillStringGridRowAsPribor(i);
 Form1.SG2.Cells[0,i] := inttostr(i);
end;
Form1.SG2.FixedRows := 1;
end;



procedure FillStringGridRowAsZavod(i: integer);
var ST : string;
 r: integer;
begin
r := form1.SG1.RowCount - 1;
WITH Form1.SG1, ZA[i] DO begin // ����� ��������� ������ Z � ������ �������:
	Cells[1,r] := name;
	Cells[2,r] := numb;
        Cells[3,r] := sect;
	Cells[4,r] := town;
	Cells[5,r] := tel;
	Cells[6,r] := cont;
        Cells[7,r] := res;
	Cells[8,r] := dop;
	Cells[9,r] := notes;
	Cells[10,r] := mdate;
	Cells[11,r] := author;
    end;
form1.SG1.RowCount := form1.SG1.RowCount + 1;
end;

procedure FillStringGridRowAsPribor(i: integer);
// UNUSED
var ST : string;
 r: integer;
begin
r := form1.SG2.RowCount - 1;
WITH Form1.SG2, PA[i] DO begin // ����� ��������� ������ Z � ������ �������:
	Cells[1,r] := mark;
	Cells[2,r] := name;
        Cells[3,r] := mfr;
	Cells[4,r] := price;
	Cells[5,r] := pricedate;
	Cells[6,r] := tod;
//	Cells[7,r] := pprop;
//	Cells[8,r] := sprop;
//        Cells[9,r] := tugost;
	Cells[10,r] := notes;
	Cells[11,r] := notes;
	Cells[12,r] := notes;
	Cells[13,r] := mdate;
	Cells[14,r] := author;
    end;
form1.SG2.RowCount := form1.SG2.RowCount + 1;
end;

procedure HideAll;
begin
Form1.SG1.Visible := false;
Form1.SG2.Visible := false;
Form1.Panel1.Visible := false;
end;

procedure TForm1.mnuSG1Click(Sender: TObject);
begin
HideAll;
SG1.Visible := true;
end;

procedure TForm1.mnuSG2Click(Sender: TObject);
begin
HideAll;
SG2.Visible := true;
end;

procedure TForm1.btnFindZClick(Sender: TObject);
var wr: word;
begin
FindRec(editFZ.Text, 1, wrz);
end;
procedure TForm1.btnFindPClick(Sender: TObject);
var wr: word;
begin
FindRec(editFP.Text, 2, wrp);
end;

procedure TForm1.Panel11MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
ResizingAtMouseMoveAndDown(1,1,x,y);
end;
procedure TForm1.Panel12MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
ResizingAtMouseMoveAndDown(2,1,x,y);
end;
procedure TForm1.Panel11MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
ResizingAtMouseMoveAndDown(1,2,x,y);
end;
procedure TForm1.Panel12MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
ResizingAtMouseMoveAndDown(2,2,x,y);
end;
procedure TForm1.Panel11MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
if InterResizing = true then begin
 InterResizing := False;
 Panel11.Cursor := crArrow;
end;
end;
procedure TForm1.Panel12MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
if InterResizing = true then begin
 InterResizing := False;
 Panel12.Cursor := crArrow;
end;
end;


procedure TForm1.LBZClick(Sender: TObject);
begin
ShowDetails(1, LBZ.ItemIndex);
currentz := LBZ.ItemIndex + 1;
end;
procedure TForm1.LBPClick(Sender: TObject);
begin
ShowDetails(2, LBP.ItemIndex);
currentp := LBP.ItemIndex + 1;
end;

procedure TForm1.mnuShowMainClick(Sender: TObject);
begin
HideAll;
Panel1.Visible := true;
end;

procedure TForm1.mnuSaveZClick(Sender: TObject);
begin
if zchanged = true then RewriteZavody( strFiZ );
Form1.SB1.Panels[0].Text := '���� "������" ��������.';
end;
procedure TForm1.mnuSavePClick(Sender: TObject);
begin
if pchanged = true then RewritePribory( strFiP );
Form1.SB1.Panels[0].Text := '���� "�������" ��������.';
end;
procedure TForm1.mnuSaveAllClick(Sender: TObject);
begin
if zchanged = true then RewriteZavody( strFiZ );
if pchanged = true then RewritePribory( strFiP );
Form1.SB1.Panels[0].Text := '��� ����� ���������.';
end;


procedure TForm1.SGZinfoSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: String);
begin
if lbz.ItemIndex = -1 then exit;
StoreFieldValue(1,ARow,Value);
intaRowZ := ARow;
//strcellbuf := value;
end;
procedure TForm1.SGPinfoSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: String);
begin
if lbp.ItemIndex = -1 then exit;
StoreFieldValue(2,ARow,Value);
intaRowP := ARow;
//intaRow := 11;
//strcellbuf := value;
end;


procedure TForm1.mnuShowAllZClick(Sender: TObject);
begin
FillLBZ;
SB1.Panels[0].Text := '����� �������: '+inttostr(nz);
end;
procedure TForm1.mnuShowAllPClick(Sender: TObject);
begin
FillLBP;
SB1.Panels[0].Text := '����� ��������: '+inttostr(np);
end;

procedure TForm1.mnuDelZClick(Sender: TObject);
var i: integer;
begin
//DeleteZ(LBZ.ItemIndex+1);
//SB1.Panels[0].Text := '�������: '+inttostr(lbz.Items.Count);
 for i := 0 to (LBZ.Items.Count - 1) do begin
  try
    if LBZ.Selected[i] then
    begin
     DeleteZ(i+1);
    end;
   finally
   { do something here }
   end;
  end;
end;
procedure TForm1.mnuDelPClick(Sender: TObject);
var i: integer;
begin
//DeleteP(LBP.ItemIndex+1);
 for i := 0 to (LBp.Items.Count - 1) do begin
  try
    if LBp.Selected[i] then
    begin
     Deletep(i+1);
    end;
   finally
   { do something here }
   end;
  end;
//SB1.Panels[0].Text := '��������: '+inttostr(lbp.Items.Count);
end;

procedure TForm1.mnuAddZClick(Sender: TObject);
begin
AddZ;
SB1.Panels[0].Text := '�������: '+inttostr(lbz.Items.Count);
end;
procedure TForm1.mnuAddPClick(Sender: TObject);
begin
AddP;
SB1.Panels[0].Text := '��������: '+inttostr(lbp.Items.Count);
end;

procedure TForm1.mnuRestartClick(Sender: TObject);
begin
Restart(3);
end;

procedure TForm1.mnuShowFilesTimeClick(Sender: TObject);
begin
SB1.Panels[0].Text :=
 '����� ���������� ���������. ������: ' + Zfiletime +
 ' �������: ' + Pfiletime ;
end;


procedure TForm1.mnuSearchAllClick(Sender: TObject);
begin
mnuSearchAll.Checked := not mnuSearchAll.Checked;
if mnuSearchAll.Checked
 then begin
   Form1.SB1.Panels[0].Text := '����� �� ���� ����� �������';
   wrp := maxwrp;
   wrz := maxwrz;
  end
 else begin
   Form1.SB1.Panels[0].Text := '����� �� ���� ����� ��������';
   wrp := defwrp;
   wrz := defwrz;
  end;
end;


procedure TForm1.mnuSetAnotherHalfClick(Sender: TObject);
begin
SetActiveHalf(not IsZavodyHalf);
end;

procedure TForm1.EditFZEnter(Sender: TObject);
begin
SetActiveHalf( True );
end;
procedure TForm1.EditFPEnter(Sender: TObject);
begin
SetActiveHalf( False );
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const a=10;
begin
 if ( Y > Panel11.Height - a ) and ( Y < Panel11.Height + a )
  then Panel1.Cursor := crVSplit;
 if (InterResizing=true) {and (X > 4*a) and (X < Form1.Width - 5*a )}then
  begin
   Panel11.Height := Y;
   Panel12.Height := Panel1.height - 4 - Panel11.Height;
  end;

end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
if InterResizing = true then begin
 InterResizing := False;
 Panel1.Cursor := crArrow;
end;
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const a=10;
begin
if ( Y > Panel11.Height - a ) and ( Y < Panel11.Height + a )
 then  InterResizing := true;
end;

procedure TForm1.SGZinfoEnter(Sender: TObject);
var t: byte;
begin
intaRowZ := -1;
SGZentered := true;
if not IsZavodyHalf then setactivehalf(true);
end;
procedure TForm1.SGPinfoEnter(Sender: TObject);
var t: byte;
begin
SetActiveHalf( False );
intaRowP := -1;
SGPentered := true;
if IsZavodyHalf then setactivehalf(false);
end;


procedure TForm1.SGZinfoGetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: String);
begin
if not SGZentered then exit;
if zrecchanged then exit;
if intarowZ <> -1 then
 if SGZinfo.Cells[1,intarowZ] <> strcellbuf then
  zrecchanged := true;
intaRowZ := ARow;
strcellbuf := value;
end;
procedure TForm1.SGPinfoGetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: String);
begin
if not SGPentered then exit;
if precchanged then exit;
if intarowP <> -1 then
 if SGPinfo.Cells[1,intarowP] <> strcellbuf then
  precchanged := true;
intaRowP := ARow;
strcellbuf := value;
end;


procedure TForm1.SGZinfoExit(Sender: TObject);
var t:byte;
begin
if CLBz[currentz] = 0 then exit;
if intarowZ <> -1 then
 if SGZinfo.Cells[1,intarowZ] <> strcellbuf then begin
  zchanged := true;
  zrecchanged := true;
 end;
intaRowZ := -1;
SGZentered := False;
if zrecchanged
 then begin
  zA[ CLBz[currentz] ].mdate := DateToStr(Date)+' '+TimeToStr(Time);
  if copy(zA[ CLBz[currentz] ].author, length(zA[ CLBz[currentz] ].author)-1, 2) <> 'a>'
   then zA[ CLBz[currentz] ].author := theauthor + ' m>';
  zrecchanged := false;;
 end;
end;
procedure TForm1.SGPinfoExit(Sender: TObject);
var t: byte;
begin
if CLBp[currentp] = 0 then begin
// ShowMessage('������� ������!!');
 exit;
end;
if intarowP <> -1 then
 if SGPinfo.Cells[1,intarowP] <> strcellbuf then begin
  pchanged := true;
  precchanged := true;
 end;
intaRowP := -1;
SGPentered := False;
if precchanged
 then begin
  PA[ CLBP[currentp] ].mdate := DateToStr(Date)+' '+TimeToStr(Time);
  PA[ CLBP[currentp] ].author := theauthor + ' m>';
  precchanged := false;;
 end;
end;

procedure InitTablesExit;
begin
Form1.SGZinfoExit(Form1);
Form1.SGPinfoExit(Form1);
end;

procedure TForm1.mnuSetDafaultProportionClick(Sender: TObject);
begin
Panel11.Visible := True;
Panel12.Visible := True;
Panel11.Height := initp11h;
Panel12.Height := initp12h;
end;

procedure TForm1.mnuExpandZClick(Sender: TObject);
begin
Panel12.Visible := False;
Panel11.Visible := true;
Panel11.Height := Panel1.Height;
end;

procedure TForm1.mnuExpandPClick(Sender: TObject);
begin
Panel11.Visible := False;
Panel12.Visible := true;
Panel12.Height := Panel1.Height;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
SetSizes;
end;

procedure TForm1.SGPinfoDblClick(Sender: TObject);
var
 s: string;
 i: integer;
begin
if  SGPinfoRowFocused[3] then begin //mfr
  s := SGPinfo.Cells[1,2];
  EditFZ.Text := MakeFZStr(s);
  FindRec(editFZ.Text, 1, wrz);
end;
if  SGPinfoRowFocused[11] then showdesc;
end;

procedure TForm1.SGZinfoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var i, m: integer;
begin
SGShowHint(1,x,y);
for i := 1 to zfqty do SGZinfoRowFocused[i] := false;
m := 0;
for i := 1 to zfqty do
 with SGZinfo do begin
  if     (Y > m) and (Y < m+rowheights[i-1])
   then SGZinfoRowFocused[i] := true;
  m := m + rowheights[i-1] + gridlinewidth;
 end;
SGMM(shift,1,x,y);
end;
procedure TForm1.SGPinfoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var i, m: integer;
begin
SGShowHint(2,x,y);
for i := 1 to pfqty do SGPinfoRowFocused[i] := false;
m := 0;
for i := 1 to pfqty do
 with SGPinfo do begin
  if     (Y > m) and (Y < m+rowheights[i-1])
   then SGPinfoRowFocused[i] := true;
  m := m + rowheights[i-1] + gridlinewidth;
 end;
SGMM(shift,2,x,y);
end;

procedure TForm1.mnuEquatemfrClick(Sender: TObject);
var
 i, c : integer;
 wrong,correct: string;
begin
if (currentp = 0) or (currentz = 0) then exit;
if SGZentered then begin
 correct := SGZinfo.cells[1,0] + ' / ' + SGZinfo.cells[1,3];
 wrong := PA[ CLBP[currentp] ].mfr;
 SGPinfo.cells[1,2] := correct;
 PA[ CLBP[currentp] ].mfr := correct;
 c := 1;
 if length(wrong) > 2 then  // to avoid unnecessary and objectionable changes
  for i := 1 to np do
   if PA[i].mfr = wrong then begin
    PA[i].mfr := correct;
    c := c + 1;
   end;
  Form1.SB1.Panels[0].Text := '������������� ������� ��� ' + inttostr(c) + ' ��������';
end;
if SGPentered then begin //reverse unification
 SGZinfo.cells[1,0] := SGPinfo.cells[1,2];
 ZA[ CLBZ[currentz] ].name := SGZinfo.cells[1,0];
end;
end;

procedure SGShowHint(rectype:integer; X, Y: Integer);
var
 i,j: integer;
 h: integer;
 SG: TStringGrid;
begin
if rectype = 1 then SG := Form1.SGZinfo;
if rectype = 2 then SG := Form1.SGPinfo;
with SG do begin
 if x > ColWidths[0] then begin
  showhint := false;
  exit;
 end;
 showhint := true;
 for i := rowcount-1 downto 0 do begin
  h := 0;
  for j := 0 to i do h := h + RowHeights[j] + GridLineWidth;
   if y > h then begin
   Hint := Cells[0,i+1];
   exit;
  end;
  if i = 0 then Hint := Cells[0,0];
//  else begin
//   Hint := Cells[0,0];
//   exit;
//  end;
 end;
end;//with
end;

procedure TForm1.mnuRestartZClick(Sender: TObject);
begin
Restart(1);
end;

procedure TForm1.mnuRestartPClick(Sender: TObject);
begin
Restart(2);
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var id: integer;
begin
if zchanged or pchanged then begin
 id := application.MessageBox ('�� ��� ����� ���������. ���������?', pchar(appname), mb_yesnocancel+mb_iconquestion);
 if id = idYes then begin
  if zchanged = true then RewriteZavody( strFiZ );
  if pchanged = true then RewritePribory( strFiP );
 end;
 if id = idCancel then CanClose := False;

end;
end;

procedure TForm1.mnuDiscardPriborClick(Sender: TObject);
var n: integer;
begin
n := LBP.ItemIndex+1;
if Form1.LBP.ItemIndex = -1 then exit;
pA[CLBp[n]].author := theauthor;
pA[CLBp[n]].notes := '���� ' + pA[CLBp[n]].notes;
pA[CLBp[n]].mdate := DateToStr(Date)+' '+TimeToStr(Time);
ShowDetails(2, Form1.LBp.ItemIndex);
pchanged := true;

end;

procedure TForm1.extractZClick(Sender: TObject);
var i,k: integer;
begin
//ExtractRec(1, LBZ.ItemIndex+1);
k := 0;
  for i := 0 to (LBZ.Items.Count - 1) do begin
  try
    if LBZ.Selected[i] then
    begin
     ExtractRec(1, i+1);
     k := k + 1;
     if k > MaxRec4Extract then begin
      ShowMessage('�������� ������ '+inttostr(MaxRec4Extract)+' �������');
      break;
     end;
    end;
   finally
   { do something here }
   end;
  end;
//RewritePrnFile( strprnfile, LBZ.ItemIndex+1 );
//if PrintDialog1.Execute then
//    Print(PathName);
//TForm1.SGZ.

end;

procedure TForm1.extractPClick(Sender: TObject);
var i,k: integer;
begin
//ExtractRec(2, LBP.ItemIndex+1);
k := 0;
  for i := 0 to (LBp.Items.Count - 1) do begin
  try
    if LBp.Selected[i] then
    begin
     ExtractRec(2, i+1);
     k := k + 1;
     if k > MaxRec4Extract then begin
      ShowMessage('�������� ������ '+inttostr(MaxRec4Extract)+' �������');
      break;
     end;
    end;
   finally
   { do something here }
   end;
  end;
end;

procedure TForm1.mnuOnlyChemClick(Sender: TObject);
begin
ShowChemOnly;
end;

procedure TForm1.mnuSelectFieldsClick(Sender: TObject);
begin
frmSFdlg.Show;
end;

procedure TForm1.mnuAutoSearchClick(Sender: TObject);
begin
mnuAutoSearch.Checked := not mnuAutoSearch.Checked;
WriteIniFile;
end;

procedure TForm1.EditFZChange(Sender: TObject);
begin
if mnuAutoSearch.Checked and (length(EditFZ.Text) >= 4) then
 FindRec(editFZ.Text, 1, wrz);
end;

procedure TForm1.EditFPChange(Sender: TObject);
begin
if mnuAutoSearch.Checked and (length(EditFP.Text) >= 4) then
 FindRec(editFP.Text, 2, wrp);
end;

procedure TForm1.mnuCopyP2newClick(Sender: TObject);
var r: pribstruc;
begin
r := PA[CLBP[ LBP.ItemIndex+1] ];
AddP;
PA[np] := r;
ShowDetails(2,Form1.LBP.ItemIndex);
end;

procedure TForm1.SGZinfoDblClick(Sender: TObject);
begin
//with SGZinfo do //begin
if SGZinfoRowFocused[4] then ShowTownInfo;
if SGZinfoRowFocused[7] then ShowZweb;
end;

procedure TForm1.mnuTownInfoClick(Sender: TObject);
begin
ShowTownInfo;
end;

procedure TForm1.mnuShowDescClick(Sender: TObject);
begin
ShowDesc;
end;

procedure TForm1.mnuCopyWebDescLinkClick(Sender: TObject);
begin
CopyWebDescLink;
end;
procedure TForm1.SGZinfoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
SGMD(button,shift,1,x,y);
end;
procedure TForm1.SGZinfoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
SGMU(button,shift,1,x,y);
end;

procedure TForm1.SGPinfoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
SGMD(button,shift,2,x,y);
end;
procedure TForm1.SGPinfoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
SGMU(button,shift,2,x,y);
end;

procedure TForm1.MM1Change(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
InitTablesExit;
end;

procedure TForm1.mnuWholeWordSearchClick(Sender: TObject);
begin
mnuWholeWordSearch.Checked := not mnuWholeWordSearch.Checked;
end;

procedure TForm1.mnuZInfoClick(Sender: TObject);
begin
ShowZweb;
end;

procedure TForm1.mnuInterRecClick(Sender: TObject);
begin
if IsZavodyHalf then InterRecZ(' + ') else InterRecP(' + ');
end;

procedure TForm1.N3Click(Sender: TObject);
var i,k: integer;
// SA: array[1..MaxRec4Extract] of string;
 s: string;
begin
k := 0; s := '';
for i := 0 to (LBp.Items.Count - 1) do begin
  try
    if LBp.Selected[i] then
    begin
     k := k + 1;
//     SA[k]   := PA[CLBP[i+1]].name + ' ' + PA[CLBP[i+1]].mark;
     s := s + filter(PA[CLBP[i+1]].name) + #9 + filter(PA[CLBP[i+1]].mark) + #10;
     if k > MaxRec4Extract then begin
      ShowMessage('����������� ������ '+inttostr(MaxRec4Extract)+' �������');
      break;
     end;
    end;
   finally
   { do something here }
   end;
end;
receptacle := receptacle + s;
Clipboard.AsText := receptacle;

end; //procedure TForm1.N3Click(Sender: TObject);


procedure TForm1.N4Click(Sender: TObject);
begin
receptacle := '';
end;

end.
