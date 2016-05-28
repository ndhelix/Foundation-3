UNIT UNIT_LR7;
INTERFACE
{$R+}
USES SysUtils, Dialogs, StdCtrls, Grids, Forms, extctrls, Controls,
  Windows, Messages, Classes, Graphics, ShellAPI, FileCtrl, clipbrd, inifiles;


CONST nPconst = 63000; nZconst = 20000; //max possible values

TYPE

ZAVSTRUC = RECORD // Запись завода
 name   : string; //
 numb   : string; //
 sect   : string; // theme
 town   : string; //
 tel    : string; //
 cont   : string; //
 res    : string; // Ресурс (наличие прайса, каталога, сайта…)
 dop    : string; //
 notes  : string; // ПРИМЕЧАНИЯ
 mdate  : string; // Дата внесения последнего изменения
 author : string; // Автор последнего изменения
END;

PRIBSTRUC = RECORD // Запись прибора
 mark   : string; //
 name   : string; //
 mfr    : string; //
 price  : string; //
 pricedate : string; //
 tod    : string; // time of delivery -    СРОК ПОСТАВКИ РАБ.ДНЕЙ МАКСИМУМ
 pprop  : string;// purchase prop
 sprop  : string; // sale prop
 ex     : string; //extra - реклама
 tugost : string; // ТУ или ГОСТ
 desc   : string; // description link
 anal   : string; // analogues
 notes  : string; // ПРИМЕЧАНИЯ
 mdate  : string; // Дата внесения последнего изменения
 author : string; // Автор последнего изменения
END;

const
zfqty = 11; pfqty = 15; //qty of fields
msgUpdatedTryAgain = 'Операция не была выполнена из-за обновления базы.';
msgWaitSaving = 'Подождите... идет сохранение...';
msgWaitLoading = 'Подождите... идет загрузка базы...';
defwrz = 1024+128; //defaul search pattern
//defwrz = 2047; //defaul search pattern
defwrp = 16384+8192;  // =24576
//defwrp = 8192;
maxwrz = 2047;
maxwrp = 32767;
MaxRec4Extract = 100;

VAR
PA : ARRAY [0 ..nPconst] OF PRIBSTRUC; //pribory base
ZA : ARRAY [0 ..nZconst] OF ZAVSTRUC; // zavody base
CLBZ : array[1..nZconst] of integer;
CLBP : array[1..nPconst] of integer;
FNZ: array[1..zfqty] of string; //contains field names
FNP: array[1..pfqty] of string;
DPA: array[1..15] of string; // desc path array
SGZinfoRowFocused: array[1..zfqty] of boolean;
SGPinfoRowFocused: array[1..pfqty] of boolean;
TTCA: array[1..2,1..2048] of string; //town tel codes array
TTDA: array[1..2,1..256] of string; //town time difference array
np,nz,nttd,nttc,dpaq: word; //qty
wrp,wrz: word; //wr - which rec to find
currentz, currentp : word;
B : CHAR; // - для пропуска символа при вводе
strFiZ, strFiP, strFittd, strFittc, strfidescpath: string;
Zfiletime, Pfiletime: string;
appname: string;
Fi: TEXTFILE; // - файл для исходных данных
zchanged,pchanged: boolean;  //unsaved changes were made?
zrecchanged,precchanged: boolean;  //current rec changed?
isZsearch,isPsearch: boolean;  //whether LB's are full or not
IsZavodyHalf: boolean;
intaRowZ,intaRowP: integer; strCellBuf: string; //to check whether the Value has changed
theauthor : string;
SGInterResizing: boolean;
IsWinNT : boolean;
path: string; //path to ini file
receptacle: string; //копилка

FUNCTION FILTER ( STR : string ): string;
procedure InitGlobals;
procedure InsertRecord(rectype:integer;s:ansistring;i:integer);
procedure FillArrays(FillType:byte);
procedure FindRec(ss: string; RecType: byte; wf:word{which fields to search in});
procedure FillLBZ;
procedure CopyWebDescLink;
procedure FillLBP;
procedure ShowDetails(RecType:byte; n: word);
procedure RewriteZavody(filename: string);
procedure RewritePribory(filename: string);
procedure ShowTownInfo;
procedure StoreFieldValue(RecType,ARow : byte; Value:string);
procedure DeleteZ(n:integer);
procedure DeleteP(n:integer);
procedure AddZ;
procedure AddP;
procedure Restart(FillType:byte);
procedure ShowDesc; //show desc in appropriate app
procedure ClearSGDesc( RecType: byte);
function GetFileTime(FileName: string): string;
procedure ResizingAtMouseMoveAndDown( Sender: Byte; EventType: Byte;X,Y: Integer);
procedure SetActiveHalf( IsZavody: boolean);
procedure ExtractRec(RecType:byte; n:integer);
function ReplaceStr(const S, FindString, ReplaceString: string): string;
procedure ShowZweb;
function MakeFZStr(const S: string): string;
procedure ShowChemOnly;
procedure InterRecZ(sepstr:string);
procedure InterRecP(sepstr:string);
function strent(s1,s2: string; cs: integer) : boolean; //whether one str contains another or vice versa
procedure WriteIniFile;

IMPLEMENTATION
uses unit1, SelRec;

const
 zfn = 'zavody.txt';
 pfn = 'pribory.txt';

// -- Удаление пробелов в начале и в конце строки: ---
FUNCTION FILTER ( STR : String ): String;
VAR I, J, L : INTEGER;
BEGIN L := LENGTH ( STR ); //- определение длины строки
FOR I := 1 TO L DO // - перебор номеров символов слева направо
 IF STR[I] <> ' ' THEN
   FOR J := L DOWNTO I DO // - перебор номеров символов справа
    IF STR[J] <> ' ' THEN
     BEGIN
      FILTER := COPY (STR, i, j-i+1 ); // - копирование строки
      EXIT // - выход из функции
     END;
FILTER := ''; // - возврат пустой строки
END;

function IsWebLink (s:string):boolean;
begin
result := false;
if (pos('www.', s)<>0) or (pos('http:', s)<>0)
 then result := true;
end;

function ExtractFirstWebAddress(s:string):string;
var p: integer;
begin
result := '';
if not IsWebLink(s) then exit;
p := pos(' ', s);
while p <> 0 do begin
 if IsWebLink( copy(s,1,p-1) ) then begin
  result := copy(s,1,p-1);
  exit;
 end; //if
 delete(s,1,p);
 p := pos(' ', s);
end;;//while
if IsWebLink( s ) then 
  result := s;
end;

procedure FillFieldNameArrays;
begin
FNZ[1] := ZA[0].name;
FNZ[2] := ZA[0].numb;
FNZ[3] := ZA[0].sect;
FNZ[4] := ZA[0].town;
FNZ[5] := ZA[0].tel;
FNZ[6] := ZA[0].cont;
FNZ[7] := ZA[0].res;
FNZ[8] := ZA[0].dop;
FNZ[9] := ZA[0].notes;
FNZ[10] := ZA[0].mdate;
FNZ[11] := ZA[0].author;

FNP[1] := PA[0].mark;
FNP[2] := PA[0].name;
FNP[3] := PA[0].mfr;
FNP[4] := PA[0].price;
FNP[5] := PA[0].pricedate;
FNP[6] := PA[0].tod;
FNP[7] := PA[0].pprop;
FNP[8] := PA[0].sprop;
FNP[9] := PA[0].ex;
FNP[10] := PA[0].tugost;
FNP[11] := PA[0].desc;
FNP[12] := PA[0].anal;
FNP[13] := PA[0].notes;
FNP[14] := PA[0].mdate;
FNP[15] := PA[0].author;
end;

procedure FM(s2,s1: string); //fatal message
begin
Application.MessageBox(pchar(s1), pchar(s2), MB_OK + MB_DEFBUTTON1);
Application.Terminate;
end; //procedure FM(s: string);

function GetOSInfo : string;
var
  Platform: string;
  BuildNumber: Integer;
begin
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        Platform := 'Windows 9x';
        BuildNumber := Win32BuildNumber and $0000FFFF;
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        Platform := 'Windows NT';
        BuildNumber := Win32BuildNumber;
      end;
      else
      begin
        Platform := 'Windows';
        BuildNumber := 0;
      end;
  end;
    Platform := 'ОС ' + Platform;
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) or
    (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if Win32CSDVersion = '' then
      Result := Format('%s %d.%d (Build %d)', [Platform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber])
    else
      Result := Format('%s %d.%d (Build %d%s)', [Platform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber, Win32CSDVersion]);
  end
  else
    Result := Format('%s %d.%d', [Platform, Win32MajorVersion,
      Win32MinorVersion]);
end;

function Int2Bool (i: integer) : boolean;
begin
if i = 0 then Result := False else Result := True;
end;

procedure InitGlobals;
var
 Ini: Tinifile;
begin
//searchfields := 1;
if pos('NT', GetOSInfo) = 0
 then IsWinNT := false
 else IsWinNT := true;
if not IsWinNT then form1.mnuShowAllP.Visible := false;
wrz := defwrz; wrp := defwrp;
zchanged:=false; pchanged:=false;
zrecchanged:=false; precchanged:=false;
isZsearch:=false; isPsearch:=false;
SGInterResizing := false;
receptacle := '';

{if not FileExists('fd3.ini') then FM('Фатальная ошибка','Файл не найден: fd3.ini');
AssignFile (Fi, 'fd3.ini'); Reset (Fi);
Readln(Fi);Readln(Fi);Readln(Fi);//TiniFile
Readln(Fi,path);
Readln(Fi);
Readln(Fi, theauthor);
CloseFile(Fi);}
Ini:=TiniFile.Create(extractfilepath(paramstr(0))+'fd3.ini'); //открываем файл
path:=Ini.ReadString('Common','Path','e:\LAB\FD3\BASES\');
theauthor := Ini.ReadString('Common','Author','DefaultAuthor');
form1.mnuAutoSearch.Checked := int2bool(Ini.ReadInteger('Common','AutoSearch',0));
Ini.Free;
strFiZ := path + zfn;
strFiP := path + pfn;
strFittd := path + 'ttdiff.txt';
strFittc := path + 'ttcodes.txt';
strFidescpath := path + 'descpath.txt';
end;

procedure InsertRecord(rectype:integer;s:ansistring ; i:integer);
const SepChar = #9;
var
  StrLen: integer;
  TNum: integer;
  TEnd: integer;

begin
StrLen := Length(s);
TNum := 1;
TEnd := StrLen;
case rectype of
 1: // insert zavod
  while (TEnd <> 0) do
   begin
    TEnd := Pos(SepChar, s);
    if TEnd <> 0 then
    begin
      case tnum of
       1: ZA[i].name    := Copy(s, 1, TEnd - 1);
       2: ZA[i].numb    := Copy(s, 1, TEnd - 1);
       3: ZA[i].sect    := Copy(s, 1, TEnd - 1);
       4: ZA[i].town    := Copy(s, 1, TEnd - 1);
       5: ZA[i].tel     := Copy(s, 1, TEnd - 1);
       6: ZA[i].cont    := Copy(s, 1, TEnd - 1);
       7: ZA[i].res     := Copy(s, 1, TEnd - 1);
       8: ZA[i].dop     := Copy(s, 1, TEnd - 1);
       9: ZA[i].notes     := Copy(s, 1, TEnd - 1);
       10: ZA[i].mdate     := Copy(s, 1, TEnd - 1);
       11: ZA[i].author   := s;//Copy(s, 1, TEnd - 1);
      end;//case
      Delete(s, 1, TEnd);
      Inc(TNum);
    end
    else
    begin
      ZA[i].author := s;
    end;
   end; //while
 2: // insert pribor
  while (TEnd <> 0) do
   begin
    TEnd := Pos(SepChar, s);
    if TEnd <> 0 then
    begin
      case tnum of
       1: PA[i].mark            := Copy(s, 1, TEnd - 1);
       2: PA[i].name            := Copy(s, 1, TEnd - 1);
       3: PA[i].mfr             := Copy(s, 1, TEnd - 1);
       4: PA[i].price           := Copy(s, 1, TEnd - 1);
       5: PA[i].pricedate       := Copy(s, 1, TEnd - 1);
       6: PA[i].tod             := Copy(s, 1, TEnd - 1);
       7: PA[i].pprop           := Copy(s, 1, TEnd - 1);
       8: PA[i].sprop           := Copy(s, 1, TEnd - 1);
       9: PA[i].ex              := Copy(s, 1, TEnd - 1);
       10: PA[i].tugost         := Copy(s, 1, TEnd - 1);
       11: PA[i].desc           := Copy(s, 1, TEnd - 1);
       12: PA[i].anal           := Copy(s, 1, TEnd - 1);
       13: PA[i].notes          := Copy(s, 1, TEnd - 1);
       14: PA[i].mdate          := Copy(s, 1, TEnd - 1);
       15: PA[i].author         := s;//Copy(s, 1, TEnd - 1);
      end;//case
      Delete(s, 1, TEnd);
      Inc(TNum);
    end
    else
    begin
       PA[i].author         := s;//Copy(s, 1, TEnd - 1);
    end;
   end; //while
end; //case
end;


procedure FillArrays(FillType:byte);
var
 s: ansistring;
 i: integer;
label LP;
begin
if FillType = 2 then goto LP;
//filling zavody
Form1.SB1.Panels[0].Text := msgWaitLoading;
Application.ProcessMessages;
i:=-1;
AssignFile (Fi, strFiZ);
{$I-}
Reset (Fi);
{$I+}
if IOResult <> 0 then begin ShowMessage('Файл "Заводы" недоступен. Попробуйте перезагрузиться.'); exit; end;
repeat
 i := i + 1;
 readln(Fi, s);
 insertrecord(1,s,i);
until eof(fi);
CloseFile(Fi);
Zfiletime := GetFileTime(strFiZ);
nz := i;//-1;
if FillType = 1 then exit;
LP:
//filling pribory
Application.ProcessMessages;
Form1.SB1.Panels[0].Text := msgWaitLoading;
Pfiletime := GetFileTime(strFiP);
i:=-1;
AssignFile (Fi, strFiP);
{$I-}
Reset (Fi);
{$I+}
if IOResult <> 0 then begin ShowMessage('Файл "Приборы" недоступен. Попробуйте перезагрузиться.'); exit; end;
repeat
 i := i + 1;
 readln(Fi, s);
 insertrecord(2,s,i);
until eof(fi);
CloseFile(Fi);
np := i;//-1;
FillFieldNameArrays;
end;

function rec2str(rectype:byte; n,sel:word; sepstr : string) : string;
var s: string;
begin
s := '';

if rectype = 1 then
with ZA[n] do
 begin
  if sel >= 1024 then begin s:=s+name+sepstr; sel:=sel-1024 end;
  if sel >= 512 then begin s:=s+numb+sepstr; sel:=sel-512 end;
  if sel >= 256 then begin s:=s+sect+sepstr; sel:=sel-256 end;
  if sel >= 128 then begin s:=s+town+sepstr; sel:=sel-128 end;
  if sel >= 64 then begin s:=s+tel+sepstr; sel:=sel-64 end;
  if sel >= 32 then begin s:=s+cont+sepstr; sel:=sel-32 end;
  if sel >= 16 then begin s:=s+res+sepstr; sel:=sel-16 end;
  if sel >= 8 then begin s:=s+dop+sepstr; sel:=sel-8 end;
  if sel >= 4 then begin s:=s+notes+sepstr; sel:=sel-4 end;
  if sel >= 2 then begin s:=s+mdate+sepstr; sel:=sel-2 end;
  if sel >= 1 then begin s:=s+author+sepstr; sel:=sel-1 end;
 end;

if rectype = 2 then
with PA[n] do
 begin
  if sel >= 16384 then begin s:=s+mark+sepstr;    sel:=sel-16384 end;
  if sel >= 8192 then begin s:=s+name+sepstr;     sel:=sel-8192 end;
  if sel >= 4096 then begin s:=s+mfr+sepstr;      sel:=sel-4096 end;
  if sel >= 2048 then begin s:=s+price+sepstr;    sel:=sel-2048 end;
  if sel >= 1024 then begin s:=s+pricedate+sepstr;sel:=sel-1024 end;
  if sel >= 512  then begin s:=s+tod+sepstr;      sel:=sel-512 end;
  if sel >= 256  then begin s:=s+pprop+sepstr;    sel:=sel-256 end;
  if sel >= 128  then begin s:=s+sprop+sepstr;    sel:=sel-128 end;
  if sel >= 64   then begin s:=s+ex+sepstr;       sel:=sel-64 end;
  if sel >= 32   then begin s:=s+tugost+sepstr;   sel:=sel-32 end;
  if sel >= 16   then begin s:=s+desc+sepstr;     sel:=sel-16 end;
  if sel >= 8    then begin s:=s+anal+sepstr;     sel:=sel-8 end;
  if sel >= 4    then begin s:=s+notes+sepstr;    sel:=sel-4 end;
  if sel >= 2    then begin s:=s+mdate+sepstr;    sel:=sel-2 end;
  if sel >= 1    then begin s:=s+author+sepstr;   sel:=sel-1 end;
 end;

s := copy(s, 1, length(s)-length(sepstr));
Result := s;
end; //function rec2str

procedure AddItemToComboBox(RecType: byte; ss:string);
var
 CB: Tcombobox;
 i: integer;
begin
case rectype of
 1: CB := form1.EditFZ;
 2: CB := form1.EditFP;
end;
for i := cb.Items.Count - 1 downto 0 do
 if ss = CB.Items[i] then exit;
if cb.Items.Count < cb.DropDownCount then
 cb.Items.Add('emptyvalue');
if cb.Items.Count > 1 then
 for i := cb.Items.Count - 2 downto 0 do
  CB.Items[i+1] := CB.Items[i];
CB.Items[0] := ss;
end;

procedure FindRec(ss: string; RecType: byte; wf:word);

function IsMatch(RecType: byte; RecN: word; s:string; wf:word) : boolean;
var ts,s1,s2: string;
p: integer;
label L1;
begin
Result := false;
if s = '@@chem' then begin
  s := 'CHEM';
  if rectype = 1 then ts := ZA[RecN].sect;
  if rectype = 2 then ts := PA[RecN].ex;
  goto L1;
end;
s := filter(s);
ts := AnsiLowerCase( rec2str( rectype, recn, wf, #9 ) );

L1:
p := pos(s, ts);
if p = 0 then exit;
s1 := copy(ts, p-1, 1); //symbol prior to s
s2 := copy(ts, p+length(s), 1); //symbol next to s
if Form1.mnuWholeWordSearch.Checked then
  if ( (p = 1) or ( s1 = ' ' ) or ( s1 = #9 ) ) and
     ( (p = length(ts)-length(s)+1) or ( s2 = ' ' ) or ( s2 = #9 ) )
    then   Result := True
    else
else Result := True; //not Form1.mnuWholeWordSearch.Checked
end;

var
 sa : array[1..8] of string;
 sc : integer; //substrings qty
 p, c, i,j, n : integer; //
 IsRealMatch: boolean;
 LBF: TListBox;
 slb: string;//str for LB

const MaxFind = 1000;

begin //procedure FindRec
ss := filter(ss);
if ss = '' then exit;
AddItemToComboBox(RecType, ss);
case rectype of
 1: LBF := form1.LBZ;
 2: LBF := form1.LBP;
end;//case
ss := AnsiLowerCase(ss);
c := 0;
if Form1.mnuWholeWordSearch.Checked then begin
  c := 1;
  sa[c] := ss;
end
else
repeat
 inc(c);
 p := pos(' ',ss);
 if (p = 0) or (c > 7) then begin
  sa[c] := ss; //making "or" search array
  break;
 end;
 sa[c] := Copy(ss, 1, p - 1);
 if sa[c] = '' then c := c-1; //to avoid '  ' (double space) in search string
 Delete(ss, 1, p);
until c > 8;

{ //testing - writing substr in LB
for i := 1 to c do
 lbz.Items.Add (sa[i] + ' - ' + inttostr(length(sa[i])));
lbz.Items.Add ('total: ' + inttostr(c) + ' substr')
end;
}

case rectype of
 1: begin
     n := nz;
     isZsearch:=true;
    end;
 2: begin
     n := np;
     isPsearch:=true;
    end;
end;//case
LBF.Clear;
for i := 1 to n do begin//scanning Zavody / Pribory
 IsRealMatch := true;
 for j := 1 to c do
  if not IsMatch(RecType, i, sa[j], wf) then begin
   IsRealMatch := false;
   break;
  end;
 if not IsRealMatch then continue;
 case rectype of
  1: slb := ZA[i].name + ' ' + ZA[i].town;
  2: slb := PA[i].name + ' ' + PA[i].mark;
 end;//case
 LBF.Items.Add (slb);
 case rectype of
  1: CLBZ[LBF.Items.Count] := i;
  2: CLBP[LBF.Items.Count] := i;
 end;//case
 if LBF.Items.Count >= MaxFind then break;
end; // for i := 1 to nz do
ClearSGDesc(RecType);
if LBF.Items.Count > 0 then ShowDetails(RecType, 0);
 case rectype of
  1: begin
      currentz := 1;
      Form1.LBZ.ItemIndex := 0;
     end;
  2: begin
      currentp := 1;
      Form1.LBP.ItemIndex := 0;
     end;
 end;//case

Form1.SB1.Panels[0].Text := 'Записей найдено: '+inttostr(LBF.Items.Count);
end;

procedure ShowZweb;
var s: string;
begin
 s := Form1.SGZinfo.cells[1,6];
 s := ExtractFirstWebAddress(s);
 if s = ''
  then ShowMessage('Не обнаружена ссылка на web-ресурс')
  else ShellExecute(form1.Handle, 'open', pchar(s), nil, nil, SW_SHOWNORMAL);
end;

procedure ShowTownInfo;
const strnodata = 'нет данных';
var
 s, ttc,ttd, thetown,thediff: string;
 i: integer;
begin
with Form1.SGZinfo do begin
 ttc := strnodata;
 ttd := strnodata;
 thetown := ''; thediff := '';
 s := cells[1,3];
 for i := 1 to nttc do
  if strent(s,TTCA[1,i],0) then begin
    ttc := TTCA[2,i];
    thetown := TTCA[1,i];
    break;
  end;
 for i := 1 to nttd do
  if strent(s,TTDA[1,i],0) then begin
    ttd := TTDA[2,i];
    thediff := TTDA[1,i];
    break;
  end;
ShowMessage( s+#10#13+'Код: ' + ttc + '  (' + thetown + ')' + #10#13 +
        'Разница во времени: ' + ttd + '  (' + thediff + ')');
end;//if
end;

procedure FillLBZ;
var
 i: integer;
begin
Form1.LBZ.Clear;
 for i := 1 to nz do begin
  Form1.LBZ.Items.Add (ZA[i].name + ' ' + ZA[i].town);
  CLBZ[Form1.LBZ.Items.Count] := i;
 end;
isZsearch:=false;
end;

procedure FillLBP;
var
 i: integer;
begin
Form1.LBP.Clear;
 for i := 1 to np do begin
  Form1.LBP.Items.Add (PA[i].name + ' ' + PA[i].mark);
  CLBP[Form1.LBP.Items.Count] := i;
 end;
isPsearch:=false;
end;

procedure ShowDetails(RecType:byte; n: word);
var
 LBF: TListBox;
 SGD: TStringGrid; //SG for details
 recz: zavstruc;
 recp: pribstruc;
begin
n := n + 1;//LB index begins with '0'
case rectype of
 1: begin
  LBF := form1.LBZ;
  SGD := form1.SGZinfo;
  recz := ZA[ CLBZ[n] ];
  with SGD do begin
   cells[1,0] := recz.name;
   cells[1,1] := recz.numb;
   cells[1,2] := recz.sect;
   cells[1,3] := recz.town;
   cells[1,4] := recz.tel;
   cells[1,5] := recz.cont;
   cells[1,6] := recz.res;
   cells[1,7] := recz.dop;
   cells[1,8] := recz.notes;
   cells[1,9] := recz.mdate;
   cells[1,10] := recz.author;
  end;//with
    end;
 2: begin
  LBF := form1.LBP;
  SGD := form1.SGPinfo;
  recp := PA[ CLBP[n] ];
  with SGD do begin
   cells[1,0] := recp.mark;
   cells[1,1] := recp.name;
   cells[1,2] := recp.mfr;
   cells[1,3] := recp.price;
   cells[1,4] := recp.pricedate;
   cells[1,5] := recp.tod;
   cells[1,6] := recp.pprop;
   cells[1,7] := recp.sprop;
   cells[1,8] := recp.ex;
   cells[1,9] := recp.tugost;
   cells[1,10] := recp.desc;
   cells[1,11] := recp.anal;
   cells[1,12] := recp.notes;
   cells[1,13] := recp.mdate;
   cells[1,14] := recp.author;
//   cells[1,8] := recp.notes;
  end;//with
    end;
end;//case
end;

procedure RewriteZavody(filename: string);
var Fout:textfile;
 i:word;
begin
Form1.SB1.Panels[0].Text := msgWaitSaving;
Application.ProcessMessages;
assignfile(Fout,filename); rewrite(fout);
for i := 0 to nz do
 with ZA[i] do
  writeln(fout, name+#9+numb+#9+sect+#9+town+#9+tel+#9+cont+#9+res+#9+dop+#9+notes+#9+mdate+#9+author);
closefile(Fout);
Zfiletime := GetFileTime(strFiZ);
zchanged:=false;
end;
procedure RewritePribory(filename: string);
var Fout:textfile;
 i:word;
begin
Form1.SB1.Panels[0].Text := msgWaitSaving;
Application.ProcessMessages;
assignfile(Fout,filename); rewrite(fout);
for i := 0 to np do
 with PA[i] do
  writeln(fout, mark+#9+name+#9+mfr+#9+price+#9+pricedate+#9+tod+#9+pprop+#9+sprop+#9+ex+#9+tugost+#9+desc+#9+anal+#9+notes+#9+mdate+#9+author);
closefile(Fout);
Pfiletime := GetFileTime(strFiP);
pchanged:=false;
end;

procedure StoreFieldValue(RecType,ARow : byte; Value:string);
begin
//try
WITH FORM1 do
case rectype of
 1:
  case arow of
   0: ZA[ CLBZ[LBZ.ItemIndex+1] ].name := Value;
   1: ZA[ CLBZ[LBZ.ItemIndex+1] ].numb := Value;
   2: ZA[ CLBZ[LBZ.ItemIndex+1] ].sect := Value;
   3: ZA[ CLBZ[LBZ.ItemIndex+1] ].town := Value;
   4: ZA[ CLBZ[LBZ.ItemIndex+1] ].tel := Value;
   5: ZA[ CLBZ[LBZ.ItemIndex+1] ].cont := Value;
   6: ZA[ CLBZ[LBZ.ItemIndex+1] ].res := Value;
   7: ZA[ CLBZ[LBZ.ItemIndex+1] ].dop := Value;
   8: ZA[ CLBZ[LBZ.ItemIndex+1] ].notes := Value;
   9: ZA[ CLBZ[LBZ.ItemIndex+1] ].mdate := Value;
   10: ZA[ CLBZ[LBZ.ItemIndex+1] ].author := Value;
  end; //case arow of
 2:
  case arow of
   0: PA[ CLBP[LBP.ItemIndex+1] ].mark := Value;
   1: PA[ CLBP[LBP.ItemIndex+1] ].name:= Value;
   2: PA[ CLBP[LBP.ItemIndex+1] ].mfr:= Value;
   3: PA[ CLBP[LBP.ItemIndex+1] ].price:= Value;
   4: PA[ CLBP[LBP.ItemIndex+1] ].pricedate:= Value;
   5: PA[ CLBP[LBP.ItemIndex+1] ].tod:= Value;
   6: PA[ CLBP[LBP.ItemIndex+1] ].pprop:= Value;
   7: PA[ CLBP[LBP.ItemIndex+1] ].sprop:= Value;
   8: PA[ CLBP[LBP.ItemIndex+1] ].ex:= Value;
   9: PA[ CLBP[LBP.ItemIndex+1] ].tugost:= Value;
   10: PA[ CLBP[LBP.ItemIndex+1] ].desc := Value;
   11: PA[ CLBP[LBP.ItemIndex+1] ].anal := Value;
   12: PA[ CLBP[LBP.ItemIndex+1] ].notes := Value;
   13: PA[ CLBP[LBP.ItemIndex+1] ].mdate := Value;
   14: PA[ CLBP[LBP.ItemIndex+1] ].author := Value;
//   8: ZA[ CLBZ[Form1.LBZ.ItemIndex+1] ].notes := Value;
  end; //case arow of
end;//case rectype of
//except
//on ERangeError do exit;
//end;
end;

procedure DeleteZ(n:integer);
var i : integer;
begin
if Form1.LBZ.ItemIndex = -1 then exit;
if application.MessageBox(pchar(ZA[ CLBZ[n] ].name), pchar('Удалить завод?'), 1) <> 1
   then exit;
ZA[CLBZ[n]].author := theauthor + ' d>';
ZA[CLBZ[n]].notes := 'УДАЛЕН : ' + ZA[CLBZ[n]].notes;
ZA[CLBZ[n]].mdate := DateToStr(Date)+' '+TimeToStr(Time);
ShowDetails(1, Form1.LBZ.ItemIndex);
zchanged := true;
end;
procedure DeleteP(n:integer);
var i : integer;
begin
if Form1.LBP.ItemIndex = -1 then exit;
if application.MessageBox(pchar(PA[CLBP[n]].name + ' ' + PA[CLBP[n]].mark),
                          pchar('Удалить прибор?'), 1) <> 1
   then exit;
pA[CLBp[n]].author := theauthor + ' d>';
pA[CLBp[n]].notes := 'УДАЛЕН : ' + pA[CLBp[n]].notes;
pA[CLBp[n]].mdate := DateToStr(Date)+' '+TimeToStr(Time);
ShowDetails(2, Form1.LBp.ItemIndex);
pchanged := true;
end;

procedure AddZ;
var t: byte;
begin
nz := nz + 1;
with ZA[nz] do begin
   name := 'Новый завод';
   numb:='';sect:='';town:='';tel:='';
   cont:='';res:='';dop:='';notes:='';
   mdate := DateToStr(Date)+' '+TimeToStr(Time);
   author := theauthor + ' a>';
end; //with ZA[nz] do begin
Form1.LBZ.Items.Add (ZA[nz].name);
CLBZ[Form1.LBZ.Items.Count] := nz;
Form1.LBZ.ItemIndex := Form1.LBZ.Items.Count - 1;
ShowDetails(1,Form1.LBZ.ItemIndex);
zchanged := true;
end;
procedure AddP;
var t: byte;
begin
np := np + 1;
with PA[np] do begin
   mark := 'Новый прибор';
   name:='';mfr:='';price:='';pricedate:='';
   pprop:='';sprop:='';
   tod:='';tugost:='';desc:='';anal:='';
   notes:='';//notes:='';
   mdate := DateToStr(Date)+' '+TimeToStr(Time);
   author := theauthor + ' a>';
end; //with ZA[nz] do begin
Form1.LBP.Items.Add ( PA[np].name + ' ' + PA[np].mark );
CLBP[Form1.LBP.Items.Count] := np;
Form1.LBP.ItemIndex := Form1.LBP.Items.Count - 1;
ShowDetails(2,Form1.LBP.ItemIndex);
pchanged := true;
end;

procedure FillTTA;
var
 i, g, pos9: integer;
 strf, s1,s2, s: string;
begin
strf := strFittc;
for g := 1 to 2 do begin
 if g = 2 then strf := strFittd;
 i := 0;
 AssignFile (Fi, strf);
 {$I-}
 Reset (Fi);
 {$I+}
 if IOResult <> 0 then begin ShowMessage('Некритичная ошибка. Файл '+strf+' недоступен. '+#10#13+'Соответствующие данные будут недоступны.'); continue; end;
  readln(Fi, s);  readln(Fi, s);
 repeat
  i := i + 1;
  readln(Fi, s);
  pos9 := pos(#9, s);
  if pos9 = 0 then continue;
  s1 := copy(s, 1, pos9-1);
  s2 := copy(s, pos9+1, length(s)-pos9);
  if g = 1 then begin
   TTCA[1,i] := s1;
   TTCA[2,i] := s2;
  end;
  if g = 2 then begin
   TTDA[1,i] := s1;
   TTDA[2,i] := s2;
  end;
 until eof(fi);
 CloseFile(Fi);
 if g = 1 then nttc := i;
 if g = 2 then nttd := i;
end;
end;

procedure FillDPA;
var
 i: integer;
 Fi: textfile;
 s: string;
begin
 AssignFile (Fi, strfidescpath);
 {$I-}
 Reset (Fi);
 {$I+}
 if IOResult <> 0 then begin ShowMessage('Некритичная ошибка. Файл '+strfidescpath+' недоступен. '+#10#13+'Соответствующие данные будут недоступны.'); exit; end;
readln(fi);readln(fi);
i := 0;
repeat
 inc(i);
 readln(fi,s);
 DPA[i] := s;
until eof(fi);
closefile(fi);
dpaq := i;
end;

function Gethref2(s:string) : string;
var ss: string; a: integer;
begin
s := ReplaceStr(s, '\site\', '\SITE\');
a := pos('\SITE\', s);
if a = 0 then exit;
s := copy(s, a+6, length(s)-a-5);
s := ReplaceStr(s, 'laborant', 'http://www.laborant.ru');
s := ReplaceStr(s, 'measurem', 'http://www.measurement.ru');
s := ReplaceStr(s, 'himiya', 'http://www.himiya.ru');
Result := ReplaceStr(s, '\', '/');
end;

function short_el2_2_long(s: string) : string;
var
s1, s2: string;
p: integer;
begin
//12345678910
//10-7
//el2#t07_13#386
//http://laborant.ru/el2/rec.php?tbl_id=kkspi&rec_id=39
//http://laborant.ru/el2/rec.php?tbl_id=t07_13&rec_id=376
s := copy(s, 5, length(s)-4);
p := pos('#', s);
s1 := copy(s, 1, p-1);
if copy(s1,1,3) = 't07' then s1 := s1 + '_view';
s2 := copy(s, p+1, length(s)-p);
Result := 'http://laborant.ru/el2/rec.php?tbl_id=' + s1 + '&rec_id=' + s2;
end;//function short_el2_2_long

procedure CopyWebDescLink;
var s: string; i: integer;
begin
 s := Form1.SGPinfo.Cells[1,10];
 if copy(s,1,4) = 'el2#' then begin //el2 link
  s := short_el2_2_long(s);
  Clipboard.asText := s;
  ShowMessage('Ссылка на описание на сайте скопирована в буфер' +#10#13 + Clipboard.asText);
  exit;
 end;
 if length(s) > 5 then
  for i := 1 to dpaq do
   if fileexists(DPA[i]+s) or directoryexists(DPA[i]+s) then begin
    Clipboard.asText := Gethref2( DPA[i] + s);
    ShowMessage('Ссылка на описание на сайте скопирована в буфер' +#10#13 + Clipboard.asText);
    exit;
   end;
ShowMessage('Файл описания не найден');
end;

procedure ShowDesc; //show desc in appropriate app


var s: string; i: integer;
begin//procedure ShowDesc
s := Form1.SGPinfo.Cells[1,10];
if copy(s,1,4) = 'el2#' then begin //el2 link
  s := short_el2_2_long(s);
  s := ReplaceStr(s, 'laborant.ru/el2', '192.168.5.10');
  ShellExecute(form1.Handle, 'open', pchar(s), nil, nil, SW_SHOWNORMAL);
  exit;
end;
if length(s) > 5 then //extracting local link
  for i := 1 to dpaq do
   if fileexists(DPA[i]+s) or directoryexists(DPA[i]+s) then begin
    ShellExecute(form1.Handle, 'open', pchar(DPA[i]+s), nil, nil, SW_SHOWNORMAL);
    exit;
   end;
if IsWebLink(s) then begin //extracting www link
  ShellExecute(form1.Handle, 'open', pchar(s), nil, nil, SW_SHOWNORMAL);
  exit;
end;
ShowMessage('Файл описания не найден');
end;

procedure Restart(FillType:byte);
begin
InitGlobals;
FillArrays(FillType);
FillSGZinfoHead;
FillSGPinfoHead;
FillDPA;
//if (filltype = 1) or (filltype = 3) then FillLBZ;
//if (filltype > 1) then FillLBP;
if filltype = 3 then
 FillTTA;
Form1.SB1.Panels[0].Text := 'Заводов: '+inttostr(nz)+'. Приборов: '+inttostr(np);
IsZavodyHalf := True;
//Form1.LBZ.Items.
end;

function strent(s1,s2: string; cs: integer) : boolean;
begin
if cs = 0 then begin
 s1 := ansilowercase(s1);
 s2 := ansilowercase(s2);
end;
result := false;
if ( pos( s1,s2 ) <> 0 ) or ( pos( s2,s1 ) <> 0 ) then result := true;
end;

procedure ExtractRec(RecType:byte; n:integer);
var
 nq, i: integer;
 SA: array[1..20] of string;
begin
case RecType of
1: begin
   SA[1]  := FNZ[1] + ': ' + ZA[CLBZ[n]].name;
   SA[2]  := FNZ[2] + ': ' + ZA[CLBZ[n]].numb;
   SA[3]  := FNZ[3] + ': ' + ZA[CLBZ[n]].sect;
   SA[4]  := FNZ[4] + ': ' + ZA[CLBZ[n]].town;
   SA[5]  := FNZ[5] + ': ' + ZA[CLBZ[n]].tel;
   SA[6]  := FNZ[6] + ': ' + ZA[CLBZ[n]].cont;
   SA[7]  := FNZ[7] + ': ' + ZA[CLBZ[n]].res;
   SA[8]  := FNZ[8] + ': ' + ZA[CLBZ[n]].dop;
   SA[9]  := FNZ[9] + ': ' + ZA[CLBZ[n]].notes;
   SA[10] := FNZ[10] + ': ' + ZA[CLBZ[n]].mdate;
   SA[11] := FNZ[11] + ': ' + ZA[CLBZ[n]].author;
   nq := 13;
  end;
2: begin
   SA[1]   := FNP[1] + ': ' + PA[CLBP[n]].mark;
   SA[2]   := FNP[2] + ': ' + PA[CLBP[n]].name;
   SA[3]   := FNP[3] + ': ' + PA[CLBP[n]].mfr;
   SA[4]   := FNP[4] + ': ' + PA[CLBP[n]].price;
   SA[5]   := FNP[5] + ': ' + PA[CLBP[n]].pricedate;
   SA[6]   := FNP[6] + ': ' + PA[CLBP[n]].tod;
   SA[7]   := FNP[7] + ': ' + PA[CLBP[n]].pprop;
   SA[8]   := FNP[8] + ': ' + PA[CLBP[n]].sprop;
   SA[9]   := FNP[9] + ': ' + PA[CLBP[n]].ex;
   SA[10]  := FNP[10] + ': ' + PA[CLBP[n]].tugost;
   SA[11]  := FNP[11] + ': ' + PA[CLBP[n]].desc;
   SA[12]  := FNP[12] + ': ' + PA[CLBP[n]].anal;
   SA[13]  := FNP[13] + ': ' + PA[CLBP[n]].notes;
   SA[14]  := FNP[14] + ': ' + PA[CLBP[n]].mdate;
   SA[15]  := FNP[15] + ': ' + PA[CLBP[n]].author;
   nq := 15;
  end;
end;
SA[nq-1] := '------------------------------------------------';
SA[nq] := '';
with frmSelRec do begin
show;
for i := 1 to nq do
 memo1.Lines.Add (SA[i]);
end; //with frmSelRec do begin
end;

function GetFileTime(FileName: string): string;
var  FHandle: Integer; Fin: textfile;
begin
AssignFile (Fin, FileName);
{$I-}
Reset (Fin);
{$I+}
if IOResult <> 0 then begin ShowMessage('Файл недоступен'); exit; end;
CloseFile (Fin);
  FHandle := FileOpen(FileName, 0);
  try
    Result := DateTimeToStr(FileDateToDateTime(FileGetDate(FHandle)));
//    Result := copy(Result, length(Result)-8, 8);
  finally
    FileClose(FHandle);
  end;
end;

procedure ClearSGDesc( RecType: byte);
var
 SG: TStringGrid;
 i: integer;
begin
if RecType = 1 then SG := Form1.SGZinfo
               else SG := Form1.SGPinfo;
for i := 0 to SG.RowCount-1 do
 SG.Cells[1,i] := '';
end;

procedure ResizingAtMouseMoveAndDown( Sender: Byte; EventType: Byte; X,Y: Integer);
const a=10;
var
 LB: TListBox;
 SG: TStringGrid;
 Ed: TComboBox;
 Btn: TButton;
 Pan: TPanel;
begin
with form1 do
 case sender of
  1: begin
      LB := lbz;
      SG := sgzinfo;
      Ed := Editfz;
      Btn:= btnfindz;
      Pan:= Panel11;
     end;
  2: begin
      LB := lbp;
      SG := sgpinfo;
      Ed := Editfp;
      Btn:= btnfindp;
      Pan:= Panel12;
     end;
 end;///case
case EventType of
 1: begin  //MouseMove
        if ( X > LB.Width - a ) and ( X < LB.Width + a )
         then Pan.Cursor := crHSplit;
        if (InterResizing=true) and (X > 4*a) and (X < Form1.Width - 5*a )then
         begin
          LB.Width := X;
          btn.Width := X;
          Ed.width := X;
          sg.Width := form1.width - a - LB.Width;
          sg.ColWidths[1]:=sg.Width-sg.ColWidths[0] - 4;
         end;
    end;
 2: begin //MouseDown
         if ( X > LB.Width - a ) and ( X < LB.Width + a )
                  then  InterResizing := true;
    end;
end;//case
end;

procedure SetActiveHalf(IsZavody: boolean);
var
 LB: TListBox;
 SG: TStringGrid;
 Ed: TComboBox;
 Btn: TButton;
 Pan: TPanel;
begin
with form1 do
 if iszavody then
     begin
      LB := lbz;
      SG := sgzinfo;
      Ed := Editfz;
      Btn:= btnfindz;
      Pan:= Panel11;
     end
   else
     begin
      LB := lbp;
      SG := sgpinfo;
      Ed := Editfp;
      Btn:= btnfindp;
      Pan:= Panel12;
     end;
IsZavodyHalf := IsZavody;
//Ed.SetFocus;
form1.btnfindp.Default := false;
form1.btnfindz.Default := false;
btn.Default := true;
end;

//Функция заменяет в строке S все вхождения подстроки FindString на подстроку,  
//переданную в качестве аргумента ReplaceString

function ReplaceStr(const S, FindString, ReplaceString: string): string;
var
  L1, L2, I: Integer;
begin
  Result := S;
//  if pos(findstring,s) = 0 then exit;
  L1 := Length(FindString);
  if L1 >  0 then
  begin
    L2 := Length(ReplaceString);
    repeat
      I := Pos(FindString, Result);
      if I >  0 then
      begin
        Delete(Result, I, L1);
        if L2 >  0 then Insert(ReplaceString, Result, I)
      end;
      if pos(FindString, ReplaceString) <> 0 then break;
    until I = 0
  end
end;

function MakeFZStr(const S: string): string;
begin
Result := ReplaceStr(S, '/', ' ');
Result := ReplaceStr(Result, '\', ' ');
Result := ReplaceStr(Result, '-', ' ');
Result := ReplaceStr(Result, '@', ' ');
Result := ReplaceStr(Result, '"', ' ');
Result := ReplaceStr(Result, ',', ' ');
Result := ReplaceStr(Result, '(', ' ');
Result := ReplaceStr(Result, ')', ' ');
Result := ReplaceStr(Result, '.', ' ');
Result := ReplaceStr(Result, 'городе', '');
Result := ReplaceStr(Result, '  ', ' ');
end;

procedure ShowChemOnly;
begin
FindRec('@@chem', 1, 256);
FindRec('@@chem', 2, 16);
Form1.SB1.Panels[0].Text := 'Только Химия';
end;

function UnifyStr(const Ss: string): string;
var s: string;
begin
s := ReplaceStr(Ss, '/', '');
s := ReplaceStr(s, '\', '');
s := ReplaceStr(s, '-', '');
s := ReplaceStr(s, '@', '');
s := ReplaceStr(s, '"', '');
s := ReplaceStr(s, '#', '');
s := ReplaceStr(s, '$', '');
s := ReplaceStr(s, '%', '');
s := ReplaceStr(s, '^', '');
s := ReplaceStr(s, '&', '');
s := ReplaceStr(s, '''', '');
s := ReplaceStr(s, '+', '');
s := ReplaceStr(s, '=', '');
s := ReplaceStr(s, ']', '');
s := ReplaceStr(s, '[', '');
s := ReplaceStr(s, '{', '');
s := ReplaceStr(s, '}', '');
s := ReplaceStr(s, ':', '');
s := ReplaceStr(s, ';', '');
s := ReplaceStr(s, ',', '');
s := ReplaceStr(s, '(', '');
s := ReplaceStr(s, ')', '');
s := ReplaceStr(s, '.', '');
s := ReplaceStr(s, 'город', '');
s := ansilowercase(s);
s := ReplaceStr(s, ' типа', '');
s := ReplaceStr(s, 'типов ', '');
s := ReplaceStr(s, 'серии ', '');
s := ReplaceStr(s, 'прибор', '');
//s := ReplaceStr(s, ' ', '');
Result := s;
end;

function UnifyZRec(rec: ZAVSTRUC): ZAVSTRUC;
var r: ZAVSTRUC;
begin
r.name := UnifyStr(rec.name);
r.numb := UnifyStr(rec.numb);
r.sect := UnifyStr(rec.sect);
r.town := UnifyStr(rec.town);
r.tel := UnifyStr(rec.tel);
r.cont := UnifyStr(rec.cont);
r.res := UnifyStr(rec.res);
r.dop := UnifyStr(rec.dop);
r.notes := UnifyStr(rec.notes);
r.mdate := rec.mdate;
r.author := rec.author;
//r.mdate := UnifyStr(rec.mdate);
//r.author := UnifyStr(rec.author);
result := r;
end;
function UnifyPRec(rec: PRIBSTRUC): PRIBSTRUC;
var r: PRIBSTRUC;
begin
r.mark := UnifyStr(rec.mark);
r.name := UnifyStr(rec.name);
r.mfr := UnifyStr(rec.mfr);
r.price := UnifyStr(rec.price);
r.pricedate := UnifyStr(rec.pricedate);
r.tod := UnifyStr(rec.tod);
r.pprop := UnifyStr(rec.pprop);
r.sprop := UnifyStr(rec.sprop);
r.ex := UnifyStr(rec.ex);
r.tugost := UnifyStr(rec.tugost);
r.desc := UnifyStr(rec.desc);
r.anal := UnifyStr(rec.anal);
r.notes := UnifyStr(rec.notes);
r.mdate := rec.mdate;
r.author := rec.author;
result := r;
end;

procedure InterRecZ(sepstr:string);
var
 c,i1,i2: integer;
 Reverse: boolean;
 i,j : word;
 r1,r2,ru1,ru2: ZAVSTRUC;
 sr1,sr2,sr_ : string; //wholerec as str
label SelErr;
begin
i1 := -1; i2 := -1;
for i := 0 to (Form1.LBZ.Items.Count - 1) do begin
    if Form1.LBZ.Selected[i] then
     if i1 = -1 then i1 := i+1
                else if i2 = -1 then i2 := i+1
                                else goto SelErr;
  end;
if i2 = -1 then goto SelErr;
sr1 := rec2str(1, CLBZ[i1], maxwrz-2-1, #9);
sr2 := rec2str(1, CLBZ[i2], maxwrz-2-1, #9);
if length(sr1) > length(sr2)
then begin
       Reverse:=True;
       r1 := ZA[CLBZ[i1]];
       r2 := ZA[CLBZ[i2]];
     end
else begin
       Reverse:=False;
       r2 := ZA[CLBZ[i1]];
       r1 := ZA[CLBZ[i2]];
       sr_ := sr1;
       sr1 := sr2;
       sr2 := sr_;
      end;

ru1 := UnifyZRec(r1); ru2 := UnifyZRec(r2);
//if (ru1.numb = '') and (ru2.numb <> '') and (pos(ru2.numb,sr1)=0) then   r1.numb := r2.numb;
//if (ru1.sect = '') and (ru2.sect <> '') and (pos(ru2.sect,sr1)=0) then  r1.sect := r2.sect;
//if length(ru1.town) < length(ru2.town) then r1.town := r2.town;
if ru2.numb <> '' then
 if pos(ru2.numb, sr1) = 0 then
  if ru1.numb = '' then r1.numb := r2.numb
                   else r1.numb := r1.numb + sepstr + r2.numb;
if ru2.sect <> '' then
 if pos(ru2.sect, sr1) = 0 then
  if ru1.sect = '' then r1.sect := r2.sect
                  else r1.sect := r1.sect + sepstr + r2.sect;
if ru2.town <> '' then
 if pos(ru2.town, sr1) = 0 then
  if ru1.town = '' then r1.town := r2.town
                  else r1.town := r1.town + sepstr + r2.town;
if ru2.tel <> '' then
 if pos(ru2.tel, sr1) = 0 then
  if ru1.tel = '' then r1.tel := r2.tel
                  else r1.tel := r1.tel + sepstr + r2.tel;
if ru2.cont <> '' then
 if pos(ru2.cont, sr1) = 0 then
  if ru1.cont = '' then r1.cont := r2.cont
                  else r1.cont := r1.cont + sepstr + r2.cont;
if ru2.res <> '' then
 if pos(ru2.res, sr1) = 0 then
  if ru1.res = '' then r1.res := r2.res
                  else r1.res := r1.res + sepstr + r2.res;
if ru2.dop <> '' then
 if pos(ru2.dop, sr1) = 0 then
  if ru1.dop = '' then r1.dop := r2.dop
                  else r1.dop := r1.dop + sepstr + r2.dop;
if ru2.notes <> '' then
 if pos(ru2.notes, sr1) = 0 then
  if ru1.notes = '' then r1.notes := r2.notes
                  else r1.notes := r1.notes + sepstr + r2.notes;
if r1.author = '' then
  if r2.author = '' then r1.author := 'Соединенная'
                   else r1.author := r2.author
 else r1.author := r1.author + sepstr + r2.author;
If not Reverse
 then begin
  ZA[CLBZ[i2]].author := 'd>';
  ZA[CLBZ[i1]] := r1;
 end
 else begin
  ZA[CLBZ[i1]].author := 'd>';
  ZA[CLBZ[i2]] := r1;
 end;


exit;
SelErr:
ShowMessage('Должны быть выбраны ДВЕ записи!');
exit;
end;

procedure InterRecP(sepstr:string);
var
 c,i1,i2: integer;
 Reverse: boolean;
 i,j : word;
 r1,r2,ru1,ru2: PRIBSTRUC;
 sr1,sr2,sr_ : string; //wholerec as str
label SelErr;
begin
i1 := -1; i2 := -1;
for i := 0 to (Form1.LBp.Items.Count - 1) do begin
    if Form1.LBp.Selected[i] then
     if i1 = -1 then i1 := i+1
                else if i2 = -1 then i2 := i+1
                                else goto SelErr;
  end;
if i2 = -1 then goto SelErr;
sr1 := rec2str(2, CLBp[i1], maxwrp-2-1, #9);
sr2 := rec2str(2, CLBp[i2], maxwrp-2-1, #9);
if length(sr1) > length(sr2)
then begin
       Reverse:=True;
       r1 := pA[CLBp[i1]];
       r2 := pA[CLBp[i2]];
     end
else begin
       Reverse:=False;
       r2 := pA[CLBp[i1]];
       r1 := pA[CLBp[i2]];
       sr_ := sr1;
       sr1 := sr2;
       sr2 := sr_;
      end;

ru1 := UnifypRec(r1); ru2 := UnifypRec(r2);

if ru2.mfr <> '' then
 if pos(ru2.mfr, sr1) = 0 then
  if ru1.mfr = '' then r1.mfr := r2.mfr
                  else r1.mfr := r1.mfr + sepstr + r2.mfr;

if (ru1.price = '') and (ru2.price <> '') {and (pos(ru2.numb,sr1)=0)} then begin
  r1.price := r2.price;
  if ru2.pricedate <> '' then r1.pricedate := r2.pricedate;
  if ru2.tod <> '' then r1.tod := r2.tod;
end
else begin
  if ru1.pricedate = '' then r1.pricedate := r2.pricedate;
  if ru1.tod = '' then r1.tod := r2.tod;
end;

if ru2.pprop <> '' then
 if pos(ru2.pprop, sr1) = 0 then
  if ru1.pprop = '' then r1.pprop := r2.pprop
                  else r1.pprop := r1.pprop + sepstr + r2.pprop;

if ru2.sprop <> '' then
 if pos(ru2.sprop, sr1) = 0 then
  if ru1.sprop = '' then r1.sprop := r2.sprop
                  else r1.sprop := r1.sprop + sepstr + r2.sprop;

if ru2.ex <> '' then
 if pos(ru2.ex, sr1) = 0 then
  if ru1.ex = '' then r1.ex := r2.ex
                  else r1.ex := r1.ex + sepstr + r2.ex;

if ru2.tugost <> '' then
 if pos(ru2.tugost, sr1) = 0 then
  if ru1.tugost = '' then r1.tugost := r2.tugost
                  else r1.tugost := r1.tugost + sepstr + r2.tugost;

if (ru1.desc = '') and (ru2.desc <> '') and (pos(ru2.desc,sr1)=0) then
  r1.desc := r2.desc;

if ru2.anal <> '' then
 if pos(ru2.anal, sr1) = 0 then
  if ru1.anal = '' then r1.anal := r2.anal
                  else r1.anal := r1.anal + sepstr + r2.anal;

if ru2.notes <> '' then
 if pos(ru2.notes, sr1) = 0 then
  if ru1.notes = '' then r1.notes := r2.notes
                  else r1.notes := r1.notes + sepstr + r2.notes;

if r1.author = '' then
  if r2.author = '' then r1.author := 'Соединенная'
                   else r1.author := r2.author + sepstr
 else r1.author := r1.author + sepstr + r2.author;

If not Reverse
 then begin
  PA[CLBP[i2]].author := 'd>';
  PA[CLBP[i1]] := r1;
 end
 else begin
  PA[CLBP[i1]].author := 'd>';
  PA[CLBP[i2]] := r1;
 end;


exit;
SelErr:
ShowMessage('Должны быть выбраны ДВЕ записи!');
exit;
end;

procedure WriteIniFile;
var Ini: Tinifile;
begin
with form1 do begin
Ini:=TiniFile.Create(extractfilepath(paramstr(0))+'fd3.ini'); //открываем файл
Ini.WriteString('Common','Path',path);
Ini.WriteString('Common','Author',theauthor);
Ini.WriteInteger('Common','AutoSearch',ord(mnuAutoSearch.Checked));//boolean
Ini.Free;

end; //with
end;

END.

