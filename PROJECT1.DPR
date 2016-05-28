program Project1;

{%ToDo 'PROJECT1.todo'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UNIT_LR7 in 'unit_lr7.pas',
  Splash in 'splash.pas' {SplashForm},
  SelRec in 'SelRec.pas' {frmSelRec},
  SelFields in 'SelFields.pas' {frmSFdlg};

{$R *.RES}

begin
  Application.Initialize;
  SplashForm := TSplashForm.Create(Application);
  SplashForm.Show;
  SplashForm.Update;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmSelRec, frmSelRec);
  Application.CreateForm(TfrmSFdlg, frmSFdlg);
  SplashForm.Hide;
  SplashForm.Free;
  Application.Run;
end.
