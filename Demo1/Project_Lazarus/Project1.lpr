program Project1;

uses
  Interfaces,
  Forms,
  Unit1 in '..\common\Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
