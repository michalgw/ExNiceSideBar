unit Unit1;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NiceSideBar, ImgList;
  
  // When switching compilation from Delphi XE11 to Delphi 7, the automatically
  // added unit System.ImageList must be removed manually.

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList2: TImageList;
    ImageList1: TImageList;
    Shape1: TShape;     
    Shape2: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    NiceSideBar1: TNiceSideBar;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Image1Click(Sender: TObject);
    procedure ImageButton1Click(Sender: TObject);
    procedure NiceSideBar1Hover(Sender: TObject; Index, SubIndex: Integer;
      Caption: String);
    procedure NiceSideBar1Select(Sender: TObject; Index, SubIndex: Integer;
      Caption: String);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF FPC}
    procedure WMNCHittest(var Msg: TLMessage); message LM_NCHITTEST;
    {$ELSE}
    procedure WMNCHittest(var Msg: TMessage); message WM_NCHITTEST;
    {$ENDIF}
  protected
    //procedure CreateParams(var Params: TCreateParams); override;  
  public

  end;

var
  Form1: TForm1;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}
   {
procedure TForm1.CreateParams(var Params: TCreateParams);
begin
  inherited;
  //Params.Style := (Params.Style and WS_POPUP or WS_BORDER) and not WS_DLGFRAME;
end;
  }
procedure TForm1.Image1Click(Sender: TObject);
begin  // not used
  //ShowMessage('Wah, gampang sekali!!!!');
end;

procedure TForm1.ImageButton1Click(Sender: TObject);
begin  // not used
  //ShowMessage('Ngapain, sih?');
end;

procedure TForm1.WMNCHittest(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  Msg.Result := HTCAPTION;
end;

procedure TForm1.NiceSideBar1Hover(Sender: TObject; Index,
  SubIndex: Integer; Caption: String);
begin
  Label3.Caption := 'Hovered Item: ' + Caption;
end;

procedure TForm1.NiceSideBar1Select(Sender: TObject; Index,
  SubIndex: Integer; Caption: String);
begin
  Label4.Caption := 'Selected Item: ' + Caption;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
//  WindowState := wsMinimized;
  SendMessage(Handle, {$IFDEF FPC}LM_SYSCOMMAND{$ELSE}WM_SYSCOMMAND{$ENDIF}, SC_MINIMIZE, 0);
end;

procedure TForm1.Panel2Click(Sender: TObject);
begin
  if (WindowState = wsMaximized) then
    WindowState := wsNormal
  else
    WindowState := wsMaximized;
  (*
  if (WindowState = wsMaximized)
    then SendMessage(Handle, {$IFDEF FPC}LM_SYSCOMMAND{$ELSE}WM_SYSCOMMAND{$ENDIF}, SC_RESTORE, 0)
    else SendMessage(Handle, {$IFDEF FPC}LM_SYSCOMMAND{$ELSE}WM_SYSCOMMAND{$ENDIF}, SC_MAXIMIZE, 0);
  *)
end;

procedure TForm1.Panel3Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  NiceSideBar1.Items[1].Enabled := not NiceSideBar1.Items[1].Enabled;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  NiceSideBar1.Items[1].ItemEnabled[2] := not NiceSideBar1.Items[1].ItemEnabled[2];
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  NiceSideBar1.Items[1].Visible := not NiceSideBar1.Items[1].Visible;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  NiceSideBar1.Items[1].ItemVisible[2] := not NiceSideBar1.Items[1].ItemVisible[2];
end;

end.
