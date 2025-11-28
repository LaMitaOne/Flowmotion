unit USampleform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UFLowmotion;

type
  TFSampleform = class(TForm)
    Flowmotion1: TFlowmotion;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
  public
    { Public-Deklarationen }
  end;

var
  FSampleform: TFSampleform;

implementation

{$R *.dfm}


procedure TFSampleform.FormCreate(Sender: TObject);
begin
  Doublebuffered := True;
  Flowmotion1.DoubleBuffered := True;
end;

procedure TFSampleform.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  if Flowmotion1.Visible then begin
    try
      if Msg.WheelDelta > 0 then
      begin
         Flowmotion1.SelectPreviousImage;
      end;
      if Msg.WheelDelta < 0 then
      begin
         Flowmotion1.SelectNextImage;
      end;
      Msg.Result := 1;
    except

    end;
    Exit;
  end;
end;

procedure TFSampleform.Button1Click(Sender: TObject);
begin
  if Flowmotion1.ImageCount = 0 then Flowmotion1.Clear(false);
  Flowmotion1.AddImageAsync(Extractfilepath(Application.ExeName) + inttostr(random(11)+1) + '.jpg');
end;

procedure TFSampleform.Button2Click(Sender: TObject);
var
  IMList,Pathlist, Captionlist: TStringList;
  i : Integer;
begin
  IMList:= TStringList.create;
  Pathlist:= TStringList.create;
  Captionlist:= TStringList.create;
 try
  Flowmotion1.Clear(false);
   for i := 1 to 12 do begin
     IMList.add(Extractfilepath(Application.ExeName) + inttostr(i) + '.jpg');
     Pathlist.add('Folder or whatever');
     Captionlist.add('Caption');
   end;
   Flowmotion1.MaxZoomSize := trunc(Clientwidth / 3);
   Flowmotion1.AddImagesAsync(IMList,Captionlist,Pathlist);
  finally
   IMList.Free;
   Pathlist.Free;
   Captionlist.Free;
  end;
end;

procedure TFSampleform.Button3Click(Sender: TObject);
begin
  Flowmotion1.Clear(true);
end;

end.
