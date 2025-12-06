unit USampleform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UFLowmotion, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Samples.Spin;

type
  TFSampleform = class(TForm)
    Flowmotion1: TFlowmotion;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Button1: TButton;
    Button5: TButton;
    Button2: TButton;
    Button3: TButton;
    Button8: TButton;
    Button6: TButton;
    Button7: TButton;
    Button9: TButton;
    Button10: TButton;
    Button4: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    SpinEdit2: TSpinEdit;
    Label3: TLabel;
    SpinEdit3: TSpinEdit;
    Panel5: TPanel;
    Panel6: TPanel;
    Timer1: TTimer;
    ComboBox1: TComboBox;
    Label4: TLabel;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Flowmotion1AllAnimationsFinished(Sender: TObject);
    procedure Flowmotion1ImageLoad(Sender: TObject; const FileName: string;
        Success: Boolean);
    procedure Flowmotion1ItemSelected(Sender: TObject; ImageItem: TImageItem;
        Index: Integer);
    procedure Flowmotion1SelectedImageDblClick(Sender: TObject; ImageItem:
        TImageItem; Index: Integer);
    procedure Flowmotion1SelectedItemMouseDown(Sender: TObject; ImageItem:
        TImageItem; Index, X, Y: Integer; Button: TMouseButton; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure Flowmotion1SelectedImageEnterZone(Sender: TObject; ImageItem: TImageItem; const ZoneName: string);
  public
    { Public-Deklarationen }
  end;

var
  FSampleform: TFSampleform;

implementation

{$R *.dfm}


procedure TFSampleform.Button10Click(Sender: TObject);
begin
 Flowmotion1.SetBackgroundpicture('');
end;

procedure TFSampleform.Button11Click(Sender: TObject);
begin
  Flowmotion1.ImageEntryStyle := iesFromPoint;
  Flowmotion1.EntryPoint := TPoint.Create(Button5.Left, Button5.Top);
  Flowmotion1.AddImageAsync(Extractfilepath(Application.ExeName) + inttostr(random(11)+1) + '.jpg');
end;

procedure TFSampleform.Button12Click(Sender: TObject);
var
  IMList,Pathlist, Captionlist: TStringList;
  i : Integer;
begin
  IMList:= TStringList.create;
  Pathlist:= TStringList.create;
  Captionlist:= TStringList.create;
 try
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

procedure TFSampleform.Button13Click(Sender: TObject);
begin
  if Opendialog1.Execute then  begin
    Flowmotion1.ImageEntryStyle := iesFromPoint;
    Flowmotion1.EntryPoint := TPoint.Create(Button5.Left, Button5.Top);
    Flowmotion1.AddImageAsync(Opendialog1.FileName);
  end;
end;

procedure TFSampleform.Button14Click(Sender: TObject);
begin
  SpinEdit1.Value := 3;
end;

procedure TFSampleform.Button15Click(Sender: TObject);
begin
  Flowmotion1.MoveImageToPos(Flowmotion1.ImageCount-1, 0);
end;

procedure TFSampleform.Button16Click(Sender: TObject);
begin
  Flowmotion1.SelectedMovable := not Flowmotion1.SelectedMovable;
  if Flowmotion1.SelectedMovable then Button16.Caption := 'Drag selected on'
   else Button16.Caption := 'Drag selected off';

end;

procedure TFSampleform.FormCreate(Sender: TObject);
begin
  Doublebuffered := True;
  Flowmotion1.DoubleBuffered := True;
  Flowmotion1.SetBackgroundpicture(Extractfilepath(Application.ExeName) + 'back.jpg');
  Flowmotion1.AddActivationZone('ActivationZone 1', Panel5.BoundsRect);
  Flowmotion1.AddActivationZone('ActivationZone 2', Panel6.BoundsRect);
  Flowmotion1.OnSelectedImageEnterZone := Flowmotion1SelectedImageEnterZone;
end;

procedure TFSampleform.Flowmotion1SelectedImageEnterZone(Sender: TObject; ImageItem: TImageItem; const ZoneName: string);
begin
  // Show a message or perform an action based on the zone name, here u can do...whatever u like then
  if ZoneName = 'ActivationZone 1' then
    ShowMessage('Image entered ActivationZone 1! ')
  else if ZoneName = 'ActivationZone 2' then
    ShowMessage('Image entered ActivationZone 2! ');
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
  Flowmotion1.ImageEntryStyle := iesRandom;
  Flowmotion1.EntryPoint := TPoint.Create(0, 0);
  Flowmotion1.AddImage(Extractfilepath(Application.ExeName) + inttostr(random(11)+1) + '.jpg');
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
  Flowmotion1.Clear(true);
   for i := 1 to 12 do begin
     IMList.add(Extractfilepath(Application.ExeName) + inttostr(i) + '.jpg');
     Pathlist.add('Folder or whatever');
     Captionlist.add('Caption');
   end;
   //twwice
   for i := 1 to 12 do begin
     IMList.add(Extractfilepath(Application.ExeName) + inttostr(i) + '.jpg');
     Pathlist.add('Folder or whatever');
     Captionlist.add('Caption');
   end;
   Flowmotion1.MaxZoomSize := trunc(Clientwidth / 3);
   Flowmotion1.AddImages(IMList,Captionlist,Pathlist);
  finally
   IMList.Free;
   Pathlist.Free;
   Captionlist.Free;
  end;
end;

procedure TFSampleform.Button3Click(Sender: TObject);
begin          //only falling
  Flowmotion1.Clear(true);
  Button8.Enabled := False;
end;

procedure TFSampleform.Button4Click(Sender: TObject);
begin          //animated, ZoominSelected, target for selected, target for others, animationstyle
  Flowmotion1.Clear(true, true, Button4.BoundsRect, Button3.BoundsRect, iesFromPoint);
  Button8.Enabled := False;
end;

procedure TFSampleform.Button5Click(Sender: TObject);
begin
  Flowmotion1.ImageEntryStyle := iesFromPoint;
  Flowmotion1.EntryPoint := TPoint.Create(Button5.Left, Button5.Top);
  Flowmotion1.AddImage(Extractfilepath(Application.ExeName) + inttostr(random(11)+1) + '.jpg');
end;

procedure TFSampleform.Button6Click(Sender: TObject);
begin
  Flowmotion1.SelectPreviousImage;
end;

procedure TFSampleform.Button7Click(Sender: TObject);
begin
  Flowmotion1.SelectNextImage;
end;

procedure TFSampleform.Button8Click(Sender: TObject);
begin
  Flowmotion1.DeselectZoomedImage;
end;

procedure TFSampleform.Button9Click(Sender: TObject);
begin
  if Opendialog1.Execute then Flowmotion1.SetBackgroundpicture(Opendialog1.FileName);
end;

procedure TFSampleform.CheckBox1Click(Sender: TObject);
begin
  Flowmotion1.BreathingEnabled := CheckBox1.Checked;
end;

procedure TFSampleform.CheckBox2Click(Sender: TObject);
begin
  Flowmotion1.HotTrackZoom := CheckBox2.Checked;
end;

procedure TFSampleform.ComboBox1Change(Sender: TObject);
begin
   case Combobox1.ItemIndex of
     0: Flowmotion1.SetFlowLayout(flSorted);
     1: Flowmotion1.SetFlowLayout(flFreeFloat);
   end;
end;

procedure TFSampleform.Flowmotion1AllAnimationsFinished(Sender: TObject);
begin
  //stopped animations, idle

end;

procedure TFSampleform.Flowmotion1ImageLoad(Sender: TObject; const FileName:
    string; Success: Boolean);
begin
  //for progress later

end;

procedure TFSampleform.Flowmotion1ItemSelected(Sender: TObject; ImageItem:
    TImageItem; Index: Integer);
begin
  //Get positions caption and whatever
  if ImageItem <> nil then Button8.Enabled := ImageItem.IsSelected;
end;

procedure TFSampleform.Flowmotion1SelectedImageDblClick(Sender: TObject;
    ImageItem: TImageItem; Index: Integer);
begin
  //selected dblclicked

end;

procedure TFSampleform.Flowmotion1SelectedItemMouseDown(Sender: TObject;
    ImageItem: TImageItem; Index, X, Y: Integer; Button: TMouseButton; Shift:
    TShiftState);
begin
  //--

end;

procedure TFSampleform.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TFSampleform.Panel3Click(Sender: TObject);
begin
  if Colordialog1.Execute then begin
    Flowmotion1.GlowColor := Colordialog1.Color;
    Panel3.Color := Colordialog1.Color;
  end;
end;

procedure TFSampleform.Panel4Click(Sender: TObject);
begin
  if Colordialog1.Execute then begin
    Flowmotion1.HotTrackColor := Colordialog1.Color;
    Panel4.Color := Colordialog1.Color;
  end;
end;

procedure TFSampleform.SpinEdit1Change(Sender: TObject);
begin
   Flowmotion1.AnimationSpeed := SpinEdit1.Value;
end;

procedure TFSampleform.SpinEdit2Change(Sender: TObject);
begin
  Flowmotion1.GlowWidth := SpinEdit2.Value;
end;

procedure TFSampleform.SpinEdit3Change(Sender: TObject);
begin
  Flowmotion1.HotTrackWidth := SpinEdit3.Value;
end;

procedure TFSampleform.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  Button2Click(Self);
end;

end.
