program Sampleproject;

uses
  Vcl.Forms,
  USampleform in 'USampleform.pas' {FSampleform};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFSampleform, FSampleform);
  Application.Run;
end.
