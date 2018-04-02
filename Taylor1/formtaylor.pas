unit formTaylor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    CalcButton: TButton;
    edit_result: TEdit;
    edit_value: TEdit;
    edit_error: TEdit;
    procedure CalcButtonClick(Sender: TObject);
    procedure edit_resultChange(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.CalcButtonClick(Sender: TObject);
begin

end;

procedure TForm2.edit_resultChange(Sender: TObject);
begin

end;

end.

