unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries, TASeries, Forms, Controls,
  Graphics, Dialogs, Grids, StdCtrls, ComCtrls, solver_eq;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    calculate_button: TButton;
    chartGraphics: TChart;
    chartGraphicsConstantLine1: TConstantLine;
    chartGraphicsConstantLine2: TConstantLine;
    chartGraphicsFuncSeries1: TFuncSeries;
    chartGraphicsLineSeries1: TLineSeries;
    chkProportional: TCheckBox;
    type_method_combobox: TComboBox;
    function_line_edit: TEdit;
    a_line_edit: TEdit;
    b_line_edit: TEdit;
    error_line_edit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    StringGrid1: TStringGrid;
    trbMax: TTrackBar;
    trbMin: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure chartGraphicsFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure calculate_buttonClick(Sender: TObject);
    procedure chkProportionalChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure trbMaxChange(Sender: TObject);
    procedure trbMinChange(Sender: TObject);
    procedure clear_chart();
  private
    solver_eq: SolverEq;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.clear_chart();
var i: Integer;
begin
  chartGraphicsLineSeries1.Clear;
end;

procedure TForm1.calculate_buttonClick(Sender: TObject);
var tmp_result, x, y, intersect_y: Real;
  i: Integer;
  validate_result: TFooRec;
begin
  clear_chart();
  solver_eq:= SolverEq.create;
  solver_eq.MethodType:=type_method_combobox.ItemIndex;
  solver_eq.fx:= function_line_edit.Text;
  solver_eq.ErrorAllowed:= StrToFloat(error_line_edit.Text);
  solver_eq.solve_a:= StrToFloat(a_line_edit.Text);
  solver_eq.solve_b:= StrToFloat(b_line_edit.Text);
  validate_result:= solver_eq.validate();
  //Bolzano
  if not validate_result.B then begin
     ShowMessage(validate_result.S);
     Exit;
  end;
  tmp_result:= solver_eq.execute();
  with StringGrid1 do begin
    RowCount:= solver_eq.Sequence.Count;
    Cols[1].Assign(solver_eq.Sequence_a);
    Cols[2].Assign(solver_eq.Sequence_b);
    Cols[3].Assign(solver_eq.Sequence);
    Cols[4].Assign(solver_eq.Sequence_sign);
    Cols[5].Assign(solver_eq.ErrorSequence);
    for i:= 1 to RowCount - 1 do begin
      Cells[ 0, i ]:= IntToStr( i );
    end;
  end;
  chartGraphicsFuncSeries1.Pen.Color:= clBlue;
  chartGraphicsFuncSeries1.Active:= True;
  x:= tmp_result;
  y:= 0.0;
  chartGraphicsLineSeries1.ShowLines:= False;
  chartGraphicsLineSeries1.ShowPoints:= True;
  chartGraphicsLineSeries1.AddXY( x, y );
  //solver_eq.fx:= 'x^3-3*x';
  //intersect_y:= solver_eq.eval_fx(tmp_result);
  //ShowMessage('(' + FloatToStr(tmp_result) + ',' + FloatToStr(intersect_y) + ')' );


end;

procedure TForm1.chkProportionalChange(Sender: TObject);
begin
  chartGraphics.Proportional:= not chartGraphics.Proportional;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  chartGraphics.Extent.UseXMax:= true;
  chartGraphics.Extent.UseXMin:= true;
  solver_eq:= SolverEq.create;
  type_method_combobox.Items.Assign(solver_eq.MethodList);
  type_method_combobox.ItemIndex:=0;
end;

procedure TForm1.trbMaxChange(Sender: TObject);
begin
  chartGraphics.Extent.XMax:= trbMax.Position;
end;

procedure TForm1.trbMinChange(Sender: TObject);
begin
  chartGraphics.Extent.XMin:= trbMin.Position;
end;

procedure TForm1.chartGraphicsFuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY:= solver_eq.eval_fx(AX);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  clear_chart();
  solver_eq:= SolverEq.create;
  solver_eq.MethodType:=type_method_combobox.ItemIndex;
  solver_eq.fx:= function_line_edit.Text;
  chartGraphicsFuncSeries1.Pen.Color:= clBlue;
  chartGraphicsFuncSeries1.Active:= True;
end;


end.

