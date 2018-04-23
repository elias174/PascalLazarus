unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries, TASeries, Forms, Controls,
  Graphics, Dialogs, Grids, StdCtrls, ComCtrls, Spin, solver_eq, generalized_solver_eq, Matrix;

type

  { TForm1 }

  TForm1 = class(TForm)
    a_line_edit: TEdit;
    Button1: TButton;
    b_line_edit: TEdit;
    calculate_button: TButton;
    calculate_button1: TButton;
    chartGraphics: TChart;
    chartGraphicsConstantLine1: TConstantLine;
    chartGraphicsConstantLine2: TConstantLine;
    chartGraphicsFuncSeries1: TFuncSeries;
    chartGraphicsLineSeries1: TLineSeries;
    chkProportional: TCheckBox;
    error_line_edit: TEdit;
    error_line_edit1: TEdit;
    function_line_edit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label_dfx: TLabel;
    lineedit_dfx: TEdit;
    line_edit_h: TEdit;
    PageControl1: TPageControl;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    string_grid_input_equations: TStringGrid;
    grid_variables_name: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    trbMax: TTrackBar;
    trbMin: TTrackBar;
    type_method_combobox: TComboBox;
    type_method_combobox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure calculate_button1Click(Sender: TObject);
    procedure chartGraphicsFuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure calculate_buttonClick(Sender: TObject);
    procedure chkProportionalChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure trbMaxChange(Sender: TObject);
    procedure trbMinChange(Sender: TObject);
    procedure clear_chart();
    procedure clear_grid();
  private
    solver_eq: SolverEq;
    general_solver_eq: GeneralSolverEq

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

procedure TForm1.clear_grid();
var
  I: Integer;
begin
  for I := 0 to StringGrid1.ColCount - 1 do
    StringGrid1.Cols[I].Clear;
  StringGrid1.RowCount := 1;
end;

procedure TForm1.calculate_buttonClick(Sender: TObject);
var tmp_result, x, y, intersect_y: Real;
  i: Integer;
  validate_result: TFooRec;
begin
  clear_grid();
  clear_chart();
  solver_eq:= SolverEq.create;
  solver_eq.MethodType:=type_method_combobox.ItemIndex;
  solver_eq.fx:= function_line_edit.Text;
  solver_eq.solve_h:= StrToFloat(line_edit_h.Text);
  solver_eq.deriv_fx:= lineedit_dfx.Text;
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

procedure TForm1.SpinEdit1Change(Sender: TObject);
var i,j,cont1: Integer;
begin
  cont1:=0;
  i:=0;
  j:=0;
  // string_grid_input_equations.Clear;
  string_grid_input_equations.RowCount:=SpinEdit1.Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
var i,j,cont1: Integer;
begin
  cont1:=0;
  i:=0;
  j:=0;
  //string_grid_input_equations.Clear;
  string_grid_input_equations.ColCount:=SpinEdit2.Value;
  grid_variables_name.ColCount:=SpinEdit2.Value - 1;
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

procedure TForm1.calculate_button1Click(Sender: TObject);
var values, res: TMatrix;
  i: Integer;
begin
  general_solver_eq:= GeneralSolverEq.Create;
  general_solver_eq.ErrorAllowed:= StrToFloat(error_line_edit1.Text);;
  general_solver_eq.read_from_grids(string_grid_input_equations, grid_variables_name);
  //values:= TMatrix.Create(2,1);
  //values.A[0,0] := 2;
  //values.A[1,0] := 1;
  res := general_solver_eq.newthon_rapsody();

  with StringGrid2 do begin
    RowCount:= general_solver_eq.Sequence_xn.Count;
    Cols[1].Assign(general_solver_eq.Sequence_xn);
    Cols[2].Assign(general_solver_eq.Sequence_error);
    for i:= 1 to RowCount - 1 do begin
      Cells[ 0, i ]:= IntToStr( i );
    end;
  end;
end;


end.

