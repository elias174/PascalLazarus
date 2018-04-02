unit solver_eq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpexprpars;

type
  TFooRec = record
    S: String;
    B: Boolean;
  end;
  SolverEq = class
    fx: String;
    FParser: TFPExpressionParser;
    argument: TFPExprIdentifierDef;
    solve_a: Real;
    solve_b: Real;
    ErrorAllowed: Real;
    MethodList: TstringList;
    MethodType: Integer;
    ErrorSequence, Sequence, Sequence_a, Sequence_b, Sequence_sign: TstringList;
    function validate(): TFooRec;
    function execute(): Real;
    function bisection_method(): Real;
    function fake_position_method(): Real;
    function eval_fx(x: Real): Real;
    private
      Error: Real;
    public
      constructor create;
      destructor Destroy; override;
  end;

implementation
const
  Top = 100000;
  IsBisection = 0;
  IsFakePosition = 1;

constructor SolverEq.create;
begin
     FParser := TFPExpressionParser.Create(nil);
     FParser.BuiltIns := [bcMath];
     argument := FParser.Identifiers.AddFloatVariable('x', 0.0);
     Sequence:= TStringList.Create;
     Sequence_a:= TStringList.Create;
     Sequence_b:= TStringList.Create;
     Sequence_sign:= TStringList.Create;
     MethodList:= TStringList.Create;
     MethodList.AddObject( 'bisection', TObject( IsBisection ) );
     MethodList.AddObject( 'fake position', TObject( IsFakePosition ) );


     ErrorSequence:= TStringList.Create;
     Sequence.Add('');
     Sequence_a.Add('');
     Sequence_b.Add('');
     Sequence_sign.Add('');
     ErrorSequence.Add('');
     Error:= Top;
end;

function SolverEq.execute(): Real;
begin
     case MethodType of
          IsBisection: Result:= bisection_method();
          IsFakePosition: Result:= fake_position_method();
     end;
end;

destructor SolverEq.Destroy;
begin
     Sequence.Destroy;
end;

function SolverEq.validate(): TFooRec;
var balz: Real;
begin
     balz:= eval_fx(solve_a) * eval_fx(solve_b);
     Result.S:= '';
     Result.B:= True;
     if balz >= 0 then begin
       Result.S:= 'f(a)*f(b)= ' + FloatToStr(balz) + ' its >= 0 ';
       Result.B:= False;
     end;
end;

function SolverEq.eval_fx(x: Real): Real;
begin

       FParser.Expression := fx;
       argument.AsFloat:= x;
       Result:= FParser.Evaluate.ResFloat;
     //finally
     //  FParser.Free;

end;

function SolverEq.bisection_method(): Real;
var xn: Real;
  sign: Real;
  n: Integer;
  a: Real;
  b: Real;
begin
     n:= 0;
     Result:= 0;
     a:= solve_a;
     b:= solve_b;
     repeat
       xn:= (a+b) / 2;
       sign:= eval_fx(a) * eval_fx(xn);
       if n > 0 then
          Error:=abs(Result - xn);
       ErrorSequence.Add( FloatToStr(Error) );
       Sequence.Add( FloatToStr(xn) );
       Sequence_a.Add( FloatToStr(a) );
       Sequence_b.Add( FloatToStr(b) );
       Sequence_sign.Add( FloatToStr(sign) );
       if sign >= 0 then
          a:= xn
       else
          b:= xn;
       Result:= xn;
       n:= n + 1;
     until ( Error <= ErrorAllowed ) or ( n >= Top );

end;

function SolverEq.fake_position_method(): Real;
var xn: Real;
  sign: Real;
  n: Integer;
  a: Real;
  b: Real;
begin
     n:= 0;
     Result:= 0;
     a:= solve_a;
     b:= solve_b;
     repeat
       xn:= a - ( ( eval_fx(a)* (b-a) ) / (eval_fx(b)-eval_fx(a) ) );
       sign:= eval_fx(a) * eval_fx(xn);
       if n > 0 then
          Error:=abs(Result - xn);
       Sequence.Add( FloatToStr(xn) );
       Sequence_a.Add( FloatToStr(a) );
       Sequence_b.Add( FloatToStr(b) );
       Sequence_sign.Add( FloatToStr(sign) );
       ErrorSequence.Add(FloatToStr(Error));
       if sign >= 0 then
          a:= xn
       else
          b:= xn;
       Result:= xn;
       n:= n + 1;
     until ( Error <= ErrorAllowed ) or ( n >= Top );
end;

end.

