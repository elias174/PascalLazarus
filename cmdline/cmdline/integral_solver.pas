unit integral_solver;

{$mode objfpc}

interface

uses
  Classes, SysUtils, ParseMath, math;

type
   IntegralSolver = class
       Parse: TParseMath;
       function trapezoidal_method(n: Integer; a,b: Real; fx: String): Real;
       public
           constructor create;
   end;

implementation
constructor IntegralSolver.create;
begin
  Parse:= TParseMath.create();
  Parse.AddVariable( 'x', 0);
end;

function IntegralSolver.trapezoidal_method(n: Integer; a,b: Real; fx: String): Real;
var h,xi, fa, fb, sum_fxi: Real;
  i: Integer;
begin
  i:=1;
  Parse.Expression:= fx;
  h:= (b-a)/n;
  sum_fxi:=0;

  Parse.NewValue('x', a);
  fa:= abs(Parse.Evaluate());
  Parse.NewValue('x', b);
  fb:= abs(Parse.Evaluate());
  xi:=a;
  repeat
    xi:= xi + h;
    Parse.NewValue('x', xi);
    sum_fxi:=sum_fxi + abs(Parse.Evaluate());
    i:= i+1;
  until (i >= n);
  Result:= (fa+fb)/2;
  Result:= Result + sum_fxi;
  Result:= Result * h;

end;

end.

