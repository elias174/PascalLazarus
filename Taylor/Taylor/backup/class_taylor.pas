unit class_taylor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
  TTaylor = class
    max_value, min_value: Real;
    ErrorAllowed: Real;
    ErrorSequence,
    Sequence,
    FunctionList: TstringList;
    FunctionType: Integer;
    AngleType: Integer;
    x: Real;
    function Validate(): Boolean;
    function Execute(): Real;
    private
      Error,
      Angle: Real;
      function sen(): Real;
      function cos(): Real;
      function exponential(): Real;
      function arcsin(): Real;
      function arctan(): Real;
      function sinh(): Real;
      function cos_h(): Real;
      function arcsinh(): Real;
      function arctanh(): Real;
      function ln1x(): Real;
    public

      constructor create;
      destructor Destroy; override;

  end;

const
  IsSin = 0;
  IsCos = 1;
  IsExp = 2;

  IsArcsin = 3;
  min_arcsin = -1;
  max_arcsin = 1;

  IsArctan = 4;
  IsSinh = 5;
  IsCosh = 6;
  IsArcsinh = 7;
  IsArctanh = 8;
  IsLn1x = 9;

  AngleSexagedecimal = 0;
  AngleRadian = 1;

  Infinity = 1.0 / 0.0;


implementation

const
  Top = 100000;

constructor TTaylor.create;
begin
  max_value:= Infinity;
  min_value:= -1*Infinity;
  Sequence:= TStringList.Create;
  ErrorSequence:= TStringList.Create;
  FunctionList:= TStringList.Create;
  FunctionList.AddObject( 'sen', TObject( IsSin ) );
  FunctionList.AddObject( 'cos', TObject( IsCos ) );
  FunctionList.AddObject( 'exp', TObject( IsExp ) );
  FunctionList.AddObject( 'arcsin', TObject( IsArcsin ) );
  FunctionList.AddObject( 'arctan', TObject( IsArctan ) );
  FunctionList.AddObject( 'sinh', TObject( IsSinh ) );
  FunctionList.AddObject( 'cosh', TObject( IsCosh ) );
  FunctionList.AddObject( 'arcsinh', TObject( IsArcsinh ) );
  FunctionList.AddObject( 'arctanh', TObject( IsArctanh ) );
  FunctionList.AddObject( 'ln(1+x)', TObject( IsLn1x ) );

  Sequence.Add('');
  ErrorSequence.Add('');
  Error:= Top;
  x:= 0;

end;

destructor TTaylor.Destroy;
begin
  Sequence.Destroy;
  FunctionList.Destroy;
end;

function Power( b: Real; n: Integer ): Real;
var i: Integer;
begin
   Result:= 1;
   for i:= 1 to n do
      Result:= Result * b;

end;

function Factorial( n: Integer ): Integer;
begin

     if n > 1 then
        Result:= n * Factorial( n -1 )

     else if n >= 0 then
        Result:= 1

     else
        Result:= 0;

end;

function TTaylor.Validate(): Boolean;
begin
  case AngleType of
       AngleRadian: Angle:= x;
       AngleSexagedecimal: Angle:=x * pi/180;
  end;

  case FunctionType of
        IsSin:
          begin
               max_value:=Infinity;
               min_value:= -1 * Infinity;
          end;
        IsCos:
          begin
               max_value:=Infinity;
               min_value:= -1 * Infinity;
          end;
        IsExp:
          begin
               max_value:=Infinity;
               min_value:= -1 * Infinity;
          end;
        IsArcsin:
          begin
               max_value:= max_arcsin;
               min_value:= min_arcsin;
          end;
        IsArctan:
          begin
               max_value:= Infinity;
               min_value:= -1 * Infinity;
          end;
        IsSinh:
          begin
               max_value:=Infinity;
               min_value:= -1 * Infinity;
          end;
        IsCosh:
          begin
               max_value:=Infinity;
               min_value:= -1 * Infinity;
          end;
        IsArcsinh:
          begin
               max_value:=Infinity;
               min_value:= -1 * Infinity;
          end;
        IsArctanh:
          begin
               max_value:= 1;
               min_value:= -1;
          end;
        IsLn1x:
          begin
               max_value:= 1;
               min_value:= -1;
          end;
  end;
  Result:= (Angle >= min_value) and (Angle <= max_value);

end;

function TTaylor.Execute( ): Real;
begin

   case AngleType of
        AngleRadian: Angle:= x;
        AngleSexagedecimal: Angle:=x * pi/180;
   end;

   case FunctionType of
        IsSin: Result:= sen();
        IsCos: Result:= cos();
        IsExp: Result:= exponential();
        IsArcsin: Result:= arcsin();
        IsArctan: Result:= arctan();
        IsSinh: Result:= sinh();
        IsCosh: Result:= cos_h();
        IsArcsinh: Result:= arcsinh();
        IsArctanh: Result:= arctanh();
        IsLn1x: Result:= ln1x();

   end;


end;

function TTaylor.cos_h(): Real;
var xn: Real;
     n: Integer;
begin
  Result:= 0;
  n:= 0;

  repeat
    xn:= Result;

    Result:= Result + ( Power(Angle, 2*n) / Factorial(2*n) );
    if n > 0 then
       Error:= abs( Result - xn );
    ErrorSequence.Add(FloatToStr(Error));
    Sequence.Add( FloatToStr( Result ) );
    n:= n + 1;

  until ( Error <= ErrorAllowed ) or ( n >= Top ) ;
end;

function TTaylor.sinh(): Real;
var xn: Real;
     n: Integer;
begin
   Result:= 0;
   n:= 0;

   repeat
     xn:= Result;

     Result:= Result + ( Power(Angle, 2*n+1) / Factorial(2*n+1) );
     if n > 0 then
        Error:= abs( Result - xn );
     ErrorSequence.Add(FloatToStr(Error));
     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;


function TTaylor.arctan(): Real;
var xn: Real;
     n: Integer;
begin
   Result:= 0;
   n:= 0;

   repeat
     xn:= Result;

     Result:= Result + ( Power(-1,n) * Power(Angle, 2*n+1) ) / ( (2*n+1) );
     if n > 0 then
        Error:= abs( Result - xn );
     ErrorSequence.Add(FloatToStr(Error));
     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;

function TTaylor.arctanh(): Real;
var xn: Real;
     n: Integer;
begin
  Result:= 0;
  n:= 0;

  repeat
    xn:= Result;

    Result:= Result + ( Power(Angle, 2*n+1) ) / ( (2*n+1) );
    if n > 0 then
       Error:= abs( Result - xn );
    ErrorSequence.Add(FloatToStr(Error));
    Sequence.Add( FloatToStr( Result ) );
    n:= n + 1;

  until ( Error <= ErrorAllowed ) or ( n >= Top ) ;
end;

function TTaylor.arcsinh(): Real;
var xn: Real;
     n: Integer;
begin
   Result:= 0;
   n:= 0;

   repeat
     xn:= Result;

     Result:= Result + ( (Power(-1, n) * Factorial(2*n) * Power(Angle, 2*n+1) ) / (Power(4, n) * Power(Factorial(n), 2) * (2*n+1)) );
     if n > 0 then
        Error:= abs( Result - xn );
     ErrorSequence.Add(FloatToStr(Error));
     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;


function TTaylor.arcsin(): Real;
var xn: Real;
     n: Integer;
begin
   Result:= 0;
   n:= 0;

   repeat
     xn:= Result;

     Result:= Result + (Factorial(2*n)*Power(Angle,2*n+1))/(Power(4,n)*Power(Factorial(n),2)*(2*n+1));
     if n > 0 then
        Error:= abs( Result - xn );
     ErrorSequence.Add(FloatToStr(Error));
     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;

function TTaylor.exponential(): Real;
var xn: Real;
     n: Integer;
begin
   Result:= 0;
   n:= 0;

   repeat
     xn:= Result;

     Result:= Result + (Power(Angle, n)/Factorial(n));
     if n > 0 then
        Error:= abs( Result - xn );
     ErrorSequence.Add(FloatToStr(Error));
     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;

function TTaylor.ln1x(): Real;
var xn: Real;
     n: Integer;
begin
  Result:= 0;
  n:= 1;
  repeat
    xn:= Result;

    Result:= Result + ( (Power(Angle, n) * Power(-1, n+1) ) / n);
    if n > 0 then
       Error:= abs( Result - xn );
    ErrorSequence.Add(FloatToStr(Error));
    Sequence.Add( FloatToStr( Result ) );
    n:= n + 1;

  until ( Error <= ErrorAllowed ) or ( n >= Top ) ;
end;

function TTaylor.sen(): Real;
var xn: Real;
     n: Integer;
begin
   Result:= 0;
   n:= 0;

   repeat
     xn:= Result;

     Result:= Result + Power(-1, n)/Factorial( 2*n + 1 ) * Power(Angle, 2*n + 1);
     if n > 0 then
        Error:= abs( Result - xn );
     ErrorSequence.Add(FloatToStr(Error));
     Sequence.Add( FloatToStr( Result ) );
     n:= n + 1;

   until ( Error <= ErrorAllowed ) or ( n >= Top ) ;

end;


function TTaylor.cos(): Real;
var xn: real;
    n: Integer;

begin
  Result:= 0;
  n:= 0;

  repeat
    xn:= Result;
    Result:= Result + Power( -1, n)/Factorial(2*n) * Power( Angle, 2*n );

    if n > 0 then
       Error:= abs( Result - xn );

    Sequence.Add( FloatToStr( Result ) );
    ErrorSequence.Add(FloatToStr(Error));
    n:= n + 1;
  until ( Error < ErrorAllowed ) or ( n >= Top );

end;

end.

