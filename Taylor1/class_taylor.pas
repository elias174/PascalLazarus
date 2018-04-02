unit class_taylor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTaylor = class
    ErrorAllowed: Real;
    Sequence: TStringList;
    function sen(x: Real): Real;
    private
      Error: Real;
      //function Power(base: Real; n: Integer): Real;
      //function Factorial( n:Integer): Integer;
    public
      constructor create;
      destructor Destroy; override;
  end;

implementation

const
  Stop = 100000;

constructor TTaylor.create;
begin
  Sequence:= TStringList.Create;
end;

destructor TTaylor.Destroy;
begin
  Sequence.Destroy;
end;

function Power(base: Real; n: Integer): Real;
var i: Integer;
begin
  Result:=1;
  for i:=1 to n do
      Result:=Result * base;

end;

function Factorial( n:Integer): Integer;

begin
    if n>1 then:
       Result := n*Factorial(n-1);
    else if n>=0 then:
       Result:=1
    else:
       Result:=0;
end;

function TTaylor.sen(x: Real): Real;
var n:Integer;
  xn: Real;
begin
  Result:=0;
  n:=0;

  repeat
    xn:Result;
    Result := Result + Power(-1, n) / Factorial(2*n + 1) * Power(x, 2*n+1);
    if n>0 then
       Error:=abs(Result - xn);
    Sequence.Add(FloatToStr(Result));
    n:=n+1;
  until (Error);
end;

end.

