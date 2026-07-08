procedure foo;
{$ifdef A}
  {$ifdef B}
  var i: integer;
  {$endif}
begin
end;
{$endif}
