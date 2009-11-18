with Ada.Text_IO; use Ada.Text_IO;
with GNAT.MD5;
 
procedure MD5_Digest is
begin
  Put_Line(GNAT.MD5.Digest("1"));
  Put_Line(GNAT.MD5.Digest("Foo bar baz"));
  Put_Line(GNAT.MD5.Digest("Foo bar baz Foo bar baz Foo bar baz Foo bar baz Foo bar baz"));
end MD5_Digest;


-- 520c28a8ac3459af817a1abfb3bd152e