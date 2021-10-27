# McJSON
--------
A Delphi/Lazarus/C++Builder small class for fast JSON parsing.

# Origin
--------
Some points of interest:
 . Object-pascal native code, using classes only TList.
 . Compatible with Delphi 7 up to now.
 . Compatible with Lazarus.
 . Compatible with C++Builder 2006 up to now.
 . Inspired in "JSON Delphi libray" (LkJson)

Example:

```pascal
var
  Json: TJson;
  Str: String
begin
  Json := TJson.Create();

  //put
  Json.Put('field1', null);
  Json.Put('field2', True);
  Json.Put('field3', 3.14);
  Json.Put('field4', 'hello world');

  //another way
  Json['field5'].AsBoolean := False;
  Json['field6'].AsString := 'hello world';

  //object
  with Json['field7'].AsObject do
  begin
    Put('subfield1', 2.7182818284);
    Put('subfield2', 'json4delphi');
  end;

  //array
  with Json['field8'].AsArray do
  begin
    Put(6.6260755e-34);
    Put('The magic words are squeamish ossifrage');
  end;

  //get
  Str := Json['field4'].AsString;

  //parse
  Json.Parse('{"a":1}');

  //stringify
  Str := Json.Stringify;
end;
```
