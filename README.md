# McJSON
A Delphi/Lazarus/C++Builder small class for fast JSON parsing.

## Origin
Some points of interest:
 - Object-pascal native code, using classes only TList.
 - Compatible with Delphi 7 up to now.
 - Compatible with Lazarus.
 - Compatible with C++Builder 2006 up to now.
 - Inspired on badunius/myJSON
 - Compared with "JSON Delphi libray" (LkJson)

Example:

```pascal
function Test99(out Msg: string): Boolean;
var
  Json: TMcJsonItem;
  i: Integer;
begin
  Msg := 'Test: Github readme.md content';
  Json := TMcJsonItem.Create();
  try
    try
      // add some pairs.
      Json.Add('key1').AsInteger := 1;
      Json.Add('key2').AsBoolean := True;
      Json.Add('key3').AsNumber  := 1.234;
      Json.Add('key4').AsString  := 'value 1';
      // add an array
      Json.Add('array').ItemType := jitArray;
      for i := 1 to 3 do
        Json['array'].Add.AsInteger := i;
      // save and load
      Json.SaveToFile('example.json');
      Json.LoadFromFile('example.json');
      // remove an item
      Json.Remove('array');
      // test final result
      Result := (Json.AsJSON = '{"key1": 1,"key2": true,"key3": 1.234,"key4": "value 1"}');
    except
      Result := False;
    end;
  finally
    Json.Free;
  end;
end; 
```
Will produce `example.json:
```json
{
  "key1": 1,
  "key2": true,
  "key3": 1.234,
  "key4": "value 1",
  "array": [
    1,
    2,
    3
  ]
}
