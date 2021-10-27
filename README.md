# McJSON
A **Delphi / Lazarus / C++Builder** simple and small class for fast JSON parsing.

## Origin
Some points of interest:
 * Object-pascal native code using only TList.
 * Compatible from Delphi 7 up to now.
 * Compatible from Lazarus.
 * Compatible from C++Builder 2006 up to now.
 * Just one unit, just one class.
 * Inspired on [badunius/myJSON](https://github.com/badunius/myJSON).
 * Performance compared with [myJSON](https://github.com/badunius/myJSON) and "JSON Delphi libray" ([LkJson](https://sourceforge.net/projects/lkjson/))

Example:

```pascal
uses
  McJSON;
...  
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
Will produce `\test\example.json`:
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
```

## Performance
A teste comparation have been done with the original myJSON and LkJson units:
* Generate a JSON with 50k items
* Save to file
* Parse from memory (copy object forcing a parse) 
* Load from file (and parsing)
* Access 1k items randomly

The next table summarizes the results:

Library    | Generate | Save | Parse  | Load   | Access |
-----------|----------|------|--------|--------|--------|
myJSON     |   50.00s | .07s | 5.1min | 7.7min |  1.60s |
LkJson     |     .30s | .13s |   .47s |   .36s |   .00s |
**McJSON   |     .08s | .09s |   .11s |   .16s |   .70s |

Notes about `LkJson`:
* Good performance generating and parsing and even better with random access due to HashTable.
* TLkJSONBase and other derivated classes forces to cast objects using the "as" operator. In C++Builder, this requires `dynamic_cast` making the code verbosy.

Notes about `myJSON`:
* Performance deteriored due the recurrent use of wsTrim().

Notes about `McJSON`:
* Good performance, but not better, with random access due to the use of TList.
