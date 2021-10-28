# McJSON
A **Delphi / Lazarus / C++Builder** simple and small class for fast JSON parsing.

## Motivation
Some points of interest:
 * Simple Object-Pascal native code using TList as internal data structure.
 * Single-pass string parser. 
 * Compatible (aimed):
   * Delphi 7 up to now.
   * Lazarus.
   * C++Builder 2006 up to now.
 * Tested with:
   * BDS 2006 (Delphi and BCP)
   * Lazarus 2.3.0 (FPC 3.2.2)
   * C++Builder 10.2.
 * Just one unit (`McJSON`), just one class(`TMcJsonItem`).
 * Inspired by [badunius/myJSON](https://github.com/badunius/myJSON).
 * Performance [tests](#performance-tests) using C++Builder and comparing:
   *  [myJSON](https://github.com/badunius/myJSON) 
   *  [LkJson](https://sourceforge.net/projects/lkjson/)
   *  [JsonTools](https://github.com/sysrpl/JsonTools)

### Object-Pascal Example

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
      // save a backup to file
      if (Json['array'].Count = 3) then
        Json.SaveToFile('example.json');
      // remove an item
      Json.Delete('array');
      // oops, load the backup
      if (Json.Count = 4) then
        Json.LoadFromFile('example.json');
      // test final result
      Result := (Json.AsJSON = '{"key1":1,"key2":true,"key3":1.234,"key4":"value 1","array":[1,2,3]}');
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

### C++Builder Example

```C++
#include "McJson.hpp"
...
bool Test99(AnsiString& Msg)
{
  bool Result;
  TMcJsonItem* Json = NULL;
  Msg = "Test: Github readme.md content";
  Json = new TMcJsonItem();
  try
  {
    try
    { // add some pairs.
      Json->Add("key1")->AsInteger = 1;
      Json->Add("key2")->AsBoolean = true;
      Json->Add("key3")->AsNumber  = 1.234;
      Json->Add("key4")->AsString  = "value 1";
      // add an array
      Json->Add("array")->ItemType = jitArray;
      for (int i = 1; i <= 3 ; i++)
        Json->Items["array"]->Add()->AsInteger = i;
      // save a backup to file
      if (Json->Items["array"]->Count == 3)
        Json->SaveToFile("example.json");
      // remove an item
      Json->Delete("array");
      // oops, load the backup
      if (Json->Count == 4)
        Json->LoadFromFile("example.json");
      // test final result
      Result = (Json->AsJSON ==
                "{\"key1\":1,\"key2\":true,\"key3\":1.234,\"key4\":\"value 1\",\"array\":[1,2,3]}");
      if (Result) ShowMessage("Test99 OK");
      else        ShowMessage("Test99 NOT OK");
    }
    catch(...)
    {
      Result = false;
    }
  }
  __finally
  {
    if (Json) delete (Json);
  }
  return (Result);
}
```

## Performance Tests
A test comparation have been done with the original `myJSON`, `LkJson` and `JsonTools` units:
* C++Builder VCL examples built with BDS 2006 (the older version I have).
* Generate a JSON with 50k items.
* Save to file.
* Parse from memory (copy object forcing a parse).
* Load from file (and parsing).
* Access 1k items randomly.
* Very old 32 bits machine: Intel Core 2 CPU T5500 1.66GHz 4 GB RAM.

The next table summarizes the results[^1]:

Library    | Generate | Save | Parse  | Load   | Access |
:----------|---------:|-----:|-------:|-------:|-------:|
`myJSON`   |   50.00s | .07s | 5.1min | 7.7min |  1.60s |
`LkJson`   |     .30s | .13s |   .47s |   .36s |   .00s |
`JsonTools`|   48.00s | .70s | 39.00s | 40.00s |   .48s |
`McJSON`   |     .08s | .09s |   .11s |   .16s |   .54s |

[^1]: Metric: average time in seconds (s) for 5 consecutive executions. Some results converted to minutes (min).

### Notes about `myJSON`
* Performance deteriored due the recurrent use of wsTrim().
* Generate using: `Json->Item["key"]->setStr("value")`.
* Parse using: `JsonP->Code = Json->getJSON()`.

### Notes about `LkJson`
* Good performance generating and parsing and even better with random access due to "Balanced Search Tree" `TlkBalTree`.
* TLkJSONBase and other derivated classes force to cast objects using the "as" operator. In C++Builder, this requires `dynamic_cast` making the code verbosy.
* Generate using: `Json->Add("key", "value")`.
* Parse using: `JsonP = dynamic_cast<TlkJSONObject*>(TlkJSON::ParseText(NULL, TlkJSON::GenerateText(NULL, Json)))`.

### Notes about `JsonTools`
* Very nice and interesting code focused on the concept of Tokens. 
* Also uses TList as internal data structure. 
* It needs a performance review.
* Generate using: `Json->Add("key", "value")`.
* Parse using: `JsonP->Value = Json->AsJson`.

### Notes about `McJSON`
* Good performance, but not the better about random access due to the use of TList.
* Simple and smart interface using "AsXXX" getters and setters (not invented here).
* Generate using: `Json->Add("key")->AsString = "value"`.
* Parse using: `JsonP->AsJSON = Json->AsJSON`.
