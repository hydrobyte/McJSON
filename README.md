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
   *  [uJSON](https://sourceforge.net/projects/is-webstart/) (Delphi Web Utils)

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

## Use Cases
Please considere read Unit Tests in `test` folder for a complete list of `McJSON` use cases.

### Object and Array Setters
not so common out there.
```pascal
N.AsJSON := '{"o": {"k1":"v1", "k2":"v2"}}';
N['o'].AsString := 'str';
```
Results in:
```json
{
   "o":{
      "k1":"str",
      "k2":"str"
   }
}
```

## Performance Tests
A performance test have been done with the original `myJSON`, `LkJson`, `JsonTools` and `uJSON` units.
Here is a summary of the tests.
* Generate a JSON with 50k items like: `{... {"keyi":"valuei"}... }`
* Save to file.
* Parse from memory (copy object forcing a parse).
* Load from file (and parsing).
* Access 1k items randomly.

And about the compiler and machine used:
* C++Builder VCL examples built with BDS 2006 (the older version I have).
* Very old 32 bits machine: Intel Core 2 CPU T5500 1.66GHz 4 GB RAM.

The next table summarizes the results[^1]:

Library    | Generate  | Save     | Parse    | Load     | Access  | Total      |
:----------|----------:|---------:|---------:|---------:|--------:|-----------:|
`McJSON`   |     .08 s |    .09 s |    .11 s |    .16 s |   .54 s |     1.05 s |
`LkJson`   |     .30 s |    .13 s |    .47 s |    .36 s |   .00 s |     1.22 s |
`JsonTools`|   48.00 s |    .70 s |  39.00 s |  40.00 s |   .48 s |    1.2 min |
`myJSON`   |   50.00 s |    .07 s |  5.1 min |  7.7 min |  1.60 s |   13.1 min |
`uJSON`[^2]|  18.6 min | 20.1 min | 17.5 min |   4.31 s | 53.02 s |   57.6 min |


[^1]: Metric: average time in seconds (s) for 5 consecutive executions. Total is the average of partial tests. Some results converted to minutes (min).
[^2]: With `uJSON`, there seems to be a performance problem related to `toString()`.

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

### Notes about `uJSON`
* Less verbosy in C++ than `LkJson`, but the colection of classes also will force casting with `dynamic_cast`.
* Uses TStringList as a "Hash Map" [string] -> [object address]. The comas here is because I think the string entry is not a true hash within TStringList.
* In some aspectes, the methods interface might became puzzling.
* It needs a performance review.
* This unit is used in other projects, e.g. [Diffbot API Delphi Client Library](https://github.com/diffbot/diffbot-delphi-client) (same author).
* Generate using: `Json->put("key", "value")`.
* Parse using: `JsonP = new TJSONObject(Json->toString())`.
* `SaveToFile` doesn't exist, so it has used `TStringList->SaveToFile()` after filling `Text` with `Json->toString()`.

### Notes about `McJSON`
* Good performance, but not the better about random access due to the use of TList.
* Simple and smart interface using "AsXXX" getters and setters (not invented here).
* Generate using: `Json->Add("key")->AsString = "value"`.
* Parse using: `JsonP->AsJSON = Json->AsJSON`.
