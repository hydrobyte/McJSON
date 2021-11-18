# McJSON
A **Delphi / Lazarus / C++Builder** simple and small class for fast JSON parsing.

* [Motivation](#motivation)
* [Examples](#examples)
* [Use cases](#use-cases)
* [Known issues](#known-issues)
* [Performance tests](#performance-tests)

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
   * C++Builder XE2 and 10.2.
 * Just one unit (`McJSON`), just one class(`TMcJsonItem`).
 * Inspired by [badunius/myJSON](https://github.com/badunius/myJSON).
 * Improved parser after applying Tan Li Hau's [article](https://lihautan.com/json-parser-with-javascript/#understand-the-grammar).
 * Performance [tests](#performance-tests) using C++Builder and comparing:
   *  [myJSON](https://github.com/badunius/myJSON) 
   *  [LkJson](https://sourceforge.net/projects/lkjson/)
   *  [JsonTools](https://github.com/sysrpl/JsonTools)
   *  [uJSON](https://sourceforge.net/projects/is-webstart/) (Delphi Web Utils)

## Examples
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
      Json.Add('array', jitArray);
      for i := 1 to 3 do
        Json['array'].Add.AsInteger := i;
      // save a backup to file
      if (Json['array'].Count = 3) then
        Json.SaveToFile('test99.json');
      // remove an item
      Json.Delete('array');
      // oops, load the backup
      if (Json.Count = 4) then
        Json.LoadFromFile('test99.json');
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
Will produce `\test\test99.json`:
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
      Json->Add("array", jitArray);
      for (int i = 1; i <= 3 ; i++)
        Json->Items["array"]->Add()->AsInteger = i;
      // save a backup to file
      if (Json->Items["array"]->Count == 3)
        Json->SaveToFile("test99.json");
      // remove an item
      Json->Delete("array");
      // oops, load the backup
      if (Json->Count == 4)
        Json->LoadFromFile("test99.json");
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

### Parse a JSON string
Just use the `AsJSON` property
```pascal
var
  N: TMcJsonItem;
begin  
  N := TMcJsonItem.Create;
  N.AsJSON := '{"i": 123, "f": 123.456, "s": "abc", "b": true, "n": null}';
  // use N here
  N.Free;
end;  
```
If you want to check if a JSON string is valid:
```pascal
Answer := N.Check( '{"i":[123}' ); 
// Answer will be false due to exception:
// Error while parsing text: "expected , got }" at pos "10"
```

### Array or object items
Here is how to access all items (children) of a JSON object and change their value type and content.
```pascal
N.AsJSON := '{"o": {"k1":"v1", "k2":"v2"}}';
// type and value: from string to integer
for i := 0 to N['o'].Count-1 do  
  N['o'].Values[i].AsInteger := i+1;   
```
Results in:
```json
{
   "o": {
      "k1":1,
      "k2":2
   }
}
```

### Shortener for array item access
We can use the `Values[index]` and `Items['key']` properties to access items inside objects and arrays. 
Since version `0.9.5`, we can use the `At(index, 'key')` as a shortener.
```pascal
N.AsJSON := '{"a": [{"k1":1,"k2":2},{"k1":10,"k2":20}]}';
// how to access k2 of second object.
i := N['a'].Values[1].Items['k2'].AsInteger; // i will be equal to 20
i := N['a'].Values[1]['k2'].AsInteger;       // uses the Items[] as default property
i := N['a'].At(1, 'k2').AsInteger;           // shortener
```
And there are other uses without the `key` parameter:
```pascal
N.AsJSON := '{"k1":1,"k2":2,"k3":3,"k4":4}';
i := N.Values[2].AsInteger; // i will be equal to 3
i := N.At(2).AsInteger;     // shortener
```

### Enumerate
Using Delphi enumerator you can browse item's object children and values.
```pascal
var
  N, item: TMcJsonItem;
begin
  N := TMcJsonItem.Create;
  N.AsJSON := '{"o": {"k1":"v1", "k2":"v2"}}';
  for item in N['o'] do
    // use item here, e.g. item.Key, item.Value, item.AsString
```

### Object and array value setters
Change all values of an object with multiple items.
Not so common out there.
```pascal
N.AsJSON := '{"o": {"k1":"v1", "k2":"v2"}}';
N['o'].AsString := 'str';
```
Results in:
```json
{
   "o": {
      "k1": "str",
      "k2": "str"
   }
}
```
And if it is necessary to change the type of `o`:
```pascal
N['o'].ItemType := jitValue;
N['o'].AsString := 'str';
```
Results in:
```json
{
  "o": "str"
}
```

### Object and array type convertions
Convert from array to object type and vice-versa.
Also, not so common out there.
```pascal
N.AsJSON := '{ "k1": ["1", "2"], "k2": {"1": "a", "2": "b"} }';
N['k1'].ItemType := jitObject; // convert array to object with items
N['k2'].ItemType := jitArray ; // convert object with items to array 
```
Results in:
```json
{
   "k1": {
      "0": "1",
      "1": "2"
   },
   "k2": [
      "a",
      "b"
   ]
}
```

### Insert items
Insert some items using keys and position.
```pascal
P.Insert('c', 0).AsInteger := 3;
P.Insert('b', 0).AsInteger := 2;
P.Insert('a', 0).AsInteger := 1;
```
Results in:
```json
{
  "a": 1,
  "b": 2,
  "c": 3
}
```
Also, it is possible to insert objects in arrays.
```pascal
Q.AsJSON := '{"x":0}';
P.ItemType := jitArray;
P.Insert(Q, 1);
```
Results in:
```json
[
  1, 
  {
    "x": 0
  }, 
  2, 
  3
]
```
*Important*: since version 0.9.3, `Add()` and  `Insert()` will clone arguments of type `TMcJsonItem`. So, we have to free memory for `Q` too:
```pascal
P.Free;
Q.Free;
```

### Inspect the content of an object
Let's see how to inspect all the inner data structure, types and values of a `TMcJsonItem` object.
```c++
//---------------------------------------------------------------------------
void
TFormMain::Inspect(TMcJsonItem* AMcJItem, AnsiString Ident)
{
  if (!AMcJItem) return;
  // log current
  MyLog( Ident + ItemToStr(AMcJItem) );
  // log child
  if ( AMcJItem->HasChild )
  {
    Ident = "  " + Ident;
    for (int i=0; i < AMcJItem->Count; i++)
    { // use Value not Child because are note using Key[].
      Inspect( AMcJItem->Values[i], Ident );
    }
  }
}
//---------------------------------------------------------------------------
String
TFormMain::ItemToStr(TMcJsonItem* AMcJItem) const
{
  String Ans = "";
  if (AMcJItem)
    Ans =             AMcJItem->GetTypeStr() +
          "; "      + AMcJItem->GetValueStr() +
          "; Key="  + AMcJItem->Key +
          "; Value="+ AMcJItem->Value +
          "; JSON=" + AMcJItem->AsJSON;
  return (Ans);
}
//---------------------------------------------------------------------------
```
And using a example like `testInspect.json`:
```json
{
   "foo": "bar",
   "array": [
      100,
      20
   ],
   "arrayObj": [
      {
         "key1": 1.0
      },
      {
         "key2": 2.0
      }
   ],
   "Msg": [
      "#1 UTF8 example: motivação",
      "#2 Scapes: \b\t\n\f\r\\uFFFF\"\\"
   ]
}
```

Calling `Inspect()` with a `Json` object loaded with `testInspect.json`:
```c++
TMcJsonItem* Json = new TMcJsonItem();
if (Json)
{
  Json->LoadFromFile("testInspect.json");
  Inspect(Json);
  delete (Json);
}
```

Results in:
```
object; string; Key=; Value=; JSON={"foo":"bar","array":[100,20],"arrayObj":[{"key1":1.0},{"key2":2.0}],"Msg":["#1 UTF8 example: motivação","#2 Scapes: \b\t\n\f\r\u\"\\"]}
   value; string; Key=foo; Value=bar; JSON="foo":"bar"
   array; string; Key=array; Value=; JSON="array":[100,20]
     value; number; Key=; Value=100; JSON=100
     value; number; Key=; Value=20; JSON=20
   array; string; Key=arrayObj; Value=; JSON="arrayObj":[{"key1":1.0},{"key2":2.0}]
     object; string; Key=; Value=; JSON={"key1":1.0}
       value; number; Key=key1; Value=1.0; JSON="key1":1.0
     object; string; Key=; Value=; JSON={"key2":2.0}
       value; number; Key=key2; Value=2.0; JSON="key2":2.0
   array; string; Key=Msg; Value=; JSON="Msg":["#1 UTF8 example: motivação","#2 Scapes: \b\t\n\f\r\uFFFF\"\\"]
     value; string; Key=; Value=#1 UTF8 example: motivação; JSON="#1 UTF8 example: motivação"
     value; string; Key=; Value=#2 Scapes: \b\t\n\f\r\uFFFF\"\\; JSON="#2 Scapes: \b\t\n\f\r\uFFFF\"\\"
```

### A note about empty keys
Since version `0.9.0`, empty keys will be parsed and checked withou errors:
```pascal
N.AsJSON := '{"": "value"}';
```
And `ToString()` will produce a valid JSON object:
```json
{
  "": "value"
}
```
Internally, it will use the C_EMPTY_KEY constant string as content of the fKey field.

### A note about line breaks
Since version `0.9.2`, strings with not escaped line breakes will be parsed with errors:
```pascal
N.AsJSON := '{"key": "value' + #13 + '"}';
```
Will raise exception:
```
Error while parsing text: "line break" at pos "14"
```

### Load from and Save to Files
`McJSON` can load from ASCII and UTF-8 files (without BOM). See `LoadFromFile` method.
The `SaveToFile` method will write using UTF-8 encoding.

## Known issues
The world is not perfect and neither am I.
Here are some known issues:
* Trying to follow and confirm the [specification](https://www.json.org/json-en.html) using [JSONLint](https://jsonlint.com/).

## Performance tests
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
`uJSON`    |  18.6 min | 20.1 min | 17.5 min |   4.31 s | 53.02 s |   57.6 min |


[^1]: Metric: average time in seconds (s) for 5 consecutive executions. Total is the average of partial tests. Some results converted to minutes (min).

### Notes about `McJSON`
* Good performance, but not the better about random access due to the use of TList.
* Simple and smart interface using "AsXXX" getters and setters (not invented here).
* Generate using: `Json->Add("key")->AsString = "value"`.
* Parse using: `JsonP->AsJSON = Json->AsJSON`.

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

### Notes about `myJSON`
* Performance deteriored due the recurrent use of wsTrim().
* Generate using: `Json->Item["key"]->setStr("value")`.
* Parse using: `JsonP->Code = Json->getJSON()`.

### Notes about `uJSON`
* Less verbosy in C++ than `LkJson`, but the colection of classes also will force casting with `dynamic_cast`.
* Uses TStringList as a "Hash Map" [string] -> [object address]. The comas here is because I think the string entry is not a true hash within TStringList.
* In some aspectes, the methods interface might became puzzling.
* It needs a performance review.
* With `uJSON`, there seems to be a performance problem related to `toString()`.
* This unit is used in other projects, e.g. [Diffbot API Delphi Client Library](https://github.com/diffbot/diffbot-delphi-client) (same author).
* Generate using: `Json->put("key", "value")`.
* Parse using: `JsonP = new TJSONObject(Json->toString())`.
* `SaveToFile` doesn't exist, so it has used `TStringList->SaveToFile()` after filling `Text` with `Json->toString()`.

