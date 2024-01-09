# Json

The first task is to write a comfortable Json ADT.

## Simplified Json
We'll be taking a slightly simplified definition of Json here, to avoid getting bogged down in technical details that are not terribly relevant to what we're doing.

In the context of this exercise, a Json value can be one of:
- a string.
- an integer.
- a boolean.
- null.
- an array of Json values.
- an object, which is a collection of key / value pairs where keys are strings and values are Json values.

For example:

```json
{
  "string" : "this is a string",
  "integer": 123,
  "bool"   : true,
  "null"   : null,
  "array"  : ["a string", null, 321],
  "object" : {
    "key": "value"
  }
}
```

To make things interesting though, you need to write comfortable creation helpers, so that users have to manipulate as little as possible of the underlying implementation when creating values.

## Json documents

For the sake of training, we're going to pretend that compound and scalar data types need to treated differently. What this means, concretely, is that you need to have a type representing all possible Json values, and another that is restricted to array and object.

## Pretty printing

Write a simple pretty printer for Json values - a function that turns them in their string representation. There's no need to do anything fancy like different whitespace handling mechanisms (human readable or compact, for example), but you do need to implement two version:
- one that prints any Json value.
- one that only prints Json documents.

## Null simplification

Another interesting Json interpreter is one that returns a transformed version of a Json document in which all `null` values in an object are removed.

For example, take the following input:

```json
{
  "is_null": null,
  "not_null" : {
    "not_null_either": 123,
    "also_null": null
  }
}
```

Your function should yield:

```json
{
  "not_null" : {
    "not_null_either": 123
  }
}
```
