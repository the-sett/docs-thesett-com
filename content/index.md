---
{
  "type": "error",
  "code": 201
}
---
This error comes up all the time, but don't worry. It looks like this:

|> Source
    code =
        Source code position 0
        Source code position 1
    params = { "intelligence": "clever" }
    pos = [{ "r1": 0, "c1": 0, "r2": 0, "c2": 5 }
          ,{ "r1": 1, "c1": 7, "r2": 1, "c2": 10 }
          ]

And then you can fix this by just doing...
