---
{
  "type": "error",
  "code": 201
}
---
An unresolved reference error means that you have tried to use a declaration
that has not been made. This could be because you have forgotten it, or it could
be that you made a typo and did not quite get the name correct.

An example error message looks like:

|> Error
    code =
        Source code position 0
        Source code position 1
    params = {}
    pos = [{ "r1": 0, "c1": 0, "r2": 0, "c2": 5 }
          ]

Fix this by carefully checking that the referred to name has been declared and
that the names exactly match.
