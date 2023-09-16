namespace Rinha.Interpreter

open Rinha.AST.Nodes

type Value =
| Int of decimal
| Str of string
| Bool of bool
| Tuple of Value*Value
| Fun of Function
| Error of string
| Null

type Evaluator = Term -> Value

type Environment = Map<string,Value>