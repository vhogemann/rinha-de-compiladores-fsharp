namespace Rinha.Interpreter

open System.Numerics
open Rinha.AST.Nodes

type Value =
| Int of BigInteger
| Str of string
| Bool of bool
| Tuple of Value*Value
| Fun of Function
| Error of string
| Null

type Evaluator = Term -> Value

type Environment = Map<string,Value>