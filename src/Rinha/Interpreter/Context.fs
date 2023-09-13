namespace Rinha.Interpreter

open System.Text
open Rinha.AST.Nodes

type RuntimeError = { description: string; location: Term }

type Context =
    { locals: Map<string, Term>
      output: StringBuilder
      result: Term }

type Eval<'T> = 'T -> Context -> Result<Context, RuntimeError>

module Context =
    let empty: Context =
        { locals = Map.empty
          output = StringBuilder()
          result = Term.Null }

    let declare varName varValue context =
        { context with
            locals = context.locals |> Map.add varName varValue }

    let literal term = { empty with result = term }
    let result context = context.result

    let withResult result context = { context with result = result }

module Var =
    let eval (context: Context) (term: Var) =
        if (context.locals |> Map.containsKey term.text) then
            Ok
                { context with
                    result = context.locals.[term.text] }
        else
            Error
                { description = $"Variable {term.text} not declared"
                  location = Var term }

module Let =
    let eval (term: Let) (context: Context) =
        context |> Context.declare term.name.text term.value |> Ok

module Print =
    let eval (evaluator: Eval<Term>) (term: Print) (context: Context) =
        let context = context |> evaluator term.value
        Ok context
