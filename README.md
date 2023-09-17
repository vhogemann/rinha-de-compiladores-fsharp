
<div align="center">

![banner](banner.png)

</div>

# Tree-Walking Interpreter em F#

Implementação de um Tree-Walking Interpreter em F# para a [Rinha de Compiler]().

## Como executar

### Usando Docker

A imagem está configurada para executar um arquivo AST em JSON em `/var/rinha/source.rinha.json` e imprimir o resultado 
no console. Monte um volume da sua máquina contendo esse arquivo apontando para `/var/rinha` e execute o container.

Exemplo:
```bash
docker build -t rinha-de-compiler-fsharp .
docker run rinha-de-compiler-fsharp -v $(pwd)/rinha/:/var/rinha
```

### Usando .NET

Para executar o projeto usando o .NET, é necessário ter o SDK instalado na sua máquina. Para mais informações,
acesse a [documentação oficial](https://dotnet.microsoft.com/download).

Após instalar o SDK, execute o seguinte comando na raiz do projeto:
```bash
dotnet run --project src/Rinha -- source.rinha.json
```

Substitua `source.rinha.json` pelo caminho do arquivo AST em JSON que você deseja executar. Por exemplo
você pode usar um dos arquivos disponíveis no projeto de testes:

```bash
dotnet run --project src/Rinha -- src/Test/JSON/fib.json
```