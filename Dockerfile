FROM mcr.microsoft.com/dotnet/aspnet:7.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["src/Rinha/Rinha.fsproj", "./"]
RUN dotnet restore "Rinha.fsproj"
COPY ./src/Rinha/ ./
WORKDIR "/src/"
RUN dotnet build "Rinha.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Rinha.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Rinha.dll"]