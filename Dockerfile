FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build

# Install node
ARG NODE_MAJOR=20
RUN apt-get update
RUN apt-get install -y ca-certificates curl gnupg
RUN mkdir -p /etc/apt/keyrings
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
RUN echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list
RUN apt-get update && apt-get install nodejs -y

WORKDIR /workspace
COPY . .
RUN dotnet tool restore
RUN dotnet run Bundle

FROM mcr.microsoft.com/dotnet/aspnet:8.0-alpine
COPY --from=build /workspace/deploy /app
WORKDIR /app

# Create directory for excel files
RUN mkdir -p /data/excel

# Set default environment variable
ENV CAG_REGISTER_FILE_PATH=/data/excel

EXPOSE 5000
ENTRYPOINT [ "dotnet", "Server.dll" ]
