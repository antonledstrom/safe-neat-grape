@echo off
cls

dotnet fake build --target run -e DockerUser=safe-template -e DockerImageName=safe-template