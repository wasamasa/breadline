@echo off
%CHICKEN_CSC% -C %CFLAGS% -L -lreadline %LDFLAGS% %*
