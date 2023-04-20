#!/bin/bash

docker build -t lily-typechecker:latest .
docker run -it --rm --name lily-typechecker lily-typechecker:latest