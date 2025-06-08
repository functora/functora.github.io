#!/bin/bash
set -e
stack build --stack-yaml=stack-11.yaml
stack build --stack-yaml=stack-12.yaml
stack build --stack-yaml=stack-14.yaml
stack build --stack-yaml=stack-15.yaml
stack build --stack-yaml=stack-16.yaml
stack build --stack-yaml=stack-17.yaml
stack build --stack-yaml=stack-18.yaml
