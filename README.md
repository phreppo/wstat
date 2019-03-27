# wstat

[![CircleCI](https://circleci.com/gh/parof/wstat.svg?style=svg)](https://circleci.com/gh/parof/wstat)

## Installation prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (version 1.7.1 or newer)

On Unix System install as:
```bash
curl -sSL https://get.haskellstack.org/ | sh
```
Or
```bash
wget -qO- https://get.haskellstack.org/ | sh
```

## Get Started

Before the first use build dependecies:
```bash
./init
```

Build the project using:
```bash
./build
```

Test the project using:
```bash
./spec
```

## Build a new Concrete Domain

1. build the domain, add the module in the ```src/Domains``` directory
2. add the new domain's name in the DomainsList module
3. add the corrispettive initial-state builder in the InitialStateBuilder module
4. add in the main the analysis runner concretized with the relative initial-state builder