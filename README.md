# wstat

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

Before building the project build the parser for the while language:
```bash
./generateGrammar
```

Build the project using Stack
```bash
stack build
```


