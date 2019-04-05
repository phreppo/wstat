# wstat

[![CircleCI](https://circleci.com/gh/parof/wstat.svg?style=svg)](https://circleci.com/gh/parof/wstat)

Wstat is a statical analyzer for the _While_ toy language. It relies on [Abstract Interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation) for run a _sound_ analysis

## The language

The syntax of the While language is given by the following grammar.

```python
Stmt  : var ':=' AExpr
      | Stmt ';' Stmt
      | 'skip'
      | 'if' BExpr 'then' Stmt 'else' Stmt 'endif'
      | 'while' BExpr 'do' Stmt 'done'

AExpr : '(' AExpr ')'
      | int
      | var
      | AExpr '+' AExpr
      | AExpr '-' AExpr
      | AExpr '*' AExpr
      | AExpr '/' AExpr
      | '[' int ',' int ']'
      | '[' '-' int ',' int ']'
      | '[' '-' int ',' '-' int ']'
      | '[' 'neginf' ',' '-' int ']'
      | '[' 'neginf' ',' int ']'
      | '[' '-' int ',' 'posinf' ']'
      | '[' int ',' 'posinf' ']'
      | '[' 'neginf' ',' 'posinf' ']'

BExpr : '(' BExpr ')'
      | bool
      | 'not' BExpr
      | BExpr 'and' BExpr
      | BExpr 'or'  BExpr
      | AExpr '!='  AExpr
      | AExpr  '='  AExpr
      | AExpr '<='  AExpr
      | AExpr '>='  AExpr
      | AExpr  '<'  AExpr
      | AExpr  '>'  AExpr
```

## Abstract Domains

There are three different [abstract domains](https://en.wikipedia.org/wiki/Abstract_interpretation#Examples_of_abstract_domains):

- **Simple Sign Domain**: 
![alt text](img/simpleSignDomain.png "Simple sign Domain")
- **Sign Domain**: 
![alt text](img/signDomain.png "Sign domain")
- **Interval Domain**: 
![alt text](img/intervalDomain.png "Interval domain")

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

Build the project:
```bash
./build
```

Test the project:
```bash
./spec
```

Run the project:
```bash
./r
```

## Building a new Concrete Non-relational Domain

1. Build the (non-relational) domain, add the module in the ```src/Domains``` directory
2. Add the new domain's name in the Domains.DomainsList module
3. Add the corrispettive initial-state builder in the InitialStateBuilder module
4. Add in the main the procedure to run the analysis instantiated with the relative initial-state builder