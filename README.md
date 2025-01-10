# Title

## Description

This COBOL program is a simple payroll system.

### IDENTIFICATION DIVISION
This section identifies the program.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PayrollSystem.
```

### DATA DIVISION
This section defines the data structures and variables used in the program.

#### WORKING-STORAGE SECTION
This section declares variables and tables.

```cobol
WORKING-STORAGE SECTION.
01  MAX-EMPLOYEES           PIC 9(3) VALUE 5.
```
- `MAX-EMPLOYEES`: A numeric variable that holds the maximum number of employees, set to 5.

#### EMPLOYEE-TABLE
A table to store employee details.

```cobol
01  EMPLOYEE-TABLE.
    05  EMPLOYEE-ENTRY OCCURS 5 TIMES.
        10  EMPLOYEE-ID        PIC X(5).
        10  EMPLOYEE-NAME      PIC X(20).
        10  DEPARTMENT         PIC X(10).
        10  GROSS-SALARY       PIC 9(7)V99.
        10  BONUS              PIC 9(5)V99.
        10  DEDUCTIONS         PIC 9(5)V99.
        10  NET-SALARY         PIC 9(7)V99.
        10  TAX-DEDUCTION      PIC 9(5)V99.
```
- `EMPLOYEE-ENTRY OCCURS 5 TIMES`: Defines an array with 5 entries for employee details.
- Each entry contains fields for ID, name, department, gross salary, bonus, deductions, net salary, and tax deduction.


## Demo

## Features

- feature:1
- feature:2

## Requirement

## Usage

## Installation

## References

## Licence

Released under the [MIT license](https://gist.githubusercontent.com/shinyay/56e54ee4c0e22db8211e05e70a63247e/raw/f3ac65a05ed8c8ea70b653875ccac0c6dbc10ba1/LICENSE)

## Author

- github: <https://github.com/shinyay>
- twitter: <https://twitter.com/yanashin18618>
- mastodon: <https://mastodon.social/@yanashin>
