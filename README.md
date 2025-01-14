# COBOL Sample Application

This repository contains a COBOL program that demonstrates basic data handling and processing.

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

#### SORTED-EMPLOYEE-TABLE
A table to store sorted employee details.

```cobol
01  SORTED-EMPLOYEE-TABLE.
    05  SORTED-EMPLOYEE-ENTRY OCCURS 5 TIMES.
        10  SORT-EMPLOYEE-ID   PIC X(5).
        10  SORT-EMPLOYEE-NAME PIC X(20).
        10  SORT-DEPARTMENT    PIC X(10).
        10  SORT-GROSS-SALARY  PIC 9(7)V99.
        10  SORT-BONUS         PIC 9(5)V99.
        10  SORT-DEDUCTIONS    PIC 9(5)V99.
        10  SORT-NET-SALARY    PIC 9(7)V99.
        10  SORT-TAX-DEDUCTION PIC 9(5)V99.
```
- Similar to `EMPLOYEE-TABLE`, but used for storing sorted employee details.

#### DEPARTMENT-TOTALS
A table to store total salaries by department.

```cobol
01  DEPARTMENT-TOTALS.
    05  DEPARTMENT-TOTAL OCCURS 5 TIMES.
        10  DEPT-NAME          PIC X(10).
        10  TOTAL-SALARY       PIC 9(7)V99.
```
- `DEPARTMENT-TOTAL OCCURS 5 TIMES`: Defines an array with 5 entries for department totals.
- Each entry contains fields for department name and total salary.

#### Other Variables
Additional variables used in the program.

```cobol
01  EMPLOYEE-INDEX           PIC 9(3).
01  INNER-INDEX              PIC 9(3).
01  TAX-RATE                 PIC 9V99 VALUE 0.20.
01  BONUS-RATE               PIC 9V99 VALUE 0.10.
01  DEDUCTION-RATE           PIC 9V99 VALUE 0.05.
01  DEPARTMENT-INDEX         PIC 9(3).
01  TEMP-ID                  PIC X(5).
01  TEMP-NAME                PIC X(20).
01  TEMP-DEPARTMENT          PIC X(10).
01  TEMP-SALARY              PIC 9(7)V99.
```
- These variables are used for indexing, storing rates, and temporary storage during sorting.

### PROCEDURE DIVISION
This section contains the executable code.

#### MAIN-PROCEDURE
The main procedure that orchestrates the execution of other procedures.

```cobol
PROCEDURE DIVISION.
MAIN-PROCEDURE.
    PERFORM INITIALIZE-EMPLOYEES.
    PERFORM SORT-EMPLOYEES.
    PERFORM CALCULATE-NET-SALARIES.
    PERFORM CALCULATE-DEPARTMENT-TOTALS.
    PERFORM DISPLAY-EMPLOYEES.
    PERFORM DISPLAY-DEPARTMENT-TOTALS.
    STOP RUN.
```
- Calls various procedures to initialize, sort, calculate, and display data.

#### INITIALIZE-EMPLOYEES
Initializes the employee table with sample data.

```cobol
INITIALIZE-EMPLOYEES.
    MOVE "E001" TO EMPLOYEE-ID(1).
    MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
    MOVE "HR" TO DEPARTMENT(1).
    MOVE 70000.00 TO GROSS-SALARY(1).
    ...
```
- Moves sample data into the `EMPLOYEE-TABLE`.

#### SORT-EMPLOYEES
Sorts the employees by their IDs.

```cobol
SORT-EMPLOYEES.
    PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
        MOVE EMPLOYEE-ID(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
        ...
    END-PERFORM.
    ...
```
- Copies data from `EMPLOYEE-TABLE` to `SORTED-EMPLOYEE-TABLE`.
- Uses a bubble sort algorithm to sort employees by ID.

#### CALCULATE-NET-SALARIES
Calculates the net salaries for each employee.

```cobol
CALCULATE-NET-SALARIES.
    PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
        COMPUTE SORT-BONUS(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * BONUS-RATE
        ...
    END-PERFORM.
```
- Computes bonus, deductions, tax deduction, and net salary for each employee.

#### CALCULATE-DEPARTMENT-TOTALS
Calculates the total salaries for each department.

```cobol
CALCULATE-DEPARTMENT-TOTALS.
    PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
        MOVE SPACES TO DEPT-NAME(DEPARTMENT-INDEX)
        ...
    END-PERFORM.
    ...
```
- Initializes department totals.
- Aggregates net salaries by department.

#### DISPLAY-EMPLOYEES
Displays the sorted employee details.

```cobol
DISPLAY-EMPLOYEES.
    DISPLAY "Employee Payroll Information".
    ...
    PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
        DISPLAY "Employee ID: " SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
        ...
    END-PERFORM.
```
- Displays each employee's details.

#### DISPLAY-DEPARTMENT-TOTALS
Displays the total salaries for each department.

```cobol
DISPLAY-DEPARTMENT-TOTALS.
    DISPLAY "Department Salary Totals".
    ...
    PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
        IF DEPT-NAME(DEPARTMENT-INDEX) NOT = SPACES
            DISPLAY "Department: " DEPT-NAME(DEPARTMENT-INDEX)
            ...
    END-PERFORM.
```
- Displays each department's total salary.

This COBOL program is a simple payroll system that initializes employee data, sorts it, calculates net salaries, aggregates department totals, and displays the results.

## Demo

## Features

- feature:1
- feature:2

## Requirement

## Usage

## Installation

## References

- [JDoodle](https://www.jdoodle.com/)

## Licence

Released under the [MIT license](https://gist.githubusercontent.com/shinyay/56e54ee4c0e22db8211e05e70a63247e/raw/f3ac65a05ed8c8ea70b653875ccac0c6dbc10ba1/LICENSE)

## Author

- github: <https://github.com/shinyay>
- twitter: <https://twitter.com/yanashin18618>
- mastodon: <https://mastodon.social/@yanashin>
