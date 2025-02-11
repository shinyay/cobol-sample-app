# COBOL サンプルアプリケーション

このリポジトリには、基本的なデータ処理を示す COBOL プログラムが含まれています。

## 説明

この COBOL プログラムは、シンプルな給与システムです。

- [ワークショップシナリオ](./workshop-scenario.md)

### IDENTIFICATION DIVISION
このセクションはプログラムを識別します。

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PayrollSystem.
```

### DATA DIVISION
このセクションは、プログラムで使用されるデータ構造と変数を定義します。

#### WORKING-STORAGE SECTION
このセクションは、変数とテーブルを宣言します。

```cobol
WORKING-STORAGE SECTION.
01  MAX-EMPLOYEES           PIC 9(3) VALUE 5.
```
- `MAX-EMPLOYEES`: 従業員の最大数を保持する数値変数で、5に設定されています。

#### EMPLOYEE-TABLE
従業員の詳細を格納するテーブル。

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
- `EMPLOYEE-ENTRY OCCURS 5 TIMES`: 従業員の詳細を5つのエントリで定義する配列。
- 各エントリには、ID、名前、部署、総給与、ボーナス、控除、純給与、税控除のフィールドが含まれます。

#### SORTED-EMPLOYEE-TABLE
ソートされた従業員の詳細を格納するテーブル。

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
- `EMPLOYEE-TABLE`と似ていますが、ソートされた従業員の詳細を格納するために使用されます。

#### DEPARTMENT-TOTALS
部署ごとの総給与を格納するテーブル。

```cobol
01  DEPARTMENT-TOTALS.
    05  DEPARTMENT-TOTAL OCCURS 5 TIMES.
        10  DEPT-NAME          PIC X(10).
        10  TOTAL-SALARY       PIC 9(7)V99.
```
- `DEPARTMENT-TOTAL OCCURS 5 TIMES`: 部署ごとの総給与を5つのエントリで定義する配列。
- 各エントリには、部署名と総給与のフィールドが含まれます。

#### その他の変数
プログラムで使用される追加の変数。

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
- これらの変数は、インデックス付け、レートの格納、ソート中の一時的な格納に使用されます。

### PROCEDURE DIVISION
このセクションには、実行可能なコードが含まれます。

#### MAIN-PROCEDURE
他の手続きを調整するメイン手続き。

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
- データの初期化、ソート、計算、表示を行うさまざまな手続きを呼び出します。

#### INITIALIZE-EMPLOYEES
サンプルデータで従業員テーブルを初期化します。

```cobol
INITIALIZE-EMPLOYEES.
    MOVE "E001" TO EMPLOYEE-ID(1).
    MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
    MOVE "HR" TO DEPARTMENT(1).
    MOVE 70000.00 TO GROSS-SALARY(1).
    ...
```
- サンプルデータを`EMPLOYEE-TABLE`に移動します。

#### SORT-EMPLOYEES
従業員をIDでソートします。

```cobol
SORT-EMPLOYEES.
    PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
        MOVE EMPLOYEE-ID(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
        ...
    END-PERFORM.
    ...
```
- `EMPLOYEE-TABLE`から`SORTED-EMPLOYEE-TABLE`にデータをコピーします。
- バブルソートアルゴリズムを使用して従業員をIDでソートします。

#### CALCULATE-NET-SALARIES
各従業員の純給与を計算します。

```cobol
CALCULATE-NET-SALARIES.
    PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
        COMPUTE SORT-BONUS(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * BONUS-RATE
        ...
    END-PERFORM.
```
- 各従業員のボーナス、控除、税控除、純給与を計算します。

#### CALCULATE-DEPARTMENT-TOTALS
各部署の総給与を計算します。

```cobol
CALCULATE-DEPARTMENT-TOTALS.
    PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
        MOVE SPACES TO DEPT-NAME(DEPARTMENT-INDEX)
        ...
    END-PERFORM.
    ...
```
- 部署の合計を初期化します。
- 部署ごとに純給与を集計します。

#### DISPLAY-EMPLOYEES
ソートされた従業員の詳細を表示します。

```cobol
DISPLAY-EMPLOYEES.
    DISPLAY "Employee Payroll Information".
    ...
    PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
        DISPLAY "Employee ID: " SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
        ...
    END-PERFORM.
```
- 各従業員の詳細を表示します。

#### DISPLAY-DEPARTMENT-TOTALS
各部署の総給与を表示します。

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
- 各部署の総給与を表示します。

この COBOL プログラムは、従業員データを初期化し、ソートし、純給与を計算し、部署ごとの合計を集計し、結果を表示するシンプルな給与システムです。

## デモ

- [Online COBOL](https://www.jdoodle.com/ia/1zPE)

## 特徴

- feature:1
- feature:2

## 要件

## 使用方法

## インストール

## 参考文献

- [JDoodle](https://www.jdoodle.com/)

## ライセンス

[MITライセンス](https://gist.githubusercontent.com/shinyay/56e54ee4c0e22db8211e05e70a63247e/raw/f3ac65a05ed8c8ea70b653875ccac0c6dbc10ba1/LICENSE)の下で公開されています。

## 作者

- github: <https://github.com/shinyay>
- twitter: <https://twitter.com/yanashin18618>
- mastodon: <https://mastodon.social/@yanashin>
