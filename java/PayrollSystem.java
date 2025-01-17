import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Manages the overall payroll system, including initializing employees, sorting employees, calculating net salaries,
 * calculating department totals, and displaying information.
 */
public class PayrollSystem {
    // Constants
    private static final int MAX_EMPLOYEES = 5;
    private static final double TAX_RATE = 0.20;
    private static final double BONUS_RATE = 0.10;
    private static final double DEDUCTION_RATE = 0.05;

    // Attributes
    private List<Employee> employees = new ArrayList<>();
    private List<DepartmentTotal> departmentTotals = new ArrayList<>();

    /**
     * Initializes the employee data with hardcoded values.
     */
    public void initializeEmployees() {
        employees.add(new Employee("E001", "Alice Johnson", "HR", 70000.00));
        employees.add(new Employee("E002", "Bob Smith", "IT", 85000.00));
        employees.add(new Employee("E003", "Charlie Brown", "Finance", 60000.00));
        employees.add(new Employee("E004", "David Wilson", "IT", 95000.00));
        employees.add(new Employee("E005", "Eve Davis", "HR", 75000.00));
    }

    /**
     * Sorts the employees by their IDs.
     */
    public void sortEmployees() {
        Collections.sort(employees, Comparator.comparing(Employee::getId));
    }

    /**
     * Calculates the net salaries for all employees.
     */
    public void calculateNetSalaries() {
        BonusCalculator bonusCalculator = new BonusCalculator();
        for (Employee employee : employees) {
            double bonus = bonusCalculator.calculateBonus(employee.getGrossSalary());
            double deductions = employee.getGrossSalary() * DEDUCTION_RATE;
            double taxDeduction = employee.getGrossSalary() * TAX_RATE;
            double netSalary = employee.getGrossSalary() + bonus - taxDeduction - deductions;

            employee.setBonus(bonus);
            employee.setDeductions(deductions);
            employee.setTaxDeduction(taxDeduction);
            employee.setNetSalary(netSalary);
        }
    }

    /**
     * Calculates the total salaries for each department.
     */
    public void calculateDepartmentTotals() {
        for (Employee employee : employees) {
            DepartmentTotal departmentTotal = departmentTotals.stream()
                .filter(dt -> dt.getDepartmentName().equals(employee.getDepartment()))
                .findFirst()
                .orElseGet(() -> {
                    DepartmentTotal newDeptTotal = new DepartmentTotal(employee.getDepartment());
                    departmentTotals.add(newDeptTotal);
                    return newDeptTotal;
                });

            departmentTotal.setTotalSalary(departmentTotal.getTotalSalary() + employee.getNetSalary());
        }
    }

    /**
     * Displays the employee payroll information.
     */
    public void displayEmployees() {
        System.out.println("Employee Payroll Information");
        System.out.println("-----------------------------");
        for (Employee employee : employees) {
            System.out.println("Employee ID: " + employee.getId());
            System.out.println("Name: " + employee.getName());
            System.out.println("Department: " + employee.getDepartment());
            System.out.println("Gross Salary: $" + employee.getGrossSalary());
            System.out.println("Bonus: $" + employee.getBonus());
            System.out.println("Deductions: $" + employee.getDeductions());
            System.out.println("Tax Deduction: $" + employee.getTaxDeduction());
            System.out.println("Net Salary: $" + employee.getNetSalary());
            System.out.println("-----------------------------");
        }
    }

    /**
     * Displays the department salary totals.
     */
    public void displayDepartmentTotals() {
        System.out.println("Department Salary Totals");
        System.out.println("-----------------------------");
        for (DepartmentTotal departmentTotal : departmentTotals) {
            System.out.println("Department: " + departmentTotal.getDepartmentName());
            System.out.println("Total Salary: $" + departmentTotal.getTotalSalary());
            System.out.println("-----------------------------");
        }
    }

    /**
     * The main method to run the payroll system.
     *
     * @param args command-line arguments
     */
    public static void main(String[] args) {
        PayrollSystem payrollSystem = new PayrollSystem();
        payrollSystem.initializeEmployees();
        payrollSystem.sortEmployees();
        payrollSystem.calculateNetSalaries();
        payrollSystem.calculateDepartmentTotals();
        payrollSystem.displayEmployees();
        payrollSystem.displayDepartmentTotals();
    }
}
