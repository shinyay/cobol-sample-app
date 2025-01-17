/**
 * Represents an employee with attributes like ID, name, department, gross salary, bonus, deductions, net salary, and tax deduction.
 */
public class Employee {
    // Attributes
    private String id;
    private String name;
    private String department;
    private double grossSalary;
    private double bonus;
    private double deductions;
    private double netSalary;
    private double taxDeduction;

    /**
     * Initializes the employee with the given ID, name, department, and gross salary.
     * Other attributes are initialized to default values.
     *
     * @param id          the employee's ID
     * @param name        the employee's name
     * @param department  the department the employee belongs to
     * @param grossSalary the employee's gross salary
     */
    public Employee(String id, String name, String department, double grossSalary) {
        this.id = id;
        this.name = name;
        this.department = department;
        this.grossSalary = grossSalary;
        this.bonus = 0.0;
        this.deductions = 0.0;
        this.netSalary = 0.0;
        this.taxDeduction = 0.0;
    }

    // Getters and setters for all attributes

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDepartment() {
        return department;
    }

    public void setDepartment(String department) {
        this.department = department;
    }

    public double getGrossSalary() {
        return grossSalary;
    }

    public void setGrossSalary(double grossSalary) {
        this.grossSalary = grossSalary;
    }

    public double getBonus() {
        return bonus;
    }

    public void setBonus(double bonus) {
        this.bonus = bonus;
    }

    public double getDeductions() {
        return deductions;
    }

    public void setDeductions(double deductions) {
        this.deductions = deductions;
    }

    public double getNetSalary() {
        return netSalary;
    }

    public void setNetSalary(double netSalary) {
        this.netSalary = netSalary;
    }

    public double getTaxDeduction() {
        return taxDeduction;
    }

    public void setTaxDeduction(double taxDeduction) {
        this.taxDeduction = taxDeduction;
    }
}
