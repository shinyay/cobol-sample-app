/**
 * Represents the total salary for a department with attributes like department name and total salary.
 */
public class DepartmentTotal {
    // Attributes
    private String departmentName;
    private double totalSalary;

    /**
     * Initializes the department with the given name and sets the total salary to 0.
     *
     * @param departmentName the name of the department
     */
    public DepartmentTotal(String departmentName) {
        this.departmentName = departmentName;
        this.totalSalary = 0;
    }

    // Getters and setters for all attributes

    public String getDepartmentName() {
        return departmentName;
    }

    public void setDepartmentName(String departmentName) {
        this.departmentName = departmentName;
    }

    public double getTotalSalary() {
        return totalSalary;
    }

    public void setTotalSalary(double totalSalary) {
        this.totalSalary = totalSalary;
    }
}
