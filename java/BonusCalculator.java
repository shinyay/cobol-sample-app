/**
 * Encapsulates the logic for calculating bonuses based on gross salary.
 */
public class BonusCalculator {
    // Constants
    private static final double BONUS_RATE = 0.10;

    /**
     * Calculates the bonus based on the given gross salary.
     *
     * @param grossSalary the gross salary of the employee
     * @return the calculated bonus
     */
    public double calculateBonus(double grossSalary) {
        return grossSalary * BONUS_RATE;
    }
}
