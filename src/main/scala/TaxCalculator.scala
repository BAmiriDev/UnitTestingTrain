class TaxCalculator {

  // Tax bands (simplified to make testing a bit easier)
  private val personalAllowance: Int = 10000
  private val basicRateLimit: Int = 50000
  private val higherRateLimit: Int = 125000
  private val capitalGainsTaxAllowance: Int = 3000

  // Tax rates
  private val personalAllowanceRate: Double = 0
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45
  private val capitalGainsLowerTaxRate: Double = 0.1
  private val capitalGainsHigherTaxRate: Double = 0.2

  // A method to calculate the total amount of tax to be paid, returned as a double
  def calculateTax(income: Double): Double = {
    if (income <= personalAllowance) {
      personalAllowanceRate
    } else if (income <= basicRateLimit) {
      income * basicRate
    } else if (income <= higherRateLimit) {
      income * higherRate
    } else {
      income * additionalRate
    }
  }

  // A method which can tell you if someone is a higher rate taxpayer
  def isHigherRateTaxpayer(income: Double): Boolean = {
    income > basicRateLimit && income <= higherRateLimit
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {
    if (income <= personalAllowance) {
      "£10,000"
    } else if (income <= basicRateLimit) {
      "£50,000"
    } else if (income <= higherRateLimit) {
      "£125,000"
    } else {
      "No limit"
    }
  }


  // A method to calculate the capital gains tax, returned as a double
  def calculateCapitalGainsTax(capitalGains: Double, income: Double): Double = {
    if (capitalGains < capitalGainsTaxAllowance) {
      0
    } else if (income < basicRateLimit) {
      capitalGains * capitalGainsLowerTaxRate
    } else {
      capitalGains * capitalGainsHigherTaxRate
    }
  }

  // method to calculate total tax
  def calculateTotalTax(income: Double, capitalGains: Double): Double = {
    calculateTax(income) + calculateCapitalGainsTax(capitalGains, income)
  }


}