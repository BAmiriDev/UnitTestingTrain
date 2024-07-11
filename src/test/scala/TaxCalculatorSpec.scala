import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  val taxCalculator: TaxCalculator = new TaxCalculator

  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the income is below the personal tax limit" in {
        val result: Double = taxCalculator.calculateTax(5000)
        assert(result == 0)
      }
      "the income is in the range of basic tax limit" in {
        val income = 12000
        val basicTaxRate = 0.2
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == income * basicTaxRate)
      }
      "the income is in the range of higher tax limit" in {
        val income = 70000
        val higherTaxRate = 0.4
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == income * higherTaxRate)
      }
      "the income is in the range of additional tax limit" in {
        val income = 150000
        val additionalTaxRate = 0.45
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == income * additionalTaxRate)
      }
    }
  }

  "TaxCalculator.isHigherRateTaxpayer" should {
    "return true" when {
      "the income is above the higher rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(100000)
        assert(result)
      }
    }

    "return false" when {
      "the income is below or more the higher rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(40000)
        assert(!result)
      }
    }
  }

  "TaxCalculator.formattedCurrentTaxAllowance" should {
    "return the correct formatted string" when {
      "the income is below the personal tax limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(5000)
        assert(result == "£10,000")
      }

      "the income is within the basic rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(20000)
        assert(result == "£50,000")
      }

      "the income is within the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(120000)
        assert(result == "£125,000")
      }

      "the income is above the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(130000)
        assert(result == "No limit")
      }
    }
  }
  "TaxCalculator.calculateCapitalGainsTax" should {
    "return the total tax on capital gains" when {
      "capital gains are below the allowance" in {
        val result: Double = taxCalculator.calculateCapitalGainsTax(2000, 20000)
        assert(result == 0)
      }
      "income is bellow basic rate" in {
        val capitalGains = 4000
        val income = 20000
        val taxRate = 0.1
        val result: Double = taxCalculator.calculateCapitalGainsTax(capitalGains, income)
        assert(result == capitalGains * taxRate)
      }
      "income is above basic rate" in {
        val capitalGains = 4000
        val income = 60000
        val taxRate = 0.2
        val result: Double = taxCalculator.calculateCapitalGainsTax(capitalGains, income)
        assert(result == capitalGains * taxRate)
      }
    }
  }
  "TaxCalculator.calculateTotalTax" should {
    "calculate total income tax AND capital gains tax from shares" when {
      "the income is below the personal tax limit and below personal capital gains allowance" in {
        val income = 20000
        val capitalGains = 5000
        val expectedIncomeTax = taxCalculator.calculateTax(income)
        val expectedCapitalGainsTax = taxCalculator.calculateCapitalGainsTax(capitalGains, income)
        val expectedTotalTax = expectedIncomeTax + expectedCapitalGainsTax
        val result: Double = taxCalculator.calculateTotalTax(income, capitalGains)
        assert(result == expectedTotalTax)
      }


    }
  }
}
