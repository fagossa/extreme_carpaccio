import models.{OrderResponse, Orders}
import org.scalatest.{FunSpec, Matchers}

class OrderSpec extends FunSpec with Matchers {

  describe("order posting") {

    it("should match basic result2") {
      val orders = Orders(
        List(4.1, 8.03, 86.83, 65.62, 44.82),
        List(10, 3, 5, 4, 5), "AT", models.Standard)
      orders.calculateResponse shouldBe Some(OrderResponse(1166.62))
    }

    it("should match basic result") {
      val orders = Orders(
        List(56.6, 18.38, 42.14, 99.62, 8.1, 78.73, 74.49, 15.22),
        List(4, 3, 8, 1, 7, 3, 10, 5), "PL", models.Standard)
      orders.calculateResponse shouldBe Some(OrderResponse(2150.42))
    }

    it("should match basic result3") {
      val orders = Orders(
        List(91.41, 12.3, 38.64, 97.14, 26.25, 44.42, 11.53, 59.07, 31.82),
        List(7, 4, 4, 6, 2, 9, 9, 6, 1), "DE", models.Standard)
      orders.calculateResponse shouldBe Some(OrderResponse(2757.24))
    }

    it("should match basic result4") {
      val orders = Orders(
        List(72.95, 38.44, 42.62, 30.31, 21.6, 6.07, 81.99, 35.8, 86.44),
        List(2, 4, 4, 5, 1, 6, 5, 5, 4), "ES", models.Standard)
      orders.calculateResponse shouldBe Some(OrderResponse(1863.53))
    }

  }
}