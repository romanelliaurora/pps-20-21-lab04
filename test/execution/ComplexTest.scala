package execution

import org.junit.jupiter.api.Assertions.{assertEquals, assertTrue}
import org.junit.jupiter.api.Test
import u04lab.code.Complex

class ComplexTest {
  @Test
  def testIncremental() {
    val a: Array[Complex]= Array(Complex(10,20), Complex(1,1), Complex(7,0))

    //val c = a(0) + a(1) + a(2)
    assertEquals(Complex(18.0, 21.0),  a(0) + a(1) + a(2));
    assertEquals(Complex(-10.0,30.0),  a(0) * a(1));
    assertEquals(Complex(-10.0,30.0), Complex(-10.0,30.0));
    assertTrue(a.eq(a));
    assertTrue(Complex(-10.0,30.0).equals(Complex(-10.0,30.0)));



  }

}
