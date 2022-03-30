package u05lab.ex1

import org.junit.Assert.assertEquals
import org.junit.Test

class ListTest {

  private val l = List(1, 2, 3, 4)

  @Test
  def testZipRight(): Unit =
    assertEquals(l.zipRight, List((1, 0), (2, 1), (3, 2), (4, 3)))

  @Test
  def testPartition(): Unit =
    assertEquals(l.partition(_ % 2 == 0), (List(2, 4), List(1, 3)))

  @Test
  def testSpan(): Unit =
    assertEquals(l.span(_ % 2 != 0), (List(1), List(2, 3, 4)))
    assertEquals(l.span(_ < 3), (List(1, 2), List(3, 4)))

}
