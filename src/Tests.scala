import org.junit.Test

import LambdaTerms.Appl
import LambdaTerms.Abst
import LambdaTerms.Var
import LambdaTerms.Term
import LambdaTerms.reduce

class Tests {
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")
  val n = Var("n")
  val m = Var("m")
  val s = Var("s")

  val id = Abst(x, x)

  @Test
  def testId() {
    doTest(Appl(id, y), y)
  }

  @Test
  def testIdWithTheSameName() {
    doTest(Appl(id, x), x)
  }

  @Test
  def testApplication() {
    val yx = Appl(Appl(Abst(y, Abst(x, Appl(x, y))), x), y)
    val xy = Appl(Appl(Abst(x, Abst(y, yx)), y), x)
    doTest(xy, Appl(x, y))
  }

  val s_comb = Abst(x, Abst(y, Abst(z, Appl(Appl(x, z), Appl(y, z)))))
  val k_comb = Abst(x, Abst(y, x))

  @Test
  def testSKK() {
    val skk = Appl(Appl(s_comb, k_comb), k_comb)
    doTest(skk, id)
  }

  @Test
  def testSKxy() {
    val skxy = Appl(Appl(Appl(s_comb, k_comb), x), y)
    doTest(skxy, y)
  }

  val true_  = Abst(x, Abst(y, x))
  val false_ = Abst(x, Abst(y, y))
  val and = Abst(m, Abst(n, Appl(Appl(m,  n), m)))
  val not = Abst(m, Abst(x, Abst(y, Appl(Appl(m, y), x))))

  @Test
  def testNot() {
    doTest(Appl(not, false_), true_)
    doTest(Appl(not, true_), false_)
  }

  @Test
  def testAnd() {
    doTest(Appl(Appl(and, true_), false_), false_)
  }

  val succ = Abst(n, Abst(s, Abst(x, Appl(s, Appl(Appl(n, s), x)))))
  val plus = Abst(m, Abst(n, Abst(s, Abst(x, Appl(Appl(m, s), (Appl(Appl(n, s), x)))))))
  val mult = Abst(m, Abst(n, Abst(s, Appl(m, Appl(n, s)))))
  val exp = Abst(m, Abst(n, Appl(n, m)))

  def num(n: Int) = {
    def nTimesAppl(n: Int): Term = if (n == 0) x else Appl(s, nTimesAppl(n - 1))
    Abst(s, Abst(x, nTimesAppl(n)))
  }

  @Test
  def testSucc() {
    doTest(Appl(succ, num(13)), num(14))
  }

  @Test
  def testPlus() {
    doTest(Appl(Appl(plus, num(1)), num(2)), num(3))
  }

  @Test
  def testMult() {
    doTest(Appl(Appl(mult, num(3)), num(4)), num(12))
  }

  @Test
  def testExp() {
    doTest(Appl(Appl(exp, num(2)), num(3)), num(8))
  }

  val zero = Abst(s, Abst(x, x))
  def numIter(n: Int) : Term = if (n == 0) zero else Appl(succ, numIter(n - 1))

  @Test
  def testNumIter() {
    doTest(numIter(42), num(42))
  }

  def doTest(term: Term, expectedReduction: Term) {
    val reducedTerm: Term = reduce(term)
    assert(equalTerms(reducedTerm, expectedReduction), s"Terms are not equal:\n$reducedTerm\n$expectedReduction")
  }

  def equalTerms(t: Term, s: Term, equalVariables: Map[Var, Var] = Map.empty) : Boolean = {
    t match {
      case Appl(t_func, t_arg) => s match {
        case Appl(s_func, s_arg) =>
          equalTerms(t_func, s_func, equalVariables) && equalTerms(t_arg, s_arg, equalVariables)
        case _ => false
      }
      case Abst(t_var, t_expr) => s match {
        case Abst(s_var, s_expr) =>
          equalTerms(t_expr, s_expr, equalVariables + (t_var -> s_var))
        case _ => false
      }
      case t_var: Var => s match {
        case s_var: Var => t_var == s_var || equalVariables.get(t_var) == Some(s_var)
        case _ => false
      }
    }
  }
}
