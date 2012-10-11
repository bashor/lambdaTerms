object LambdaTerms {
  sealed trait Term

  case class Appl(fun: Term, arg: Term) extends Term {
    override def toString = s"($fun $arg)"
  }

  case class Abst(v: Var, expr: Term) extends Term {
    override def toString = s"(\\$v.$expr)"
  }

  case class Var(name: String) extends Term {
    override def toString = name
  }

  object Vars {
    var index = 0

    def createNew() = {
      index += 1
      Var(s"v${index - 1}")
    }
  }

  def reduce(term: Term): Term = reduce(replaceBoundedVarsWithNewNames(term), Map.empty)

  private def reduce(term: Term, substitutions: Map[Var, Term]): Term = term match {
    case Appl(func, arg) => reduce(func, substitutions) match {
      case Abst(v, expr) => reduce(expr, substitutions + (v -> arg))
      case f => Appl(f, reduce(arg, substitutions))
    }
    case Abst(v, expr) => Abst(v, reduce(expr, substitutions))
    case v: Var => if (substitutions.contains(v))
      reduce(replaceBoundedVarsWithNewNames(substitutions(v)), substitutions - v) else v
  }

  private def replaceBoundedVarsWithNewNames(term: Term, nameBinding: Map[Var, Var] = Map.empty) : Term = term match {
    case Appl(fun, arg) =>
      Appl(replaceBoundedVarsWithNewNames(fun, nameBinding), replaceBoundedVarsWithNewNames(arg, nameBinding))
    case Abst(v, expr) => {
      val newVar = Vars.createNew()
      Abst(newVar, replaceBoundedVarsWithNewNames(expr, nameBinding + (v -> newVar)))
    }
    case v: Var => if (nameBinding.contains(v)) nameBinding(v) else v
  }
}
