var counter = 0
def freshVar(name: String) =
  counter += 1
  s"${name}${counter}"

def error(msg: String) = throw new Exception(msg)

enum Ty:
  case Base(name: String)
  case Fun(t1: Ty, t2: Ty)
  case Meta(name: String = freshVar("t"), var t: Option[Ty] = None)

  override def toString() =
    this.force match
      case Base(name) => name
      case Fun(t1, t2) => s"(${t1.toString} -> ${t2.toString})"
      case Meta(name, None) => s"?${name}"
      case _ => error("Bug: should be unreachable")

  def force: Ty = this match
    case Ty.Meta(_, Some(t1)) => t1.force
    case _ => this

enum Expr:
  case Const(t: Ty)
  case Var(x: String)
  case Lam(x: String, e: Expr)
  case App(e1: Expr, e2: Expr)

def unify(t1: Ty, t2: Ty): Unit =
  (t1.force, t2.force) match
    case (Ty.Base(n1), Ty.Base(n2)) =>
      if n1 != n2 then error(s"Type mismatch: $n1 != $n2")
    case (Ty.Fun(t11, t12), Ty.Fun(t21, t22)) =>
      unify(t11, t21)
      unify(t12, t22)
    case (t1 @ Ty.Meta(_, None), _) => t1.t = Some(t2)
    case (_, t2 @ Ty.Meta(_, None)) => t2.t = Some(t1)
    case (_, _) => error(s"Type mismatch: $t1 != $t2")

def check(env: Map[String, Ty], e: Expr, t: Ty): Unit =
  e match
    case Expr.Const(t1) => unify(t1, t)
    case Expr.Var(x) =>
      env.get(x) match
        case Some(t1) => unify(t1, t)
        case None => error(s"Unbound variable $x")
    case Expr.Lam(x, e1) =>
      val t1 = Ty.Meta()
      val t2 = Ty.Meta()
      unify(t, Ty.Fun(t1, t2))
      check(env + (x -> t1), e1, t2)
    case Expr.App(e1, e2) =>
      val t1 = Ty.Meta()
      check(env, e1, Ty.Fun(t1, t))
      check(env, e2, t1)

def infer(e: Expr): Ty =
  val t = Ty.Meta()
  check(Map.empty, e, t)
  t

def lam(f: Expr => Expr) =
  val x = freshVar("x")
  Expr.Lam(x, f(Expr.Var(x)))

def app(e1: Expr, e2: Expr) = Expr.App(e1, e2)

val intTy = Ty.Base("int")
val n = Expr.Const(intTy)

infer(lam(x => x))
infer(app(lam(x => x), n))
infer(lam(x => lam(y => app(x, y))))
