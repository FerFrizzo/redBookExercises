val rng = scala.util.Random

rng.nextDouble()

rng.nextDouble()

rng.nextInt
rng.nextInt(10)

def rollDie = {
  val rng = scala.util.Random
  rng.nextInt(6)
}

trait RNG {
  def nextInt:(Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    ( if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    val (i2, _) = rng.nextInt

    (if (i > i2) (i2 / i).toDouble else (i / i2).toDouble, r)

  }

  def double2(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt2(rng)
    ((i / (Int.MaxValue + 1)).toDouble, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt2(rng)
    val (d, r2) = double2(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double2(rng)
    val (i, r2) = nonNegativeInt2(r)
    ((d, i), r2)
  }

  def doubleInt2(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double2(rng)
    val (d2, r2) = double2(r)
    val (d3, r3) = double2(r2)
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(), rng)
    else {
      val (i, r) = rng.nextInt
      val (i2, r2) = ints(count - 1) (r)
      (i :: i2, r2)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(c: Int, r: RNG, l: List[Int]): (List[Int], RNG) =
      if (c == 0) (l, r)
      else {
        val (i, r1) = r.nextInt
        go(c - 1, r1, i :: l )
      }
    go(count, rng, List.empty[Int])

  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt2)(i => i - i % 2)

  def doubleMapped: Rand[Double] =
    map(nonNegativeInt2)(i => (i / (Int.MaxValue + 1)).toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double2)

  val randDoubleInt: Rand[(Double, Int)] = both(double2, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def ints3(count: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt2(rng)
    val mod = i % n
    if (i + (n-1) - mod > 0)
      (mod, rng2)
    else
      nonNegativeLessThan(n)(rng)
  }
//    map(nonNegativeInt2){ i =>
//      val mod = i % n
//      if (i + (n - 1) - mod >= 0) mod
//      else nonNegativeLessThan(n)
//    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] =
    flatMap(nonNegativeInt2){ i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan2(n)
    }

  def map3[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map4[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ i =>
      flatMap(rb) { s =>
        unit(f(i, s))
      }
    }

  def map5[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map3(rb)(b => f(a, b)) )

  val zero = rollDie(Simple(5))._1

  def rollDie: Rand[Int] = map(nonNegativeLessThan2(6))(_ + 1)

}

import State._

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State( s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    @scala.annotation.tailrec
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }



}