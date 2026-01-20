package org.appliedtopology.tda4j

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.specs2.{ScalaCheck, Specification, mutable as s2mutable}
import org.specs2.specification.core.Fragment
import org.specs2.execute.Result
import org.specs2.specification.AllExpectations

import scala.annotation.tailrec
import scala.collection.immutable.{Seq, Set}
import scala.collection.mutable
import scala.math.{cos, sin}
import scala.reflect.ClassTag

// see https://stackoverflow.com/questions/52795217/scala-tail-recursive-method-has-an-divide-and-remainder-error/65362753#65362753
def binomialBigint(n: Int, k: Int): BigInt = {
  if (k < 0 || n < k) 0
  else {
    @tailrec
    def binomialtail(nIter: Int, kIter: Int, ac: BigInt): BigInt =
      if (kIter > k) ac
      else binomialtail(nIter + 1, kIter + 1, (nIter * ac) / kIter)

    if (k == 0 || k == n) 1
    else binomialtail(n - k + 1, 1, BigInt(1))
  }
}

def binomial(n: Int, k: Int): Int = binomialBigint(n,k).intValue

def matrixGen[T: ClassTag](g: Gen[T], dimension: Gen[Int], size: Gen[Int]): Gen[Array[Array[T]]] =
  for
    dim <- dimension
    sz <- size
    values <- Gen.listOfN(dim * sz, g)
  yield values.toArray.grouped(dim).toArray

class VietorisRipsSpec extends s2mutable.Specification with ScalaCheck with AllExpectations {
  "This is a specification of the Vietoris-Rips simplex stream implementation\n\n".txt

  val N = 50
  val maxF = 0.75
  val maxD = 3

  given Ordering[Int] = Ordering.Int

  "Vietoris Rips streams should" >> {
    "have sorted layers" >> forAll(matrixGen(Gen.double, Gen.chooseNum(5,10), Gen.chooseNum(15,50))) { (pts) =>
      val metricSpace = EuclideanMetricSpace(pts)
      val vrstream = RecursiveStackVietorisRipsSimplexStream(metricSpace)
      println(s"${pts.size} x ${pts(0).size}")
      var spxseq = vrstream.iterateDimension(0).toSeq
      (spxseq.size === binomial(pts.length, 1))
      spxseq.map(vrstream.filtrationValue) must beSorted
      spxseq = vrstream.iterateDimension(1).toSeq
      (spxseq.size === binomial(pts.length, 2))
      spxseq.map(vrstream.filtrationValue) must beSorted
      spxseq = vrstream.iterateDimension(2).toSeq
      (spxseq.size === binomial(pts.length, 3))
      spxseq.map(vrstream.filtrationValue) must beSorted
    }
  }
}
