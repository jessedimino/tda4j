package org.appliedtopology.tda4j

import org.specs2.mutable

import scala.math.Numeric.DoubleIsFractional

import scala.util.Random
//random seed
val r = new Random(42)


class APISpec extends mutable.Specification {
  """Test case class for developing the non-Scala facing API functionality
    |and the non-expert API functionality""".stripMargin

  //here we just define some given and the context in which to do the computations
  given (Double is Field) = Field.DoubleApproximated(1e-25)
  given ctx: TDAContext[Int, Double, Double]()
  import ctx.{given, *}

  //There's two different contexts that we can use for the persistent homology computations
  //This one uses Kruskal's algorithm as a way to build up the homology dimension by dimension
  given shc: SimplicialHomologyByDimensionContext[Int, Double] = SimplicialHomologyByDimensionContext[Int,Double]()
  //This is the standard homology computation
  //given shc: SimplicialHomologyContext[Int, Double, Double] = SimplicialHomologyContext[Int,Double,Double]()

  //this was originally here, we can worry about the chain computations later
  /*
  "we should be able to create and compute with chains" >> {
    1.0 ⊠ ∆(1, 2) - ∆(2, 3) must beEqualTo(
      Chain(Simplex(1, 2) -> 1.0, Simplex(2, 3) -> -1.0)
    )
  }
  */


    //define some set of points on the circle as a test case
    val as = (1 to 5).map(_ => r.nextDouble * 2.0 * math.Pi)
    val xys = as.toSeq.map(a => Seq(math.cos(a), math.sin(a)))
    //println(as)
    //println(xys)




  //once we have our points, we encode them in a metric space so that we can compute distances for filtration
    // The [Int] is so that we can call the points as indexes 0,1,2,...
  val metricSpace: FiniteMetricSpace[Int] = EuclideanMetricSpace(xys)

  println("Distance Matrix")
  IntMetricSpace[Int](metricSpace).distance_matrix.foreach(println)


  //Once we have the metric space set up, and the appropriate vertex type, we can start creating the vietoris rips stream
  //We can specify the maximum number of dimensions that we want to consider sequentially
  //So having maxDimension = 3 means that we enumerate all of the dimension 0,1,2 simplexes down the stream
  val vrstream = NaiveVietorisRipsSimplexStream(metricSpace, maxDimension = 3)



  //Once we have the stream initialized, we can pass it to the PeristentHomology method to actually do all of the neat homology computations
  //We can also specify the maxDimension here, though this might be vestigial already
  val homology = shc.persistentHomology(vrstream, maxDimension = Some(3))


  //The big issue that I can see is that, occasionally components will have a death time before their birth time
  //I've also tested and this problem persists in both homology contexts
  //A lot of debugging work still needs to be done
  homology.advanceTo(2,2.0)
  println("Cycles Born By")
  println(homology.cyclesBornBy)
  println("Barcode")
  //println(homology.diagramAt(2.0))
  println(homology.barcode)

}
