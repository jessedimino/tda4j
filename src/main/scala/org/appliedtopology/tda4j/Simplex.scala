package org.appliedtopology.tda4j

import scala.collection.mutable
import scala.collection.immutable.SortedSet
import math.Ordering.Implicits.sortedSetOrdering

/** Simplices really are just sets, outright. We provide an implementation of the [OrderedCell] typeclass
 * for simplicial complex structures, to enable their use.
 *
 */

type Simplex[VertexT] = SortedSet[VertexT]
  //simplex is just a sorted set of vertices

extension [VertexT : Ordering](spx : Simplex[VertexT])
  //extension of ordered vertices to simplex
  def show : String = spx.mkString(s"∆(", ",", ")")
    //function to print simplexes

object Simplex:
  //define the simplex object
  def from[VertexT : Ordering, T <: Seq[VertexT]](vertices : T) : Simplex[VertexT] = SortedSet.from(vertices)
    //function to construct simplex from vertices with an ordering, and T which is a subtype of seq[VertexT]
    //returns a simplex by just literally creating the sorted set from the vertices
  def apply[VertexT : Ordering](vertices : VertexT*) : Simplex[VertexT] = from(vertices)
    //apply the ordering to the vertices to get a simplex

/** Convenience method for defining simplices
 *
 * The character ∆ is typed as Alt+J on Mac GB layout, and has unicode code 0x0394.
 */
def ∆[VertexT : Ordering](vertices : VertexT*) : Simplex[VertexT] = Simplex.from(vertices)

def simplexOrdering[VertexT](using vtxOrd : Ordering[VertexT]) : Ordering[Simplex[VertexT]] = sortedSetOrdering(vtxOrd)
  //simplex ordering is the sorted set ordering on the vertex ordering
  //whatever that actually means
def Simplex_is_OrderedCell[VertexT](using vtxOrd : Ordering[VertexT])(setOrdering : Ordering[Simplex[VertexT]] = simplexOrdering(using vtxOrd)): (Simplex[VertexT] is OrderedCell) =
  new(Simplex[VertexT] is OrderedCell) {
    override lazy val ordering = setOrdering
    extension (spx: Simplex[VertexT]) {
      override def dim = spx.size - 1
      override def boundary[CoefficientT: Field as fr] =
        if (spx.dim <= 0) Chain()
        else Chain.from(
          spx.to(Seq)
            .zipWithIndex
            .map((vtx, i) => spx -- spx.slice(i, i + 1))
            .zip(Iterator.unfold(fr.one)(s => Some((s, fr.negate(s)))))
        )
    }
  }
given default_Simplex_is_OrderedCell: [VertexT : Ordering] => (Simplex[VertexT] is OrderedCell) =
  Simplex_is_OrderedCell[VertexT]()
