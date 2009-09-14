/*
 *   ScalaGraphs, a functional graph library for Scala
 *
 *   Based on the work by Martin Erwig, see
 *       http://web.engr.oregonstate.edu/~erwig/fgl/
 *
 *   Copyright (C) 2009 Nicolas Trangez  <eikke eikke com>
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License as published by the Free Software Foundation, version 2.1
 *   of the License.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *   MA  02110-1301  USA
 */

package com.eikke.scalagraphs

object Types {
    type Node = Int
    type Adj[B] = Seq[Tuple2[B, Node]]
    type Context[A, B] = Tuple4[Adj[B], Node, A, Adj[B]]
}
import Types._

trait BaseGraph[A, B] {
    def &:(context: Context[A, B]) = new Graph[A, B](context, this)

    val isEmpty: Boolean

    def context: Context[A, B]
    def graph: BaseGraph[A, B]
}

case object Empty extends BaseGraph[Any, Any] {
    val isEmpty = true

    def context = throw new NoSuchFieldException
    def graph = throw new NoSuchFieldException
}

case class Graph[A, B](context: Context[A, B], graph: BaseGraph[A, B])
    extends BaseGraph[A, B] {
    val isEmpty = false
}

object Funs {
    def isEmpty(graph: BaseGraph[_, _]) = graph.isEmpty

    def gmap[A, B, C, D](f: (Context[A, B]) => Context[C, D])(graph: BaseGraph[A, B]): BaseGraph[C, D] =
        graph match {
            case Empty => Empty.asInstanceOf[BaseGraph[C, D]];
            case _ => {
                val (ctx, rest) = (graph.context, graph.graph)
                f(ctx) &: gmap(f)(rest)
            }
        }

    def grev[A, B](graph: BaseGraph[A, B]) = 
        gmap((ctx: Context[_, _]) => (ctx._4, ctx._2, ctx._3, ctx._1))(graph)

    def ufold[A, B, C](fun: (Context[A, B]) => (C) => C)(arg: C)(graph: BaseGraph[A, B]): C =
        graph match {
            case _ if graph eq Empty => arg;
            case _ => {
                val (ctx, rest) = (graph.context, graph.graph)
                fun(ctx)(ufold(fun)(arg)(rest))
            }
        }

    def nodes[A, B](graph: BaseGraph[A, B]) =
        ufold((ctx: Context[A, B]) => (acc: List[Node]) => ctx._2 :: acc)(Nil)(graph)
}

import Funs._

object Test {
    def main(args: Array[String]): Unit = {
       println("Empty graph is empty: " + isEmpty(Empty))
       println("")
       println("")

       // Definition of the example graph in the paper
       val graph = (("left", 2) :: ("up", 3) :: Nil, 1, 'a',
                    ("right", 2) :: Nil) &:
                   (Nil, 2, 'b', ("down", 3) :: Nil) &:
                   (Nil, 3, 'c', Nil) &:
                   Empty

       println("Graph: " + graph)
       println("")
       println("Is it empty? " + isEmpty(graph))
       println("")

       println("Reversed: " + grev(graph))
       println("")

       println("All nodes:")
       nodes(graph) foreach(println(_: Node))
   }
}
