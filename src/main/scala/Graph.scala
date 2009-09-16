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

/**
 * Basic type definitions
 *
 * @author Nicolas Trangez
 */
object Types {
    /**
     * Identifier type of a node in a graph
     */
    type Node = Int
    /**
     * Definition type of adjecent nodes
     *
     * Adjecent nodes define edges in the graph by providing the identifier of
     * the node pointed to, and an edge label. Labels can be set to Unit when
     * not used.
     */
    type Adj[+B] = Seq[Tuple2[B, Node]]
    /**
     * Definition of a graph context type
     *
     * A graph is built inductively using contexts.
     */
    type Context[+A, +B] = Tuple4[Adj[B], Node, A, Adj[B]]
}
import Types._

/**
 * Common trait of all graphs
 *
 * @author Nicolas Trangez
 */
trait BaseGraph[+A, +B] {
    /**
     * Constructor for inductive graphs
     *
     * Graphs can be constructed by prepending Context elements to the Empty
     * graph, like this:
     *
     * <code><pre>
     * val graph = (("left", 2) :: ("up", 3) :: Nil, 1, 'a',
     *              ("right", 2) :: Nil) &amp;:
     *             (Nil, 2, 'b', ("down", 3) :: Nil) &amp;:
     *             (Nil, 3, 'c', Nil) &amp;:
     *             Empty
     * </pre></code>
     *
     * @see Context
     * @see Empty
     */
    def &:[AA >: A, BB >: B](context: Context[AA, BB]) = {
        val nodes = Funs.nodes(this)

        (context._1 ++ context._4) foreach(
            (node: Tuple2[BB, Node]) => require(nodes contains (node._2)))

        new Graph[AA, BB](context, this)
    }
}

/**
 * Empty graph representation
 *
 * This object is the tail graph of every constructed graph.
 *
 * @author Nicolas Trangez
 */
case object Empty extends BaseGraph[Nothing, Nothing]

/**
 * Non-empty graph representation
 *
 * Every graph defined inductively on top of Empty will be of this type.
 *
 * @see    Empty
 *
 * @author Nicolas Trangez
 */
 case class Graph[+A, +B](context: Context[A, B], graph: BaseGraph[A, B])
    extends BaseGraph[A, B]

/**
 * Functional implementations of several graph algorithms based on
 * inductively-defined graphs.
 *
 * @author Nicolas Trangez
 */
object Funs {
    /**
     * Empty check for graphs
     *
     * @param  graph graph to check
     * @return       <code>true</code> if the graph is empty; <code>false</code>
     *               otherwise.
     */
    def isEmpty(graph: BaseGraph[_, _]): Boolean = graph match {
        case Empty => true;
        case _ => false;
    }

    /**
     * Map a given function on all contexts in a graph
     *
     * @param f      function to apply on all contexts to form a new graph
     * @param graph  graph to modify
     *
     * @return       new graph constructed by applying <code>f</code> on all
     *               contexts in <code>graph</code>
     */
    def gmap[A, B, C, D](f: (Context[A, B]) => Context[C, D])(graph: BaseGraph[A, B]): BaseGraph[C, D] =
        graph match {
            case Empty => Empty.asInstanceOf[BaseGraph[C, D]];
            case g: Graph[_, _] => f(g.context) &: gmap(f)(g.graph)
        }

    /**
     * Reverse all edges in a given graph
     *
     * @param graph  graph to recreate with reversed edges
     *
     * @return       new graph formed by reversing all edges in
     *               <code>graph</code>
     */
    def grev[A, B](graph: BaseGraph[A, B]) = 
        gmap((ctx: Context[_, _]) => (ctx._4, ctx._2, ctx._3, ctx._1))(graph)

    /**
     * Calculate a value by folding all nodes in a graph
     *
     * @param fun    fold function
     * @param arg    initial input value to fun
     * @param graph  graph to fold
     *
     * @return       calculated value
     */
    def ufold[A, B, C](fun: (Context[A, B]) => (C) => C)(arg: C)(graph: BaseGraph[A, B]): C =
        graph match {
            case Empty => arg;
            case g: Graph[_, _] => fun(g.context)(ufold(fun)(arg)(g.graph))
        }

    /**
     * List identifiers of all nodes in a graph
     *
     * @param graph  graph to list nodes from
     *
     * @return       list of all node identifiers in the graph
     */
    def nodes[A, B](graph: BaseGraph[A, B]) =
        ufold((ctx: Context[A, B]) => (acc: List[Node]) => ctx._2 :: acc)(Nil)(graph)

    /**
     * Create an undirected graph based on a given graph by adding the inverse
     * of all edges
     *
     * @param graph  graph to undirect
     *
     * @return       undirected graph
     */
    def undir[A, B](graph: BaseGraph[A, B]): BaseGraph[A, B] = {
        def combine(p: Adj[B], s: Adj[B]): Adj[B] = Set((p ++ s) :_*) toList
        def helper(ctx: Context[A, B]): Context[A, B] = {
            val adj = combine(ctx._1, ctx._4)
            (adj, ctx._2, ctx._3, adj)
        }

        gmap(helper)(graph)
    }
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
