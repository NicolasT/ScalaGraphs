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
    type Adj[+B] = Seq[(B, Node)]
    /**
     * Definition of a graph context type
     *
     * A graph is built inductively using contexts.
     */
    type Context[+A, +B] = (Adj[B], Node, A, Adj[B])
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
            (node: (BB, Node)) => require(nodes contains (node._2)))

        new Graph[AA, BB](context, this)
    }

    /**
     * Extract one node from a graph, and return the leftover graph
     *
     * If the node can't be found in the graph, the context value will be None,
     * and the original graph will be returned.
     *
     * @param node  node to extract
     *
     * @return      optional context and leftover graph
     *
     * @todo        write tests
     * @todo        find a better method name (<code>&amp;v:</code> would be
     *              ideal)
     */
    def &:(node: Node): (Option[Context[A, B]], BaseGraph[A, B]) = {
        // Implementation
        //
        // 1. Generate a list of all contexts of graph
        // 2. Reverse the list
        // 3. Create a new graph of all contexts, until the element to be looked
        // up is found
        // 4. Add all other elements to the graph, add their outgoing edges to
        // the looked up context to the incoming edges of the looked up context
        // 5. Done

        val contexts = Funs.ufold((ctx: Context[A, B]) => (acc: List[Context[A, B]]) => ctx :: acc)(Nil)(this)
        val reverse_contexts = contexts.reverse
        val (pre, post) = reverse_contexts.span(_._2 != node)

        if(post isEmpty)
            return (None, this)

        val base = ((Empty: BaseGraph[A, B]) /: pre)((graph, context) => context &: graph)

        // TODO Make this code functional/recursive!
        var the_ctx = post.head
        var the_graph = base

        for(ctx <- post.tail) {
            // Create new context without links to node
            val new_ctx = (ctx._1 filter(_._2 != node), ctx._2, ctx._3, ctx._4 filter(_._2 != node))
            the_graph = new_ctx &: the_graph

            val new_ins = ctx._4.filter(_._2 == node).map(n => (n._1, ctx._2))
            val new_outs = ctx._1.filter(_._2 == node).map(n => (n._1, ctx._2))

            the_ctx = (new_ins ++ the_ctx._1, the_ctx._2, the_ctx._3, new_outs ++ the_ctx._4)
            ()
        }

        (Some(the_ctx), the_graph)
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
            case Empty => Empty;
            case Graph(ctx, parent) => f(ctx) &: gmap(f)(parent)
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
        gmap((ctx: Context[A, B]) => (ctx._4, ctx._2, ctx._3, ctx._1))(graph)

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
            case Graph(ctx, parent) => fun(ctx)(ufold(fun)(arg)(parent))
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

    /**
     * Calculate list of successor nodes of a given node in a graph, or None
     * if the node can't be found
     *
     * @param v  node to calculate successors of
     *
     * @return   optional successor node list
     *
     * @todo     write tests
     */
    def gsuc[A, B](v: Node)(graph: BaseGraph[A, B]): Option[Seq[Node]] =
        v &: graph match {
            case (None, _) => None;
            case (Some(ctx), _) => Some(ctx._4 map (_._2))
        }

    /**
     * Calculate list of predecessor nodes of a given node in a graph, or None
     * if the node can't be found
     *
     * @param v  node to calculate predecessors of
     *
     * @return   optional predecessor node list
     *
     * @todo     write tests
     */
    def gpred[A, B](v: Node)(graph: BaseGraph[A, B]): Option[Seq[Node]] =
        v &: graph match {
            case (None, _) => None;
            case (Some(ctx), _) => Some(ctx._1 map (_._2))
        }


    /**
     * Calculate the degree of a node, or None if the node can't be found
     *
     * @param v  node to calculate degree of
     *
     * @return   optional degree
     *
     * @todo     write tests
     */
    def deg[A, B](v: Node)(graph: BaseGraph[A, B]): Option[Int] =
        v &: graph match {
            case (None, _) => None;
            case (Some(ctx), _) => Some(ctx._1.length + ctx._4.length)
        }

    /**
     * Delete one node from a graph
     *
     * @param v      node to delete
     * @param graph  graph to delete node from
     *
     * @return       graph without given node
     *
     * @todo         write tests
     */
    def del[A, B](v: Node)(graph: BaseGraph[A, B]): BaseGraph[A, B] =
        (v &: graph)._2

    /**
     * Calculate a list of all successors of a given context
     *
     * @param ctx  context to retrieve successors from
     *
     * @return     list of successors of the given context
     */
    def suc[A, B](ctx: Context[A, B]) = ctx._4 map(_._2) toList

    /**
     * Calculate a node list of all nodes in a graph in depth-first order
     *
     * This is an implementation of depth-first search.
     *
     * @param nodes  list of starting nodes
     * @param graph  graph to traverse
     *
     * @return       list of nodes in DFS order
     *
     * @todo         write tests
     */
    def dfs[A, B](nodes: List[Node])(graph: BaseGraph[A, B]): List[Node] = {
        if(nodes isEmpty)
            return Nil
        if(isEmpty(graph))
            return Nil

        nodes.head &: graph match {
            case (None, g) => dfs(nodes tail)(g);
            case (Some(ctx), g) => nodes.head :: dfs(suc(ctx) ::: nodes.tail)(g)
        }
    }

    import java.io.OutputStream
    /**
     * Dump a graph structure as a Graphviz dot file
     *
     * This is only of proof-of-concept quality
     *
     * @param graph  graph to dump
     * @param out    outputstream to write dot data to
     *
     * @todo         write tests
     * @todo         cleanup, enhance implementation
     */
    def write[A, B](graph: BaseGraph[A, B])(out: OutputStream): Unit = {
        def writeNextContext(g: BaseGraph[A, B]): Unit =
            g match {
                case Empty => ();
                case Graph(ctx, parent) => {
                    out.write(("N" + ctx._2 + " [label=\"(" + ctx._2 + " :: ").getBytes)
                    out.write((ctx._3 + ")\"]\n").getBytes)
                    for((l, n) <- ctx._1) {
                        out.write(("N" + n + " -> N" + ctx._2).getBytes)
                        if(l != ())
                            out.write((" [label=\"" + l + "\"]").getBytes)
                        out.write("\n".getBytes)
                    }
                    for((l, n) <- ctx._4) {
                        out.write(("N" + ctx._2 + " -> N" + n).getBytes)
                        if(l != ())
                            out.write((" [label=\"" + l + "\"]").getBytes)
                        out.write("\n".getBytes)
                    }

                    writeNextContext(parent)
                }
            }

        out.write("digraph {\n".getBytes)
        writeNextContext(graph)
        out.write("}\n".getBytes)
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

       val g1 = (Nil, 1, 123, Nil) &: Empty
       val g2 = (Nil, 2, 456, ((), 1) :: Nil) &: g1
       val g3 = (("a", 2) :: ("b", 1) :: Nil, 3, 789, Nil) &: g2
       val g4 = (((), 3) :: ((), 2) :: Nil, 4, 012,
         ((), 1) :: ("c", 2) :: ("d", 3) :: Nil) &: g3

       import java.io.FileOutputStream
       val fp = new FileOutputStream("testout.dot")
       write(g4)(fp)
       fp.close()
   }
}
