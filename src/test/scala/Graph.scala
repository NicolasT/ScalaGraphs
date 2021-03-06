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

package com.eikke.scalagraphs.test

import org.scalatest.Spec
import org.scalatest.matchers.{ShouldMatchers,BeMatcher,MatchResult}

import com.eikke.scalagraphs.Funs.{isEmpty,gmap,grev,nodes,undir}
import com.eikke.scalagraphs.Types.Context

class EmptyGraphSpec extends Spec with ShouldMatchers {
    describe("The Empty graph") {
        it("should be empty") {
            assert(isEmpty(Empty))
        }

        it("should return itself on gmap") {
            val new_ = gmap((ctx: Context[Any, Any]) => ctx)(Empty)
            new_ should be theSameInstanceAs (Empty)
        }

        it("should return itself on grev") {
            grev(Empty) should be theSameInstanceAs (Empty)
        }

        it("should have no nodes") {
            nodes(Empty) should be ('empty)
        }
    }
}

class SingleNodeGraphSpec extends Spec with ShouldMatchers {
    describe("A single-node graph") {
        it("should not be empty") {
            val graph = (Nil, 1, 123, Nil) &: Empty
            assert(!isEmpty(graph))
        }

        it("should have Empty as graph") {
            val graph = (Nil, 1, 123, Nil) &: Empty
            graph.graph should be theSameInstanceAs (Empty)
        }

        it("should contain one node") {
            val graph = (Nil, 1, 123, Nil) &: Empty
            nodes(graph) should have length (1)
        }

        it("should have a constant hashcode") {
            val graph1 = (Nil, 1, 123, Nil) &: Empty
            val graph2 = (Nil, 1, 123, Nil) &: Empty

            graph1.hashCode should equal (graph2.hashCode)
        }

        it("should have a different hashcode on different objects") {
            val graph1 = (Nil, 1, 123, Nil) &: Empty
            val graph2 = (Nil, 2, "abc", Nil) &: Empty

            graph1.hashCode should not equal (graph2.hashCode)
        }

        it("should be equal") {
            val graph1 = (Nil, 1, 123, Nil) &: Empty
            val graph2 = (Nil, 1, 123, Nil) &: Empty

            graph1 should equal (graph2)
        }

        it("should not be the same instance") {
            val graph1 = (Nil, 1, 123, Nil) &: Empty
            val graph2 = (Nil, 1, 123, Nil) &: Empty

            graph1 should not be theSameInstanceAs (graph2)
        }

        it("can not have edges") {
            intercept[IllegalArgumentException] {
                val graph = (("test", 2) :: Nil, 1, 123, Nil) &: Empty
                ()
            }
        }
    }
}

class MultiNodeGraphSpec extends Spec with ShouldMatchers {
    describe("A multi-node graph") {
        it("should be instanciatable") {
            val graph = (Nil, 2, 456, Nil) &: (Nil, 1, 123, Nil) &: Empty
            ()
        }

        it("could possibly contain different types") {
            val graph = (Nil, 2, "abc", Nil) &: (Nil, 1, 123, Nil) &: Empty
            ()
        }

        it("should have a correct node count") {
            val graph = (Nil, 3, 789, Nil) &: (Nil, 2, 456, Nil) &:
                        (Nil, 1, 123, Nil) &: Empty
            nodes(graph) should have length (3)
        }

        it("should have correct nodes") {
            val graph = (Nil, 3, 789, Nil) &: (Nil, 2, 456, Nil) &:
                        (Nil, 1, 123, Nil) &: Empty
            Set(nodes(graph) :_*) should equal (Set(1, 2, 3))
        }

        it("should be equal") {
            def gen() = (Nil, 3, 789, Nil) &: (Nil, 2, 456, Nil) &:
                        (Nil, 1, 123, Nil) &: Empty
            val graph1 = gen()
            val graph2 = gen()

            graph1 should equal (graph2)
            graph1.hashCode should equal (graph2.hashCode)
        }

        it("can not link to unknown nodes") {
            intercept[IllegalArgumentException] {
                val graph = (("test", 3) :: Nil, 2, 456, Nil) &:
                    (Nil, 1, 123, Nil) &: Empty
                ()
            }
        }

        it("can contain legal edges") {
            val graph = (("test", 1) :: Nil, 2, 456, Nil) &:
                    (Nil, 1, 123, Nil) &: Empty
            ()
        }
    }
}


class UndirSpec extends Spec with ShouldMatchers {
    describe("The undir function") {
        it("should return Empty on Empty input") {
            undir(Empty) should equal (Empty)
        }

        it("should not modify a single-node graph") {
            val in = (Nil, 1, 123, Nil) &: Empty

            undir(in) should equal (in)
        }

        it("should undir a multi-node graph") {
            val in = (((), 1) :: ((), 2) :: Nil, 3, 789, ((), 1) :: Nil) &:
                     (((), 1) :: Nil, 2, 456, Nil) &:
                     (Nil, 1, 123, Nil) &: Empty

            val out = (((), 1) :: ((), 2) :: Nil, 3, 789,
                        ((), 1) :: ((), 2) :: Nil) &:
                      (((), 1) :: Nil, 2, 456, ((), 1) :: Nil) &:
                      (Nil, 1, 123, Nil) &: Empty

            undir(in) should equal (out)
        }
    }
}

class VarianceSpec extends Spec with ShouldMatchers {
    case class A(i: Int)
    case class B(j: Int) extends A(j)

    class GraphTypeMatcher extends BeMatcher[Graph[A, String]] {
        def apply(left: Graph[A, String]) =
            MatchResult(true, "Invalid type", "Something else")
    }
    val correctType = new GraphTypeMatcher

    describe("A graph") {
        it("should be covariant in A and B") {
            val graph = (("e1", 1) :: Nil, 2, B(456), ("e2", 1) :: Nil) &:
                            (Nil, 1, A(123), Nil) &: Empty

            graph should be (correctType)
        }
    }
}

class PatternMatchSpec extends Spec with ShouldMatchers {
    describe("A graph") {
        it("should match patterns") {
            val graph = (Nil, 2, 456, Nil) &: (Nil, 1, 123, Nil) &: Empty

            graph match {
                case Graph((_, 2, x1, _), g1) => {
                    x1 should equal (456)
                    g1 should be theSameInstanceAs (graph.graph)
                }
                case _ => assert(false)
            }

            graph.graph match {
                case Graph((_, 1, x2, _), g2) => {
                    x2 should equal (123)
                    g2 should be theSameInstanceAs (Empty)
                }
                case _ => assert(false)
            }
        }
    }
}

// vim: set ts=4 sw=4 et:
