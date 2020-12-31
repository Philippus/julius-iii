package nl.gn0s1s.julius

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

trait ExtendedList:
  extension [A](source: List[A])
    private def replaceSlice(target: List[A], replacement: List[A]): List[A] =
      if source.containsSlice(target) && target != replacement && target.nonEmpty then
        val (left, right) = source.splitAt(source.indexOfSlice(target))
        left ::: replacement ::: right.drop(target.length).replaceSlice(target, replacement)
      else source

    @tailrec final def substitute(substitutes: ListMap[List[A], List[A]]): List[A] =
      if substitutes.isEmpty then source
      else source.replaceSlice(substitutes.head._1, substitutes.head._2).substitute(substitutes.tail)
end ExtendedList
