val scala3Version = "3.0.0-RC1"

name := "julius-iii"
organization := "nl.gn0s1s"
version := "1.0.2"
startYear := Some(2016)
homepage := Some(url("https://github.com/philippus/julius_iii"))
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

scalaVersion := scala3Version

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.3" % Test
