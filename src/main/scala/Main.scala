
//import CSVParser.{withHeader, withoutHeader}
import sun.reflect.generics.tree.ReturnType

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source



object Main extends App{
  CSVParser.parseWithHeader(Source.fromFile("biostats.csv").mkString, CSVParser.Config.default) match {
    case Right(list) => print(list.captions + "\n" + list.csv.rows.mkString("\n"));
      println(list.getByName(1,"Name"));
      println(list.captions.get("Name"))
    case Left(error) => print(error)
  }

}

