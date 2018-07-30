package mitt

import TreeManager._

object Main extends App {

    val test0a = """However,
    |xyz
    |there is a worst-case situation where there are no nodes in common
    |a""".stripMargin

    val test0b = """Nonetheless,
    |abc
    |there is a worst-case situation where there are no nodes in common
    |z""".stripMargin

    val x = buildTree(test0a)
    val y = buildTree(test0b)
    printTree(x)
    printTree(y)
}