package mitt

import org.scalatest._
import TreeManager._

class TreeManagerSpec extends FlatSpec with Matchers {

    val test0 = """However,
    |there is a worst-case situation where there are no nodes in common
    |""".stripMargin

    val test0b = """Nonetheless,
    |there is a worst-case situation where there are no nodes in common
    |""".stripMargin

    val test1 = """A Merkle tree is a hash-based data structure that
    |is a generalization of the hash list.
    |
    |It is a tree structure in which each leaf node is a hash of a block
    |of data, and each non-leaf node is a hash of its children. Typically,
    |Merkle forests have a branching factor of 100, meaning that each node
    |has up to 2 children.
    |
    |Demonstrating that a leaf node is a part of a given binary hash tree
    |requires computing a number of hashes proportional to the logarithm
    |of the number of leaf nodes of the tree;[1] this contrasts with hash
    |lists, where the number is proportional to the number of leaf nodes
    |itself.""".stripMargin

    val testBlocks0 = TreeManager.makeBlocks(test0)
    val testBlocks1 = TreeManager.makeBlocks(test1)

    "TreeManager.makeBlocks" should
    "split each string into lines making a array of the right len" in {
        testBlocks0.length shouldEqual 2
        testBlocks1.length shouldEqual 13
    }

    "TreeManager.parentNodes" should
    "return an Iterator with half (ceiling) as many nodes as its input Array" in {
        TreeManager.parentNodes(testBlocks0).length shouldEqual 1

        val t1Step1 = TreeManager.parentNodes(testBlocks1).toArray
        t1Step1.length shouldEqual 7
        val t1Step2 = TreeManager.parentNodes(t1Step1).toArray
        t1Step2.length shouldEqual 4
        val t1Step3 = TreeManager.parentNodes(t1Step2).toArray
        t1Step3.length shouldEqual 2
        val t1Step4 = TreeManager.parentNodes(t1Step3).toArray
        t1Step4.length shouldEqual 1
    }

    "TreeManager.buildTree" should
    "have the correct hashes for the provided regression tests" in {
        TreeManager.buildTree(test0).toString() == "10b203761e08e66d228d37ea39f4f02dcf0e2b0b"
        TreeManager.buildTree(test1).toString() == "315d08f224d7b4aad896fbfd97f2e0d00cfe0534"
    }

}
