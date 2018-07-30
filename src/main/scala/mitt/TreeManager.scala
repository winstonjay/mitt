package mitt

import scala.collection.mutable.ArrayBuffer
import Utils.sha1

// notes:
// https://blog.sourced.tech/post/difftree/
// https://benhoyt.com/writings/pygit/
// https://brilliant.org/wiki/merkle-tree/


/**
 *  TreeManager implements methods for creating an manipulating
 *  merkle trees.
 */
object TreeManager {

    type diffPair = ArrayBuffer[(Node, Node)]

    def buildTree(data: String) : Node = {
        buildUp(makeBlocks(data))(0)
    }

    // _buildUp : take an initial Array of data blocks (leaf nodes) and
    // recursively build a tree in a bottom-up manner making parent
    // nodes unitil we are left with 1 root node.
    private def buildUp(blocks: Array[Node]) : Array[Node] = {
        if (blocks.length == 1) blocks
        else buildUp(parentNodes(blocks).toArray)
    }

    // parentNodes :
    def parentNodes(blocks: Array[Node]) : Iterator[Node] = {
        for {
            pair <- blocks.grouped(2)
        } yield new Node(pair.mkString,
                         pair(0),
                         if (pair.length > 1) pair(1) else null)
    }

    // makeBlocks : splits the input into each of the data blocks which will
    // form the leaf nodes of the tree.
    def makeBlocks(str: String) : Array[Node] = {
        str.split("\n").map(new Node(_))
    }

    def hashCmp(a: Node, b: Node) : diffPair = {
        _hashCmp(a, b, ArrayBuffer[(Node, Node)]())
    }

    private def _hashCmp(a: Node, b: Node, arr: diffPair) : diffPair = {
        if (a.hash == b.hash) {
            arr
        } else {
            if (a.isLeaf && b.isLeaf)
                arr.append((a, b))
            if (a.hasLeftChild && b.hasLeftChild)
                _hashCmp(a.leftChild, b.leftChild, arr)
            if (a.hasRightChild && b.hasRightChild)
                _hashCmp(a.rightChild, b.rightChild, arr)
            arr
        }
    }

    // printTree : for a given Node it and all of its children.
    def printTree(t: Node, depth: Int = 0) {

        printf("%s%s... %s\n",
               " " * (2*depth),
               t.hash.slice(0, 5),
               if (t.isLeaf) t.data else "")

        if (t.hasLeftChild) {
            printTree(t.leftChild, depth + 1)
        }
        if (t.hasRightChild) {
            printTree(t.rightChild, depth + 1)
        }
    }

}


/**
 *  Node : Stores an hash of its child nodes. If it is a leaf node its own
 *  data string.
 */
class Node(str: String, left: Node = null, right: Node = null) {

    val hash = sha1(str)
    val data = if (isLeaf) str else ""

    def hasLeftChild  = left != null
    def hasRightChild = right != null
    def leftChild  = left
    def rightChild = right
    def isLeaf     = (left == null && right == null)

    override def toString() = hash
}