package com.blogspot.ramannanda.scala.algorithms.ds.balanced

import scala.annotation.tailrec


case class RedBlackTreeImpl[K](key: K)(implicit ev: Ordering[K]) {
  val nil: RBNode[K] = RBNode[K](null.asInstanceOf[K], false, nil, nil, nil)
  var rootNode: RBNode[K] = new RBNode[K](key, false, nil, nil, nil)

  def leftRotate(x: RBNode[K]): Unit = {
    val y = x.right
    x.right = y.left
    if (y.left != nil) {
      y.left.p = x
    }
    y.p = x.p
    if (x.p == nil) {
      rootNode = y
    }
    else if (x.equals(x.p.left)) {
      x.p.left = y
    }
    else {
      x.p.right = y
    }
    y.left = x
    x.p = y
  }

  def rightRotate(y: RBNode[K]): Unit = {
    val x = y.left
    y.left = x.right
    if (x.right != nil) {
      x.right.p = y
    }
    if (y.p == nil) {
      rootNode = x
    }
    else if (y.equals(y.p.left)) {
      y.p.left = x
    }
    else y.p.right = x
    y.p = x
    x.right = y
  }

  def fixRedBlackTree(node: RBNode[K]): Unit = {
    var z = node
    while (z.p.isRed) {
      if (z.p.equals(z.p.p.left)) {
        val y = z.p.p.right
        if (y.isRed) {
          z.p.isRed = false
          y.isRed = false
          z.p.p.isRed = true
          z = z.p.p
        }
        else {
          if (z.equals(z.p.right)) {
            z = z.p
            leftRotate(z)
          }
          z.p.isRed = false
          z.p.p.isRed = true
          rightRotate(z.p.p)
        }
      }
      else {
        if (z.p.equals(z.p.p.right)) {
          val y = z.p.p.left
          if (y.isRed) {
            z.p.isRed = false
            y.isRed = false
            z.p.p.isRed = true
            z = z.p.p
          }
          else {
            if (z.equals(z.p.left)) {
              z = z.p
              rightRotate(z)
            }
            z.p.isRed = false
            z.p.p.isRed = true
            leftRotate(z.p.p)
          }
        }
      }
    }
    rootNode.isRed = false
  }

  def insert(key: K)(implicit ev: Ordering[K]): Unit = {
    import ev.mkOrderingOps
    @tailrec
    def insertRec(key: K, node: RBNode[K]): RBNode[K] = {
      if (key > node.data) {
        if (node.right == nil) {
          val childNode = RBNode[K](key, true, nil, nil, node)
          node.right = childNode
          childNode
        } else {
          insertRec(key, node.right)
        }
      }
      else {
        if (node.left == nil) {
          val childNode = RBNode[K](key, true, nil, nil, node)
          node.left = childNode
          childNode
        }
        else {
          insertRec(key, node.left)
        }
      }
    }

    val addedNode = insertRec(key, rootNode)
    fixRedBlackTree(addedNode)

  }

  def searchNode(key: K)(implicit ev: Ordering[K]): RBNode[K] = {
    import ev.mkOrderingOps
    @tailrec
    def searchNodeRec(k: K, parent: RBNode[K]): RBNode[K] = {
      if (parent == null) return null
      if (k > parent.data) {
        searchNodeRec(k, parent.right)
      }
      else if (k < parent.data) {
        searchNodeRec(k, parent.left)
      }
      else if (k == parent.data) {
        parent
      }
      else null
    }

    searchNodeRec(key, rootNode)
  }

  def transplant(u: RBNode[K], v: RBNode[K]) = {
    if (u.p == nil) {
      rootNode = v
    }
    else if (u.equals(u.p.left)) {
      u.p.left = v
    }
    else {
      u.p.right = v
    }
    v.p = u.p
  }

  @tailrec
  final def findMinimum(rBNode: RBNode[K]): RBNode[K] = {
    if (rBNode.left != nil) {
      findMinimum(rBNode.left)
    }
    else {
      rBNode
    }
  }

  def deleteFixUp(xIn: RBNode[K]): Unit = {
    var x = xIn
    while (!x.equals(rootNode) && !x.isRed) {
      if (x.equals(x.p.left)) {
        var w = x.p.right
        //case 1
        if (w.isRed) {
          w.isRed = false
          x.p.isRed = true
          leftRotate(x.p)
          //fix new sibling
          w = x.p.right
        }
        //two cases
        if (!w.left.isRed && !w.right.isRed) {
          w.isRed = true
          x = x.p
        }
        else {
          //case 3
          if (!w.right.isRed) {
            w.left.isRed = false
            w.isRed = true
            rightRotate(w)
            w = x.p.right //fix sibling
          }
          w.isRed = x.p.isRed
          x.p.isRed = false
          w.right.isRed = false
          leftRotate(x.p)
          rootNode = x
        }
      }
      //mirrors the cases
      else {
        var w = x.p.left
        //case 1
        if (w.isRed) {
          w.isRed = false
          x.p.isRed = true
          rightRotate(x.p)
          //fix new sibling
          w = x.p.left
        }
        //two cases
        if (!w.left.isRed && !w.right.isRed) {
          w.isRed = true
          x = x.p
        }
        else {
          //case 3
          if (!w.left.isRed) {
            w.right.isRed = false
            w.isRed = true
            leftRotate(w)
            w = x.p.left //fix sibling
          }
          w.isRed = x.p.isRed
          x.p.isRed = false
          w.left.isRed = false
          rightRotate(x.p)
          rootNode = x
        }
      }
    }
    x.isRed = false
  }

  def delete(key: K)(implicit ev: Ordering[K]): Unit = {
    val z = searchNode(key)
    if (z != null) {
      //set y to either the node to be removed or the node moved
      var y = z
      var yOrigRed = y.isRed
      var x: RBNode[K] = null
      if (z.left == nil) {
        x = z.right
        transplant(z, x)
      }
      else if (z.right == nil) {
        x = x.left
        transplant(z, x)
      }
      else {
        y = findMinimum(z.right)
        yOrigRed = y.isRed
        x = y.right
        if (!y.p.equals(z)) {
          transplant(y, y.right)
          y.right = z.right
          y.right.p = y
          y.left = z.left
          y.left.p = y
          y.isRed = z.isRed
        }
        if (!yOrigRed) {
          deleteFixUp(x)
        }
      }
    }

  }


  case class RBNode[K](data: K, var isRed: Boolean = false,
                       var left: RBNode[K],
                       var right: RBNode[K],
                       var p: RBNode[K]) {
    override def toString: String = s"Node value $data, isRed:$isRed"


  }

}


