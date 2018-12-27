package com.blogspot.ramannanda.scala.lc.medium

object FlippingBinaryTree {

  case class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  def flipRec(root: TreeNode): TreeNode = {
    if (root == null) return null
    var leftTemp: TreeNode = null
    var rightTemp: TreeNode = null
    leftTemp = flipRec(root.left)
    rightTemp = flipRec(root.right)
    root.right = leftTemp
    root.left = rightTemp
    root
  }


  def flipEquiv(root1: TreeNode, root2: TreeNode): Boolean = {
    if (root1 == null && root2 == null)
      return true
    if (root1 == null || root2 == null) {
      return false
    }
    if (root1.value != root2.value) return false
    (flipEquiv(root1.left, root2.left) && flipEquiv(root1.right, root2.right)) || (flipEquiv(root1.left, root2.right) && flipEquiv(root1.right, root2.left))
  }


}
