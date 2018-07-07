package com.blogspot.ramannanda.scala.algorithms.ds

import scala.annotation.tailrec

case class TrieNode() {
  var isTerminal = false
  var children: Array[TrieNode] = null

  override def toString: String = s"The node is terminal${isTerminal}"
}

case class Trie(rootNode: TrieNode) {
  def insertNode(key: String): Unit = {
    @tailrec
    def insertNodeRec(parent: TrieNode, subKey: String): Unit = {
      if (subKey.isEmpty) {
        parent.isTerminal = true
      }
      else {
        if (parent.children == null) {
          parent.children = new Array[TrieNode](26)
          parent.children(subKey.charAt(0) - 'a') = TrieNode()
        }
        else if (parent.children(subKey.charAt(0) - 'a') == null) {
          parent.children(subKey.charAt(0) - 'a') = TrieNode()
        }
        insertNodeRec(parent.children(subKey.charAt(0) - 'a'), subKey.substring(1))
      }
    }

    insertNodeRec(rootNode, key)
  }

  //searches are case insensitive only english character symbols
  def search(key: String): Boolean = {
    @tailrec
    def searchRec(parent: TrieNode, subKey: String): Boolean = {
      if (subKey.isEmpty) {
        if (parent.isTerminal) true else false
      }
      else {
        if (parent.children == null || parent.children(subKey.charAt(0) - 'a') == null) false
        else searchRec(parent.children(subKey.charAt(0) - 'a'), subKey.substring(1))
      }
    }

    searchRec(rootNode, key.toLowerCase)
  }
}