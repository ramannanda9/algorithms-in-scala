package com.blogspot.ramannanda.scala.algorithms.ds

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}

class TrieTest extends FlatSpec with Matchers with LazyLogging {

  "trieSearch" should "give false for empty Trie and empty input" in {
    Trie(TrieNode()).search("")
  }
  it should "give true after inserting and searching same key" in {
    val trie = Trie(TrieNode())
    trie.insertNode("abc")
    assert(trie.search("abc"))
  }

  "trieInsert" should "after insert twice and search return the same key" in {
    val trie = Trie(TrieNode())
    trie.insertNode("abc")
    trie.insertNode("abc")
    assert(trie.search("abc"))
  }
  it should "after insert different elements on same prefix tree return true" in {
    val trie = Trie(TrieNode())
    trie.insertNode("abc")
    trie.insertNode("abcde")
    assert(trie.search("abc"))
    assert(trie.search("abcde"))
  }
  it should "after inserting an element and prefix of the same tree return false" in {
    val trie = Trie(TrieNode())
    trie.insertNode("abc")
    trie.insertNode("efg")
    assert(trie.search("abc"))
    assert(trie.search("efg"))
    assert(!trie.search("ab"))
  }
}