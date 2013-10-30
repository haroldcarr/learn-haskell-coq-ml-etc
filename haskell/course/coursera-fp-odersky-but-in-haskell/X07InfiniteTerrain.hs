{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Oct 29 (Tue) 18:58:08 by carr.

package streams

/**
 * This trait defines an infinite terrain, where the block can
 * go on any position.
 *
 * It keeps the `startPos` and the `goal` positions abstract.
 *
 * Using this trait is useful for testing. It can be used to find
 * the shortest path between two positions without terrain
 * restrictions.
 */
trait InfiniteTerrain extends GameDef {
  val terrain: Terrain = (pos: Pos) => true
}
-}
