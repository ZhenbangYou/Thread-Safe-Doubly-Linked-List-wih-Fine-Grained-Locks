import java.util.concurrent.locks.{Lock, ReentrantLock}
import scala.annotation.tailrec


class SyncList[T](implicit f: T => Ordered[T]) {
    private case class Node(value: T,
                            var prev: Option[Node] = None,
                            var next: Option[Node] = None,
                            lock: Lock = new ReentrantLock())

    private var head: Option[Node] = None
    private val listLock: Lock = new ReentrantLock()

    def find(item: T): Boolean = {
        listLock.lock()
        head match {
            case None =>
                listLock.unlock()
                false
            case Some(head) =>
                head.lock.lock()
                listLock.unlock()
                var cur = head
                while (true) {
                    if (cur.value == item) {
                        cur.lock.unlock()
                        return true
                    } else {
                        cur.next match {
                            case None =>
                                cur.lock.unlock()
                                return false
                            case Some(next) =>
                                next.lock.lock()
                                cur.lock.unlock()
                                cur = next
                        }
                    }
                }
                throw new Exception("should not reach here")
        }
    }

    def insert(item: T): Unit = {
        listLock.lock()
        head match {
            case None =>
                head = Some(Node(item))
                listLock.unlock()
            case Some(head) =>
                head.lock.lock()
                listLock.unlock()
                var cur = head
                var prev: Option[Node] = None
                while (true) {
                    if (cur.value >= item) {
                        val newNode = Node(item, cur.prev, Some(cur))
                        prev match {
                            case None =>
                                this.head = Some(newNode)
                                cur.prev = Some(newNode)
                                cur.lock.unlock()
                            case Some(prev) =>
                                prev.next = Some(newNode)
                                cur.prev = Some(newNode)
                                prev.lock.unlock()
                                cur.lock.unlock()
                        }
                        return
                    } else {
                        (prev, cur.next) match {
                            case (None, None) =>
                                cur.next = Some(Node(item, Some(cur), None))
                                cur.lock.unlock()
                                return
                            case (None, Some(next)) =>
                                next.lock.lock()
                                prev = Some(cur)
                                cur = next
                            case (Some(prev), None) =>
                                cur.next = Some(Node(item, Some(cur), None))
                                prev.lock.unlock()
                                cur.lock.unlock()
                                return
                            case (Some(innerPrev), Some(next)) =>
                                next.lock.lock()
                                innerPrev.lock.unlock()
                                prev = Some(cur)
                                cur = next
                        }
                    }
                }
                throw new Exception("should not reach here")
        }
    }

    def delete(item: T): Boolean = {
        listLock.lock()
        head match {
            case None =>
                listLock.unlock()
                false
            case Some(head) =>
                head.lock.lock()
                listLock.unlock()
                var cur = head
                var prev: Option[Node] = None
                while (true) {
                    if (cur.value == item) {
                        prev match {
                            case None =>
                                this.head = cur.next
                                cur.lock.unlock()
                            case Some(prev) =>
                                prev.next = cur.next
                                prev.lock.unlock()
                                cur.lock.unlock()
                        }
                        return true
                    } else if (cur.value > item) {
                        prev match {
                            case None =>
                                cur.lock.unlock()
                            case Some(prev) =>
                                prev.lock.unlock()
                                cur.lock.unlock()
                        }
                        return false
                    } else {
                        (prev, cur.next) match {
                            case (None, None) =>
                                cur.lock.unlock()
                                return false
                            case (None, Some(next)) =>
                                next.lock.lock()
                                prev = Some(cur)
                                cur = next
                            case (Some(prev), None) =>
                                prev.lock.unlock()
                                cur.lock.unlock()
                                return false
                            case (Some(innerPrev), Some(next)) =>
                                next.lock.lock()
                                innerPrev.lock.unlock()
                                prev = Some(cur)
                                cur = next
                        }
                    }
                }
                throw new Exception("should not reach here")
        }
    }

    def toList: List[T] = {
        listLock.lock()
        val res = collection.mutable.ArrayBuffer[T]()

        @tailrec
        def traverse(cur: Option[Node]): Unit = {
            cur match {
                case None => ()
                case Some(cur) =>
                    res.append(cur.value)
                    traverse(cur.next)
            }
        }

        traverse(head)
        listLock.unlock()
        res.toList
    }
}
