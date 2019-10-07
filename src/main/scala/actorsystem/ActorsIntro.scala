package actorsystem

import actorsystem.ActorsIntro.BankAccount.{Deposit, Statement, TransactionFailure, TransactionSuccess, Withdraw}
import actorsystem.ActorsIntro.Person.LiveTheLife
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorsIntro extends App {
  /**
   * Exercise
   *
   * 1. a Counter actor
   *  - Increment
   *  - Decrement
   *  - Print
   *
   * 2. a bank account as an actor
   *  receives
   *    - Deposit amount
   *    - Withdraw amount
   *    - Statement
   *   replies with
   *    - Success
   *    - Failure
   *
   *   interact with some other kind of actor
   */

  // Domain of the Counter
  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }
  class Counter extends Actor {
    import Counter._

    var count = 0

    override def receive: Receive = {
      case Increment => count += 1
      case Decrement => count -= 1
      case Print => println(s"[counter] My current count is $count")
    }
  }

  import Counter._
  val system = ActorSystem("myActorSystme")
  val counter = system.actorOf(Props[Counter], "myCounter")

  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  object BankAccount {
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object Statement

    case class TransactionSuccess(message: String)
    case class TransactionFailure(message: String)
  }

  class BankAccount extends Actor {

    var funds = 0

    override def receive: Receive = {
      case Deposit(amount) =>
        if(amount < 0) sender() ! TransactionFailure("Invalid deposit amount")
        else {
          funds += amount
          sender() ! TransactionSuccess(s"Successfully deposited $amount")
        }
      case Withdraw(amount) =>
        if(amount < 0) sender() ! TransactionFailure("Invalid withdraw amount")
        else if (amount > funds) sender() ! TransactionFailure("insufficient funds")
        else {
          funds -= amount
          sender() ! TransactionSuccess(s"Successfully withdrew $amount")
        }
      case Statement => sender() ! s"Your balance is $funds"
    }
  }

  object Person {
    case class LiveTheLife(account: ActorRef)
  }
  class Person extends Actor {
    import Person._
    import BankAccount._

    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(90000)
        account ! Withdraw(500)
        account ! Statement
      case message => println(message.toString)
    }
  }

  val account = system.actorOf(Props[BankAccount], "bankAccount")
  val person = system.actorOf(Props[Person], "billionaire")

  person ! LiveTheLife(account)
}
