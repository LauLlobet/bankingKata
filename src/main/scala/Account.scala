import java.time.LocalDate
import java.util.concurrent.atomic.AtomicInteger

object Account {
   val idCounter :AtomicInteger = new AtomicInteger(0)
  case class AccountSnapshot(date: LocalDate, amount: Double, balance: Double )
}

class Account {

  /**
    * id should be unique for each new account. (Don't use RNG)
    */
  val id: Int = Account.idCounter.getAndIncrement()
  var snapshots: List[Account.AccountSnapshot] = List.empty

  /**
    *
    * @return current balance of the account
    */
  def balance: Double = math.floor(_balance*100.0)/100.0;

  def modifyBalance(amount: Double) = {
    _balance += amount;
    snapshots = Account.AccountSnapshot(LocalDate.now(),amount,balance) :: snapshots
  }

  private var _balance =  .0

  def deposit(quantity: Double): Unit = {
    if (quantity > 0) {
      modifyBalance(quantity)
    }
  }

  /**
    *
    * @param quantity amount of money to withdrawal
    * @return money withdrawed. 0 if none
    */
  def withdrawal(quantity: Double): Double = {
    if (_balance < quantity || quantity <= 0)
      0
    else {
      modifyBalance(-quantity)
      quantity
    }
  }

  /**
    * Prints all the history starting with the recent one.
    * An example statement would be:
    * Date        Amount  Balance
    * 23.8.2016    -100      400
    * 24.12.2015   +500      500
    */
  def printStatements(): Unit = {
    println("Date        Amount  Balance");
    snapshots.foreach { x =>
      println(s"${x.date} ${x.amount}  ${x.balance}")
    }
  }

}
