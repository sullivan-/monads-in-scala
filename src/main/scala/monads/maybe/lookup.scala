package monads.maybe

object lookup {

  case class Name(name: String)
  case class Number(number: String)
  case class Registration(registration: String)
  case class TaxOwed(taxOwed: Double)

  private val seedTestData: List[Int] = (1 to 100).toList
  private def intToName(i: Int): Name = Name(i.toString)
  private def intToNumber(i: Int): Number = Number(i.toString)
  private def intToRegistration(i: Int): Registration = Registration(i.toString)
  private def intToTaxOwed(i: Int): TaxOwed = TaxOwed(i.toDouble)

  val names: List[Name] = seedTestData map intToName
  val numbers: List[Number] = seedTestData map intToNumber
  val registrations: List[Registration] = seedTestData map intToRegistration
  val taxesOwed: List[TaxOwed] = seedTestData map intToTaxOwed

  private def seedMap[A, B](
    intFilter: (Int) => Boolean,
    intToA: (Int) => A,
    intToB: (Int) => B): Map[A, B] = {
    import language.postfixOps
    seedTestData filter intFilter map {
      i => intToA(i) -> intToB(i)
    } toMap
  }

  // only those divisible by 2 are in the number map
  private val numberMap: Map[Name, Number] =
    seedMap(_ % 2 == 0, intToName, intToNumber)

  // only those divisible by 3 are in the registration map
  private val registrationMap: Map[Number, Registration] =
    seedMap(_ % 3 == 0, intToNumber, intToRegistration)

  // only those divisible by 5 are in the tax map
  private val taxOwedMap: Map[Registration, TaxOwed] =
    seedMap(_ % 5 == 0, intToRegistration, intToTaxOwed)

  private def lookup[A, B](map: Map[A, B])(a: A): Maybe[B] =
    Maybe(map.get(a))

  def lookupNumber(name: Name): Maybe[Number] =
    lookup(numberMap)(name)

  def lookupRegistration(number: Number): Maybe[Registration] =
    lookup(registrationMap)(number)

  def lookupTaxOwed(registration: Registration): Maybe[TaxOwed] =
    lookup(taxOwedMap)(registration)

  def lookupRegistration1(name: Name): Maybe[Registration] =
    for (
      number <- lookupNumber(name);
      registration <- lookupRegistration(number))
    yield registration

  def lookupRegistration2(name: Name): Maybe[Registration] =
    lookupNumber(name) flatMap lookupRegistration

  def lookupTaxOwed1(name: Name): Maybe[TaxOwed] =
    for (
      number <- lookupNumber(name);
      registration <- lookupRegistration(number);
      taxOwed <- lookupTaxOwed(registration))
    yield taxOwed

  def lookupTaxOwed2(name: Name): Maybe[TaxOwed] =
    lookupNumber(name) flatMap lookupRegistration flatMap lookupTaxOwed
}
