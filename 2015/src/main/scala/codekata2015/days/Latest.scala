package codekata2015
package days
import zio.RIO

object Latest extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 22

  sealed trait EffectType
  case object MagicMissile extends EffectType
  case object Drain extends EffectType
  case object Shield extends EffectType
  case object Poison extends EffectType
  case object Recharge extends EffectType

  case class Effect(
      name: EffectType,
      cost: Int,
      duration: Int,
      damage: Int,
      heal: Int,
      manaHeal: Int,
      armor: Int
  ) {
    def tick: Option[Effect] = copy(duration = duration - 1).some.filter(_.duration > 0)
  }

  val magicMissile = Effect(MagicMissile, 53, 0, 4, 0, 0, 0)
  val drain        = Effect(Drain, 73, 0, 2, 2, 0, 0)
  val shield       = Effect(Shield, 113, 6, 0, 0, 0, 7)
  val poison       = Effect(Poison, 173, 6, 3, 0, 0, 0)
  val recharge     = Effect(Recharge, 229, 5, 0, 0, 101, 0)

  val actions: Seq[Effect] = Seq(magicMissile, drain, shield, poison, recharge)

  case class Boss(hp: Int, damage: Int)
  case class Player(hp: Int = 10, mp: Int = 250)

  def turn(
      player: Player,
      boss: Boss,
      playerTurn: Option[Effect],
      effects: Seq[Effect]
  ): (Player, Boss, Seq[Effect]) = {
    val (p, b) = (
      effects.foldLeft(player) { case (p, e) => p.copy(hp = p.hp + e.heal, mp = p.mp + e.manaHeal) },
      effects.foldLeft(boss) { case (b, e) => b.copy(hp = b.hp - e.damage) },
    )
    val nextRoundEffects = effects.flatMap(_.tick) ++ playerTurn.filter(_.tick.isDefined).toSeq
    if (p.hp <= 0 || b.hp <= 0) (p, b, Nil)
    else playerTurn.fold2(
      // Boss turn
      (p.copy(hp = player.hp - Math.max(1, boss.damage - effects.map(_.armor).sum)), b, nextRoundEffects),
      // Player turn
      { action =>
        (
          p.copy(hp = p.hp + action.heal, mp = p.mp - action.cost),
          b.copy(hp = b.hp - action.damage),
        nextRoundEffects
        )
      }
    )
  }
  override def part1: Option[Part] =
    new Part {
      override def solution: RIO[Any, Any] = {
        val player = Player()
        val boss   = inputs
//        Seq(poison.some, None, magicMissile.some, None)

//        Seq(recharge.some, None, shield.some, None, drain.some, None, poison.some, None, magicMissile.some, None)
//          .foldLeft((player, boss, Seq.empty[Effect])) { case ((p, b, es), a) =>
//            s"Turn: ${a.map("Player " + _).getOrElse("Boss")}".debug
//            turn(p, b, a, es).debug
//          }

//        def ifPlayed(a: Option[Effect], p: Player, b: Boss, es: Seq[Effect], mpSoFar: Int): Option[(Int, Seq[Effect])] = {
//          if (p.hp < 1) None
//          else if (b.hp < 1) mpSoFar.some
//          else a.fold2(
//            // Boss turn
//            {
//              val (np, nb, nes) = turn(p, b, None, es)
//              val stillInEffect = nes.flatMap(_.tick).map(_.name).toSet
//              // We can take any action which doesn't correspond to an effect that is still in effect
//              // next turn (that is, that isn't ending at the start of the turn).
//              (actions.filterNot(a => stillInEffect.contains(a.name))
//                .flatMap(a => ifPlayed(a.some, np, nb, nes, mpSoFar).map(res => a -> res))
//                .foldLeft(Option.empty[Int] -> Seq.empty) { case (cur, n) => cur.fold2(n.some, Math.min(n, _).some)}
//                )
//            },
//            // Player turn
//            pa => {
//              val (np, nb, nes) = turn(p, b, pa.some, es)
//              ifPlayed(None, np, nb, nes, mpSoFar + pa.cost)
//            }
//          )
//        }
//        actions.flatMap(a => ifPlayed(a.some, player, boss, Nil, 0))
//          .foldLeft(Option.empty[Int]) { case (cur, n) => cur.fold2(n.some, Math.min(n, _).some)}
      }.zio
    }.some
  override def part2: Option[Part] = None

  def inputs = in3

  val in2 = Boss(13, 8)
  val in3 = Boss(14, 8)

  def puzzleIn = Boss(58, 8)

  override def in: String = ""
}
