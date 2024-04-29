package ex3

object Solitaire extends App:

  extension (p: (Int, Int)) private def bounded(width: Int, height: Int): Boolean =
    p._1 >= 0 && p._1 < width && p._2 >= 0 && p._2 < height
  def possibleMoves(point: (Int, Int), width: Int, height: Int): Set[(Int, Int)] =
    Set(
      (point._1 - 3, point._2    ),
      (point._1 + 3, point._2    ),
      (point._1    , point._2 - 3),
      (point._1    , point._2 + 3),
      (point._1 - 2, point._2 - 2),
      (point._1 + 2, point._2 - 2),
      (point._1 + 2, point._2 + 2),
      (point._1 - 2, point._2 + 2)
    ).filter(_.bounded(width, height))

  def test(point: (Int, Int), width: Int, height: Int): Set[(Int, Int)] =
    Set(
      (point._1 - 1, point._2    ),
      (point._1 + 1, point._2    ),
      (point._1    , point._2 - 1),
      (point._1    , point._2 + 1),
    ).filter(_.bounded(width, height))

  def next(sol: Seq[(Int, Int)], width: Int, height: Int): Set[Seq[(Int, Int)]] =
    possibleMoves(sol.head, width, height).filterNot(sol.contains).map(sol.prepended)

  def isSolution(sol: Seq[(Int, Int)], width: Int, height: Int): Boolean =
    sol.distinct.size == width * height

  private def computeSolutions(start: Set[Seq[(Int,Int)]], width: Int, height: Int, step: Int): Set[Seq[(Int,Int)]] =
    if step < width * height then
      start.flatMap(s => computeSolutions(next(s, width, height), width, height, step + 1))
    else
      start

  def placeMarks(width: Int, height: Int): Set[Seq[(Int, Int)]] = computeSolutions(Set(Seq((width/2, height/2))), width, height, 1)
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")


  //println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))
  val v = placeMarks(5,5)
  v.foreach(s => println(render(s, width = 5, height = 5) + "\n"))
