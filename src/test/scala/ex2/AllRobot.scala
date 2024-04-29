package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AllRobotSpec extends AnyFlatSpec with Matchers:
  "A Battery Robot" should "not act when low in battery" in:
    val robot = new RobotWithBattery(50, SimpleRobot((0, 0), Direction.North))

    robot.act()
    robot.position should be((0, 1))

    robot.act()
    robot.position should be((0, 2))

    robot.act()
    robot.position should be((0, 2))
