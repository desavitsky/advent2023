package com.github.desavitsky.day17

enum Direction:
  case Right, Left, Up, Down

  def opposite:Direction = this match
    case Direction.Right => Left
    case Direction.Left => Right
    case Direction.Up => Down
    case Direction.Down => Up