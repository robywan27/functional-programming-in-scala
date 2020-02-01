import examples.chapter4_error_handling.{None, Option, Some}

def map[B](f: A => B): Option[B] =
  this match {
    case None => None
    case Some(a) => Some(f(a))
  }

def getOrElse[B >: A](default: => B): B =
  this match {
    case None => default
    case Some(a) => a
  }


def flatMap[B](f: A => Option[B]): Option[B] =
  this match {
    case None => None
    case Some(a) => f(a)
  }

def flatMap2[B](f: A => Option[B]): Option[B] =
  map(f) getOrElse None


def orElse[B >: A](ob: => Option[B]): Option[B] =
  this match {
    case None => ob
    case Some(_) => this
  }

def orElse2[B >: A](ob: => Option[B]): Option[B] =
  this map (Some(_)) getOrElse ob


def filter(p: A => Boolean): Option[A] =
  this match {
    case None => None
    case Some(a) => if (p(a)) Some(a) else None
  }

def filter2(p: A => Boolean): Option[A] =
  flatMap(a => if (p(a)) Some(a) else None)