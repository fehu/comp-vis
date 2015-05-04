package feh.tec.cvis.db

import org.h2.jdbc.JdbcSQLException
import slick.dbio.{Effect, NoStream, DBIOAction}
import slick.jdbc.JdbcBackend
import feh.util._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

trait HasDbConnections {

  def failure(thr: Throwable): Nothing
  
  protected class DbConnection[B <: JdbcBackend](mkDb: () => B#Database) {
    protected var created = false
    protected lazy val db = try mkDb() $$ {created = true}
                            catch failurePF

    def tryCreateTables(a: DBIOAction[Unit, NoStream, Effect.Schema]): Future[Unit] =
      runWithErrorCatch(failureOnCreatePF orElse failurePF)(a)
    
    def run[R](a: DBIOAction[R, NoStream, Nothing]): Future[R] = runWithErrorCatch(failurePF)(a)

    def close(timeout: FiniteDuration) = if(created) Await.result(Future{ db.close() }, timeout)
    
    protected def runWithErrorCatch[R](errPf: PartialFunction[Throwable, Future[R]])
                                      (a: DBIOAction[R, NoStream, Nothing]): Future[R] =
      try db.run(a) $$ {_ onFailure errPf}
      catch errPf

    protected def failurePF[R]: PartialFunction[Throwable, R] = { case th: Throwable => failure(th) }
    
    protected def failureOnCreatePF[R]: PartialFunction[Throwable, R] = {
      case x: JdbcSQLException  if x.getMessage.contains("Table \"")
                                && x.getMessage.contains("\" already exists;") => null.asInstanceOf[R]
    }

  }

  object DbConnection{
    def apply[B <: JdbcBackend](db: => B#Database): DbConnection[B] = new DbConnection(() => db)
  }

}