package feh.tec.cvis.db

import java.util.UUID

import feh.tec.cvis.DescriptorsSupport.IDescriptor
import feh.tec.cvis.db.SingleChannelDescriptorsWithStats.query
import feh.util._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait HasDescriptorCache{
  self: HasDbConnections =>

  lazy val descriptorCache: DescriptorCache = new DescriptorCache.Impl

  trait DescriptorCache{
    def get(id: UUID)(implicit db: DbConnection[_]): Future[IDescriptor]

    def guard(d: IDescriptor)(implicit db: DbConnection[_]): Future[Unit]

    def guardInCache(d: IDescriptor)

    def clear()
  }

  object DescriptorCache{

    class Impl extends DescriptorCache{
      protected lazy val cache = mutable.HashMap.empty[UUID, IDescriptor]

      def guardInCache(d: IDescriptor): Unit = d.id.foreach(cache += _ -> d)

      def get(id: UUID)(implicit db: DbConnection[_]): Future[IDescriptor] =
        cache.get(id)
             .map(Future(_))
             .getOrElse{ db.run(query.get(id)) $$ setOnSuccess _ }

      def guard(d: IDescriptor)(implicit db: DbConnection[_]): Future[Unit] =
        db.run(query.insert(d)) $$ (_.onSuccess{ case _ => guardInCache(d) })

      def clear(): Unit = cache.clear()

      private def setOnSuccess(f: Future[IDescriptor]) = f.onSuccess{ case d => guardInCache(d) }
    }

  }

}

