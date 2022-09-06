package io.chrisdavenport.sonatype

import org.http4s._
import org.http4s.implicits._
import org.http4s.client._
import org.http4s.circe._
import org.http4s.headers.Authorization
import cats.syntax.all._
import cats.effect._
import io.circe._
import org.http4s.ember.client.EmberClientBuilder


trait SonatypeReleaseHttp[F[_]]{
  import SonatypeReleaseHttp._
  def getStagingProfiles: F[List[StagingProfile]]
  def getStagingProfile(orgId: String): F[StagingProfile]

  def getStagingRepositories: F[List[StagingRepository]] 
  def getStagingRepository(stagingRepoId: String): F[StagingRepository]

  def createStagingRepo(orgId: String, profileId: String): F[CreatedStagingRepository] 

  def closeStagingRepo(profileId: String, repoId: String): F[Boolean]
  def promoteStagingRepo(profileId: String, repoId: String): F[Boolean]
  def dropStagingRepo(profileId: String, repoId: String): F[Status] 
  
  // We do not have upload as we let SBT do this with the returned information
}

object SonatypeReleaseHttp {

  object BaseUri {
    val legacy = uri"https://oss.sonatype.org"
    val s01 = uri"https://s01.sonatype.org"

    def release(baseUri: Uri): Uri = baseUri / "service" / "local"
    // def snapshot(baseUri: Uri): Uri = baseUri / "content" / "repositories" / "snapshots"
  }

  def default[F[_]: Async](baseUri: Uri, creds: BasicCredentials): Resource[F, SonatypeReleaseHttp[F]] = 
    EmberClientBuilder.default[F]
      .build
      .map(client => credentials(client, baseUri, creds))

  def credentials[F[_]: Concurrent](client: Client[F], baseUri: Uri, credentials: BasicCredentials): SonatypeReleaseHttp[F] = 
    new SonatypeHttpImpl[F](client, baseUri, Either.right(credentials))

  case class StagingProfile(
    id: String,
    name: String,
    uri: Uri,
    raw: Json
  ) {
    override def toString = s"StagingProfile(id=$id,name=$name,uri=$uri)"
  }
  object StagingProfile {
    implicit val decoder: Decoder[StagingProfile] = new Decoder[StagingProfile]{
      def apply(c: HCursor): Decoder.Result[StagingProfile] = 
        (
          c.downField("id").as[String],
          c.downField("name").as[String],
          c.downField("resourceURI").as[Uri](Decoder[String].emap(Uri.fromString(_).leftMap(p => p.getMessage())))
          
        ).mapN{ case (id, name, uri) => StagingProfile(id, name, uri, c.value)}
    }
  }

  case class CreatedStagingRepository(
    id: String,
    description: String,
    raw: Json
  ){
    override def toString = s"CreatedStagingRepository(id=$id,description=$description)"
  }
  object CreatedStagingRepository {
    implicit val decoder: Decoder[CreatedStagingRepository] = new Decoder[CreatedStagingRepository]{
      def apply(c: HCursor): Decoder.Result[CreatedStagingRepository] = 
        (
          c.downField("stagedRepositoryId").as[String],
          c.downField("description").as[String]
          
        ).mapN{ case (id, description) => CreatedStagingRepository(id, description, c.value)}
    }
  }
    // {
    //   "profileId" : "bb1e1365c6cdf",
    //   "profileName" : "io.chrisdavenport",
    //   "profileType" : "repository",
    //   "repositoryId" : "iochrisdavenport-1612",
    //   "type" : "open",
    //   "policy" : "release",
    //   "userId" : "christopherdavenport",
    //   "userAgent" : "http4s-ember/0.23.12",
    //   "ipAddress" : "67.168.198.38",
    //   "repositoryURI" : "https://oss.sonatype.org/content/repositories/iochrisdavenport-1612",
    //   "created" : "2022-08-23T20:43:55.039Z",
    //   "createdDate" : "Tue Aug 23 20:43:55 UTC 2022",
    //   "createdTimestamp" : 1661287435039,
    //   "updated" : "2022-08-23T20:43:58.881Z",
    //   "updatedDate" : "Tue Aug 23 20:43:58 UTC 2022",
    //   "updatedTimestamp" : 1661287438881,
    //   "description" : "Staging profile for io.chrisdavenport",
    //   "provider" : "maven2",
    //   "releaseRepositoryId" : "releases",
    //   "releaseRepositoryName" : "Releases",
    //   "notifications" : 0,
    //   "transitioning" : false
    // }
  case class StagingRepository(
    profileId: String,
    profileName: String,
    profileType: String,
    repositoryId: String,
    repositoryType: String,
    repositoryUri: Uri,
    description: String,
    raw: Json
  )

  object StagingRepository {
    implicit val decoder: Decoder[StagingRepository] = new Decoder[StagingRepository]{
      def apply(c: HCursor): Decoder.Result[StagingRepository] = 
        (
          c.downField("profileId").as[String],
          c.downField("profileName").as[String],
          c.downField("profileType").as[String],
          c.downField("repositoryId").as[String],
          c.downField("type").as[String],
          c.downField("repositoryURI").as[Uri](Decoder[String].emap(s => Uri.fromString(s).leftMap(_.message))),
          c.downField("description").as[String]
        ).mapN{
          case (profileId, profileName, profileType, repositoryId, repositoryType, uri, description) => 
            StagingRepository(profileId, profileName, profileType, repositoryId, repositoryType, uri, description, c.value)
        }
    }
  }


  // Concepts: 
  // Staging Profile - An Organization that one can stage and publish things to
  // Staging Repo - A repository that can have data posted to it for publication
  class SonatypeHttpImpl[F[_]: Concurrent](
    client: Client[F],
    baseUri: Uri,
    // Either Token String passed directly or basic auth
    creds: Either[String, BasicCredentials]
  ) extends SonatypeReleaseHttp[F] {      

    private val staticHeaders = Headers(
      Authorization(Credentials.Token(AuthScheme.Basic, creds.map(_.token).merge))
    )

    // Based on provided credentials which profiles can you stage to and what are there identifiers
    // // https://oss.sonatype.org/nexus-staging-plugin/default/docs/path__staging_profiles.html
    def getStagingProfiles: F[List[StagingProfile]] = {
      val req = Request[F](Method.GET, baseUri / "staging" / "profiles", HttpVersion.`HTTP/1.1`, staticHeaders)
      client.expect[Json](req)
        .flatMap( json => 
          json.hcursor.downField("data")
            .as[List[StagingProfile]]
            .liftTo[F]
        )
    }

    def getStagingProfile(orgId: String): F[StagingProfile] = getStagingProfiles.flatMap(profiles => 
      profiles.groupByNel(_.name)
        .get(orgId)
        .map(_.head)
        .liftTo(new Throwable(s"No Staging Profile for $orgId present"))
    )

    // https://oss.sonatype.org/nexus-staging-plugin/default/docs/path__staging_profile_repositories.html
    def getStagingRepositories: F[List[StagingRepository]] = {
      val request = Request[F](
        Method.GET,
        baseUri / "staging" / "profile_repositories",
        HttpVersion.`HTTP/1.1`,
        staticHeaders
      )
      client.expect[Json](request)
        .flatMap(json => json.hcursor.downField("data").as[List[StagingRepository]].liftTo[F])
    }

    // https://oss.sonatype.org/nexus-staging-plugin/default/docs/path__staging_repository_-repositoryIdKey-.html
    def getStagingRepository(stagingRepoId: String): F[StagingRepository] = {
      val request = Request[F](Method.GET, baseUri / "staging" / "repository" / stagingRepoId, HttpVersion.`HTTP/1.1`, staticHeaders)
      client.expect[Json](request)
        .flatMap(json => json.hcursor.downField("data").as[StagingRepository].liftTo[F])
    }

    // orgId to improve description
    // Profile is the unique identifier
    def createStagingRepo(orgId: String, profileId: String): F[CreatedStagingRepository] = {
      // https://oss.sonatype.org/service/local/staging/profiles/bb1e1365c6cdf
      val request = Request[F](
        Method.POST,
        baseUri / "staging" / "profiles" / profileId / "start",
        HttpVersion.`HTTP/1.1`,
        staticHeaders
      ).withEntity(
        Json.obj("data" -> Json.obj("description" -> Json.fromString(s"Staging profile for $orgId")))
      )

      client.expect[Json](request).flatMap(
        _.hcursor
          .downField("data")
          .as[CreatedStagingRepository]
          .liftTo[F]
      )
    }

    def closeStagingRepo(profileId: String, repoId: String): F[Boolean] = {
      val request = Request[F](
        Method.POST,
        baseUri / "staging" / "profiles" / profileId / "close",
        HttpVersion.`HTTP/1.1`,
        staticHeaders
      ).withEntity(
        Json.obj("data" -> Json.obj(
          "stagedRepositoryId" -> Json.fromString(repoId),
          "description" -> Json.fromString("Closing Staging Repository")
        ))
      )
      client.successful(request)
    }

    // Should I use promote independently?
    // https://oss.sonatype.org/nexus-staging-plugin/default/docs/path__staging_profiles_-profileIdKey-_finish.html
    def promoteStagingRepo(profileId: String, repoId: String): F[Boolean] = {
      val request = Request[F](
        Method.POST,
        baseUri / "staging" / "profiles" / profileId / "finish",
        HttpVersion.`HTTP/1.1`,
        staticHeaders
      ).withEntity(
        Json.obj("data" -> Json.obj(
          "stagedRepositoryId" -> Json.fromString(repoId),
          "description" -> Json.fromString(s"Promoting Staging Repository")
        ))
      )
      client.successful(request)
    }

    // https://oss.sonatype.org/nexus-staging-plugin/default/docs/path__staging_profiles_-profileIdKey-_drop.html
    def dropStagingRepo(profileId: String, repoId: String): F[Status] = {
      val request = Request[F](
        Method.POST,
        baseUri / "staging" / "profiles" / profileId / "drop",
        HttpVersion.`HTTP/1.1`,
        staticHeaders
      ).withEntity(
        Json.obj("data" -> Json.obj(
          "stagedRepositoryId" -> Json.fromString(repoId),
          "description" -> Json.fromString(s"Dropping Staging Repository")
        ))
      )
      client.status(request)
    }

  }



}