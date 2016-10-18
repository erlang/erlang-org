%% Specific modules to include in cover.
{
 incl_mods,
 [
  erlorg,
  erlorg_articles,
  erlorg_articles_repo,
  erlorg_base_handler,
  erlorg_binary,
  erlorg_community_handler,
  erlorg_datetime,
  erlorg_downloads_handler,
  erlorg_versions_handler,
  erlorg_events_handler,
  erlorg_eeps_handler,
  erlorg_index_handler,
  erlorg_link_categories,
  erlorg_link_categories_repo,
  erlorg_links,
  erlorg_links_repo,
  erlorg_mailinglists,
  erlorg_news_handler,
  erlorg_plain_handler,
  erlorg_releases,
  erlorg_releases_repo,
  erlorg_req,
  erlorg_rss_handler,
  erlorg_store_pgsql,
  erlorg_sup
 ]
}.
%% Export coverage data for jenkins.
{export, "logs/cover.data"}.
