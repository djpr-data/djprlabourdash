# This script knits the briefing document then emails it to Matt Cowgill
# This script is sourced from data-raw/internal_data.R when data is updated
# It's only run in non-interactive contexts (ie. on GitHub Actions)

pkgload::load_all()

if (!interactive()) {
  print("Knitting briefing document...")
  briefing_doc <- knit_briefing()
  email <- blastula::compose_email(body = "Latest jobs briefing attached")
  email <- blastula::add_attachment(email, briefing_doc)
  print("Sending briefing email...")
  blastula::smtp_send(email, to = '${{secrets.EMAIL_TO}}', from = '${{secrets.EMAIL_FROM}}', subject = "Latest jobs briefing", credentials = blastula::creds_envvar(user = '${{secrets.EMAIL_FROM}}', provider = "gmail"))
}
