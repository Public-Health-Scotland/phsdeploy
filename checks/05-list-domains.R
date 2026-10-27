# Check 5: list the custom domains on the Connect Cloud account.
#
# Expect two domains with the same name: a wildcard domain (one address per
# product) and a fixed domain (the landing page). The checks only ever use
# the wildcard domain.
#
# Needs: none.
#
# Changes: none.

source("checks/_helpers.R")

local({
  local_rsconnect_config()
  cld <- phs_connect_cld()

  check_step("Domains")
  domains <- cloud_list_domains(cld)
  print(domains)
  check_ok(
    "Found {sum(domains$wildcard %in% TRUE)} wildcard and {sum(domains$wildcard %in% FALSE)} fixed domain{?s}."
  )

  check_step("Addresses they give")
  for (i in seq_len(nrow(domains))) {
    domain <- domains[i, ]
    kind <- if (isTRUE(domain$wildcard)) "wildcard" else "fixed"
    cat(sprintf("  %-8s %s\n", kind, cloud_public_url(domain, "example-product")))
  }
})

check_done(5)
