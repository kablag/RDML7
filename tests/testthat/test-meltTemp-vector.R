test_that("meltTemp vector survives RDML XML round trip", {

  x <- dataType(
    targetId = idReferenceType("target1"),
    meltTemp = c(87.8, 82.2)
  )

  expect_equal(
    x$meltTemp,
    c(87.8, 82.2)
  )

  xml <- .rdmlXmlNode(
    x,
    "data"
  )

  expect_match(
    xml,
    "<meltTemp>87.8</meltTemp>"
  )

  expect_match(
    xml,
    "<meltTemp>82.2</meltTemp>"
  )

  expect_lt(
    regexpr("<meltTemp>87.8</meltTemp>", xml)[[1L]],
    regexpr("<meltTemp>82.2</meltTemp>", xml)[[1L]]
  )
})

