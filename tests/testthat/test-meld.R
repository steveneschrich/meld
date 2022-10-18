test_that("meld works", {
  expect_equal(c("A","B","C") %^% c("A",NA,"C"), c("A","B","C"))
  expect_equal(vec_meld(c("A","B","C"), c("A",NA,"C")), c("A","B","C"))
})

test_that("mismatch meld", {
  expect_error(c("A","B","C") %^% c("A",NA,"D"))
})
test_that("melding data frames", {
  expect_equal(
    meld(
      data.frame(F1=c("A","B","C"), F2=c("A",NA,"C")),
      new = c("F1","F2")
    ),
    data.frame(new=c("A","B","C"))
  )
  expect_error(
    suppressWarnings(
      meld(
        data.frame(F1=c("A","B","C"), F2=c("B",NA,"C")),
        new = c("F1","F2")
      )
    )
  )
})

test_that("melding data frames when unequal", {
  expect_equal(
    suppressWarnings({
    meld(
      data.frame(F1=c("A","B","B1","B2","C"), F2=c("A",NA,"B1","B12","C")),
      new = c("F1","F2"),
      drop = TRUE
    )}),
    data.frame(new=c("A","B","B1","C"))
  )
  expect_error(
     suppressWarnings(
       meld(
         data.frame(F1=c("A","B","B1","B2","C"), F2=c("A",NA,"B1","B12","C")),
         new = c("F1","F2")
       )
     )
  )
})

