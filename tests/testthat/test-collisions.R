test_that("identify collisions works", {
  expect_equal(identify_collisions(data.frame(A.x=c(1,2), A.y=c(2,3), B=c(0,2),
                                              C.x=c(1,2), C.y=c(2,3))),
               c("A","C")
  )
  expect_equal(
    identify_collisions(dplyr::left_join(iris,iris,by="Species")),
    colnames(iris)[1:4]
  )
})

test_that("Melding collisions works", {
  foo <- dplyr::mutate(iris, rn = dplyr::row_number())
  expect_equal(
    meld_collisions(dplyr::left_join(foo,foo, by="rn")),
    foo
  )

})
