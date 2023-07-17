test_that("Return the coordinates and the distance to the next defibrillator", {
  expect_equal(closestDefi("München",11.50,48.10), "The distance to the next defibrillator is located in 529 meters.The coordinates are: (11.4976269,48.0957883).")
})
