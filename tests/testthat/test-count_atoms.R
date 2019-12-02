context("test-count_atoms")

test_that("counting atoms in formula", {
    expect_equal(count_atoms("C6H12O6","C"), 6)
    expect_equal(count_atoms("C6 H12 O6","C"), 6)
    expect_equal(count_atoms("C06H12O6","C"), 6)
    expect_equal(count_atoms("C0006H12O6","C"), 6)
    expect_equal(count_atoms("C6H12O6","H"), 12)
    expect_equal(count_atoms("C6H00012O6","H"), 12)
    expect_equal(count_atoms("C6H12O6","O"), 6)
    expect_equal(count_atoms("C6H12O006","O"), 6)
    expect_equal(count_atoms("C6H12O6O3","O"), 9)
    
})

test_that("Atom no present return 0", {
    expect_equal(count_atoms("C6H12O6","N"), 0)
})

test_that("abort if input incorrect", {
    expect_error(count_atoms(C6H12O6,"C"), regexp = "object 'C6H12O6' not found")
    expect_error(count_atoms(as.factor("C6H12O6"),"C"), regexp = "formula is not of class character")
    expect_error(count_atoms(42,"C"), regexp = "formula is not of class character")
    expect_error(count_atoms("C6H12O6",C), regexp = "element is not of class character")
    expect_error(count_atoms("C6H12O6",42), regexp = "element is not of class character")
    expect_error(count_atoms("C6H12O6","NHS"), regexp = "element is not valid")
    expect_error(count_atoms("C", "C6H12O6"), regexp = "element longer than formula, did you invert the position of the 2?")
})







