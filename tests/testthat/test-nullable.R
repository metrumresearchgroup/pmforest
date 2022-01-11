describe("nullable", {
  real_val <- 2
  null_val <- NULL
  alternative <- 1
  it("returns real val if not null", {
    res_func <- nullable(real_val, alternative)
    res_infix <- real_val %||% alternative
    expect_equal(res_func, real_val)
    expect_equal(res_infix, real_val)
  })
  it("returns alternative if null", {
    res_func <- nullable(null_val, alternative)
    res_infix <- null_val %||% alternative
    expect_equal(res_func, alternative)
    expect_equal(res_infix, alternative)
  })
})
