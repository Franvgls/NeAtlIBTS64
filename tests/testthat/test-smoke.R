# test-smoke.R — las funciones principales no deben fallar con datos HH razonables

# Un helper que abre device nulo durante cada test y lo cierra al salir del bloque.
# local_pdf() es de testthat/withr y se limpia solo.
with_null_device <- function(code) {
  withr::with_pdf(tempfile(fileext = ".pdf"), code)
}

test_that("qcHaulsDist arranca sin error con datos SP-NORTH", {
  hh_sp <- readRDS(testthat::test_path("fixtures", "fake_hh_spnorth.rds"))
  with_null_device(
    expect_no_error(
      qcHaulsDist(Survey = hh_sp, years = 2024, quarter = 4,
                  getICES = FALSE, error = "Dist", pc.error = 2)
    )
  )
})

test_that("qcHaulsPosit arranca sin error (SP-NORTH e IE-IGFS)", {
  hh_sp <- readRDS(testthat::test_path("fixtures", "fake_hh_spnorth.rds"))
  hh_ie <- readRDS(testthat::test_path("fixtures", "fake_hh_ieigfs.rds"))
  with_null_device({
    expect_no_error(
      qcHaulsPosit(Survey = hh_sp, years = 2024, quarter = 4,
                   getICES = FALSE, Hpoints = FALSE, ti = FALSE)
    )
    expect_no_error(
      qcHaulsPosit(Survey = hh_ie, years = 2024, quarter = 4,
                   getICES = FALSE, Hpoints = TRUE, ti = FALSE)
    )
  })
})

test_that("SurveyMap.IBTS arranca sin error (Sweeps y Country)", {
  hh_sp <- readRDS(testthat::test_path("fixtures", "fake_hh_spnorth.rds"))
  with_null_device({
    expect_no_error(
      SurveyMap.IBTS(Survey = hh_sp, Year = 2024, Quarter = 4,
                     sweeplngt = TRUE,  country = FALSE, getICES = FALSE)
    )
    expect_no_error(
      SurveyMap.IBTS(Survey = hh_sp, Year = 2024, Quarter = 4,
                     sweeplngt = FALSE, country = TRUE,  getICES = FALSE)
    )
  })
})

test_that("MapLengths arranca sin error", {
  skip("MapLengths requiere datos HL (CPUE por tallas); fixture HH insuficiente")
  # Firma real: MapLengths(esp, dtSurv, dtyear, dtq, tmin, tmax, ...)
  # Se validará en una segunda tanda con fixtures HL sintéticos.
})
