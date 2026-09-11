helper_dir <- normalizePath(
  file.path(testthat::test_path(), "..", "..", "data-raw", "R"),
  mustWork = FALSE
)
if (!dir.exists(helper_dir)) {
  testthat::skip("data-raw helpers are available in the source checkout")
}

source(file.path(helper_dir, "pipeline_helpers.R"), local = environment())
source(file.path(helper_dir, "extract_2026.R"), local = environment())
source(file.path(helper_dir, "tidy_2026.R"), local = environment())
source(file.path(helper_dir, "validate.R"), local = environment())
source(file.path(helper_dir, "validate_2026.R"), local = environment())

fixture_pdf <- function() {
  pages <- c(
    "cover",
    paste(
      c(
        "SISTEMA DE PONTOS",
        "Automóveis 0 4 8 10 11",
        "Geladeiras 0 6 10 12 13",
        "Microcomputadores 0 6 11 13 15",
        "Banheiros 0 9 15 19 20",
        "Lavadora de louças 0 5",
        "Lavadora de roupas 0 6",
        "Micro-ondas 0 7",
        "Água encanada 0 10",
        "Serviço doméstico 0 5",
        "Sem Instrução 0",
        "Ensino Fundamental Incompleto 3",
        "Ensino Fundamental Completo 4",
        "Ensino Médio Incompleto 5",
        "Ensino Médio Completo 6",
        "Ensino Superior Incompleto 6",
        "Ensino Superior Completo 8"
      ),
      collapse = "\n"
    ),
    paste(
      c(
        "Distribuição das classes",
        "1 - A 3,6% 4,4% 4,9% 1,5% 6,1% 0,8%",
        "2 - B1 4,9% 6,3% 6,7% 2,0% 6,2% 1,5%",
        "3 - B2 15,1% 19,0% 20,3% 7,5% 18,2% 6,7%",
        "4 - C1 21,2% 25,1% 28,6% 13,2% 21,2% 12,4%",
        "5 - C2 28,1% 28,3% 27,9% 27,5% 29,1% 29,6%",
        "6 - DE 27,1% 16,9% 11,6% 48,3% 19,2% 49,0%",
        "1 - A 5,7% 5,6% 7,7% 5,1% 5,0% 6,7% 17,4% 4,2% 2,8% 3,9%",
        "2 - B1 7,0% 6,3% 9,2% 7,6% 6,3% 8,8% 13,1% 4,6% 3,1% 4,0%",
        "3 - B2 18,9% 19,4% 22,7% 22,3% 16,6% 21,5% 22,4% 12,4% 9,7% 12,5%",
        "4 - C1 24,1% 29,0% 29,9% 27,6% 21,8% 24,2% 21,2% 18,3% 16,1% 16,6%",
        "5 - C2 27,4% 28,1% 22,5% 28,2% 28,4% 23,2% 17,7% 29,5% 30,6% 31,2%",
        "6 - DE 16,9% 11,6% 8,0% 9,2% 21,9% 15,6% 8,2% 31,0% 37,7% 31,8%",
        "Cortes do Critério Brasil",
        "1 – A 73 – 100",
        "2 - B1 66 – 72",
        "3 - B2 55 – 65",
        "4 - C1 46 - 54",
        "5 - C2 35 – 45",
        "6 - DE 0 – 34"
      ),
      collapse = "\n"
    ),
    paste(
      c(
        "Renda Média Domiciliar",
        "A R$ 28.331,26",
        "B1 R$ 13.636,18",
        "B2 R$ 7.874,72",
        "C1 R$ 4.526,88",
        "C2 R$ 2.648,30",
        "DE R$ 1.177,55"
      ),
      collapse = "\n"
    )
  )

  result <- structure(
    list(text = pages, data = NULL),
    class = c("cceb_pdf", "list")
  )

  return(result)
}

test_that("the 2026 PDF links can be discovered", {
  html <- paste0(
    "<a href='https://abep.org/wp-content/uploads/2026/03/CCEB_2026.pdf'>",
    "Português</a>",
    "<a href='https://abep.org/wp-content/uploads/2026/03/CCEB-2026-Eng.pdf'>",
    "Inglês</a>"
  )
  links <- get_cceb_links(2026L, html = html)

  expect_identical(links$lang, c("pt", "en"))
  expect_identical(
    links$file_name,
    c("CCEB_2026.pdf", "CCEB-2026-Eng.pdf")
  )
})

test_that("the 2026 source is extracted and tidied", {
  data <- tidy_2026(extract_2026_raw(fixture_pdf()))

  validate_2026_data(data)
  expect_identical(nrow(data$cceb_points), 37L)
  expect_identical(nrow(data$cceb_distribution), 96L)
  expect_identical(
    data$cceb_points$points[
      data$cceb_points$variable == "automobiles"
    ],
    c(0L, 4L, 8L, 10L, 11L)
  )
  expect_identical(data$cceb_income$income_mean[[1]], 28331.26)
  expect_identical(
    data$cceb_editions$effective_date[[1]],
    as.Date("2026-02-05")
  )
})

test_that("a missing source point row fails loudly", {
  pdf <- fixture_pdf()
  pdf$text[[2]] <- sub(
    "Banheiros 0 9 15 19 20\n",
    "",
    pdf$text[[2]],
    fixed = TRUE
  )

  expect_snapshot(error = TRUE, extract_2026_raw_points(pdf))
})

test_that("a changed distribution total fails validation", {
  data <- tidy_2026(extract_2026_raw(fixture_pdf()))
  data$cceb_distribution$share[[1]] <- data$cceb_distribution$share[[1]] + 0.05

  expect_snapshot(error = TRUE, validate_2026_data(data))
})
