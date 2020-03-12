recreate_limits <- function(limits_old = wqbc::limits, limits_new = limits_new){

  x <- limits_new %>%
    dplyr::filter(Use == "Aquatic Life - Freshwater",
                  Direction == "Upper Limit",
                  Media == "Water") %>%
    dplyr::mutate(Use = "Freshwater Life") %>%
    dplyr::mutate(Term = dplyr::if_else(Days == 30, "Long", "Short"))

  ### cases with multiple EMS_CODE
  x <- x %>%
    dplyr::group_by(Variable) %>%
    dplyr::mutate(EMS_Code2 = dplyr::first(EMS_Code)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(EMS_Code2 == EMS_Code)

  ### remove cases with ConditionNotes
  x <- x %>%
    dplyr::filter(is.na(ConditionNotes)) %>%
    dplyr::mutate(Variable = paste(Variable, Component),
                  UpperLimit = Limit) %>%
    dplyr::select(Variable, EMS_Code, Use,
                  Term, Condition, UpperLimit, Units)

  ### remove Mercury deuplicate
  x <- x %>%
    dplyr::filter(!(Variable == "Mercury Total" & UpperLimit == 0.02))

  y <- x %>%
    dplyr::group_by(Variable, Use, Term, Condition) %>%
    dplyr::mutate(n = dplyr::n())

  ### ensure that no duplicates
  expect_true(all(y$n == 1))

  x
}
