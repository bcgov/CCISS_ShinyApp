observeEvent(input$cciss_instructions_select_sites, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_select_sites")
})

observeEvent(input$cciss_instructions_feasibility_report, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_feasibility_report")
})

observeEvent(input$cciss_instructions_bec_futures, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_bec_futures")
})

observeEvent(input$cciss_instructions_silvics_ecology, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_silvics_ecology")
})

observeEvent(input$cciss_instructions_species_portfolio, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_species_portfolio")
})

observeEvent(input$cciss_instructions_export, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_export")
})