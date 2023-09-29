
observeEvent(input$cciss_instructions_select_sites, {
  showModal(
    modalDialog(
      tags$iframe(
        style="height:600px; width:100%", src="select_sites.pdf"
      ),
      size = "xl",
      easyClose = TRUE
    )
  )
})

observeEvent(input$cciss_instructions_feasibility_report, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_feasibility_report")
})

observeEvent(input$cciss_instructions_bec_futures, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_bec_futures")
})

observeEvent(input$cciss_instructions_bec_futures_spatial, {
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

# Nav
observeEvent(input$cciss_about_nav, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_about")
})

observeEvent(input$cciss_instructions_select_sites_nav, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_select_sites")
})

observeEvent(input$cciss_instructions_feasibility_report_nav, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_feasibility_report")
})

observeEvent(input$cciss_instructions_bec_futures_nav, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_bec_futures")
})

observeEvent(input$cciss_instructions_silvics_ecology_nav, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_silvics_ecology")
})

observeEvent(input$cciss_instructions_species_portfolio_nav, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_species_portfolio")
})

observeEvent(input$cciss_instructions_export_nav, {
  updateNavbarPage(inputId = "cciss_navbar", selected = "cciss_instructions")
  updateTabsetPanel(inputId = "cciss_instructions_set", selected = "cciss_instructions_export")
})