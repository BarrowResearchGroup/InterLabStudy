# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

#package version: 0.0.1.1
#last update 28/11/2019

#pkgload::load_all()
options( "golem.app.prod" = TRUE)
InterLabStudy::run_app() # add parameters here (if any)
