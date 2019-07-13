##=======================================================================================
## General Sparse Matrix Recommender Dashboard - UI part
## Copyright (C) 2019  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Written by Anton Antonov,
## antononcube @ gmail . com,
## Windermere, Florida, USA.
##
##=======================================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dashboardthemes)

dashboardPage(
  dashboardHeader( title = "SMRMon Dashboard" ),

  dashboardSidebar(
    sidebarMenu(
      menuItem( "SMRMon Object", tabName = "SMRMonObject", icon = icon("map") ),
      menuItem( "Search & history", tabName = "SearchAndHistory", icon = icon("search-plus") ),
      menuItem( "Recommendations", tabName = "Recommendations", icon = icon("blender") ),
      menuItem( "Statistics", tabName = "Statistics", icon = icon("chart-bar") ),
      hr(),
      menuItem( "Sliders", tabName = "Sliders", icon = icon("sliders-h"),
                uiOutput( "multiSliders" )
      )
    )
  ),

  dashboardBody(
    ### Changing theme
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),

    tabItems(

      ## SMRMon object oboarding
      tabItem( tabName = "SMRMonObject",
               fluidRow(

                 h2( "SMRMon object oboarding" ),

                 box(
                   title = "SMRMon object to experiment with",

                   textInput( inputId = "smrObjName", label = "SMRMon object name:", value = "smrResumes" ),

                   textInput( inputId = "dfWideFormName", label = "Data frame to extend with:", value = "dfResumesWide" ),

                   width = 3
                 ),

                 box(
                   title = "Properties summary",

                   verbatimTextOutput("smrSummary"),

                   div( DT::dataTableOutput("tagTypeRangesTable"), style = "font-size: 75%; width: 75%"),

                   width = 6
                 )

               )
      ),


      ## Search and history accumulation.
      tabItem( tabName = "SearchAndHistory",
               fluidRow(

                 h2( "Search and history accumulation" ),

                 fluidRow(

                   box(
                     title = "Search",

                     textInput( inputId = "searchString", label = "Search string:",
                                value = ".*",
                                placeholder =  "pattern | <tag-type>:<tag>"),

                     checkboxInput( inputId = "fixedSearhPattern", label = "Fixed ?:", value = T ),

                     width = 6
                   ),


                   box(
                     title = "History items and ratings",

                     textInput( inputId = "itemList", label = "Item list:", value = "10" ),

                     textInput( inputId = "itemRatings", label = "Item star ratings:", value = "3"),

                     width = 6
                   )

                 ),

                 fluidRow(

                   box(
                     title = "Search results",

                     DT::dataTableOutput("searchResultsTable"),

                     width = 6
                   ),

                   box(
                     title = "History",

                     DT::dataTableOutput("historyTable"),

                     width = 6
                   )
                 )
               )
      ),

      ## Recommendations and related profile
      tabItem( tabName = "Recommendations",
               fluidRow(

                 h2( "Recommendations" ),

                 # box(
                 #   title = "Sliders",
                 #
                 #   div( uiOutput( "multiSliders" ), style = "font-size: 90%"),
                 #
                 #   width = 3
                 # ),

                 box(
                   title = "Recommendations (based on history)",

                   numericInput( inputId = "nrecs", label = "Number of Recommendations", min = 1, max = 900, step = 1, value = 12 ),

                   DT::dataTableOutput("recommendationsTable"),

                   width = 6
                 ),

                 box(
                   title = "Profile (based on history)",

                   DT::dataTableOutput("historyProfileTable"),

                   width = 6
                 )
               )
      ),


      ## Statistics
      tabItem( tabName = "Statistics",
               fluidRow(

                 h2( "Statistics" ),

                 box(
                   title = "Query",

                   textInput( inputId = "smrPropertyQuery", label = "Properties query:", value = "show the tag types" ),

                   verbatimTextOutput( "propertiesQueryResult" )
                 )

               )
      )



    )
  )
)
