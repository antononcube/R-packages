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

dashboardPage(
  dashboardHeader( title = "SMRMon Dashboard" ),

  dashboardSidebar(
    sidebarMenu(
      menuItem( "SMRMon Object", tabName = "SMRMonObject", icon = icon("map") ),
      menuItem( "Search & history", tabName = "SearchAndHistory", icon = icon("search-plus") ),
      menuItem( "Recommendations", tabName = "Recommendations", icon = icon("blender") ),
      menuItem( "Statistics", tabName = "Statistics", icon = icon("chart-bar") )
    )
  ),

  dashboardBody(
    tabItems(

      ## SMRMon object oboarding
      tabItem( tabName = "SMRMonObject",
               fluidRow(

                 h2( "SMRMon object oboarding" ),

                 box(
                   title = "SMRMon object to experiment with",

                   textInput( inputId = "smrObjName", label = "SMRMon object name:", value = "smrMushroom" )
                 ),

                 box(
                   title = "Properties summary",

                   verbatimTextOutput("smrSummary"),

                   div( DT::dataTableOutput("tagTypeRangesTable"), style = "font-size: 75%; width: 75%")

                 )

               )
      ),


      ## Search and history accumulation.
      tabItem( tabName = "SearchAndHistory",
               fluidRow(

                 h2( "Search and history accumulation" ),

                 box(
                   title = "Search",

                   textInput( inputId = "searchString", label = "Search string:",
                              value = ".*",
                              placeholder =  "pattern | <tag-type>:<tag>"),

                   checkboxGroupInput( inputId = "searchTagTypes", label = "Search tag types:",
                                       choices = as.character(1:10), selected = "1",
                                       inline = TRUE ),

                   checkboxInput( inputId = "fixedSearhPattern", label = "Fixed ?:", value = T )
                 ),

                 box(
                   title = "Search results",

                   DT::dataTableOutput("searchResultsTable")
                 ),

                 box(
                   title = "History",

                   DT::dataTableOutput("historyTable")
                 )
               )
      ),

      ## Recommendations and related profile
      tabItem( tabName = "Recommendations",
               fluidRow(

                 h2( "Recommendations" ),

                 box(
                   title = "Sliders",
                   div( uiOutput( "multiSliders" ), style = "font-size: 75%; width: 75%" )
                 ),

                 box(
                   title = "Profile",

                   DT::dataTableOutput("profileTable")
                 ),

                 box(
                   title = "Recommendations",

                   DT::dataTableOutput("recommendationsTable")
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
