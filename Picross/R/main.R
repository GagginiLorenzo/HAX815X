library(shiny)
library(rmarkdown)
library(bslib)
library(png)
# scripte qui génère l'application ( ui et server ), il ne s'occupe que de la fenetre du jeux,
# l'ui globale du site serra faites avec quarto plus tard
#source : https://gallery.shinyapps.io/094-image-interaction-basic/
#source : https://shiny.posit.co/r/articles/build/plot-interaction-advanced/

######### nn() = dim de la grille


######## m = grille de base

gri<- function(m,input,height,width){
  print(input)
  cli=0
  if (is.null(input)==FALSE){
    cli=cli+1}
  if (cli>0)
  {
    print("cl")
    m[seq_len(ceiling(100)) , ] <- 0.75
    m[, seq_len(ceiling(100)) ]  <- 0.75
  }
  return (array(c(m,m,m), dim = c(height, width, 3)))}

nonogram<-function()
{

  ui <- page_fillable(


    fluidRow(textOutput(outputId = "fin")),
    layout_columns(

      card(mainPanel(verbatimTextOutput("guess_row"),width = 100),height = 5*100),
      card(mainPanel(verbatimTextOutput("guess_col"),width = 100),height = 5*100),
      card(card_header(
               br(),
               # In a imageOutput, passing values for click, dblclick, hover, or brush
               # will enable those interactions.
               imageOutput("image1", height = 5*100,
                           # Equivalent to: click = clickOpts(id = "image_click")
                           click = "image_click",
                           dblclick = dblclickOpts(
                             id = "image_dblclick"
                           ),
                           hover = hoverOpts(
                             id = "image_hover"
                           ),
                           brush = brushOpts(
                             id = "image_brush"
                           )
               ),
               br()
        )),
      card(actionButton("nn",label = "taille de grille"),
           numericInput("n", "n", 5)),
      col_widths = c(-1,11, 1, 5,2),
      row_heights = c(1,10, 5,1)
      )
  )


  server <- function(input, output, session) {
    ######INIT###


    ######JEUX#####

    ######################


    observeEvent(input$nn,{output$image1  <- renderImage(
          {

            nn<-eventReactive(input$"nn", {
              input$n
            })

            width  <-  nn()*100+1
            height <- nn()*100+1
            m <- matrix(1, nrow = height, ncol = width)

            m[seq_len(ceiling(nn())) * 100 - 99, ] <- 0.75
            m[, seq_len(ceiling(nn())) * 100 - 99]  <- 0.75
            m[,width]<- 0.75
            m[height,]<- 0.75


            guess=m #grille a deviner
            nb_guess=sample(c(5:floor(nn()^2*(2/3))),size = 1) #nombre de case a deviner

            ql_guess<- (100*runif(n=nb_guess,min=0,max=(nn()))) %/% 100
            qc_guess<- (100*runif(n=nb_guess,min=0,max=(nn()))) %/% 100
            for (square in (1:nb_guess))
            {
              print(ql_guess)
              print(qc_guess)
              print(square)
              guess[seq((ql_guess[square]*100+1),(ql_guess[square]+1)*100,1),seq((qc_guess[square]*100+1),(qc_guess[square]+1)*100,1)]<-0.75
            }


            indice_guess=data.frame(cbind(ql_guess,qc_guess))
            indice_guess_list <- split(indice_guess, indice_guess$ql_guess)

            guess_row=list()
            guess_col=list()

            for (i in (1:length(indice_guess_list)))
            {
              df<-unique.data.frame(indice_guess_list[[i]])
              df=df[order(df$qc_guess),]
              df$sequence <- c(FALSE, head(df$qc_guess,-1)) + 1 == df$qc_guess
              longueur_vecteur<-length(df$sequence)
              indice<-1
              vec<-c(1)
              if(longueur_vecteur>1){
                for (i in (2:longueur_vecteur)){
                  if (df$sequence[i]==FALSE){
                    indice=indice+1
                    vec[indice]=0
                  }
                  vec[indice]=vec[indice]+1
                }
              }
              vec=list(vec)
              guess_row[length(guess_row)+1]=vec
            }
            print("BBBBBBB")
            print(guess_row)
            ############

            #######colclue

            indice_tguess=data.frame(cbind(ql_guess,qc_guess))
            indice_tguess_list <- split(indice_guess, indice_guess$qc_guess)


            for (i in (1:length(indice_tguess_list)))
            {
              df<-unique.data.frame(indice_tguess_list[[i]])
              df=df[order(df$ql_guess),]
              df$sequence <- c(FALSE, head(df$ql_guess,-1)) + 1 == df$ql_guess
              longueur_vecteur<-length(df$sequence)
              indice<-1
              vec<-c(1)
              if(longueur_vecteur>1){
                for (i in (2:longueur_vecteur)){
                  if (df$sequence[i]==FALSE){
                    indice=indice+1
                    vec[indice]=0
                  }
                  vec[indice]=vec[indice]+1
                }
              }
              vec=list(vec)
              guess_col[length(guess_col)+1]=vec
              print("AAAAAA")
              print(guess_col)
            }
            ########



            output$guess_row<- renderPrint(width = 50,{
              head=1
              scale=nn()-4
              if (nn()==4) { scale<-nn()-1}
              for (k in (1:length(guess_col)))
              {

                cat(c("  |  ",guess_col[[k]]))

              }})

            output$guess_col <- renderPrint(width = 50,{
              head=1
              scale=nn()-4
              if (nn()==4) { scale<-nn()-1}
              for (k in (1:length(guess_row)))
              {
                cat(rep("\n",head))
                cat(c(k," | ",guess_row[[k]]))
                cat(rep("\n",scale))
              }})
            M <-reactiveValues(val=list(m,guess)) #table de jeu

            observeEvent(input$image_click,
                         {

                           if(all.equal(M$val[[1]] ,M$val[[2]])==1)
                           {
                             output$fin <- renderText("FINI :D")
                           }
                           else
                           {
                             print("clicked")
                             ql<-input$image_click$y %/% 100
                             qc<-input$image_click$x %/% 100
                             print(ql)
                             print(qc)
                             if(M$val[[1]][ql*100+2,qc*100+2] == 0.75)
                             {
                               M$val[[1]][seq((ql*100+1),(ql+1)*100,1),seq((qc*100+1),(qc+1)*100,1)] <- 1
                             }
                             else{
                               M$val[[1]][seq((ql*100+1),(ql+1)*100,1),seq((qc*100+1),(qc+1)*100,1)] <- 0.75
                             }
                             M$val[[1]][seq_len(ceiling(nn())) * 100 - 99, ] <- 0.75
                             M$val[[1]][, seq_len(ceiling(nn())) * 100 - 99]  <- 0.75
                             M$val[[1]][,width]<- 0.75
                             M$val[[1]][height,]<- 0.75
                           }
                         })
            outfile <- tempfile(fileext = ".png")


            img <- array(c(M$val[[1]],M$val[[1]],M$val[[1]]), dim = c(height, width, 3))
            #img <- array(c(guess,guess,guess), dim = c(height, width, 3))
            writePNG(img, target = outfile)

            list(
              src = outfile,
              contentType = "image/png",
              width = width,
              height = height,
              alt = "This is alternate text"
            )

          },deleteFile = TRUE
        )})
      }
  shinyApp(ui, server)
}
nonogram()
