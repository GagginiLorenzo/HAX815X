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

