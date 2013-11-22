library(scales)
library(extrafont)
suppressWarnings(loadfonts())

############################################################################################
###    Get a data.frame data
############################################################################################


### Load csv data

quora <- read.csv("data/quora.csv")
quora <- quora[,1:4]
# reformat %
quora[,4] <- as.numeric(gsub("\\%", "", as.character(quora[,4])))
colnames(quora) <- c("canton", "eligible", "threshold", "quorum")
quora[,1] <- as.character(quora[,1])

init <- read.csv("data/init.csv")
init[,1] <- gsub("( |c\\))", "", as.character(init[,1]))
init <- init[,1:2]
colnames(init) <- c('canton', 'initiative')


### merge datasets

idx <- match(quora$canton, init$canton)
stopifnot(all(idx))
df <- cbind(quora, initiative = init[idx,2 ])

# ignore NA and swiss average
idNA <- c(1,which(is.na(df$quorum)))

## round quorum value to 1 digit
df$quorum <- round(df$quorum, 1)

############################################################################################
###    slopegraph https://gist.github.com/leeper/7158678
############################################################################################


slopegraph <- function(
	df,
	xlim = c(.5,ncol(df)+.5),
	ylim = c(min(df)-diff(range(df))/100,max(df)+diff(range(df))/100),
	main = NULL,
	bty = 'n',
	yaxt = 'n',
	xaxt = 'n',
	xlab = '',
	ylab = '',
	add.exp = NULL, # an expression to add something between drawing the blank canvas and adding the plot content (i.e., behind the slopegraph)
	labels = names(df),
	labpos.left = 2,
	labpos.right = 4,
	col.lines = par('fg'),
	col.lab = par('fg'),
	col.num = par('fg'),
	col.xaxt = par('fg'),
	offset.x = .1,
	offset.lab = .1,
	cex.lab = 1,
	cex.num = 1,
	font.lab = 1,
	font.num = 1,
	lty = par("lty"),
	lwd = par("lwd"),
	mai = NULL,
	rescaleByColumn = T,
	...)
{
    if(ncol(df) < 2)
        stop('`df` must have at least two columns')
    # draw margins
    if(is.null(mai))
        par(mai=c(1, 0, if(is.null(main)) 0 else 1, 0))
    else
        par(mai=mai)

    plot(NA, y=NULL, xlim=xlim, ylim=ylim, main=main,
         bty=bty, yaxt=yaxt, xaxt=xaxt, xlab=xlab, ylab=ylab, ...)
    # optional expression
    if(!is.null(add.exp))
        eval(add.exp)
    # x-axis
    axis(1, 1:ncol(df), labels = labels, col=col.xaxt, col.ticks=col.xaxt)

	if(rescaleByColumn) {
    	range.bycol <- sapply(df, function(c) diff(range(c, na.rm = T)))
		rescale <-  range(df[,which.max(range.bycol)])
		df.rescale <- sapply(df, rescale, rescale)
	} else {
		df.rescale <- df
	}
	rownames(df.rescale) <- rownames(df)
    # left-side labels
    l <- df.rescale[,1] # I MAY WANT TO BIN THESE SO THAT CLOSE VALUES DON'T OVERLAP
    leftlabs <- lapply(split(rownames(df.rescale),l), paste, collapse=', ')
#browser()
    text(1-offset.lab,  as.numeric(names(leftlabs)),
         col = col.lab, leftlabs, pos = labpos.left, cex=cex.lab, font=font.lab)
    # right-side labels
    r <- df.rescale[,ncol(df)] # I MAY WANT TO BIN THESE SO THAT CLOSE VALUES DON'T OVERLAP
    rightlabs <- lapply(split(rownames(df.rescale),r), paste, collapse=',')
    text(ncol(df)+offset.lab, as.numeric(names(rightlabs)),
         col=col.lab, rightlabs, pos=labpos.right, cex=cex.lab, font=font.lab)
    # numeric value labels
    # deal with duplicate value labels (i.e., not double printing anything)
    df2 <- do.call(cbind,lapply(df, function(y) {y[duplicated(y)] <- ''; y}))
    # print them
    apply(cbind(df.rescale,df2),1, function(y)
  		  text(1:ncol(df), as.numeric(y[1:ncol(df)]), y[(ncol(df)+1):(2*ncol(df))],
              col=col.num, cex=cex.num, font=font.num)
    )
    # draw lines
    col.lines <- rep(col.lines, length.out=nrow(df))
    lty <- rep(lty, length.out=nrow(df))
    lwd <- rep(lwd, length.out=nrow(df))

    for(i in 1:nrow(df.rescale)){
        mapply(function(x1,y1,x2,y2,...){
            ysloped <- (y2-y1)*offset.x
            segments(x1+offset.x, if(y1==y2) y1 else (y1+ysloped),
                     x2-offset.x, if(y1==y2) y2 else (y2-ysloped),
                     col=col.lines[i],
                     lty=lty[i],
                     lwd=lwd[i]
                    )},
               1:(length(df.rescale[i,])-1), # x1-positions
               df.rescale[i,][-length(df.rescale[i,])], # y1-positions
               2:(length(df.rescale[i,])), # x2-positions
               df.rescale[i,][-1] # y2-positions
               )
    }
}


# EXAMPLE

# exclude NA and swiss values
dff <- df
rownames(dff) <- dff[,1]
dff <- dff [,-1]
dff <- dff[-idNA,3:4]

pdf("slope_qrVsinit.pdf", width = 10, height = 7)
slopegraph(dff, col.line='gray', xlim = c(-0.3, 2.5), labels = colnames(dff), cex.lab = 0.6, cex.num = 0.6, offset.x = 0.027)
dev.off()
