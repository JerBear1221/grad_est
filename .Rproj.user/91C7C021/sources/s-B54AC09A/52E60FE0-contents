#' @title Estimate grad admissions decisions
#
#' @description This function uses submissions on GradCafe to estimate deadline for graduate admissions.The function has two arguments: "x" and "y". These are links to your respective program on grad cafe. Try adding the abbreviated university name (USC) and the full university name (University of South Carolina / Southern California).
#
#
#' @param x
#
#' @param y
#
#' @return paste("You should hear SOMETHING  by", estd)
#' @return paste("You should hear about Interviews by", estdi)
#' @return paste("paste("You should hear about Acceptance / Rejections by ", estdar))
#
#' @export
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

grad=function(x,y){
  #just one link?
  if(missing(y)) {
    #inserting the link
    pat="U.*$"
    require(rvest)
    require(stringr)
    require(dplyr)
    #converting link to text
    h=read_html(x)
    nodes=h %>%
      html_nodes(".tcol3")
    #cleaning up text
    gadmin=sapply(nodes, html_text)[2:length(nodes)]
    gadmin=sub('.*on', '', gadmin)
    a=sub('.*on', '', gadmin)
    gadmin=sub(pat, '', gadmin) %>%
      as.Date("%d %B %Y")
    #finishing
    gadmint_c=gadmin %>%
      as.Date("%m-%d")
    origin_date <- as.Date("2021-01-01")
    #date estimates
    lengthp=as.numeric(julian(gadmint_c, origin_date))
    lengthp=ifelse(lengthp >=200, lengthp-365, lengthp)
    lengthp=ifelse(lengthp > 5.5*sd(lengthp) | lengthp < -5.5*sd(lengthp), NA, lengthp)
    estd=as.Date("2021-01-01")+round(mean(lengthp, na.rm=T),0)
    estd=format(estd, format="%m-%d")
    print(paste("You should hear SOMETHING  by", estd))
    invisible(estd)
    #interview estimates
    nodes_i=nodes[which(str_extract(as.character(nodes), "Interview") =="Interview")]
    gadmin_i=sapply(nodes_i, html_text)
    gadmin_i=sub('.*on', '', gadmin_i)
    gadmin_i= sub(pat, '', gadmin_i) %>%
      as.Date("%d %B %Y")
    #finishing interviews
    gadmint_ci=gadmin_i%>%
      as.Date("%m-%d")
    origin_date <- as.Date("2021-01-01")
    #interview estimates
    lengthpi=as.numeric(julian(gadmint_ci, origin_date))
    lengthpi=ifelse(lengthpi >=200, lengthpi-365, lengthpi)
    lengthpi=ifelse(lengthpi > 5.5*sd(lengthpi) | lengthpi < -5.5*sd(lengthpi), NA, lengthpi)
    estdi=as.Date("2021-01-01")+round(mean(lengthpi, na.rm=T),0)
    estdi=format(estdi, format="%m-%d")
    print(paste("You should hear about Interviews by ", estdi))
    invisible(estdi)
    #acceptance/rejectance estimates
    nodes_r=nodes[c(which(str_extract(as.character(nodes), "Rejected") =="Rejected"))]
    nodes_a=nodes[c(which(str_extract(as.character(nodes), "Accepted") =="Accepted"))]
    gadmin_r=sapply(nodes_r, html_text)
    gadmin_a=sapply(nodes_a, html_text)
    gadmin_r=sub('.*on', '', gadmin_r)
    gadmin_a=sub('.*on', '', gadmin_a)
    gadmin_r= sub(pat, '', gadmin_r) %>%
      as.Date("%d %B %Y")
    gadmin_a= sub(pat, '', gadmin_a) %>%
      as.Date("%d %B %Y")
    #finishing acceptance/rejectance
    gadmint_car_r=gadmin_r %>%
      as.Date("%M-%d")
   gadmint_car_a=gadmin_a %>%
      as.Date("%m-%d")
    origin_date <- as.Date("2021-01-01")
    #acceptance/rejections estimates
    lengthpar_r=as.numeric(julian(gadmint_car_r, origin_date))
    lengthpar_a=as.numeric(julian(gadmint_car_a, origin_date))
    lengthpar_r=ifelse(lengthpar_r >=200, lengthpar_r-365, lengthpar_r)
    lengthpar_r=ifelse(lengthpar_r > 5.5*sd(lengthpar_r, na.rm=T) | lengthpar_r < -5.5*sd(lengthpar_r, na.rm=T), NA, lengthpar_r)
    lengthpar_a=ifelse(lengthpar_a >=200, lengthpar_a-365, lengthpar_a)
    lengthpar_a=ifelse(lengthpar_a > 5.5*sd(lengthpar_a) | lengthpar_a < -5.5*sd(lengthpar_a), NA, lengthpar_a)
    estdar_r=as.Date("2021-01-01")+mean(lengthpar_r, na.rm=T)
    estdar_a=as.Date("2021-01-01")+mean(lengthpar_a, na.rm=T)
    estdar_r=format(estdar_r, format="%m-%d")
    estdar_a=format(estdar_a, format="%m-%d")
    print(paste("You should hear about  Rejections by", estdar_r))
    print(paste("You should hear about  Acceptances by", estdar_a))
    invisible(estdar_r)
    invisible(estdar_a)
  } else {
    #just two links?
    #inserting the link
    pat="U.*$"
    require(rvest)
    require(dplyr)
    #converting link to text
    hx=read_html(x)
    hy=read_html(y)
    nodesx=hx %>%
      html_nodes(".tcol3")
    nodesy=hy %>%
      html_nodes(".tcol3")
    #cleaning up text
    gadminx=sapply(nodesx, html_text)[2:length(nodesx)]
    gadminy=sapply(nodesy, html_text)[2:length(nodesy)]
    gadminx=sub('.*on', '', gadminx)
    ax=sub('.*on', '', gadminx)
    gadminx=sub(pat, '', gadminx) %>%
      as.Date("%d %B %Y")
    gadminy=sub('.*on', '', gadminy)
    ay=sub('.*on', '', gadminy)
    gadminy=sub(pat, '', gadminy) %>%
      as.Date("%d %B %Y")
    gadmin=c(gadminx, gadminy)
    #finishing
    gadmint_c=gadmin%>%
      as.Date("%m-%d")
    origin_date <- as.Date("2021-01-01")
    #date estimates
    lengthp=as.numeric(julian(gadmint_c, origin_date))
    lengthp=ifelse(lengthp >=200, lengthp-365, lengthp)
    lengthp=ifelse(lengthp > 5.5*sd(lengthp) | lengthp < -5.5*sd(lengthp), NA, lengthp)
    estd=as.Date("2021-01-01")+mean(lengthp, na.rm=T)
    estd=format(estd, format="%m-%d")
    print(paste("You should hear SOMETHING  by", estd))
    invisible(estd)
    #interview estimates
    nodes_ix=nodesx[which(str_extract(as.character(nodesx), "Interview") =="Interview")]
    nodes_iy=nodesy[which(str_extract(as.character(nodesy), "Interview") =="Interview")]
    gadmin_ix=sapply(nodes_ix, html_text)
    gadmin_iy=sapply(nodes_iy, html_text)
    gadmin_ix=sub('.*on', '', gadmin_ix)
    gadmin_ix= sub(pat, '', gadmin_ix) %>%
      as.Date("%d %B %Y")
    gadmin_iy=sub('.*on', '', gadmin_iy)
    gadmin_iy= sub(pat, '', gadmin_iy) %>%
      as.Date("%d %B %Y")
    #finishing interviews
    gadmint_ci=c(gadmin_ix, gadmin_iy)
    gadmint_ci=as.character(gadmint_ci)%>%
      substring(6)%>%
      as.Date("%m-%d")
    origin_date <- as.Date("2021-01-01")
    #interview estimates
    lengthpi=as.numeric(julian(gadmint_ci, origin_date))
    lengthpi=ifelse(lengthpi >=200, lengthpi-365, lengthpi)
    lengthpi=ifelse(lengthpi > 5.5*sd(lengthpi) | lengthpi < -5.5*sd(lengthpi), NA, lengthpi)
    estdi=as.Date("2021-01-01")+round(mean(lengthpi, na.rm=T),0)
    estdi=format(estdi, format="%m-%d")
    print(paste("You should hear about Interviews by ", estdi))
    invisible(estdi)
    #acceptance/rejectance estimates
    nodes_rx=nodesx[c(which(str_extract(as.character(nodesx), "Rejected") =="Rejected"))]
    nodes_ax=nodesx[which(str_extract(as.character(nodesx), "Accepted") =="Accepted")]
    nodes_ry=nodesy[c(which(str_extract(as.character(nodesy), "Rejected") =="Rejected"))]
    nodes_ay=nodesy[which(str_extract(as.character(nodesy), "Accepted") =="Accepted")]
    gadmin_rx=sapply(nodes_rx, html_text)
    gadmin_ax=sapply(nodes_ax, html_text)
    gadmin_ry=sapply(nodes_ry, html_text)
    gadmin_ay=sapply(nodes_ay, html_text)
    gadmin_rx=sub('.*on', '', gadmin_rx)
    gadmin_ax=sub('.*on', '', gadmin_ax)
    gadmin_ry=sub('.*on', '', gadmin_ry)
    gadmin_ay=sub('.*on', '', gadmin_ay)
    gadmin_rx= sub(pat, '', gadmin_rx) %>%
      as.Date("%d %B %Y")
    gadmin_ax=sub(pat, '', gadmin_ax) %>%
      as.Date("%d %B %Y")
    gadmin_ry= sub(pat, '', gadmin_ry) %>%
      as.Date("%d %B %Y")
    gadmin_ay=sub(pat, '', gadmin_ay) %>%
      as.Date("%d %B %Y")
    gadmin_r=c(gadmin_rx, gadmin_ry)
    gadmin_a=c(gadmin_ax, gadmin_ay)
    #finishing acceptance/rejectance
    gadmint_c_r=gadmin_r%>%
      as.Date("%m-%d")
    gadmint_c_a=gadmin_a%>%
      as.Date("%m-%d")
    origin_date <- as.Date("2021-01-01")
    #acceptance/rejections estimates
    lengthpar_r=as.numeric(julian(gadmint_c_r, origin_date))
    lengthpar_a=as.numeric(julian(gadmint_c_a, origin_date))
    lengthpar_r=ifelse(lengthpar_r >=200, lengthpar_r-365, lengthpar_r)
    lengthpar_r=ifelse(lengthpar_r > 5.5*sd(lengthpar_r) | lengthpar_r < -5.5*sd(lengthpar_r), NA, lengthpar_r)
    lengthpar_a=ifelse(lengthpar_a >=200, lengthpar_a-365, lengthpar_a)
    lengthpar_a=ifelse(lengthpar_a > 5.5*sd(lengthpar_a) | lengthpar_a < -5.5*sd(lengthpar_a), NA, lengthpar_a)
    estd_a=as.Date("2021-01-01")+mean(lengthpar_a, na.rm=T)
    estd_r=as.Date("2021-01-01")+mean(lengthpar_r, na.rm=T)
    estd_r=format(estd_r, format="%m-%d")
    estd_a=format(estd_a, format="%m-%d")
    print(paste("You should hear about Acceptance by ", estd_a))
    print(paste("You should hear about Rejection by ", estd_r))
    invisible(estd_a)
    invisible(estd_r)
  }
}

