#insumos datos
municipios<-readOGR("shape_municipal", "mpio", encoding = "UTF-8")
insumos_mapa$NDepto[insumos_mapa$NDepto == "NORTE DE SAN"] <- "NORTE DE SANTANDER"
insumos_mapa$NDepto[insumos_mapa$NDepto == "BOGOTA D.C."] <- "SANTAFE DE BOGOTA D.C"
insumos_mapa$NDepto[insumos_mapa$NDepto == "SAN ANDRES"] <- "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "BOGOTA. D.C."] <- "SANTAFE DE BOGOTA D.C."
insumos_mapa$NDepto[insumos_mapa$NDepto == "VALLE"] <- "VALLE DEL CAUCA"
municipios@data$NOMBRE_DPT[municipios@data$NOMBRE_DPT == "NARI\xd1O"] <- "NARIÑO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MIRITI PARANA"] <- "MIRITI-PARANA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "PUERTO NARIÑO"] <- "PUERTO NARIÑO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ANTIOQUIA"] <- "SANTAFE DE ANTIOQUIA"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "BRICE\xa51O"] <- "BRICEÑO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "CA\xa5SGORDA"] <- "CAÑASGORDAS"
insumos_mapa$NMpio[insumos_mapa$NMpio == "CARMEN DE VIBORAL"] <- "EL CARMEN DE VIBORAL"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SANTUARIO"] <- "EL SANTUARIO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "NARI\xa5O"] <- "NARIÑO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "BOLIVAR" & insumos_mapa$NDepto=="ANTIOQUIA"] <- "CIUDAD BOLIVAR"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "PE\xa5OL"] <- "PEÑOL"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "SAN JOSE DE LA MONTA\xa5A"] <- "SAN JOSE DE LA MONTAÑA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "YONDO-CASABE"] <- "YONDO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SANTACRUZ (GUACHAVES)"] <- "SANTACRUZ"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SANTA BARBARA (ISCUANDE)"] <- "SANTA BARBARA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ROBERTO PAYAN (SAN JOSE)"] <- "ROBERTO PAYAN"
insumos_mapa$NMpio[insumos_mapa$NMpio == "OLAYA HERRERA"] <- "OLAYA  HERRERA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ALBAN (SAN JOSE)"] <- "ALBAN"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ARBOLEDA (BERRUECOS)"] <- "ARBOLEDA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "CUASPUD (CARLOSAMA)"] <- "CUASPUD"
insumos_mapa$NMpio[insumos_mapa$NMpio == "COLON (GENOVA)"] <- "COLON"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "EL PE\xa5OL"] <- "EL PEÑOL"
insumos_mapa$NMpio[insumos_mapa$NMpio == "LOS ANDES (SOTOMAYOR)"] <- "LOS ANDES"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MAGUI (PAYAN)"] <- "MAGUI"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SANTACRUZ"] <- "SANTA CRUZ"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ALTO BAUDO (PIE DE PATO)"] <- "ALTO BAUDO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "AQUITANIA (PUEBLOVIEJO)"] <- "AQUITANIA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ARIGUANI (EL DIFICIL)"] <- "ARIGUANI"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ARMERO (GUAYABAL)"] <- "ARMERO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ATRATO (YUTO)"] <- "ATRATO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "BOJAYA (BELLAVISTA)"] <- "BOJAYA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "BAJO BAUDO (PIZARRO)"] <- "BAJO BAUDO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "CALIMA (DARIEN)"] <- "DARIEN"
insumos_mapa$NMpio[insumos_mapa$NMpio == "CIUDAD BOLIVAR" & insumos_mapa$NDepto == "VALLE DEL CAUCA"] <- "BOLIVAR"
insumos_mapa$NMpio[insumos_mapa$NMpio == "COTORRA (BONGO)"] <- "COTORRA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "COLOSO (RICAURTE)"] <- "COLOSO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "EL CANTON DEL SAN PABLO (MAN."] <- "CANTON DEL SAN PABLO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "CUBARRAL"] <- "SAN LUIS DE CUBARRAL"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ISTMINA"] <- "ITSMINA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MEDIO ATRATO (BETE)"] <- "MEDIO ATRATO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "BAHIA SOLANO (MUTIS)"] <- "BAHIA SOLANO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MEDIO BAUDO (PUERTO MELUK)"] <- "MEDIO BAUDO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "RIO QUITO (PAIMADO)"] <- "RIO QUITO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "UNION PANAMERICANA (LAS ANIMAS"] <- "UNION PANAMERICANA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "EL CARMEN" & insumos_mapa$NDepto == "CHOCO"] <- "EL CARMEN DE ATRATO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ARIGUANI"] <- "ARIGUAINI"
insumos_mapa$NMpio[insumos_mapa$NMpio == "CERRO DE SAN ANTONIO"] <- "CERRO SAN ANTONIO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "EL PI\xa5ON"] <- "EL PIÑON"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "PIJI\xa5O DEL CARMEN"] <- "PIJIÑO DEL CARMEN"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ZONA BANANERA (SEVILLA)"] <- "ZONA BANANERA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "ARROYO HONDO"] <- "ARROYOHONDO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "EL PE\xa5ON"] <- "EL PEÑON"
insumos_mapa$NMpio[insumos_mapa$NMpio == "GALERAS (NUEVA GRANADA)"] <- "GALERAS"
insumos_mapa$NMpio[insumos_mapa$NMpio == "CIUDAD BOLIVAR" & insumos_mapa$NDepto == "CAUCA"] <- "BOLIVAR"
insumos_mapa$NMpio[insumos_mapa$NMpio == "CIUDAD BOLIVAR" & insumos_mapa$NDepto == "SANTANDER"] <- "BOLIVAR"
insumos_mapa$NMpio[insumos_mapa$NMpio == "EL SANTUARIO" & insumos_mapa$NDepto == "RISARALDA"] <- "SANTUARIO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "GUAYABAL DE SIQUIMA"] <- "GUAYABAL DE  SIQUIMA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "LA APARTADA (FRONTERA)"] <- "LA APARTADA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "LA ARGENTINA (PLATA VIEJA)"] <- "LA ARGENTINA"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "LA MONTA\xa5ITA"] <- "LA MONTAÑITA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MANAURE BALCON DEL CESAR (MANA"] <- "MANAURE"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "MO\xa5ITOS"] <- "MOÑITOS"
insumos_mapa$NMpio[insumos_mapa$NMpio == "VISTA HERMOSA"] <- "VISTA HERMOSA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "VILLA DE LEIVA"] <- "VILLA DE LEYVA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "VALLE DEL GUAMUEZ (LA HORMIGA)"] <- "VALLE DEL GUAMUEZ"
insumos_mapa$NMpio[insumos_mapa$NMpio == "URIBE"] <- "LA URIBE"
insumos_mapa$NMpio[insumos_mapa$NMpio == "TUCHIN"] <- "LA APARTADA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "TIQUISIO (PTO. RICO)"] <- "TIQUISIO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "TESALIA (CARNICERIAS)"] <- "TESALIA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SOTARA (PAISPAMBA)"] <- "SOTARA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SAN MARTIN DE LOS LLANOS"] <- "SAN MARTIN"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SAN JUAN DE RIOSECO"] <- "SAN JUAN DE RIO SECO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SAN JUAN DE BETULIA (BETULIA)"] <- "SAN JUAN DE BETULIA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "PURACE (COCONUCO)"] <- "PURACE"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "PUERTO CARRE\xa5O"] <- "PUERTO CARREÑO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "SALDA\xa5A"] <- "SALDAÑA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "PAZ DE ARIPORO (MORENO)"] <- "PAZ DE ARIPORO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "PATIA (EL BORDO)"] <- "PATIA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "PARATEBUENO (LA NAGUAYA)"] <- "PARATEBUENO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "PAEZ (BELALCAZAR)"] <- "PAEZ"
insumos_mapa$NMpio[insumos_mapa$NMpio == "SAN MIGUEL (LA DORADA)"] <- "SAN MIGUEL"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "LA PEÑ\xa5A"] <- "LA PEÑA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "LOPEZ (MICAY)"] <- "LOPEZ"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MORICHAL (PAPUNAGUA)"] <- "MORICHAL"
insumos_mapa$NMpio[insumos_mapa$NMpio == "PANA PANA (CAMPO ALEGRE)"] <- "PANA PANA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MORICHAL"] <- "PAPUNAUA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "VISTA HERMOSA"] <- "VISTAHERMOSA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MORICHAL (MORICHAL NUEVO)"] <- "MORICHAL NUEVO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "RIOVIEJO"] <- "RIO VIEJO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "NARI\xd1O" & insumos_mapa$NDepto == "NARIÑO"] <- "NARIÑO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "EL CARMEN" & insumos_mapa$NDepto == "SANTANDER"] <- "EL CARMEN DE CHUCURI"
insumos_mapa$NMpio[insumos_mapa$NMpio == "\tARROYOHONDO"] <- "ARROYOHONDO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "\tSAN JUAN DE RIO SECO"] <- "SAN JUAN DE RIO SECO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MORICHAL NUEVO"] <- "MORICHAL"
insumos_mapa$NMpio[insumos_mapa$NMpio == "BUENOS AIRES (PACOA)"] <- "PACOA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "MALLAMA (PIEDRANCHA)"] <- "MALLAMA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "OCA¥A"] <- "OCA\xa5A"
insumos_mapa$NMpio[insumos_mapa$NMpio == "PUERTO NARE-LA MAGDALENA"] <- "PUERTO NARE"
insumos_mapa$NDepto[insumos_mapa$NDepto == "PUERTO NARI\xd1O"] <- "PUERTO NARIÑO"
insumos_mapa$NMpio[insumos_mapa$NMpio == "NARI\xd1O" & insumos_mapa$NDepto == "NARIÑO"] <- "NARIÑO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "PUERTO NARI\xa5O"] <- "PUERTO NARIÑO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "BRICE\xa5O"] <- "BRICEÑO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "OCA\xa5A"] <- "OCAÑA"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "CA\xa5ASGORDAS"] <- "CAÑASGORDAS"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "LA PE\xa5A"] <- "LA PEÑA"
insumos_mapa$NMpio[insumos_mapa$NMpio == "YONDO-CASABE"] <- "YONDO"
insumos_mapa$NMpio[insumos_mapa$municipio == "GUAINIA MORICHAL (MORICHAL NUEVO)"] <- "MORICHAL"
View(municipios@data$NOMBRE_MPI)

View(municipios@data$NOMBRE_MPI)[260]
3155040505
table(insumos_mapa$NDepto)[10] 

PUERTO NARE
OCAÑA
MALLAMA (PIEDRANCHA)
a<-table(municipios@data$NOMBRE_DPT)
View(a)
table(insumos_mapa$NDepto)

i_m<-insumos_mapa %>%
  filter(candidato=="ALEXANDER LOPEZ MAYA")
deptos2<-merge(municipios,i_m, by.y=c("NMpio","NDepto"), by.x=c("NOMBRE_MPI","NOMBRE_DPT"), duplicateGeoms = TRUE)

A<-insumos_mapa %>%
  filter(NDepto=="NARIÑO") %>%
  filter(candidato=="AIDA MERLANO REBOLLEDO")

table(deptos2$NDepto)
table(municipios@data$NOMBRE_MPI)Ç
municipios@data$NOMBRE_MPI[1091]
View(municipios@data)
deptos2<-deptos2 %>%
  filter(is.na(AREA)==T)

table(deptos2$NMpio)
tm_shape(deptos2)+
  #tm_shape(municipios2)+
  tm_polygons("diferencia_no_va",title="indice de concentración",n=4,style="jenks", border.alpha = 0.5,palette=brewer.pal(4,"RdBu"))+
  tm_layout(title = "ALEXANDER LOPEZ M", legend.outside = TRUE, legend.outside.position = "right", title.position = c("Left","top"), frame=FALSE)


vecinos_mun<-poly2nb(deptos2, queen =T, snap = T)
vecinos_mun<-nb2listw(vecinos_mun, style = "W",zero.policy = T)
moran<-moran.test(deptos2@data$diferencia_no_va, vecinos_mun, zero.policy = T,na.action=na.omit)
moran$estimate[1]
moran_mun<-lm.morantest(lm1, vecinos_mun, alternative="two.sided", zero.policy = T,na.action=na.omit)
print(moran_mun[1])
table(deptos2@data$NOMBRE_DPT)

tm_layout(title ="JOHN MOISES BESAILE FAYAD" , title.position = c("Left","top"))



display.brewer.pal(4,"RdBu")
pal <- rev(brewer.pal(4,"RdBu"))

insumos_mapa$NMpio %in% deptos2





insumos_mapa$NDepto[insumos_mapa$NDepto == "VALLE"] <- "VALLE DEL CAUCA"
insumos_mapa$NDepto[insumos_mapa$NDepto == "NORTE DE SAN"] <- "NORTE DE SANTANDER"
insumos_mapa$NDepto[insumos_mapa$NDepto == "BOGOTA D.C."] <- "SANTAFE DE BOGOTA D.C"
insumos_mapa$NDepto[insumos_mapa$NDepto == "SAN ANDRES"] <- "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA"
insumos_mapa2<-merge(insumos_mapa,Loosemore_Hanby_senado_depto2,by="candidato")
for(i in unique(insumos_mapa2$candidato)){
  i_m<-insumos_mapa2 %>%
    filter(candidato==i)
  lh<-mean(i_m$diferencia_va.y)
  deptos2<-merge(deptos,i_m, by.y=c("NDepto"), by.x=c("NOMBRE_DPT"), duplicateGeoms = TRUE)
  vecinos_mun<-poly2nb(deptos2, queen =T, snap = T)
  vecinos_mun<-nb2listw(vecinos_mun, style = "W",zero.policy = T)
  moran=moran.test(deptos2@data$diferencia_no_va, vecinos_mun, zero.policy = T,na.action=na.omit)
  signifiancia=ifelse()
  mymap<-tm_shape(deptos2)+
    tm_polygons("diferencia_no_va",title="indice de concentración",n=4,breaks=c(-0.1455498,-0.0235,-0.00775,0.0005,0.905),labels=c("1er cuartil","2ndo cuartil","3er cuartil","4to cuartil"), border.alpha = 0.5,palette=brewer.pal(4,"RdBu"))+
    tm_credits(paste("Su indicador LH es: ",round(lh,3),"\n","Su indicador IM es: ",round(moran$estimate[1])), position=c("right", "bottom"))+
    tm_layout(main.title = i, main.title.position="center",frame=FALSE,legend.width=0.5)
  tmap_save(tm = mymap, filename = paste0(i,".png"), height=10, width = 10)
}

SUM<-summary(insumos_mapa2$diferencia_no_va)
SUM[3]


deptos@data$NOMBRE_DPT
Representacion$NDepto
Representacion$NDepto[Representacion$NDepto == "BOGOTA D.C."] <- "SANTAFE DE BOGOTA D.C"
Representacion$NDepto[Representacion$NDepto == "SAN ANDRES"] <- "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA"
Representacion$NDepto[Representacion$NDepto == "NORTE DE SAN"] <- "NORTE DE SANTANDER" 
Representacion$NDepto[Representacion$NDepto == "NARIÑO" ] <-  "NARI\xd1O"
Representacion$NDepto[Representacion$NDepto == "VALLE"] <-  "VALLE DEL CAUCA"
table(Representacion$NDepto)
departamento<-merge(deptos,Representacion, by.y=c("NDepto"), by.x=c("NOMBRE_DPT"), duplicateGeoms = TRUE)
departamento@data
dev.off()
mapa<-tm_shape(departamento)+
  tm_polygons("DIFERENCIA",midpoint = NA,title="Representación",n=4, border.alpha = 0.5, breaks=c(-4,-1,0,1,4),labels=c("Subrepresentado (entre 1-4 curules)","Subrepresentado (menos de una curul)", "Sobrepresentado (menos de una curul)","Sobrepresentado (entre 1-4 curules)") ,palette=brewer.pal(4,"RdBu"))+
  tm_layout(frame=FALSE)+
  tm_text("NOMBRE_DPT", size=0.5) 
tmap_save(tm = mapa, filename = paste0("representación2",".png"), height=10, width = 10)

nique(insumos_mapa$candidato)
insumos_mapa2<-merge(insumos_mapa,Loosemore_Hanby_senado_mun4,by="candidato")
View(insumos_mapa2)
insumos_mapa2$NMpio[9]<-PUERTO NARI<a5>O
insumos_mapa2$NMpio[insumos_mapa2$NMpio == "PUERTO NARI\xd1O"] <- "PUERTO NARIÑO"
insumos_mapa2$NMpio[insumos_mapa2$NMpio == "PUERTO NARI\xa5O"] <- "PUERTO NARIÑO"
municipios@data$NOMBRE_MPI[1058] == insumos_mapa2$NMpio[790]
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "NARI\xd1O"] <- "NARIÑO"
municipios@data$NOMBRE_MPI[municipios@data$NOMBRE_MPI == "COVE\xd1AS"] <- "COVEÑAS"

"NARI\xd1O"
SUM<-summary(insumos_mapa2$diferencia_no_va)
insumos_mapa2$NDepto[790]
View(unique(insumos_mapa2))
for(i in unique(insumos_mapa2$candidato[1])){
  i_m<-insumos_mapa2 %>%
    filter(candidato==i)
  lh<-mean(i_m$diferencia_va.y)
  deptos2<-merge(municipios,i_m, by.y=c("NMpio","NDepto"), by.x=c("NOMBRE_MPI","NOMBRE_DPT"), duplicateGeoms = TRUE)
  vecinos_mun<-poly2nb(deptos2, queen =T, snap = T)
  vecinos_mun<-nb2listw(vecinos_mun, style = "W",zero.policy = T)
  moran<-moran.test(deptos2@data$diferencia_no_va, vecinos_mun, zero.policy = T,na.action=na.omit)
  sig<-ifelse(moran$p.value<0.05,"**","")
  mymap<-tm_shape(deptos2)+
    tm_polygons("diferencia_no_va",title="Índice de concentración de votos",n=4,breaks=c(SUM[1],SUM[2],SUM[4],0.01,SUM[6]),labels=c("Primer cuartil","Segundo cuartil","Tercero cuartil","Cuarto cuartil"), border.alpha = 0.5,palette=brewer.pal(4,"RdBu"))+
    tm_credits(paste("Su indicador LH es: ",round(lh,3),"\n","Su indicador IM es: ",round(moran$estimate[1],3),sig), position=c("right", "bottom"))+
    tm_layout(main.title = i, main.title.position="center",frame=FALSE,legend.width=0.5)+
    #tm_shape(deptos)+ tm_borders(col = "black", lwd = 1.5, lty = "solid", alpha = NA)
    tmap_save(tm = mymap, filename = paste0(i,".png"), height=10, width = 10)
}


moran$p.value
View(municipios@data$NOMBRE_MPI)
View(unique(insumos_mapa2$NMpio))
SUM<-summary(insumos_mapa2$diferencia_no_va)

breaks=c(-0.1455498,-0.0004862,-0.0000394,0.0001153,0.7089548)

tm_shape(deptos2)+
  tm_polygons("diferencia_no_va",title="indice de concentración",n=4,breaks=c(-0.1455498,-0.0004862,-0.0000394,0.0001153,0.7089548), border.alpha = 0.5,palette=brewer.pal(4,"RdBu"))+
  tm_layout(title = i, legend.outside = TRUE, legend.outside.position = "right", title.position = c("Left","top"), frame=FALSE)

x<-c(0)
for(i in unique(insumos_mapa2$candidato[1])){
  i_m<-insumos_mapa2 %>%
    filter(candidato==i)
  lh<-mean(i_m$diferencia_va.y)
  deptos2<-merge(municipios,i_m, by.y=c("NMpio","NDepto"), by.x=c("NOMBRE_MPI","NOMBRE_DPT"), duplicateGeoms = TRUE)
  vecinos_mun<-poly2nb(deptos2, queen =T, snap = T)
  vecinos_mun<-nb2listw(vecinos_mun, style = "W",zero.policy = T)
  moran<-moran.test(deptos2@data$diferencia_no_va, vecinos_mun, zero.policy = T,na.action=na.omit)
  x<-append(x,moran$estimate[1])
}
View(x)



dev.off()
class(Loosemore_Hanby_senado_mun5$diferencia_va)


Loosemore_Hanby_senado_mun6<-Loosemore_Hanby_senado_mun5 %>%
  mutate(modelo_l=0.28*diferencia_va-0.03204) %>%
  mutate(etiqueta=if_else(modelo_l<`I-M`, candidato, NA_character_))
p<- ggplot(data=Loosemore_Hanby_senado_mun6,aes(x=diferencia_va, y=`I-M`)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE,col="black")+ #scale_colour_manual(values=c("purple", "green","blue4", "dodgerblue", "blue","red","dark blue", "yellow", "orange"))+
  geom_text_repel(aes(label=etiqueta), col="black",size = 1.2)+
  xlab("Loosemore Hanby Municipios")+ ylab("I de Moran")+ theme_classic() 
addSmallLegend(p)  

modelo<-lm(`I-M`~diferencia_va, data = Loosemore_Hanby_senado_mun5)
lm()
summary(modelo)

addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 3, spaceLegend = 0.1) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}
colnames(Loosemore_Hanby_senado_mun5$)
diferencia_va




