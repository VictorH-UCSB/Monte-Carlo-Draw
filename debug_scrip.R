source('set_para.R')
source('pop_gen.R')
source('pop_draw.R')
source('simulation.R')
var=0
for (i in 1 : 10000){
  var=var+simulation(10,2,1,10000,1000)[1,2]
}
var/10000
