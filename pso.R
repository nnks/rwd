# parameter
dimension = 10;
weight = 0.729844;
swarmsize = 10;
c1 = 1.496180;
c2 = 1.496180;
maxgen = 10000;

#inital parameters
X = zeros(10,dimension);
V = zeros(10,dimension);
fitness = zeros(1,10);
pbest = zeros(10,dimension);
pbest_fit = zeros(1,10);
bestfit = 1000000.0;
gbest = zeros(1,dimension);

#function sphere
sphere = function(){
  lowli = -5.12;  #lower limit
  uppli = 5.12;    #upper limit
  vmax = upli-lowli;
  
  for(i in swarmsize:1) {
    #initail particle
    for (j = 1:dimension){
      X(i,j)=lowli+(rand()/2 )*vmax;
      V(i,j)=-vmax+(rand()/2 )*(2*vmax);
      pbest(i,j) = X(i,j);
    }
  }
  #evaluate fitness value

}