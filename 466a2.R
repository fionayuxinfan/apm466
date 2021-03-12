# gasoline price tree
treePrice = data.frame()
treePrice[1,1] = 1

for (time in 1:52) {
  treePrice[, time+1] = treePrice[, time]*1.1
  treePrice[time+1, ] = rep(-1, time+1)
  treePrice[time+1, time+1] = treePrice[time, time]/1.1
}

# risk-neutral probabilities
pu = (1-1/1.1)/(1.1-1/1.1)
pd = 1-pu


## 4-upswing

# exercise value tree
upEV = data.frame(treePrice)

for (time in 53:1) {
  upEV[1:time, time] = 50*pmax(treePrice[1:time, time]-1, 0)
}

# 1-upswing
up1 = data.frame(upEV)
upex1 = data.frame(upEV)

for (time in 52:1) {
  HV = up1[1:time, time+1]*pu + up1[2:(time+1), time+1]*pd
  EV = upEV[1:time, time]
  up1[1:time, time] = pmax(EV, HV)
  upex1[1:time, time] = (EV>=HV)&(EV>0)
}

# 2-upswing
up2 = data.frame(upEV)
upex2 = data.frame(upEV)

for (time in 52:1) {
  HV = up2[1:time, time+1]*pu + up2[2:(time+1), time+1]*pd
  EV = upEV[1:time, time]+up1[1:time, time+1]*pu + up1[2:(time+1), time+1]*pd
  up2[1:time, time] = pmax(EV, HV)
  upex2[1:time, time] = (EV>=HV)&(EV>0)
}

# 3-upswing
up3 = data.frame(upEV)
upex3 = data.frame(upEV)

for (time in 52:1) {
  HV = up3[1:time, time+1]*pu + up3[2:(time+1), time+1]*pd
  EV = upEV[1:time, time]+up2[1:time, time+1]*pu + up2[2:(time+1), time+1]*pd
  up3[1:time, time] = pmax(EV, HV)
  upex3[1:time, time] = (EV>=HV)&(EV>0)
}

# 4-upswing
up4 = data.frame(upEV)
upex4 = data.frame(upEV)

for (time in 52:1) {
  HV = up4[1:time, time+1]*pu + up4[2:(time+1), time+1]*pd
  EV = upEV[1:time, time]+up3[1:time, time+1]*pu + up3[2:(time+1), time+1]*pd
  up4[1:time, time] = pmax(EV, HV)
  upex4[1:time, time] = (EV>=HV)&(EV>0)
}


## 4-downswing

# exercise value tree
downEV = data.frame(treePrice)

for (time in 53:1) {
  downEV[1:time, time] = 50000*pmax(1-treePrice[1:time, time], 0)
}

# 1-downswing
down1 = data.frame(downEV)
downex1 = data.frame(downEV)

for (time in 52:1) {
  HV = down1[1:time, time+1]*pu + down1[2:(time+1), time+1]*pd
  EV = downEV[1:time, time]
  down1[1:time, time] = pmax(EV, HV)
  downex1[1:time, time] = (EV>=HV)&(EV>0)
}

# 2-downswing
down2 = data.frame(downEV)
downex2 = data.frame(downEV)

for (time in 52:1) {
  HV = down2[1:time, time+1]*pu + down2[2:(time+1), time+1]*pd
  EV = downEV[1:time, time]+down1[1:time, time+1]*pu + down1[2:(time+1), time+1]*pd
  down2[1:time, time] = pmax(EV, HV)
  downex2[1:time, time] = (EV>=HV)&(EV>0)
}

# 3-downswing
down3 = data.frame(downEV)
downex3 = data.frame(downEV)

for (time in 52:1) {
  HV = down3[1:time, time+1]*pu + down3[2:(time+1), time+1]*pd
  EV = downEV[1:time, time]+down2[1:time, time+1]*pu + down2[2:(time+1), time+1]*pd
  down3[1:time, time] = pmax(EV, HV)
  downex3[1:time, time] = (EV>=HV)&(EV>0)
}

# 4-downswing
down4 = data.frame(downEV)
downex4 = data.frame(downEV)

for (time in 52:1) {
  HV = down4[1:time, time+1]*pu + down4[2:(time+1), time+1]*pd
  EV = downEV[1:time, time]+down3[1:time, time+1]*pu + down3[2:(time+1), time+1]*pd
  down4[1:time, time] = pmax(EV, HV)
  downex4[1:time, time] = (EV>=HV)&(EV>0)
}


# results
up4[1,1]
down4[1,1]

