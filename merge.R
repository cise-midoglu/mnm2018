source("util.R")
graphics.off()

get_node = function(train, op) {
    ## 5-21713 289, 290
    ## 5-21772 228, 229
    ## 5-21726 254, 255
    ## 7-21772 292, 206
    ## 73-04   291, 261
    ## 73-06   296, 297
    ## 73-09   304, 305

    if(train == "5-21713") nodes = c(290, 289)
    if(train == "5-21722") nodes = c(228, 229)
    if(train == "5-21726") nodes = c(254, 255)
    if(train == "7-21772") nodes = c(292, 206)
    if(train == "73-04")   nodes = c(291, 261)
    if(train == "73-06")   nodes = c(296, 297)
    if(train == "73-09")   nodes = c(304, 305)
    if(train == "73-01")   nodes = c(448, 449)
    if(train == "73-05")   nodes = c(366, 367)
    if(train == "73-08")   nodes = c(368, 369)
    if(train == "74-25")   nodes = c(462, 463)
    if(train == "74-37")   nodes = c(556, 557)
    if(train == "75-07")   nodes = c(460, 461)
    if(train == "75-46")   nodes = c(456, 457)
    if(train == "75-06")   nodes = c(568, 569)

    ## four operations: 24201, 24202, 24001, 24214    
    if(op == 24201 || op == 24202) {
        inode = 1
    }
    else { 
        inode = 2
    }
    nodes[inode]
}

get_data = function(gps, modem, op, train) {
    node = get_node(train, op)
    printf("%s %d %d\n", train, op, node)
    gps = gps[ccu_desig == train]    

    modem2 = modem[modem[, nodeid == node & operator == op & nwmccmnc == op]]
    ##modem2 = modem[modem[, nodeid == node & operator == op]]
 
    setkey(modem2, timestamp)
    setkey(gps, timestamp)
    ##roll = modem2[gps, roll=T] 
    roll = modem2[gps, roll=60*5]
    roll$devicemode[is.na(roll$devicemode)] = 0
    ##roll$devicestate[is.na(roll$devicestate)] = 0
    roll
}

find_runs = function(gps) {
    dtime = 60*30 ## 45 minutes
    tmp = gps[order(unix_time),]
    tmp = tmp[,diff :=c(0,diff(unix_time))]
    gps = tmp[, run := cumsum(diff>dtime)+1]
    gps = gps[, diff:=NULL]
}


get_roll = function(gps, modem, operator) {
    str(gps)
    i = 1
    trains = unique(gps$ccu_desig)
    trains_known = c("5-21713", "5-21722", "5-21726", "7-21772", "73-04", "73-06", "73-09", "73-01", "73-05", "73-08", "74-25", "74-37", "75-07", "75-46", "75-06")
    printf("asdf %s\n", setdiff(trains, trains_known))
    trains = intersect(trains, trains_known)
    print(trains)
    rolll = list()
    for (train in trains) {
        gps2 = gps[ccu_desig == train]
        tmp = get_data(gps2, modem, operator, train)
        rolll[[i]] = tmp
        i = i + 1
    }
    ##rolll
    t = lapply(rolll, na.omit)
    rbindlist(t)
}

if(!exists("gps")) {
    gps = readRDS("gpsi.rds")
    ##gps = readRDS("gps5i_new.rds")
    ##gps = gps[, c("timestamp", "gps_lat","gps_long", "ccu_desig")]
    ##gpsl = lapply(gpsl, find_runs)
}

if(!exists("modem")) {
    modem = readRDS("meta_new.rds")
    ##modem = modem[, c("timestamp", "nodeid","devicemode", "devicestate", "imsimccmnc")]
    ##modem = modem[, c("timestamp", "nodeid","devicemode", "devicestate", "imsimccmnc","nwmccmnc")]
    modem = modem[, c("timestamp", "nodeid","devicemode", "imsimccmnc","nwmccmnc")]

    names(modem)[names(modem) == "imsimccmnc"] = "operator"
    modem$operator = as.factor(modem$operator)
    modem$operator = as.factor(modem$nwmccmnc)
    modem = na.omit(modem)

    ##modem = modem[timestamp>1506816000] ## 1.10.2017
    ##modem = modem[timestamp>1509494400] ## 1.11.2017
}

rolll1 = lapply (gps, get_roll, modem, operator=24201)
rolll2 = lapply (gps, get_roll, modem, operator=24202)
rolll3 = lapply (gps, get_roll, modem, operator=24214)

saveRDS(rolll1, "metaf_24201.rds")
saveRDS(rolll2, "metaf_24202.rds")
saveRDS(rolll3, "metaf_24214.rds")
