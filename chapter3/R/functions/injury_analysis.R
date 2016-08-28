# Load function
source("~/Documents/Github/network.project/chapter3/R/functions/function_audpc.R")

# write the function
injury_analysis <- function(injury){
      result <- injury %>%
  dplyr::mutate(Nlh = Nl * Nt, 
           # Number of leave = number of tiller * number of leave per tiller
           # tiller injuries
           SNL.percent = (SNL/Nt*100), # Percent of SNL damage on hill is number
           DH.percent = (DH/Nt)*100, # Percent of Dead Heart in on hill is number tiller damaged by dead heart divide by number of tiller *100
           RT.percent = RT/Nt*100, # Percent of Rat damage in one hill
           GM.percent =GM/Nt*100, # Percent of GM or silver shoot
           RB.percent = RB/Nt*100, # Percent of Rice Bug injuries in one hill
           WH.percent = WH/Nt*100, # Percent of Whitehead in one hill
           DP.percent = DP/Nt*100, # Percent of Dirty Panicle in one hill
           FS.percent = FS/Nt*100, # Percent of False smut in one hill
           NB.percent = NB/Nt*100, # Percent of Neck Blast in one hill
           SHB.percent = SHB/Nt*100, # Percent of Shealth Blight injuries in one hill
           SHR.percent = SHR/Nt*100, # Percent of Shealth Rot in one hill
           SR.percent = SR/Nt*100, # Percent of Stem rot on one hill
           # leave injuries
           LF.percent = LF/Nlh*100, # Percent of Leaffolder in one hill
           WM.percent = WM/Nlh*100, # Percent of Whorl maggot injuries in one hill
           BLB.percent = BLB/Nlh*100, # Percent of Bacterial leaf Blight in one hill
           BLS.percent = BLS/Nlh*100, # Percent of Bacterial leaf streak in one hill
           BS.percent = BS/Nlh*100, # Percent of Brown Spot in one hill
           LB.percent = LB/Nlh*100, # Percent of leaf Blight in one hill
           NBS.percent = NBS/Nlh*100, # Percent of Narrow brown spot in one hill
           RS.percent = RS/Nlh*100 # Percent of Red stripe in one hill
    )  %>%
    dplyr::group_by(index, Country, Year, Season, Fieldno, visit, DVS) %>%
     dplyr::summarise(#m.RT = mean(RT.percent), # Percent of Rat damage in one hill
      #    m.SNL = mean(SNL.percent), # Percent of Snail damage in one hill
      m.SNL = mean(SNL.percent),
      m.DH = mean(DH.percent),
      m.RT = mean(RT.percent),
      m.GM = mean(GM.percent),
      m.WH = mean(WH.percent), # Percent of Whitehead in one hill
      m.DP = mean(DP.percent), # Percent of Dirty Panicle in one hill
      m.FS = mean(FS.percent), # Percent of False smut in one hill
      m.NB = mean(NB.percent), # Percent of Neck Blast in one hill
      m.SHB = mean(SHB.percent), # Percent of Shealth Blight injuries in one hill
      m.SHR = mean(SHR.percent), # Percent of Shealth Rot in one hill
      m.SR = mean(SR.percent),
      m.BPH = mean(BPH),
      m.WPH = mean(WPH),
      m.AW = mean(AW),
      m.RB = max(RB),
      m.LF = mean(LF.percent), # mean within DVS which is following the designed group
      m.WM = mean(WM.percent),
      m.BLB = mean(BLB.percent),
      m.BLS = mean(BLS.percent),
      m.BS = mean(BS.percent),
      m.LB = mean(LB.percent),
      m.NBS = mean(NBS.percent),
      m.RS = mean(RS.percent)
    ) %>%
    dplyr::group_by(index, Country, Year, Season, Fieldno) %>% 
        dplyr::summarise(SNL.max = max(m.SNL),
      DH.max = max(m.DH),
      RT.max = max(m.RT),
      GM.max = max(m.GM),
      WH.max = max(m.WH),
      DP.max = max(m.DP),
      FS.max = max(m.FS),
      NB.max = max(m.NB),
      SHB.max = max(m.SHB),
      SHR.max = max(m.SHR),
      SR.max = max(m.SR),
      BPH.audpc = audpc(m.BPH, DVS),
      WPH.audpc = audpc(m.WPH, DVS),
      AW.audpc = audpc(m.AW, DVS),
      RB.audpc = audpc(m.RB, DVS),
      LF.audpc = audpc(m.LF, DVS),
      WM.audpc = audpc(m.WM, DVS),
      BLB.audpc = audpc(m.BLB, DVS),
      BLS.audpc = audpc(m.BLS, DVS),
      BS.audpc = audpc(m.BS, DVS),
      LB.audpc = audpc(m.LB, DVS),
      NBS.audpc = audpc(m.NBS, DVS),
      RS.audpc = audpc(m.RS, DVS)
    )
      return(result)
}

