# FUNCTION
clin_dx_calc_function <- function(data, preuds3=FALSE, uds3=FALSE, uds4=FALSE, 
                                  internal=FALSE, checkbox=FALSE){
  
  # PreUDS3 Only ----
  if(preuds3==TRUE & uds3==FALSE & uds4==FALSE){

    ## Main Dx ----
    data <- data %>%
      
      # Normal Cognition Criteria 
      mutate(norm_cog_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) ~ "NC")) %>%
        
        # Amnestic multidomain dementia syndrome Criteria
        mutate(amn_multi_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ((str_detect(probad,"1") & str_detect(probadif,"1")) |
               (str_detect(possad,"1") & str_detect(possadif,"1"))) ~ "Amnestic Multidomain")) %>%
        
        # Primary Progressive Aphasia Criteria
        mutate(ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(ppaph,"1") & str_detect(ppaphif,"1") ~ "PPA")) %>%
        
        # FTD Criteria
        mutate(ftd_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(ftd,"1") & str_detect(ftdif,"1") ~ "FTD")) %>%
        
        # DLB (dementia with lewy bodies) Criteria
        mutate(dlb_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(dlb,"1") & str_detect(dlbif,"1") ~ "DLB")) %>%
        
        # MCI Criteria
        mutate(mci_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"0") &
            (str_detect(mciamem,"1") | str_detect(mciaplus,"1") | 
               str_detect(mcinon1,"1") | str_detect(mcinon2,"1")) ~ "MCI")) %>%
        
        # Impaired not MCI Criteria
        mutate(not_mci_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"0") & 
            str_detect(impnomci,"1") ~ "Impaired Not MCI")) %>%
        
        # Non-amnestic Multidomain dementia (not PCA, PPA, bvFTD, or DLB syndrome)
        ## * CBS/PSP? ----
        mutate(non_amn_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            (str_detect(cort,"1") & str_detect(cortif,"1")) |
               (str_detect(psp,"1") & str_detect(pspif,"1")) ~ "Non-amnestic multidomain"))
      
      if(checkbox == TRUE){
        data <- data %>%
          # Individual Clinical Diagnosis columns remain as well as united.
          unite(col="calc_dx", c(norm_cog_dx:non_amn_dem_dx), sep=" & ", na.rm=T, remove=F)
      }else{
        data <- data %>%
          # Only united Clinical Diagnosis column remains. 
          unite(col="calc_dx", c(norm_cog_dx:non_amn_dem_dx), sep=" & ", na.rm=T)
      }
      
      # PPA Subtype - Internal=T/F
      data <- data %>%
        
        # Semantic
        mutate(semantic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(ppaph,"1") & str_detect(ppaphif,"1") &
            str_detect(semdeman,"1") ~ "Semantic")) %>%
        
        # Nonfluent/Agrammatic
        mutate(agrammatic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(ppaph,"1") & str_detect(ppaphif,"1") &
            str_detect(pnaph,"1") ~ "Nonfluent/Agrammatic")) %>%
        
        # Semantic Dementia - agnosic variant
        mutate(semdem_agnosic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(ppaph,"1") & str_detect(ppaphif,"1") & 
            str_detect(semdemag,"1") ~ "Semantic dementia - agnosic variant")) %>%
        
        # Other/not otherwise specified
        mutate(other_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(ppaph,"1") & str_detect(ppaphif,"1") &
            str_detect(ppaothr,"1") ~ "Other/not otherwise specified")) 
      
      ## ClinDx Specify - Internal=T ----
      if(internal == TRUE){
        
        # ******* The following criteria should only be used for internal data pulls, 
        #             for external pulls, stop at the criteria above ******* 
        data <- data %>%
          
          # CBS Criteria
          mutate(cbs_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & 
              str_detect(cort,"1") & str_detect(cortif,"1") ~ "CBS")) %>%

          # PSP Syndrome Criteria
          mutate(psp_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & 
              str_detect(psp,"1") & str_detect(pspif,"1") ~ "PSP")) 
      }
      
      if(checkbox == TRUE){
        data <- data %>%
          # Combined above for Clinical Diagnosis (specifics for internal use only)
          unite(col="calc_dx_specify", c(semantic_ppa_dx:ncol(.)), sep=" & ", na.rm=T, remove=F)
      }else{
        data <- data %>%
          # Combined above for Clinical Diagnosis (specifics for internal use only)
          unite(col="calc_dx_specify", c(semantic_ppa_dx:ncol(.)), sep=" & ", na.rm=T) %>%
          mutate_all(na_if, "")
      }
      
      ## Domains ----
      data <- data %>%
        mutate(mem_domain = case_when(
          str_detect(mciamem,"1") | str_detect(mciaplus,"1") ~ "Memory")) 
        mutate(lang_domain = case_when(
          str_detect(mciaplan,"1") | str_detect(mcin1lan,"1") | 
            str_detect(mcin2lan,"1") ~ "Language")) %>%
        mutate(att_domain = case_when(
          str_detect(mciapatt,"1") | str_detect(mcin1att,"1") | 
            str_detect(mcin2att,"1") ~ "Attention")) %>%
        mutate(exec_domain = case_when(
          str_detect(mciapex,"1") | str_detect(mcin1ex,"1") | 
            str_detect(mcin2ex,"1") ~ "Executive function")) %>%
        mutate(vis_domain = case_when(
          str_detect(mciapvis,"1") | str_detect(mcin1vis,"1") | 
            str_detect(mcin2vis,"1") ~ "Visuospatial")) 
      
      if(checkbox == TRUE){
        data <- data %>%
          # Combined above for Clinical Diagnosis (specifics for internal use only)
          unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T, remove=F)
      }else{
        data <- data %>%
          # Combined above for Clinical Diagnosis (specifics for internal use only)
          unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T) %>%
          mutate_all(na_if, "")
      }
      
  }else{data <- data}
  
  # UDS3 Only ----
  if(preuds3==FALSE & uds3==TRUE & uds4==FALSE){
    
    ## Main Dx ----
    data <- data %>%
 
      # Normal Cognition Criteria 
      mutate(norm_cog_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) ~ "NC")) %>%
        
        # Amnestic multidomain dementia syndrome Criteria
        mutate(amn_multi_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            amndem == 1 ~ "Amnestic Multidomain")) %>%
        
        # PCA Criteria
        mutate(pca_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & pca == 1 ~ "PCA")) %>%
        
        # Primary Progressive Aphasia Criteria
        mutate(ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & ppasyn == 1 ~ "PPA")) %>%
        
        # FTD Criteria
        mutate(ftd_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & ftdsyn == 1 ~ "FTD")) %>%
        
        # DLB (dementia with lewy bodies) Criteria
        mutate(dlb_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & lbdsyn == 1 ~ "DLB")) %>%
        
        # MCI Criteria
        mutate(mci_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"0") &
            (str_detect(mciamem,"1") | str_detect(mciaplus,"1") | 
               str_detect(mcinon1,"1") | str_detect(mcinon2,"1")) ~ "MCI")) %>%
        
        # Impaired not MCI Criteria
        mutate(not_mci_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"0") & 
            str_detect(impnomci,"1") ~ "Impaired Not MCI")) %>%
        
        # Non-amnestic Multidomain dementia (not PCA, PPA, bvFTD, or DLB syndrome)
        mutate(non_amn_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            namndem == 1 ~ "Non-amnestic multidomain")) 
    
        if(checkbox == TRUE){
          data <- data %>%
            # Individual Clinical Diagnosis columns remain as well as united.
            unite(col="calc_dx", c(norm_cog_dx:non_amn_dem_dx), sep=" & ", na.rm=T, remove=F)
        }else{
          data <- data %>%
            # Only united Clinical Diagnosis column remains. 
            unite(col="calc_dx", c(norm_cog_dx:non_amn_dem_dx), sep=" & ", na.rm=T)
        }
    
      # PPA Subtype - Internal=T/F
      
      data <- data %>%
        # Semantic
        mutate(semantic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt==1 ~ "Semantic")) %>%
        
        # Logopenic
        mutate(logopenic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt==2 ~ "Logopenic")) %>%
        
        # Nonfluent/Agrammatic
        mutate(agrammatic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt==3 ~ "Nonfluent/Agrammatic")) %>%
        
        # Other/not otherwise specified
        mutate(other_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt==4 ~ "Other/not otherwise specified")) 
    
      ## ClinDx Specify - Internal=T ----
      if(internal == TRUE){
        
        # ******* The following criteria should only be used for internal data pulls, 
        #             for external pulls, stop at the criteria above ******* 
        data <- data %>%
        
          # Aphasic Dementia non-PPA Criteria
          mutate(aph_dem_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
              mc_namndem == 1 ~ "Aphasic Dementia non-PPA")) %>%
          
          # CBS Criteria
          mutate(cbs_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
              mc_namndem == 2 ~ "CBS")) %>%
          
          # FTD-MND Criteria
          mutate(ftd_mnd_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
              mc_namndem == 3 ~ "FTD-MND")) %>%
          
          # PPAOS Criteria
          mutate(ppaos_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
              mc_namndem == 4 ~ "PPAOS")) %>%
          
          # PSP Syndrome Criteria
          mutate(psp_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
              mc_namndem == 5 ~ "PSP")) %>%
          
          # Semantic Dementia Criteria
          mutate(sem_dem_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
              mc_namndem == 6 ~ "Semantic Dementia")) %>%
          
          # Non-amnestic multidomain (other) Criteria
          mutate(nonamnestic_other_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
              mc_namndem == 7 ~ mc_namndem_other)) %>%
          
          # PPA Other Subtype - Anomic Criteria
          mutate(anomic_ppa_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & 
              ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 1 ~ "Anomic")) %>%
          
          # PPA Other Subtype - Agrammatic & Motor Speech Criteria
          mutate(agrammatic_ppa_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & 
              ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 2 ~ "Agrammatic + Motor Speech")) %>%
          
          # PPA Other Subtype - Mixed (G/S) Criteria
          mutate(mixedgs_ppa_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & 
              ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 3 ~ "Mixed (G/S)")) %>%
          
          # PPA Other Subtype - Mixed (G/L) Criteria
          mutate(mixedgl_ppa_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & 
              ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 4 ~ "Mixed (G/L)")) %>%
          
          # PPA Other Subtype - PPA+ Criteria
          mutate(ppaplus_ppa_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & 
              ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 5 ~ "PPA +")) %>%
          
          # PPA Other Subtype - Other/Unclassifiable Criteria
          mutate(unclass_ppa_dx = case_when(
            str_detect(normcog,"0") & str_detect(demented,"1") & 
              ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 6 ~ "Other/Unclassifiable"))
      }
      
      if(checkbox == TRUE){
        data <- data %>%
          # Combined above for Clinical Diagnosis (specifics for internal use only)
          unite(col="calc_dx_specify", c(semantic_ppa_dx:ncol(.)), sep=" & ", na.rm=T, remove=F)
      }else{
        data <- data %>%
          # Combined above for Clinical Diagnosis (specifics for internal use only)
          unite(col="calc_dx_specify", c(semantic_ppa_dx:ncol(.)), sep=" & ", na.rm=T) %>%
          mutate_all(na_if, "")
      }
      
      ## Domains ----
      data <- data %>%
        mutate(mem_domain = case_when(
          str_detect(mciamem,"1") | str_detect(mciaplus,"1") ~ "Memory")) %>%
        mutate(lang_domain = case_when(
          str_detect(mciaplan,"1") | str_detect(mcin1lan,"1") | 
            str_detect(mcin2lan,"1") ~ "Language")) %>%
        mutate(att_domain = case_when(
          str_detect(mciapatt,"1") | str_detect(mcin1att,"1") | 
            str_detect(mcin2att,"1") ~ "Attention")) %>%
        mutate(exec_domain = case_when(
          str_detect(mciapex,"1") | str_detect(mcin1ex,"1") | 
            str_detect(mcin2ex,"1") ~ "Executive function")) %>%
        mutate(vis_domain = case_when(
          str_detect(mciapvis,"1") | str_detect(mcin1vis,"1") | 
            str_detect(mcin2vis,"1") ~ "Visuospatial")) 
      
      if(checkbox == TRUE){
        data <- data %>%
          # Combined above for Clinical Diagnosis (specifics for internal use only)
          unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T, remove=F)
      }else{
        data <- data %>%
          # Combined above for Clinical Diagnosis (specifics for internal use only)
          unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T) %>%
          mutate_all(na_if, "")
      }
      
  }else{data <- data}
  
  # UDS4 Only ----
  if(preuds3==FALSE & uds3==FALSE & uds4==TRUE){
    
    ## Main Dx ----
    data <- data %>%
      
      # Normal Cognition Criteria
      mutate(norm_cog_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) ~ "NC")) %>%
      
      # Amnestic multidomain dementia syndrome Criteria
      ## * Amn. Multi? ----
      mutate(amn_multi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & amndem == 1 ~ "Amnestic Multidomain")) %>%
      
      # DES (dysexecutive predominant syndrome) Criteria
      mutate(des_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & dyexecsyn==1 ~ "DES")) %>%
      
      # PCA Criteria
      mutate(pca_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pca == 1 ~ "PCA")) %>%
      
      # Primary Progressive Aphasia Criteria
      mutate(ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & ppasyn == 1 ~ "PPA")) %>%
      
      # FTD Criteria
      mutate(ftd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & ftdsyn == 1 ~ "FTD")) %>%
      
      # DLB (dementia with lewy bodies) Criteria
      mutate(dlb_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 1 ~ "DLB")) %>%
      
      # Parkinson's Disease Criteria
      mutate(pd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 2 ~ "PD")) %>%
      
      # Parkinson's Disease dementia syndrome Criteria
      mutate(pdds_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 3 ~ "PDDS")) %>%
      
      # CBS Criteria
      mutate(cbs_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & cbssyn == 1 ~ "CBS")) %>%
      
      # PSP Criteria
      mutate(psp_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pspsyn == 1 ~ "PSP")) %>%
      
      # CTE Criteria
      mutate(cte_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & ctesyn == 1 ~ "CTE")) %>%
      
      # MSA Criteria
      mutate(msa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & msasyn == 1 ~ "MSA")) %>%
      
      # Other Dx Criteria
      mutate(other_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & othsyn == 1 ~ othsynx)) %>%
      
      # MCI Criteria
      mutate(mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & mci == 1 ~ "MCI")) %>%
      
      # Impaired not MCI Criteria
      mutate(not_mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & 
          str_detect(impnomci,"1") ~ "Impaired Not MCI")) %>%
      
      # Non-amnestic Multidomain dementia (not PCA, PPA, bvFTD, or DLB syndrome)
      mutate(non_amn_dem_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 ~ "Non-amnestic multidomain")) %>%
      
      # MBI Criteria
      mutate(mbi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & mbi == 1 ~ "MBI"))
    
    if(checkbox == TRUE){
      data <- data %>%
        # Individual Clinical Diagnosis columns remain as well as united.
        unite(col="calc_dx", c(norm_cog_dx:mbi_dx), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Only united Clinical Diagnosis column remains. 
        unite(col="calc_dx", c(norm_cog_dx:mbi_dx), sep=" & ", na.rm=T)
    }
    
    # Normal Cognition - SCD - Internal=T/F
    
    data <- data %>%
      # No Subjective Cognitive Decline (SCD)
      mutate(no_scd_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==0 ~ "No SCD")) %>%
      
      # SCD Not clinically meaningful
      mutate(scd_notclin_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==1 & scddxconf==0 ~ "SCD - Not clinically meaningful")) %>%
      
      # SCD Clinically meaningful
      mutate(scd_clin_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==1 & scddxconf==1 ~ "SCD - Clinically meaningful")) 
      
    # PPA Subtype - Internal=T/F
    
    data <- data %>%
      # Semantic
      mutate(semantic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==1 ~ "Semantic")) %>%
      
      # Logopenic
      mutate(logopenic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==2 ~ "Logopenic")) %>%
      
      # Nonfluent/Agrammatic
      mutate(agrammatic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==3 ~ "Nonfluent/Agrammatic")) %>%
      
      # PPAOS
      mutate(ppaos_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==5 ~ "PPAOS")) %>%
      
      # Other/not otherwise specified
      mutate(other_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==4 ~ "Other/not otherwise specified")) 
    
    # Various Diagnosis Subtypes - Internal=T/F
    data <- data %>%
      # PSP - Richardson's
      mutate(psp_rich_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          pspsyn == 1 & pspsynt==1 ~ "Richardson's")) %>%
      
      # PSP - Non-Richardson's
      mutate(psp_nonrich_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          pspsyn == 1 & pspsynt==2 ~ "Non-Richardson's")) %>%
      
      # MSA-C Cerebellar ataxia
      mutate(msac_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==1 ~ "MSA-C: Cerebellar ataxia")) %>%
      
      # MSA-P Parkinsonism
      mutate(msap_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==2 ~ "MSA-P: Parkinsonism")) %>%
      
      # MSA dysautonomia
      mutate(msa_dysauto_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==3 ~ "MSA dysautonomia"))
    
    ## ClinDx Specify - Internal=T ----
    if(internal == TRUE){
      
      # ******* The following criteria should only be used for internal data pulls, 
      #             for external pulls, stop at the criteria above ******* 
      data <- data %>%
      
        # Aphasic Dementia non-PPA Criteria
        mutate(aph_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
            mc_namndem == 1 ~ "Aphasic Dementia non-PPA")) %>%
        
        # FTD-MND Criteria
        mutate(ftd_mnd_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 3 ~ "FTD-MND")) %>%
        
        # Semantic Dementia Criteria
        mutate(sem_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 6 ~ "Semantic Dementia")) %>%
        
        # Non-amnestic multidomain (other) Criteria
        mutate(nonamnestic_other_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 7 ~ mc_namndem_other)) %>%
        
        # PPA NOS - Anomic Criteria
        mutate(anomic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 1 ~ "Anomic")) %>%
        
        # PPA NOS - Agrammatic & Motor Speech Criteria
        mutate(agrammatic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 2 ~ "Agrammatic + Motor Speech")) %>%
        
        # PPA NOS - Mixed (G/S) Criteria
        mutate(mixedgs_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 3 ~ "Mixed (G/S)")) %>%
        
        # PPA NOS - Mixed (G/L) Criteria
        mutate(mixedgl_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 4 ~ "Mixed (G/L)")) %>%
        
        # PPA NOS - PPA+ Criteria
        mutate(ppaplus_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 5 ~ "PPA +")) %>%
        
        # PPA NOS - Other/Unclassifiable Criteria
        mutate(unclass_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 6 ~ "Other/Unclassifiable"))
    }
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(no_scd_dx:ncol(.)), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(no_scd_dx:ncol(.)), sep=" & ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
    ## Domains ----
    data <- data %>%
      mutate(mem_domain = case_when(cdommem == 1 ~ "Memory")) %>%
      mutate(lang_domain = case_when(cdomlang == 1 ~ "Language")) %>%
      mutate(att_domain = case_when(cdomattn == 1 ~ "Attention")) %>%
      mutate(exec_domain = case_when(cdomexec == 1 ~ "Executive function")) %>%
      mutate(vis_domain = case_when(cdomvisu == 1 ~ "Visuospatial")) %>%
      mutate(beh_domain = case_when(cdombeh == 1 ~ "Behavioral")) %>%
      mutate(apr_domain = case_when(cdomaprax == 1 ~ "Apraxia")) %>%
      mutate(mot_domain = case_when(bdommot == 1 ~ "Motivation")) %>%
      mutate(afreg_domain = case_when(bdomafreg == 1 ~ "Affective regulation")) %>%
      mutate(imp_domain = case_when(bdomimp == 1 ~ "Impulse control")) %>%
      mutate(soc_domain = case_when(bdomsocial == 1 ~ "Social appropriateness")) %>%
      mutate(thts_domain = case_when(bdomthts == 1 ~ "Thought content/perception"))
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
  }else{data <- data}
  
  # PreUDS3 & UDS3 ----
  if(preuds3==TRUE & uds3==TRUE & uds4==FALSE){
    
    ## Main Dx ----
    data <- data %>%
      
      # Normal Cognition Criteria 
      mutate(norm_cog_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) ~ "NC")) %>%
      
      # Amnestic multidomain dementia syndrome Criteria
      mutate(amn_multi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (amndem == 1 | (str_detect(probad,"1") & str_detect(probadif,"1")) |
             (str_detect(possad,"1") & str_detect(possadif,"1"))) ~ "Amnestic Multidomain")) %>%
      
      # PCA Criteria
      mutate(pca_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pca == 1 ~ "PCA")) %>%
      
      # Primary Progressive Aphasia Criteria
      mutate(ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) ~ "PPA")) %>%
      
      # FTD Criteria
      mutate(ftd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ftdsyn == 1 | (str_detect(ftd,"1") & str_detect(ftdif,"1"))) ~ "FTD")) %>%
      
      # DLB (dementia with lewy bodies) Criteria
      mutate(dlb_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (lbdsyn == 1 | (str_detect(dlb,"1") & str_detect(dlbif,"1"))) ~ "DLB")) %>%
      
      # MCI Criteria
      mutate(mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") &
          (str_detect(mciamem,"1") | str_detect(mciaplus,"1") | 
             str_detect(mcinon1,"1") | str_detect(mcinon2,"1")) ~ "MCI")) %>%
      
      # Impaired not MCI Criteria
      mutate(not_mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & 
          str_detect(impnomci,"1") ~ "Impaired Not MCI")) %>%
      
      # Non-amnestic Multidomain dementia (not PCA, PPA, bvFTD, or DLB syndrome)
      mutate(non_amn_dem_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (namndem == 1 | ((str_detect(cort,"1") & str_detect(cortif,"1")) |
                             (str_detect(psp,"1") & str_detect(pspif,"1"))))
        ~ "Non-amnestic multidomain")) 
    
    if(checkbox == TRUE){
      data <- data %>%
        # Individual Clinical Diagnosis columns remain as well as united.
        unite(col="calc_dx", c(norm_cog_dx:non_amn_dem_dx), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Only united Clinical Diagnosis column remains. 
        unite(col="calc_dx", c(norm_cog_dx:non_amn_dem_dx), sep=" & ", na.rm=T)
    }
    
    # PPA Subtype - Internal=T/F
    
    data <- data %>%
      # Semantic
      mutate(semantic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==1 | str_detect(semdeman,"1")) ~ "Semantic")) %>%
      
      # Logopenic
      mutate(logopenic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==2 ~ "Logopenic")) %>%
      
      # Nonfluent/Agrammatic
      mutate(agrammatic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==3 | str_detect(pnaph,"1")) ~ "Nonfluent/Agrammatic")) %>%
      
      # Semantic Dementia - agnosic variant
      mutate(semdem_agnosic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          str_detect(ppaph,"1") & str_detect(ppaphif,"1") & 
          str_detect(semdemag,"1") ~ "Semantic dementia - agnosic variant")) %>%
      
      # Other/not otherwise specified
      mutate(other_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==4 | str_detect(ppaothr,"1")) ~ "Other/not otherwise specified")) 
    
    ## ClinDx Specify - Internal=T ----
    if(internal == TRUE){
      
      # ******* The following criteria should only be used for internal data pulls, 
      #             for external pulls, stop at the criteria above ******* 
      data <- data %>%
      
      # Aphasic Dementia non-PPA Criteria
      mutate(aph_dem_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
          mc_namndem == 1 ~ "Aphasic Dementia non-PPA")) %>%
        
        # CBS Criteria
        mutate(cbs_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ((namndem == 1 & mc_namndem == 2) | 
               (str_detect(cort,"1") & str_detect(cortif,"1"))) ~ "CBS")) %>%
        
        # FTD-MND Criteria
        mutate(ftd_mnd_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 3 ~ "FTD-MND")) %>%
        
        # PPAOS Criteria
        mutate(ppaos_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 4 ~ "PPAOS")) %>%
        
        # PSP Syndrome Criteria
        mutate(psp_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ((namndem == 1 & mc_namndem == 5) | 
               (str_detect(psp,"1") & str_detect(pspif,"1"))) ~ "PSP")) %>%
        
        # Semantic Dementia Criteria
        mutate(sem_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 6 ~ "Semantic Dementia")) %>%
        
        # Non-amnestic multidomain (other) Criteria
        mutate(nonamnestic_other_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 7 ~ mc_namndem_other)) %>%
      
        # PPA Other Subtype - Anomic Criteria
        mutate(anomic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 1 ~ "Anomic")) %>%
        
        # PPA Other Subtype - Agrammatic & Motor Speech Criteria
        mutate(agrammatic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 2 ~ "Agrammatic + Motor Speech")) %>%
        
        # PPA Other Subtype - Mixed (G/S) Criteria
        mutate(mixedgs_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 3 ~ "Mixed (G/S)")) %>%
        
        # PPA Other Subtype - Mixed (G/L) Criteria
        mutate(mixedgl_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 4 ~ "Mixed (G/L)")) %>%
        
        # PPA Other Subtype - PPA+ Criteria
        mutate(ppaplus_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 5 ~ "PPA +")) %>%
        
        # PPA Other Subtype - Other/Unclassifiable Criteria
        mutate(unclass_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 6 ~ "Other/Unclassifiable"))
    }
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(semantic_ppa_dx:ncol(.)), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(semantic_ppa_dx:ncol(.)), sep=" & ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
    ## Domains ----
    data <- data %>%
      mutate(mem_domain = case_when(
        str_detect(mciamem,"1") | str_detect(mciaplus,"1") ~ "Memory")) %>%
      mutate(lang_domain = case_when(
        str_detect(mciaplan,"1") | str_detect(mcin1lan,"1") | 
          str_detect(mcin2lan,"1") ~ "Language")) %>%
      mutate(att_domain = case_when(
        str_detect(mciapatt,"1") | str_detect(mcin1att,"1") | 
          str_detect(mcin2att,"1") ~ "Attention")) %>%
      mutate(exec_domain = case_when(
        str_detect(mciapex,"1") | str_detect(mcin1ex,"1") | 
          str_detect(mcin2ex,"1") ~ "Executive function")) %>%
      mutate(vis_domain = case_when(
        str_detect(mciapvis,"1") | str_detect(mcin1vis,"1") | 
          str_detect(mcin2vis,"1") ~ "Visuospatial")) 
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
  }else{data <- data}
  
  # PreUDS3 & UDS4 ----
  if(preuds3==TRUE & uds3==FALSE & uds4==TRUE){
    
    ## Main Dx ----
    data <- data %>%
      
      # Normal Cognition Criteria 
      mutate(norm_cog_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) ~ "NC")) %>%
      
      # Amnestic multidomain dementia syndrome Criteria
      mutate(amn_multi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (amndem == 1 | (str_detect(probad,"1") & str_detect(probadif,"1")) |
             (str_detect(possad,"1") & str_detect(possadif,"1"))) ~ "Amnestic Multidomain")) %>%
      
      # DES (dysexecutive predominant syndrome) Criteria
      mutate(des_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & dyexecsyn==1 ~ "DES")) %>%
      
      # PCA Criteria
      mutate(pca_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pca == 1 ~ "PCA")) %>%
      
      # Primary Progressive Aphasia Criteria
      mutate(ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) ~ "PPA")) %>%
      
      # FTD Criteria
      mutate(ftd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ftdsyn == 1 | (str_detect(ftd,"1") & str_detect(ftdif,"1"))) ~ "FTD")) %>%
      
      # DLB (dementia with lewy bodies) Criteria
      mutate(dlb_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ((lbdsyn == 1 & lbdsynt == 1) | 
             (str_detect(dlb,"1") & str_detect(dlbif,"1"))) ~ "DLB")) %>%
      
      # Parkinson's Disease Criteria
      mutate(pd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 2 ~ "PD")) %>%
      
      # Parkinson's Disease dementia syndrome Criteria
      mutate(pdds_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 3 ~ "PDDS")) %>%
      
      # CBS Criteria
      mutate(cbs_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & cbssyn == 1 ~ "CBS")) %>%
      
      # PSP Criteria
      mutate(psp_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pspsyn == 1 ~ "PSP")) %>%
      
      # CTE Criteria
      mutate(cte_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & ctesyn == 1 ~ "CTE")) %>%
      
      # MSA Criteria
      mutate(msa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & msasyn == 1 ~ "MSA")) %>%
      
      # Other Dx Criteria
      mutate(other_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & othsyn == 1 ~ othsynx)) %>%
      
      # MCI Criteria
      mutate(mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & (mci==1 |
          str_detect(mciamem,"1") | str_detect(mciaplus,"1") | 
             str_detect(mcinon1,"1") | str_detect(mcinon2,"1")) ~ "MCI")) %>%
      
      # Impaired not MCI Criteria
      mutate(not_mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & 
          str_detect(impnomci,"1") ~ "Impaired Not MCI")) %>%
      
      # Non-amnestic Multidomain dementia (not PCA, PPA, bvFTD, or DLB syndrome)
      mutate(non_amn_dem_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (namndem == 1 | ((str_detect(cort,"1") & str_detect(cortif,"1")) |
                             (str_detect(psp,"1") & str_detect(pspif,"1"))))
        ~ "Non-amnestic multidomain")) %>%
      
      # MBI Criteria
      mutate(mbi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & mbi == 1 ~ "MBI"))
    
    if(checkbox == TRUE){
      data <- data %>%
        # Individual Clinical Diagnosis columns remain as well as united.
        unite(col="calc_dx", c(norm_cog_dx:mbi_dx), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Only united Clinical Diagnosis column remains. 
        unite(col="calc_dx", c(norm_cog_dx:mbi_dx), sep=" & ", na.rm=T)
    }
    
    # Normal Cognition - SCD - Internal=T/F
    
    data <- data %>%
      # No Subjective Cognitive Decline (SCD)
      mutate(no_scd_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==0 ~ "No SCD")) %>%
      
      # SCD Not clinically meaningful
      mutate(scd_notclin_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==1 & scddxconf==0 ~ "SCD - Not clinically meaningful")) %>%
      
      # SCD Clinically meaningful
      mutate(scd_clin_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==1 & scddxconf==1 ~ "SCD - Clinically meaningful")) 
    
    # PPA Subtype - Internal=T/F
    
    data <- data %>%
      # Semantic
      mutate(semantic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==1 | str_detect(semdeman,"1")) ~ "Semantic")) %>%
      
      # Logopenic
      mutate(logopenic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==2 ~ "Logopenic")) %>%
      
      # Nonfluent/Agrammatic
      mutate(agrammatic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==3 | str_detect(pnaph,"1")) ~ "Nonfluent/Agrammatic")) %>%
      
      # Semantic Dementia - agnosic variant
      mutate(semdem_agnosic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          str_detect(ppaph,"1") & str_detect(ppaphif,"1") & 
          str_detect(semdemag,"1") ~ "Semantic dementia - agnosic variant")) %>%
      
      # PPAOS
      mutate(ppaos_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==5 ~ "PPAOS")) %>%
      
      # Other/not otherwise specified
      mutate(other_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==4 | str_detect(ppaothr,"1")) ~ "Other/not otherwise specified")) 
    
    # Various Diagnosis Subtypes - Internal=T/F
    data <- data %>%
      # PSP - Richardson's
      mutate(psp_rich_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          pspsyn == 1 & pspsynt==1 ~ "Richardson's")) %>%
      
      # PSP - Non-Richardson's
      mutate(psp_nonrich_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          pspsyn == 1 & pspsynt==2 ~ "Non-Richardson's")) %>%
      
      # MSA-C Cerebellar ataxia
      mutate(msac_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==1 ~ "MSA-C: Cerebellar ataxia")) %>%
      
      # MSA-P Parkinsonism
      mutate(msap_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==2 ~ "MSA-P: Parkinsonism")) %>%
      
      # MSA dysautonomia
      mutate(msa_dysauto_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==3 ~ "MSA dysautonomia"))
    
    ## ClinDx Specify - Internal=T ----
    if(internal == TRUE){
      
      # ******* The following criteria should only be used for internal data pulls, 
      #             for external pulls, stop at the criteria above ******* 
      data <- data %>%
        
        # Aphasic Dementia non-PPA Criteria
        mutate(aph_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
            mc_namndem == 1 ~ "Aphasic Dementia non-PPA")) %>%
        
        # CBS Criteria
        mutate(cbs_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(cort,"1") & str_detect(cortif,"1") ~ "CBS")) %>%
        
        # FTD-MND Criteria
        mutate(ftd_mnd_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 3 ~ "FTD-MND")) %>%
        
        # PSP Syndrome Criteria
        mutate(psp_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            str_detect(psp,"1") & str_detect(pspif,"1") ~ "PSP")) %>%
        
        # Semantic Dementia Criteria
        mutate(sem_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 6 ~ "Semantic Dementia")) %>%
        
        # Non-amnestic multidomain (other) Criteria
        mutate(nonamnestic_other_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 7 ~ mc_namndem_other)) %>%
        
        # PPA Other Subtype - Anomic Criteria
        mutate(anomic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 1 ~ "Anomic")) %>%
        
        # PPA Other Subtype - Agrammatic & Motor Speech Criteria
        mutate(agrammatic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 2 ~ "Agrammatic + Motor Speech")) %>%
        
        # PPA Other Subtype - Mixed (G/S) Criteria
        mutate(mixedgs_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 3 ~ "Mixed (G/S)")) %>%
        
        # PPA Other Subtype - Mixed (G/L) Criteria
        mutate(mixedgl_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 4 ~ "Mixed (G/L)")) %>%
        
        # PPA Other Subtype - PPA+ Criteria
        mutate(ppaplus_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 5 ~ "PPA +")) %>%
        
        # PPA Other Subtype - Other/Unclassifiable Criteria
        mutate(unclass_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 6 ~ "Other/Unclassifiable"))
    }
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(no_scd_dx:ncol(.)), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(no_scd_dx:ncol(.)), sep=" & ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
    ## Domains ----
    data <- data %>%
      mutate(mem_domain = case_when(
        str_detect(mciamem,"1") | str_detect(mciaplus,"1") |
          cdommem == 1 ~ "Memory")) %>%
      mutate(lang_domain = case_when(
        str_detect(mciaplan,"1") | str_detect(mcin1lan,"1") | 
          str_detect(mcin2lan,"1") | cdomlang == 1 ~ "Language")) %>%
      mutate(att_domain = case_when(
        str_detect(mciapatt,"1") | str_detect(mcin1att,"1") | 
          str_detect(mcin2att,"1") | cdomattn == 1 ~ "Attention")) %>%
      mutate(exec_domain = case_when(
        str_detect(mciapex,"1") | str_detect(mcin1ex,"1") | 
          str_detect(mcin2ex,"1") | cdomexec == 1 ~ "Executive function")) %>%
      mutate(vis_domain = case_when(
        str_detect(mciapvis,"1") | str_detect(mcin1vis,"1") | 
          str_detect(mcin2vis,"1") | cdomvisu == 1 ~ "Visuospatial")) %>%
      mutate(beh_domain = case_when(cdombeh == 1 ~ "Behavioral")) %>%
      mutate(apr_domain = case_when(cdomaprax == 1 ~ "Apraxia")) %>%
      mutate(mot_domain = case_when(bdommot == 1 ~ "Motivation")) %>%
      mutate(afreg_domain = case_when(bdomafreg == 1 ~ "Affective regulation")) %>%
      mutate(imp_domain = case_when(bdomimp == 1 ~ "Impulse control")) %>%
      mutate(soc_domain = case_when(bdomsocial == 1 ~ "Social appropriateness")) %>%
      mutate(thts_domain = case_when(bdomthts == 1 ~ "Thought content/perception"))
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
  }else{data <- data}
  
  # UDS3 & UDS4 ----
  if(preuds3==FALSE & uds3==TRUE & uds4==TRUE){
    
    ## Main Dx ----
    data <- data %>%
      
      # Normal Cognition Criteria
      mutate(norm_cog_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) ~ "NC")) %>%
      
      # Amnestic multidomain dementia syndrome Criteria
      ## * Amn. Multi? ----
      mutate(amn_multi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & amndem == 1 ~ "Amnestic Multidomain")) %>%
      
      # DES (dysexecutive predominant syndrome) Criteria
      mutate(des_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & dyexecsyn==1 ~ "DES")) %>%
      
      # PCA Criteria
      mutate(pca_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pca == 1 ~ "PCA")) %>%
      
      # Primary Progressive Aphasia Criteria
      mutate(ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & ppasyn == 1 ~ "PPA")) %>%
      
      # FTD Criteria
      mutate(ftd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & ftdsyn == 1 ~ "FTD")) %>%
      
      # DLB (dementia with lewy bodies) Criteria
      mutate(dlb_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & (is.na(lbdsynt) | lbdsynt == 1) ~ "DLB")) %>%
      
      # Parkinson's Disease Criteria
      mutate(pd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 2 ~ "PD")) %>%
      
      # Parkinson's Disease dementia syndrome Criteria
      mutate(pdds_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 3 ~ "PDDS")) %>%
      
      # CBS Criteria
      mutate(cbs_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & cbssyn == 1 ~ "CBS")) %>%
      
      # PSP Criteria
      mutate(psp_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pspsyn == 1 ~ "PSP")) %>%
      
      # CTE Criteria
      mutate(cte_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & ctesyn == 1 ~ "CTE")) %>%
      
      # MSA Criteria
      mutate(msa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & msasyn == 1 ~ "MSA")) %>%
      
      # Other Dx Criteria
      mutate(other_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & othsyn == 1 ~ othsynx)) %>%
      
      # MCI Criteria
      mutate(mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & (mci==1 |
          str_detect(mciamem,"1") | str_detect(mciaplus,"1") | 
          str_detect(mcinon1,"1") | str_detect(mcinon2,"1")) ~ "MCI")) %>%
      
      # Impaired not MCI Criteria
      mutate(not_mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & 
          str_detect(impnomci,"1") ~ "Impaired Not MCI")) %>%
      
      # Non-amnestic Multidomain dementia (not PCA, PPA, bvFTD, or DLB syndrome)
      mutate(non_amn_dem_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 ~ "Non-amnestic multidomain")) %>%
      
      # MBI Criteria
      mutate(mbi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & mbi == 1 ~ "MBI"))
    
    if(checkbox == TRUE){
      data <- data %>%
        # Individual Clinical Diagnosis columns remain as well as united.
        unite(col="calc_dx", c(norm_cog_dx:mbi_dx), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Only united Clinical Diagnosis column remains. 
        unite(col="calc_dx", c(norm_cog_dx:mbi_dx), sep=" & ", na.rm=T)
    }
    
    # Normal Cognition - SCD - Internal=T/F
    
    data <- data %>%
      # No Subjective Cognitive Decline (SCD)
      mutate(no_scd_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==0 ~ "No SCD")) %>%
      
      # SCD Not clinically meaningful
      mutate(scd_notclin_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==1 & scddxconf==0 ~ "SCD - Not clinically meaningful")) %>%
      
      # SCD Clinically meaningful
      mutate(scd_clin_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==1 & scddxconf==1 ~ "SCD - Clinically meaningful")) 
    
    # PPA Subtype - Internal=T/F
    
    data <- data %>%
      # Semantic
      mutate(semantic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==1 ~ "Semantic")) %>%
      
      # Logopenic
      mutate(logopenic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==2 ~ "Logopenic")) %>%
      
      # Nonfluent/Agrammatic
      mutate(agrammatic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==3 ~ "Nonfluent/Agrammatic")) %>%
      
      # PPAOS
      mutate(ppaos_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==5 ~ "PPAOS")) %>%
      
      # Other/not otherwise specified
      mutate(other_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==4 ~ "Other/not otherwise specified")) 
    
    # Various Diagnosis Subtypes - Internal=T/F
    data <- data %>%
      # PSP - Richardson's
      mutate(psp_rich_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          pspsyn == 1 & pspsynt==1 ~ "Richardson's")) %>%
      
      # PSP - Non-Richardson's
      mutate(psp_nonrich_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          pspsyn == 1 & pspsynt==2 ~ "Non-Richardson's")) %>%
      
      # MSA-C Cerebellar ataxia
      mutate(msac_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==1 ~ "MSA-C: Cerebellar ataxia")) %>%
      
      # MSA-P Parkinsonism
      mutate(msap_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==2 ~ "MSA-P: Parkinsonism")) %>%
      
      # MSA dysautonomia
      mutate(msa_dysauto_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==3 ~ "MSA dysautonomia"))
    
    ## ClinDx Specify - Internal=T ----
    if(internal == TRUE){
      
      # ******* The following criteria should only be used for internal data pulls, 
      #             for external pulls, stop at the criteria above ******* 
      data <- data %>%
        
        # Aphasic Dementia non-PPA Criteria
        mutate(aph_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
            mc_namndem == 1 ~ "Aphasic Dementia non-PPA")) %>%
        
        # CBS Criteria
        mutate(cbs_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
            mc_namndem == 2 ~ "CBS")) %>%
        
        # FTD-MND Criteria
        mutate(ftd_mnd_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 3 ~ "FTD-MND")) %>%
        
        # PPAOS Criteria
        mutate(ppaos_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 4 ~ "PPAOS")) %>%
        
        # PSP Syndrome Criteria
        mutate(psp_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
            mc_namndem == 5 ~ "PSP")) %>%
        
        # Semantic Dementia Criteria
        mutate(sem_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 6 ~ "Semantic Dementia")) %>%
        
        # Non-amnestic multidomain (other) Criteria
        mutate(nonamnestic_other_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 7 ~ mc_namndem_other)) %>%
        
        # PPA NOS - Anomic Criteria
        mutate(anomic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 1 ~ "Anomic")) %>%
        
        # PPA NOS - Agrammatic & Motor Speech Criteria
        mutate(agrammatic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 2 ~ "Agrammatic + Motor Speech")) %>%
        
        # PPA NOS - Mixed (G/S) Criteria
        mutate(mixedgs_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 3 ~ "Mixed (G/S)")) %>%
        
        # PPA NOS - Mixed (G/L) Criteria
        mutate(mixedgl_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 4 ~ "Mixed (G/L)")) %>%
        
        # PPA NOS - PPA+ Criteria
        mutate(ppaplus_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 5 ~ "PPA +")) %>%
        
        # PPA NOS - Other/Unclassifiable Criteria
        mutate(unclass_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 6 ~ "Other/Unclassifiable"))
    }
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(no_scd_dx:ncol(.)), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(no_scd_dx:ncol(.)), sep=" & ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
    ## Domains ----
    data <- data %>%
      mutate(mem_domain = case_when(
        str_detect(mciamem,"1") | str_detect(mciaplus,"1") |
          cdommem == 1 ~ "Memory")) %>%
      mutate(lang_domain = case_when(
        str_detect(mciaplan,"1") | str_detect(mcin1lan,"1") | 
          str_detect(mcin2lan,"1") | cdomlang == 1 ~ "Language")) %>%
      mutate(att_domain = case_when(
        str_detect(mciapatt,"1") | str_detect(mcin1att,"1") | 
          str_detect(mcin2att,"1") | cdomattn == 1 ~ "Attention")) %>%
      mutate(exec_domain = case_when(
        str_detect(mciapex,"1") | str_detect(mcin1ex,"1") | 
          str_detect(mcin2ex,"1") | cdomexec == 1 ~ "Executive function")) %>%
      mutate(vis_domain = case_when(
        str_detect(mciapvis,"1") | str_detect(mcin1vis,"1") | 
          str_detect(mcin2vis,"1") | cdomvisu == 1 ~ "Visuospatial")) %>%
      mutate(beh_domain = case_when(cdombeh == 1 ~ "Behavioral")) %>%
      mutate(apr_domain = case_when(cdomaprax == 1 ~ "Apraxia")) %>%
      mutate(mot_domain = case_when(bdommot == 1 ~ "Motivation")) %>%
      mutate(afreg_domain = case_when(bdomafreg == 1 ~ "Affective regulation")) %>%
      mutate(imp_domain = case_when(bdomimp == 1 ~ "Impulse control")) %>%
      mutate(soc_domain = case_when(bdomsocial == 1 ~ "Social appropriateness")) %>%
      mutate(thts_domain = case_when(bdomthts == 1 ~ "Thought content/perception"))
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
  }else{data <- data}
  
  # PreUDS3, UDS3 & UDS4 ----
  if(preuds3==TRUE & uds3==TRUE & uds4==TRUE){
    
    ## Main Dx ----
    data <- data %>%
      
      # Normal Cognition Criteria 
      mutate(norm_cog_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) ~ "NC")) %>%
      
      # Amnestic multidomain dementia syndrome Criteria
      mutate(amn_multi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (amndem == 1 | (str_detect(probad,"1") & str_detect(probadif,"1")) |
             (str_detect(possad,"1") & str_detect(possadif,"1"))) ~ "Amnestic Multidomain")) %>%
      
      # DES (dysexecutive predominant syndrome) Criteria
      mutate(des_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & dyexecsyn==1 ~ "DES")) %>%
      
      # PCA Criteria
      mutate(pca_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pca == 1 ~ "PCA")) %>%
      
      # Primary Progressive Aphasia Criteria
      mutate(ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) ~ "PPA")) %>%
      
      # FTD Criteria
      mutate(ftd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ftdsyn == 1 | (str_detect(ftd,"1") & str_detect(ftdif,"1"))) ~ "FTD")) %>%
      
      # DLB (dementia with lewy bodies) Criteria
      mutate(dlb_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ((lbdsyn == 1 & (is.na(lbdsynt) | lbdsynt == 1)) | 
             (str_detect(dlb,"1") & str_detect(dlbif,"1"))) ~ "DLB")) %>%
      
      # Parkinson's Disease Criteria
      mutate(pd_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 2 ~ "PD")) %>%
      
      # Parkinson's Disease dementia syndrome Criteria
      mutate(pdds_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          lbdsyn == 1 & lbdsynt == 3 ~ "PDDS")) %>%
      
      # CBS Criteria
      mutate(cbs_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & cbssyn == 1 ~ "CBS")) %>%
      
      # PSP Criteria
      mutate(psp_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & pspsyn == 1 ~ "PSP")) %>%
      
      # CTE Criteria
      mutate(cte_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & ctesyn == 1 ~ "CTE")) %>%
      
      # MSA Criteria
      mutate(msa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & msasyn == 1 ~ "MSA")) %>%
      
      # Other Dx Criteria
      mutate(other_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & othsyn == 1 ~ othsynx)) %>%
      
      # MCI Criteria
      mutate(mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & 
          (mci==1 | str_detect(mciamem,"1") | str_detect(mciaplus,"1") | 
             str_detect(mcinon1,"1") | str_detect(mcinon2,"1")) ~ "MCI")) %>%
      
      # Impaired not MCI Criteria
      mutate(not_mci_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & 
          str_detect(impnomci,"1") ~ "Impaired Not MCI")) %>%
      
      # Non-amnestic Multidomain dementia (not PCA, PPA, bvFTD, or DLB syndrome)
      mutate(non_amn_dem_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (namndem == 1 | ((str_detect(cort,"1") & str_detect(cortif,"1")) |
                             (str_detect(psp,"1") & str_detect(pspif,"1"))))
        ~ "Non-amnestic multidomain")) %>%
      
      # MBI Criteria
      mutate(mbi_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"0") & mbi == 1 ~ "MBI"))
    
    if(checkbox == TRUE){
      data <- data %>%
        # Individual Clinical Diagnosis columns remain as well as united.
        unite(col="calc_dx", c(norm_cog_dx:mbi_dx), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Only united Clinical Diagnosis column remains. 
        unite(col="calc_dx", c(norm_cog_dx:mbi_dx), sep=" & ", na.rm=T)
    }
    
    # Normal Cognition - SCD - Internal=T/F
    
    data <- data %>%
      # No Subjective Cognitive Decline (SCD)
      mutate(no_scd_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==0 ~ "No SCD")) %>%
      
      # SCD Not clinically meaningful
      mutate(scd_notclin_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==1 & scddxconf==0 ~ "SCD - Not clinically meaningful")) %>%
      
      # SCD Clinically meaningful
      mutate(scd_clin_dx = case_when(
        str_detect(normcog,"1") & str_detect(memory,"0|0.5") & str_detect(orient,"0|0.5") &
          str_detect(judgment,"0|0.5") & str_detect(commun,"0|0.5") &
          str_detect(homehobb,"0|0.5") & (str_detect(perscare,"0|0.5") | is.na(perscare)) &
          scd==1 & scddxconf==1 ~ "SCD - Clinically meaningful")) 
    
    # PPA Subtype - Internal=T/F
    
    data <- data %>%
      # Semantic
      mutate(semantic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==1 | str_detect(semdeman,"1")) ~ "Semantic")) %>%
      
      # Logopenic
      mutate(logopenic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==2 ~ "Logopenic")) %>%
      
      # Nonfluent/Agrammatic
      mutate(agrammatic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==3 | str_detect(pnaph,"1")) ~ "Nonfluent/Agrammatic")) %>%
      
      # Semantic Dementia - agnosic variant
      mutate(semdem_agnosic_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          str_detect(ppaph,"1") & str_detect(ppaphif,"1") & 
          str_detect(semdemag,"1") ~ "Semantic dementia - agnosic variant")) %>%
      
      # PPAOS
      mutate(ppaos_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          ppasyn == 1 & ppasynt==5 ~ "PPAOS")) %>%
      
      # Other/not otherwise specified
      mutate(other_ppa_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          (ppasyn == 1 | (str_detect(ppaph,"1") & str_detect(ppaphif,"1"))) &
          (ppasynt==4 | str_detect(ppaothr,"1")) ~ "Other/not otherwise specified")) 
    
    # Various Diagnosis Subtypes - Internal=T/F
    data <- data %>%
      # PSP - Richardson's
      mutate(psp_rich_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          pspsyn == 1 & pspsynt==1 ~ "Richardson's")) %>%
      
      # PSP - Non-Richardson's
      mutate(psp_nonrich_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          pspsyn == 1 & pspsynt==2 ~ "Non-Richardson's")) %>%
      
      # MSA-C Cerebellar ataxia
      mutate(msac_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==1 ~ "MSA-C: Cerebellar ataxia")) %>%
      
      # MSA-P Parkinsonism
      mutate(msap_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==2 ~ "MSA-P: Parkinsonism")) %>%
      
      # MSA dysautonomia
      mutate(msa_dysauto_dx = case_when(
        str_detect(normcog,"0") & str_detect(demented,"1") & 
          msasyn == 1 & msasynt==3 ~ "MSA dysautonomia"))
    
    ## ClinDx Specify - Internal=T ----
    if(internal == TRUE){
      
      # ******* The following criteria should only be used for internal data pulls, 
      #             for external pulls, stop at the criteria above ******* 
      data <- data %>%
        
        # Aphasic Dementia non-PPA Criteria
        mutate(aph_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 & 
            mc_namndem == 1 ~ "Aphasic Dementia non-PPA")) %>%
        
        # CBS Criteria
        mutate(cbs_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ((namndem == 1 & mc_namndem == 2) | 
               (str_detect(cort,"1") & str_detect(cortif,"1"))) ~ "CBS")) %>%
        
        # FTD-MND Criteria
        mutate(ftd_mnd_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 3 ~ "FTD-MND")) %>%
        
        # PPAOS Criteria
        mutate(ppaos_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 4 ~ "PPAOS")) %>%
        
        # PSP Syndrome Criteria
        mutate(psp_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ((namndem == 1 & mc_namndem == 5) | 
               (str_detect(psp,"1") & str_detect(pspif,"1"))) ~ "PSP")) %>%
        
        # Semantic Dementia Criteria
        mutate(sem_dem_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 6 ~ "Semantic Dementia")) %>%
        
        # Non-amnestic multidomain (other) Criteria
        mutate(nonamnestic_other_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & namndem == 1 &
            mc_namndem == 7 ~ mc_namndem_other)) %>%
        
        # PPA Other Subtype - Anomic Criteria
        mutate(anomic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 1 ~ "Anomic")) %>%
        
        # PPA Other Subtype - Agrammatic & Motor Speech Criteria
        mutate(agrammatic_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 2 ~ "Agrammatic + Motor Speech")) %>%
        
        # PPA Other Subtype - Mixed (G/S) Criteria
        mutate(mixedgs_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 3 ~ "Mixed (G/S)")) %>%
        
        # PPA Other Subtype - Mixed (G/L) Criteria
        mutate(mixedgl_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 4 ~ "Mixed (G/L)")) %>%
        
        # PPA Other Subtype - PPA+ Criteria
        mutate(ppaplus_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 5 ~ "PPA +")) %>%
        
        # PPA Other Subtype - Other/Unclassifiable Criteria
        mutate(unclass_ppa_dx = case_when(
          str_detect(normcog,"0") & str_detect(demented,"1") & 
            ppasyn == 1 & ppasynt == 4 & mc_ppa_nos == 6 ~ "Other/Unclassifiable"))
    }
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(no_scd_dx:ncol(.)), sep=" & ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="calc_dx_specify", c(no_scd_dx:ncol(.)), sep=" & ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
    ## Domains ----
    data <- data %>%
      mutate(mem_domain = case_when(
        str_detect(mciamem,"1") | str_detect(mciaplus,"1") |
          cdommem == 1 ~ "Memory")) %>%
      mutate(lang_domain = case_when(
        str_detect(mciaplan,"1") | str_detect(mcin1lan,"1") | 
          str_detect(mcin2lan,"1") | cdomlang == 1 ~ "Language")) %>%
      mutate(att_domain = case_when(
        str_detect(mciapatt,"1") | str_detect(mcin1att,"1") | 
          str_detect(mcin2att,"1") | cdomattn == 1 ~ "Attention")) %>%
      mutate(exec_domain = case_when(
        str_detect(mciapex,"1") | str_detect(mcin1ex,"1") | 
          str_detect(mcin2ex,"1") | cdomexec == 1 ~ "Executive function")) %>%
      mutate(vis_domain = case_when(
        str_detect(mciapvis,"1") | str_detect(mcin1vis,"1") | 
          str_detect(mcin2vis,"1") | cdomvisu == 1 ~ "Visuospatial")) %>%
      mutate(beh_domain = case_when(cdombeh == 1 ~ "Behavioral")) %>%
      mutate(apr_domain = case_when(cdomaprax == 1 ~ "Apraxia")) %>%
      mutate(mot_domain = case_when(bdommot == 1 ~ "Motivation")) %>%
      mutate(afreg_domain = case_when(bdomafreg == 1 ~ "Affective regulation")) %>%
      mutate(imp_domain = case_when(bdomimp == 1 ~ "Impulse control")) %>%
      mutate(soc_domain = case_when(bdomsocial == 1 ~ "Social appropriateness")) %>%
      mutate(thts_domain = case_when(bdomthts == 1 ~ "Thought content/perception"))
    
    if(checkbox == TRUE){
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T, remove=F)
    }else{
      data <- data %>%
        # Combined above for Clinical Diagnosis (specifics for internal use only)
        unite(col="domains", c(mem_domain:ncol(.)), sep=", ", na.rm=T) %>%
        mutate_all(na_if, "")
    }
    
  }else{data <- data}
}  
      










