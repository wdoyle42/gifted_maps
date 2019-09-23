## Tests

var<-"Percent Identified in Non-Title I Schools"

groupvar<-"State"

gg_state_plot(gm,var,groupvar,var)

foo<-map_gen(gm_states,var,var);foo
 
 
helpText("                                        ",
         "                                        ",
          "Copyright, Citation, Contact information here
           Notes: Rhode Island, Massachussetts, Vermont and DC have been dropped from all variables
                      except Access to Identification and Percent Identified as Gifted/Talented
           as they have fewer than 5% of their students having access to identification.")
                   
