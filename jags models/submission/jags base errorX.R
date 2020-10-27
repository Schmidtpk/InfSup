data {

    for(t in min(Ti):max(T)){
        for(tt in 1:max_past){
            intt[t,tt]<- t-tt}}

}

model
{
    # Distribution of symptom onset over time --------------------------------------------------
    for(k in 1:max_past){
        #note: Safed in reverse from max_past to 1
        SI[k] <- dgamma(k,shape_SI, rate_SI)
    }

    # Distribution of transmission over time ------------------------------------
    for(k in 1:max_past){
        #note: Safed in reverse from max_past to 1
        transmission[k] <-
            dgamma(k,shape_transmission, rate_transmission)
    }

    for(c in 1:nc){
        # Before modeling [1,Tinit] ---------------------------------------------------------
        for (t in 1:(Tinit[c]-1)){
            for(a in 1:na){
                i[t,c,a] <- 0
            }
        }

        # Initialize i [Tinit,Ti] ---------------------------------------------------------
        for (t in Tinit[c]:(Ti[c]-1)){
            # tau = expected infected from Tinit until Ti
            # actual infected i modeled as exponentially distributed
            # with mean = tau/number_days_of_initialization
            for(a in 1:na){
                i[t,c,a] ~ dexp(1/
                                    (tau[c,a]/
                                         (Ti[c]-Tinit[c]))
                                )
            }
        }

        #  Model period for infected i [Ti,T] ----------------------------------------------
        for (t in Ti[c]:T[c]){
            for(a in 1:na){

                # Virus load cicrulating with generation distribution "transmission"
                Li[t,c,a] <-
                    inprod(
                        i[intt[t,],c,a],
                        transmission)+0.001

                # Expected infected by age group a
                Ei[t,c,a] = Li[t,c,a]*Ra[t,c,a]

                # infected a NB draw with mean = Ei und disp = K*disp_individual
                r_negbin[t,c,a] = Li[t,c,a]*i_disp[a]
                p_negbin[t,c,a] = r_negbin[t,c,a]/(r_negbin[t,c,a]+Ei[t,c,a])
                i[t,c,a] ~ dnegbin(p_negbin[t,c,a], r_negbin[t,c,a])
            }
        }

        #  Model period for symptoms [Ti,T] ----------------------------------------------
        for (t in Ti[c]:T[c]){
            for(a in 1:na){

                # expected symptoms
                Es[t,c,a] <-
                    inprod(i[intt[t,],c,a],SI)

                reports[t,c,a] ~ dpois(Es[t,c,a]*1/4+0.001)
            }
        }
    }

    # +++ PRIORS +++ -----------------------------------------------------------

    # covariate effects -------------------------------------------------------

    # for binary covariates
    for(mm in 1:nxd){
        for(a in 1:na){
            beta_effectxd[a,mm] ~ dnorm(0,1/effect_sd^2)T(-1,) }}

    # rate R -----------------------------------------------------------------------
    # Rzero constitutes reproduction rate in the absence of binary treatments
    # for baseline values of real-valued covariates
    for(c in 1:nc){
        for(a in 1:na){
            Rzero[c,a] ~ dnorm(R_mean,1/R_sd^2)T(0,) }}

    for(c in 1:nc){
        for(a in 1:na){
            for(t in Ti[c]:T[c]){
                Ra[t,c,a] <-
                    Rzero[c,a] *
                    prod(1+beta_effectxd[a,]*xd[t,c,]) *
                    Rerror[t,c,a]

                Rerror[t,c,a] ~ dnorm(1,1/error_sd^2)T(0,)
            }
        }
    }

    # dispersion --------------------------------------------------------------
    for(a in 1:na){
        i_disp[a] ~ dnorm(disp_mean,1/disp_sd^2)T(0,)
        }

    # initial infections ------------------------------------------------------
    for(c in 1:nc){
        for(a in 1:na){
            tau[c,a] ~ dnorm(tau_mean,1/tau_sd^2)T(0,)
        }
    }


    # Intervals ---------------------------------------------------------------

    # SI: Serial interval from transmission to reporting
    mean_SI ~ dnorm(mean_SI_mean,1/mean_SI_sd^2)

    # transform mean and sd to shape and rate parameter of gamma distr.
    shape_SI <- mean_SI*rate_SI
    rate_SI <- mean_SI/sd_SI^2

    # transmission: distribution of transmissions over time
    mean_transmission ~ dnorm(mean_trans_mean,1/mean_trans_sd^2)

    # transform mean and sd to shape and rate parameter of gamma distr.
    shape_transmission <- mean_transmission*rate_transmission
    rate_transmission <- mean_transmission/sd_transmission^2


    # output ------------------------------------------------------------------
    # simulate additional output for checking (illustration)

    ### Rerror
    for(c in 1:nc){
        out_Rerror_c[c] <- mean(Rerror[max(Ti):min(T),c,])}
    for(t in max(Ti):min(T)){
        out_Rerror_t[t] <- mean(Rerror[t,,])}
    for(a in 1:na){
        out_Rerror_a[a] <- mean(Rerror[max(Ti):min(T),,a])}

    ### distributions
    transmission_dist ~ dgamma(shape_transmission, rate_transmission)
    SI_dist ~ dgamma(shape_SI, rate_SI)
}
