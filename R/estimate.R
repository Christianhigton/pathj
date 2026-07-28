## This class takes care of estimating the model and return the results. It inherit from Syntax, and define the same tables
## defined by Syntax, but it fill them with the results.

Estimate <- R6::R6Class("Estimate",
                        inherit = Syntax,
                        cloneable=FALSE,
                        class=FALSE,
                        list(
                          model=NULL,
                          tab_fit=NULL,
                          tab_fitindices=NULL,
                          tab_mi_fit=NULL,
                          tab_mi_fit_summary=NULL,
                          tab_mi_pooled_fit=NULL,
                          tab_mi_status=NULL,
                          ciwidth=NULL,
                          tab_constfit=NULL,
                          tab_mi=NULL,
                          tab_effects=NULL,
                          tab_missing_summary=NULL,
                          tab_missing_patterns=NULL,
                          tab_mcar=NULL,
                          missing_info=NULL,
                          tab_report_text=NULL,
                          tab_assumptions=NULL,
                          tab_recommendations=NULL,
                          tab_group_comparison=NULL,
                          tab_mediation_decomp=NULL,
                          tab_insights=NULL,
                          tab_pcurve=NULL,
                          tab_variable_types=NULL,
                          tab_model_comparison=NULL,
                          tab_report_paragraph=NULL,
                          tab_report_html=NULL,
                          tab_lavaan_syntax=NULL,
                          tab_mermaid_syntax=NULL,
                          tab_mplus_syntax=NULL,
                          tab_openmx_syntax=NULL,
                          tab_path_legend=NULL,
                          initialize=function(options,datamatic) {
                            super$initialize(
                              options=options,
                              datamatic=datamatic)
                            self$ciwidth<-options$ciWidth/100
                          },
                          estimate=function(data) {
                            ## prepare the options based on Syntax definitions
                            group_var<-NULL
                            lavoptions<-list(model = private$.lav_structure, 
                                             data = data,
                                             se=self$options$se,
                                             estimator=self$options$estimator,
                                             ncpus=1
                            )
                            if (self$options$se == "boot")
                              lavoptions[["bootstrap"]]<-self$options$bootN
                            if (is.something(self$multigroup)) {
                              lavoptions[["group"]]<-self$multigroup$var64
                              lavoptions[["group.label"]]<-self$multigroup$levels
                              group_var<-self$multigroup$var64
                            }
                            if (self$options$estimator=="ML") {
                              lavoptions[["likelihood"]]<-self$options$likelihood
                            }

                            model_vars<-model_variable_names(private$.lav_structure, group_var)
                            display_vars<-fromb64(model_vars,self$vars)
                            type_vars<-intersect(tob64(unique(c(self$options$endogenous, self$options$covs, self$options$syntaxVars)), self$vars), names(data))
                            syntax_source <- self$options$syntaxSource
                            if (is.null(syntax_source))
                              syntax_source <- "gui"
                            if (!identical(syntax_source, "gui"))
                              type_vars<-unique(c(type_vars, intersect(model_vars, names(data))))
                            data_types<-detect_variable_types(data[, type_vars, drop=FALSE])
                            if (nrow(data_types)>0)
                              data_types$variable<-fromb64(data_types$variable,self$vars)
                            self$tab_variable_types<-data_types

                            ordered_vars<-character(0)
                            if (isTRUE(self$options$autoOrdinal)) {
                              raw_types<-detect_variable_types(data[, type_vars, drop=FALSE])
                              ordered_vars<-raw_types$variable[raw_types$type %in% c("ordinal", "binary")]
                              ordered_vars<-setdiff(ordered_vars, group_var)
                              if (length(ordered_vars)>0) {
                                lavoptions[["ordered"]]<-ordered_vars
                                if (!self$options$estimator %in% c("WLSMV", "DWLS", "WLS")) {
                                  lavoptions[["estimator"]]<-"WLSMV"
                                  lavoptions[["missing"]]<-NULL
                                  self$warnings<-list(topic="main",message="Ordinal or binary variables were detected; WLSMV estimation with ordered variables was used for this model.")
                                }
                              }
                            } else if (self$options$estimator=="WLSMV") {
                              raw_types<-detect_variable_types(data[, type_vars, drop=FALSE])
                              ordered_vars<-raw_types$variable[raw_types$type %in% c("ordinal", "binary")]
                              ordered_vars<-setdiff(ordered_vars, group_var)
                              if (length(ordered_vars)>0)
                                lavoptions[["ordered"]]<-ordered_vars
                            }

                            self$tab_missing_summary<-missing_data_summary(data, model_vars, display_vars)
                            if (isTRUE(self$options$showMissingDiagnostics)) {
                              self$tab_missing_patterns<-missing_pattern_summary(data, model_vars)
                              self$tab_mcar<-mcar_diagnostic(data, model_vars)
                            }

                            missing<-try_hard({
                              handle_missing_data(data, model_vars, self$options, group_var)
                            })
                            if (!isFALSE(missing$error)) {
                              self$errors<-missing$error
                              return(self$errors)
                            }
                            missing<-missing$obj
                            self$missing_info<-missing$info
                            if (is.something(missing$warnings)) {
                              for (w in missing$warnings)
                                self$warnings<-list(topic="main",message=w)
                            }
                            data<-missing$data
                            lavoptions[["data"]]<-data
                            if (is.something(missing$lav_missing))
                              lavoptions[["missing"]]<-missing$lav_missing
                            if (!lavoptions[["estimator"]] %in% c("ML", "MLR", "MLM", "MLMV", "MLF"))
                              lavoptions[["missing"]]<-NULL

                            ginfo("estimating the model...")
                            ## estimate the models
                            mi_models<-NULL
                            mi_params<-NULL
                            if (self$missing_info$method == "mi") {
                              mi_results<-lapply(seq_along(missing$imputed_data), function(i) {
                                d<-missing$imputed_data[[i]]
                                opts<-lavoptions
                                opts[["data"]]<-d
                                run_sem_model(opts)
                              })

                              mi_status<-do.call(rbind, lapply(seq_along(mi_results), function(i) {
                                res<-mi_results[[i]]
                                ok<-isFALSE(res$error) && is.something(res$obj)
                                conv<-FALSE
                                if (ok)
                                  conv<-isTRUE(tryCatch(res$obj@Fit@converged, error=function(e) FALSE))
                                data.frame(
                                  imputation=i,
                                  status=if (ok) "Used" else "Failed",
                                  converged=if (ok) ifelse(conv, "TRUE", "FALSE") else "",
                                  warning=if (!isFALSE(res$warning)) paste(res$warning, collapse="; ") else "",
                                  error=if (!isFALSE(res$error)) paste(res$error, collapse="; ") else "",
                                  stringsAsFactors=FALSE
                                )
                              }))
                              self$tab_mi_status<-mi_status

                              successful<-mi_status$imputation[mi_status$status == "Used"]
                              failed<-mi_status$imputation[mi_status$status == "Failed"]
                              not_converged<-mi_status$imputation[mi_status$status == "Used" & mi_status$converged != "TRUE"]

                              mi_warnings<-unique(mi_status$warning[nchar(mi_status$warning) > 0])
                              if (is.something(mi_warnings)) {
                                for (w in mi_warnings)
                                  self$warnings<-list(topic="main",message=w)
                              }

                              if (length(successful) == 0) {
                                self$errors<-mi_status$error[nchar(mi_status$error) > 0]
                                if (!is.something(self$errors))
                                  self$errors<-"All imputed SEM models failed."
                                return(self$errors)
                              }
                              if (length(failed) > 0)
                                self$warnings<-list(topic="main",message=paste0("MI pooling used ", length(successful), " of ", length(mi_results), " imputations because ", length(failed), " imputed model(s) failed. See MI Imputation Status."))
                              if (length(not_converged) > 0)
                                self$warnings<-list(topic="main",message=paste0("Some imputed SEM models did not converge: ", paste(not_converged, collapse=", "), "."))

                              self$missing_info$successful_imputations<-length(successful)
                              mi_models<-lapply(successful, function(i) mi_results[[i]]$obj)
                              mi_data<-lapply(successful, function(i) missing$imputed_data[[i]])
                              self$tab_mi_fit<-fit_measures_table(mi_models, imputation=successful)
                              self$tab_mi_fit_summary<-fit_measures_summary(self$tab_mi_fit)
                              pooled_fit<-pooled_mi_fit_table(mi_data, lavoptions)
                              self$tab_mi_pooled_fit<-pooled_fit$table
                              if (is.something(pooled_fit$warning))
                                self$warnings<-list(topic="main",message=pooled_fit$warning)
                              results<-mi_results[[successful[[1]]]]
                            } else {
                              results<-run_sem_model(lavoptions)
                            }
                            ginfo("done")
                            
                            

                            self$warnings<-list(topic="main",message=results$warning)
                            self$errors<-results$error
                            
                            if (is.something(self$errors))
                                return(self$errors)
                            
                            ## ask for the paramters estimates
                            self$model<-results$obj
                            if (self$missing_info$method == "mi") {
                              mi_params<-lapply(mi_models, function(model) {
                                lavaan::parameterestimates(
                                  model,
                                  ci=self$options$ci,
                                  standardized = T,
                                  level = self$ciwidth,
                                  boot.ci.type = self$options$bootci
                                )
                              })
                              .lav_params<-pool_results_if_needed(mi_params, ci=self$options$ci, level=self$ciwidth)
                            } else {
                              .lav_params<-lavaan::parameterestimates(
                                self$model,
                                ci=self$options$ci,
                                standardized = T,
                                level = self$ciwidth,
                                boot.ci.type = self$options$bootci
                              )
                            }
                            if (!is.data.frame(.lav_params) || nrow(.lav_params)==0) {
                              self$errors<-"No parameter estimates were returned for this model. Check that all imported syntax variables are numeric, present in the data, and have usable variance."
                              return(self$errors)
                            }

                                                      
                            ## we need some info initialized by Syntax regarding the parameters properties
                            .lav_structure<-private$.lav_structure
                             sel<-grep("==|<|>",.lav_structure$op,invert = T)
                            .lav_structure<-.lav_structure[sel,]
                            ## make some change to render the results
                            
                            .lav_params$rhs<-fromb64(.lav_params$rhs,self$vars)
                            .lav_params$lhs<-fromb64(.lav_params$lhs,self$vars)
                            .lav_params$free<-(.lav_structure$free>0)
                            if (is.something(self$multigroup) && "group" %in% names(.lav_params)) {
                              .lav_params$lgroup<-rep("All", nrow(.lav_params))
                              valid_group<-!is.na(.lav_params$group) & .lav_params$group > 0 & .lav_params$group <= length(self$multigroup$levels)
                              .lav_params$lgroup[valid_group]<-self$multigroup$levels[.lav_params$group[valid_group]]
                            } else
                              .lav_params$lgroup<-rep("1", nrow(.lav_params))
                            
                            .lav_params$endo<-rep(FALSE, nrow(.lav_params))
                            .lav_params$endo[.lav_params$lhs %in% self$options$endogenous | .lav_params$rhs %in% self$options$endogenous]<-TRUE
                            ## collect regression coefficient table
                            self$tab_coefficients<-.lav_params[.lav_params$op=="~",]
                            self$tab_effects<-self$effectsTable(.lav_params)
                            self$tab_mediation_decomp<-self$mediationDecompositionTable()

                            ## collect variances and covariances table
                            self$tab_covariances<-.lav_params[.lav_params$op=="~~",]
                            self$tab_covariances$type<-ifelse(self$tab_covariances$endo,"Residuals","Variables")
                            
                            ## collect defined parameters table
                            self$tab_defined<-.lav_params[.lav_params$op==":=",]
                            if (nrow(self$tab_defined)==0) self$tab_defined<-NULL
                            
                            # prepare and comput R2 and collect a table for them
                            tab<-self$tab_covariances
                            end<-tab[tab$lhs %in% self$options$endogenous & tab$lhs==tab$rhs,]
                            self$computeR2(end)

                            
                            ### collect intercepts
                            self$tab_intercepts<-.lav_params[.lav_params$op=="~1",]
                            if (nrow(self$tab_intercepts)==0) self$tab_intercepts<-NULL
                            
                            
                            #### fit tests ###
                            alist<-list()
                            results<-try_hard(lavaan::fitmeasures(self$model))
                     
                            if (is.something(results$obj)) {
                                    ff<-results$obj
                                    alist<-list()
                                    if (ff[["df"]]>0)
                                        alist[[1]]<-list(label="User Model",
                                               chisq=ff[["chisq"]],
                                               df=ff[["df"]],
                                               pvalue=ff[["pvalue"]],
                                               ..space..=1)
                                    
                                    self$tab_fitindices<-as.list(ff)
                                    
                                    if (is.something(self$options$tests)) {
                                      
                                      tests<-lavaan::lavTest(self$model,test = unlist(self$options$tests))
                                      ## if only an additional test is required, lavaan produces a list
                                      ## of properties of one test, not a list of tests
                                      if (length(tests)>5) tests<-list(tests)

                                      for (i in seq_along(tests)) {
                                        t<-tests[[i]]
                                        
                                        label  <-  strsplit(t$test,".",fixed = T)[[1]]
                                        label<-paste(lapply(label,function(x) stringr::str_to_title(x)),collapse = "-")
                                        chisq  <-  t$stat
                                        df     <-  t$df
                                        p      <-  t$pvalue
                                        alist[[length(alist)+1]]<-list(label=label,chisq=chisq,df=df,pvalue=p)
                                      }
                                      
                                    }

                                    try(alist[[length(alist)+1]]<-list(
                                      label="Baseline Model",
                                      chisq=ff[["baseline.chisq"]],
                                      df=ff[["baseline.df"]],
                                      pvalue=ff[["baseline.pvalue"]],
                                      ..space..=1))
                                    
                            
                                    if (is.something(self$multigroup)) {
                                       alist[[length(alist)+1]]<-list(label="Groups statistics",chisq="",df="",pvalue="",..space..=2)
                                       multitests<-self$multitest()
                                       for (r in seq_along(multitests))
                                            alist[[length(alist)+1]]<-multitests[[r]]
                                    }
                                    self$tab_fit<-alist
                                    
                            } else {
                              self$warnings<-list(topic="tab_fitindices",message=results$warning)
                              self$warnings<-list(topic="tab_fitindices",message=results$error)
                            }
                            
                            
                            # fit indices
                            alist<-list()
                            alist[[length(alist)+1]]<-c(info="Estimation Method",value=self$model@Options$estimator)
                            alist[[length(alist)+1]]<-c(info="Missing data method",value=self$missing_info$method_label)
                            alist[[length(alist)+1]]<-c(info="Missing data note",value=ifelse(isTRUE(self$missing_info$all_available), "All available data were used", ""))
                            if (self$missing_info$method == "mi") {
                              alist[[length(alist)+1]]<-c(info="Imputations",value=self$missing_info$imputations)
                              alist[[length(alist)+1]]<-c(info="Successful imputations",value=self$missing_info$successful_imputations)
                              alist[[length(alist)+1]]<-c(info="MI group strategy",value=self$missing_info$mi_strategy_label)
                              alist[[length(alist)+1]]<-c(info="MI estimates note",value="Parameter estimates are pooled with Rubin's rules")
                              alist[[length(alist)+1]]<-c(info="MI fit note",value=ifelse(is.something(self$tab_mi_pooled_fit), "Formal pooled fit is computed with lavaan.mi; per-imputation fit is also shown", "Fit indices are shown by imputation and summarized descriptively"))
                            } else {
                              alist[[length(alist)+1]]<-c(info="Imputations",value="")
                            }
                            alist[[length(alist)+1]]<-c(info="Original observations",value=self$missing_info$n_original)
                            alist[[length(alist)+1]]<-c(info="Number of observations",value=self$missing_info$n_used)
                            alist[[length(alist)+1]]<-c(info="Missing values",value=self$missing_info$total_missing)
                            alist[[length(alist)+1]]<-c(info="Cases removed",value=self$missing_info$n_removed)
                            alist[[length(alist)+1]]<-c(info="Free parameters",value=self$model@Fit@npar)
                            alist[[length(alist)+1]]<-c(info="Converged",value=self$model@Fit@converged) 
                            alist[[length(alist)+1]]<-c(info="",value="")
                            try(alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value=round(ff[["logl"]],digits=3) ))
                            try(alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value=round(ff[["unrestricted.logl"]],digits=3)))
                            alist[[length(alist)+1]]<-c(info="",value="")
                            
                            self$tab_info<-alist

                            if (length(grep("==|<|>",private$.lav_structure$op))) {
                              check<-sapply(self$constraints,function(con) length(grep("<|>",con$value))>0,simplify = T)
                              if (any(check)) {
                                self$warnings<-list(topic="main",message=WARNS[["scoreineq"]])
                              } else {
                                tab<-lavaan::lavTestScore(self$model,
                                                          univariate = self$options$scoretest,
                                                          cumulative = self$options$cumscoretest)
                                
                                if (self$options$scoretest) {
                                  names(tab$uni)<-c("lhs","op","rhs","chisq","df","pvalue")
                                  self$tab_constfit<-tab$uni
                                  self$tab_constfit$type="Univariate"
                                }
                                if (self$options$cumscoretest) {
                                  names(tab$cumulative)<-c("lhs","op","rhs","chisq","df","pvalue")
                                  tab$cumulative$type<-"Cumulative"
                                  self$tab_constfit<-rbind(self$tab_constfit,tab$cumulative)
                                }
                                
                                self$tab_constfit$lhs<-gsub(".","",self$tab_constfit$lhs,fixed = T)
                                self$tab_constfit$rhs<-gsub(".","",self$tab_constfit$rhs,fixed = T)
                                
                                self$tab_fit[[length(self$tab_fit)+1]]<-list(label="Constraints Score Test",
                                                                     chisq=tab$test$X2,
                                                                     df=tab$test$df,
                                                                     pvalue=tab$test$p.value,..space..=3)
                                
                                
                              }
                            } # end of checking constraints
                            
                            # modification indices (diagnostics)
                            if (isTRUE(self$options$modindices)) {
                              mires <- try_hard({ lavaan::modindices(self$model) })
                              if (isFALSE(mires$error)) {
                                mi <- mires$obj
                                if (nrow(mi)==0) {
                                  self$warnings<-list(topic="modindices",message="No fixed parameter available to compute modification indexes.")
                                  self$tab_mi<-data.frame(
                                    lgroup=character(0),
                                    lhs=character(0),
                                    op=character(0),
                                    rhs=character(0),
                                    mi=numeric(0),
                                    epc=numeric(0),
                                    sepc.all=numeric(0),
                                    stringsAsFactors=FALSE
                                  )
                                } else {
                                  # threshold filter
                                  if (is.something(self$options$miMin))
                                    mi <- mi[!is.na(mi$mi) & mi$mi >= self$options$miMin, , drop=FALSE]
                                  if (nrow(mi)==0) {
                                    self$warnings<-list(topic="modindices",message="No modification indexes met the selected minimum threshold.")
                                    self$tab_mi<-data.frame(
                                      lgroup=character(0),
                                      lhs=character(0),
                                      op=character(0),
                                      rhs=character(0),
                                      mi=numeric(0),
                                      epc=numeric(0),
                                      sepc.all=numeric(0),
                                      stringsAsFactors=FALSE
                                    )
                                  } else {
                                    # add group label if multigroup
                                    if (is.something(self$multigroup)) {
                                      mi$lgroup <- self$multigroup$levels[mi$group]
                                    } else {
                                      mi$lgroup <- rep("1", nrow(mi))
                                    }
                                    # decode names
                                    mi$lhs <- fromb64(mi$lhs, self$vars)
                                    mi$rhs <- fromb64(mi$rhs, self$vars)
                                    # keep relevant columns if present
                                    keep <- c("lgroup","lhs","op","rhs","mi","epc","sepc.all")
                                    cols <- intersect(keep, names(mi))
                                    mi <- mi[, cols, drop=FALSE]
                                    # order by MI descending
                                    if ("mi" %in% names(mi))
                                      mi <- mi[order(-mi$mi), , drop=FALSE]
                                    self$tab_mi <- mi
                                  }
                                }
                              } else {
                                self$warnings <- list(topic="modindices", message = mires$warning)
                                self$warnings <- list(topic="modindices", message = mires$error)
                              }
                            }

                            self$buildSyntaxTables()
                            self$buildPathLegend()
                            self$buildIntelligentReport(data, ordered_vars, lavoptions[["estimator"]])

                            ginfo("Estimation is done...")
                          }, # end of private function estimate
                          buildPathLegend=function() {
                            self$tab_path_legend<-data.frame(
                              item=c("Path label: est", "Path label: beta", "No star", "*", "**", "***"),
                              meaning=c("Unstandardized coefficient is shown on each path.",
                                        "Standardized coefficient is shown on each path.",
                                        "p >= .05 or p-value unavailable.",
                                        "p < .05.",
                                        "p < .01.",
                                        "p < .001."),
                              stringsAsFactors=FALSE
                            )
                          },
                          buildSyntaxTables=function() {
                            coefs<-self$tab_coefficients
                            covs<-self$tab_covariances
                            defs<-self$tab_defined

                            lines<-character(0)
                            if (is.something(coefs) && nrow(coefs)>0) {
                              rows<-lapply(split(coefs, coefs$lhs), function(x) {
                                paste0(x$lhs[[1]], " ~ ", paste(unique(x$rhs), collapse=" + "))
                              })
                              lines<-c(lines, unlist(rows, use.names=FALSE))
                            }
                            if (is.something(covs) && nrow(covs)>0) {
                              cov_rows<-covs[covs$lhs != covs$rhs, , drop=FALSE]
                              if (nrow(cov_rows)>0)
                                lines<-c(lines, paste0(cov_rows$lhs, " ~~ ", cov_rows$rhs))
                            }
                            if (is.something(defs) && nrow(defs)>0)
                              lines<-c(lines, paste0(defs$lhs, " := ", defs$rhs))
                            if (length(lines)==0)
                              lines<-""
                            self$tab_lavaan_syntax<-data.frame(
                              line=seq_along(lines),
                              code=lines,
                              stringsAsFactors=FALSE
                            )

                            if (is.something(self$import)) {
                              mermaid<-self$import$exports$mermaid
                              mplus<-self$import$exports$mplus
                              openmx<-self$import$exports$openmx
                            } else {
                              exported<-try_hard({
                                sem_import_parse(paste(lines, collapse="\n"), "lavaan")
                              })
                              if (!isFALSE(exported$error)) {
                                mermaid<-c("flowchart LR")
                                mplus<-character(0)
                                openmx<-character(0)
                              } else {
                                mermaid<-exported$obj$exports$mermaid
                                mplus<-exported$obj$exports$mplus
                                openmx<-exported$obj$exports$openmx
                              }
                            }
                            self$tab_mermaid_syntax<-data.frame(
                              line=seq_along(mermaid),
                              code=mermaid,
                              stringsAsFactors=FALSE
                            )
                            self$tab_mplus_syntax<-data.frame(
                              line=seq_along(mplus),
                              code=mplus,
                              stringsAsFactors=FALSE
                            )
                            self$tab_openmx_syntax<-data.frame(
                              line=seq_along(openmx),
                              code=openmx,
                              stringsAsFactors=FALSE
                            )
                          },
                          buildIntelligentReport=function(data, ordered_vars=NULL, estimator_used=NULL) {
                            if (!isTRUE(self$options$intelligentReport))
                              return()

                            report_data <- data
                            report<-try_hard({
                              generate_report(
                                self$model,
                                data=report_data,
                                teaching_mode=self$options$reportLevel,
                                cluster=tob64(self$options$clusterVariable,self$vars),
                                within=tob64(self$options$withinVariables,self$vars),
                                between=tob64(self$options$betweenVariables,self$vars),
                                pcurve_target=self$options$pcurve_target)
                            })
                            if (!isFALSE(report$error)) {
                              message<-paste("Intelligent report could not be generated:", report$error)
                              self$warnings<-list(topic="main",message=message)
                              self$tab_report_text<-data.frame(section="Intelligent Report", text=message, stringsAsFactors=FALSE)
                              self$tab_report_paragraph<-data.frame(
                                warning="AI-assisted statistical text is a draft. Check the output, reviewer expectations, theory, and common sense before using it in a manuscript.",
                                paragraph=message,
                                stringsAsFactors=FALSE
                              )
                              self$tab_report_html<-self$reportParagraphHtml(self$tab_report_paragraph$warning[[1]], self$tab_report_paragraph$paragraph[[1]])
                              self$tab_assumptions<-data.frame(
                                check="Intelligent report",
                                status_icon="Warning",
                                status="Unavailable",
                                explanation=message,
                                recommendation="Check model convergence, variable types, and imported syntax; the core model estimates may still be available.",
                                stringsAsFactors=FALSE
                              )
                              self$tab_recommendations<-data.frame(recommendation="Review the debug message and verify that the model returned valid parameter estimates.", stringsAsFactors=FALSE)
                              self$tab_group_comparison<-data.frame()
                              self$tab_mediation_decomp<-self$mediationDecompositionTable()
                              self$tab_insights<-data.frame()
                              self$tab_pcurve<-data.frame(n_tests=0L, n_significant=0L, prop_p_lt_025=NA_real_)
                              self$tab_model_comparison<-data.frame(model="Current", chisq=NA_real_, df=NA_real_, pvalue=NA_real_, cfi=NA_real_, tli=NA_real_, rmsea=NA_real_, srmr=NA_real_, aic=NA_real_, bic=NA_real_, best_fit="", stringsAsFactors=FALSE)
                              return()
                            }
                            report<-report$obj

                            report_rows<-list(
                              data.frame(section="Model Fit", text=report$fit$text, stringsAsFactors=FALSE),
                              data.frame(section="Direct Effects", text=report$paths$text, stringsAsFactors=FALSE),
                              data.frame(section="Mediation", text=report$mediation$text, stringsAsFactors=FALSE),
                              data.frame(section="Moderation", text=report$moderation$text, stringsAsFactors=FALSE),
                              data.frame(section="Multigroup", text=report$multigroup$text, stringsAsFactors=FALSE),
                              data.frame(section="Multilevel", text=report$multilevel$text, stringsAsFactors=FALSE),
                              data.frame(section="Missing Data and Estimator", text=paste0(
                                "Estimator used: ", estimator_used,
                                if (length(ordered_vars)>0) paste0("; ordered variables: ", paste(fromb64(ordered_vars,self$vars), collapse=", ")) else "",
                                ". Missing data method: ", self$missing_info$method_label, "."
                              ), stringsAsFactors=FALSE),
                              data.frame(section="Model Insights", text=report$insights$text, stringsAsFactors=FALSE),
                              data.frame(section="Modification Indices", text=report$modification_indices$text, stringsAsFactors=FALSE),
                              data.frame(section="P-Curve", text=report$p_curve$text, stringsAsFactors=FALSE)
                            )
                            self$tab_report_text<-self$decodeReportTable(do.call(rbind, report_rows))
                            self$tab_report_paragraph<-data.frame(
                              warning="AI-assisted statistical text is a draft. Check the output, reviewer expectations, theory, and common sense before using it in a manuscript.",
                              paragraph=paste(self$tab_report_text$text, collapse=" "),
                              stringsAsFactors=FALSE
                            )
                            self$tab_report_html<-self$reportParagraphHtml(self$tab_report_paragraph$warning[[1]], self$tab_report_paragraph$paragraph[[1]])

                            self$tab_assumptions<-self$decodeReportTable(report$diagnostics)
                            if (is.something(self$tab_assumptions)) {
                              for (nm in c("check","status","explanation","recommendation"))
                                if (!nm %in% names(self$tab_assumptions)) self$tab_assumptions[[nm]]<-NA_character_
                              self$tab_assumptions$status_icon<-ifelse(self$tab_assumptions$status=="Met", "OK",
                                                                       ifelse(self$tab_assumptions$status=="Violated", "Violated", "Warning"))
                              self$tab_assumptions<-self$tab_assumptions[,c("check","status_icon","status","explanation","recommendation"),drop=FALSE]
                            }

                            recs<-report$recommendations
                            recs<-as.character(unname(recs))
                            recs<-recs[!is.na(recs) & nzchar(recs)]
                            if (length(recs)==0)
                              recs<-"No additional recommendations were generated."
                            self$tab_recommendations<-data.frame(
                              step=seq_along(recs),
                              recommendation=recs,
                              stringsAsFactors=FALSE
                            )

                            self$tab_group_comparison<-self$decodeReportTable(report$multigroup$table)
                            self$tab_mediation_decomp<-self$mediationDecompositionTable()
                            self$tab_insights<-self$decodeReportTable(self$insightsTable(report$insights))
                            self$tab_pcurve<-report$p_curve$summary

                            cmp<-compare_models(Current=self$model)
                            self$tab_model_comparison<-cmp$table
                            self$tab_report_text<-rbind(
                              self$tab_report_text,
                              self$decodeReportTable(data.frame(section="Model Selection", text=cmp$text, stringsAsFactors=FALSE))
                            )
                          },
                          decodeReportTable=function(tab) {
                            if (!is.something(tab) || nrow(tab)==0)
                              return(tab)
                            for (nm in names(tab)) {
                              if (is.character(tab[[nm]]))
                                tab[[nm]]<-fromb64(tab[[nm]], self$vars)
                            }
                            tab
                          },
                          reportParagraphHtml=function(warning, paragraph) {
                            escape_html<-function(x) {
                              x<-gsub("&", "&amp;", x, fixed=TRUE)
                              x<-gsub("<", "&lt;", x, fixed=TRUE)
                              x<-gsub(">", "&gt;", x, fixed=TRUE)
                              x<-gsub("\"", "&quot;", x, fixed=TRUE)
                              x
                            }
                            warning<-escape_html(warning)
                            paragraph<-escape_html(paragraph)
                            paste0(
                              "<div style='border-left: 4px solid #2d6cdf; background: #f4f8ff; padding: 12px 14px; margin: 4px 0 12px 0; border-radius: 4px;'>",
                              "<div style='font-weight: 700; margin-bottom: 6px;'>APA reporting draft</div>",
                              "<div style='color: #6b4e00; background: #fff7d6; border: 1px solid #ead27a; padding: 8px; border-radius: 3px; margin-bottom: 10px;'>",
                              warning,
                              "</div>",
                              "<div style='white-space: pre-wrap; line-height: 1.45;'>",
                              paragraph,
                              "</div>",
                              "</div>"
                            )
                          },
                          mediationDecompositionTable=function() {
                            effects<-self$tab_effects
                            if (!is.something(effects) || nrow(effects)==0)
                              return(data.frame(
                                lgroup="1",
                                predictor="No mediation paths available",
                                mediator="",
                                outcome="",
                                direct=NA_real_,
                                indirect=NA_real_,
                                total=NA_real_,
                                percent_mediated=NA_real_,
                                stringsAsFactors=FALSE
                              ))
                            indirect<-effects[effects$effect=="Indirect",,drop=FALSE]
                            if (nrow(indirect)==0)
                              return(data.frame(
                                lgroup="1",
                                predictor="No defined indirect effects",
                                mediator="",
                                outcome="",
                                direct=NA_real_,
                                indirect=NA_real_,
                                total=NA_real_,
                                percent_mediated=NA_real_,
                                stringsAsFactors=FALSE
                              ))
                            direct<-effects[effects$effect=="Direct",,drop=FALSE]
                            total<-effects[effects$effect=="Total",,drop=FALSE]
                            rows<-lapply(seq_len(nrow(indirect)), function(i) {
                              r<-indirect[i,,drop=FALSE]
                              d<-direct[direct$predictor==r$predictor & direct$outcome==r$outcome & direct$lgroup==r$lgroup,,drop=FALSE]
                              t<-total[total$predictor==r$predictor & total$outcome==r$outcome & total$lgroup==r$lgroup,,drop=FALSE]
                              total_est<-if (nrow(t)>0) t$est[[1]] else if (nrow(d)>0) d$est[[1]] + r$est[[1]] else NA_real_
                              data.frame(
                                lgroup=r$lgroup,
                                predictor=r$predictor,
                                mediator=ifelse(grepl("\U21d2", r$pathway), paste(strsplit(r$pathway, " \U21d2 ")[[1]][-c(1,length(strsplit(r$pathway, " \U21d2 ")[[1]]))], collapse=" \U21d2 "), ""),
                                outcome=r$outcome,
                                direct=if (nrow(d)>0) d$est[[1]] else NA_real_,
                                indirect=r$est[[1]],
                                total=total_est,
                                percent_mediated=if (!is.na(total_est) && total_est != 0) 100 * r$est[[1]] / total_est else NA_real_,
                                stringsAsFactors=FALSE
                              )
                            })
                            do.call(rbind, rows)
                          },
                          insightsTable=function(insights) {
                            rows<-list()
                            add_rows<-function(tab, label) {
                              if (!is.something(tab) || nrow(tab)==0)
                                return(NULL)
                              tab<-tab[,intersect(c("lhs","rhs","beta","effect_size","pvalue"),names(tab)),drop=FALSE]
                              for (nm in c("lhs","rhs","beta","effect_size","pvalue"))
                                if (!nm %in% names(tab)) tab[[nm]]<-NA
                              data.frame(
                                insight=label,
                                outcome=tab$lhs,
                                predictor=tab$rhs,
                                beta=tab$beta,
                                effect_size=tab$effect_size,
                                pvalue=tab$pvalue,
                                stringsAsFactors=FALSE
                              )
                            }
                            rows[[length(rows)+1]]<-add_rows(insights$strongest_predictors,"Strongest predictor")
                            rows[[length(rows)+1]]<-add_rows(insights$risk_factors,"Risk/amplifying factor")
                            rows[[length(rows)+1]]<-add_rows(insights$protective_factors,"Protective/buffering factor")
                            rows<-rows[!vapply(rows,is.null,logical(1))]
                            if (!is.something(rows))
                              return(NULL)
                            do.call(rbind,rows)
                          },
                          effectsTable=function(params) {
                            direct<-params[params$op=="~",,drop=FALSE]
                            if (nrow(direct)>0) {
                              direct$lgroup<-if ("lgroup" %in% names(direct)) direct$lgroup else "1"
                              direct$effect<-"Direct"
                              direct$predictor<-direct$rhs
                              direct$outcome<-direct$lhs
                              direct$pathway<-paste(direct$rhs,direct$lhs,sep=" \U21d2 ")
                            }
                            defs<-params[params$op==":=",,drop=FALSE]
                            meta<-self$effect_decomp_structure
                            if (is.something(meta) && nrow(defs)>0) {
                              defs<-merge(meta,defs,by.x="label",by.y="lhs",all=FALSE,sort=FALSE)
                              defs$lgroup<-defs$lgroup.x
                            } else {
                              defs<-NULL
                            }
                            keep<-c("lgroup","effect","predictor","outcome","pathway","est","se","ci.lower","ci.upper","std.all","z","pvalue","stars")
                            rows<-list()
                            if (nrow(direct)>0)
                              rows[[length(rows)+1]]<-direct
                            if (is.something(defs) && nrow(defs)>0)
                              rows[[length(rows)+1]]<-defs
                            if (!is.something(rows))
                              return(NULL)
                            out<-do.call(rbind,lapply(rows,function(x) {
                              for (nm in keep)
                                if (!nm %in% names(x)) x[[nm]]<-NA
                              x$stars<-effect_stars(x$pvalue)
                              x[,keep,drop=FALSE]
                            }))
                            out
                          },
                          
                          multitest=function() {
                            
                            tab<-self$structure
                            gstat<-self$model@test$standard$stat.group
                            ### compute df for each group ###
                            sel<-grep("==|<|>",private$.lav_structure$op)
                            con<-private$.lav_structure[sel,]
                            con$lhs<-gsub(".","",con$lhs,fixed=T)
                            con$rhs<-gsub(".","",con$rhs,fixed=T)
                            fixed<-unique(c(con$lhs,con$rhs))
                            g<-tab[tab$label %in% fixed,"group"]
                            df<-table(g)
                            groups<-1:self$multigroup$nlevels
                            dfs<-sapply(groups, function(g) ifelse(hasName(df,g),df[[as.character(g)]],0)) 
                            lapply(groups, function(g) {
                              df<-dfs[g]
                              if (df>0) {
                                chisq=gstat[g]
                                pvalue=pchisq(gstat[g], df=dfs[g], lower.tail=FALSE)
                              } else {
                                chisq=0
                                pvalue=1
                              }
                              list(
                                  label=paste("Group",g),
                                  chisq=chisq,
                                  df=df,
                                  pvalue=pvalue,
                                  ..space..=2
                              )
                            })
                                                        
                          },
                          
                          computeR2=function(end) {
                            
                            end$var<-end$est/end$std.all
                            upper<-end$ci.upper
                            lower<-end$ci.lower
                            end$ci.upper<-1-(lower/end$var)
                            end$ci.lower<-1-(upper/end$var)
                            end$r2<-1-end$std.all
                             for (i in seq_along(end$r2)) 
                                    if (end$r2[[i]]>1 | end$r2[[i]]<0)  {
                                      end$r2[[i]]<-NA
                                      self$warnings<-list(topic="r2",message="Some R-square index cannot be computed for this model")
                                    }
                            
                            if (self$options$r2ci=="fisher") {
                              ### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3821705/
                              N<-lavaan::lavInspect(self$model,"ntotal")
                              r<-sqrt(end$r2)
                              f<-.5 * log((1 + r)/(1 - r))
                              zr<-f*sqrt((N-3))
                              z0<-qnorm((1-self$ciwidth)/2,lower.tail = F)
                              
                              lower<-zr-z0
                              upper<-zr+z0
                              flower<-lower/sqrt(N-3)
                              fupper<-upper/sqrt(N-3)
                              rupper<-(exp(2*fupper)-1)/(1+exp(2*fupper))
                              rupper<-rupper^2
                              rlower<-(exp(2*flower)-1)/(1+exp(2*flower))
                              rlower<-rlower^2
                              end$ci.upper<-rupper
                              end$ci.lower<-rlower
                              ####
                            }
                            self$tab_r2<-end
                            if (!self$options$r2test)
                              return()
                            
                                    end$chisq<-0
                                    end$df<-0
                                    end$pvalue<-0
                                    if (!("group" %in% names(end)))
                                               end$group<-1
                                    
                                    .lav_structure<-private$.lav_structure

                                    sel<-(self$structure$lhs %in% unique(end$lhs) & self$structure$op=="~" & self$structure$group>0)
                                    .structure<-self$structure[sel,]
                                      for (i in seq_len(nrow(end))) {
                                            if (is.na(end$r2[i]))  {
                                              end$chisq[i]<-NaN   
                                              end$df[i]<-NaN
                                              end$pvalue[i]<-NaN
                                              next()
                                            }
                                            
                                            sel<-(.structure$lhs==end$lhs[i] &  .structure$group==end$group[i])
                                           ..structure<-.structure[sel,]
                                            const<-paste(..structure$label,0,sep="==",collapse = " ; ")
                                            results<-try_hard({tests<-lavaan::lavTestWald(self$model,const)})
                                            if (results$error!=FALSE) {
                                                  self$warnings<-list(topic="r2",message="Some inferential tests cannot be computed for this model")
                                                  self$warnings<-list(topic="r2",message=results$error)
                                                  end$chisq[i]<-NaN   
                                                  end$df[i]<-NaN
                                                  end$pvalue[i]<-NaN
                                            
                                            } else {
                                                end$chisq[i]<-tests$stat   
                                                end$df[i]<-tests$df
                                                end$pvalue[i]<-tests$p.value
                                          }
                                    }
                            self$tab_r2<-end
                            
                        } ## end of r2
                        

              ) # end of private
)  # end of class
