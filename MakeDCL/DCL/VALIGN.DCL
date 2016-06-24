VALIGN : dialog {
                  label = "RFL Vertical Alignment Routines";
                  initial_focus = "OK";
                  : column {
                             : boxed_row {
                                           label = "Define";
                                           : button {
                                                      label = "From Screen";
                                                      key = "GPROF";
                                                      width = 24;
                                                    }
                                           : button {
                                                      label = "From File";
                                                      key = "RPROF";
                                                      width = 24;
                                                    }
                                           : button {
                                                      label = "From EMXS";
                                                      key = "PRO2PROF";
                                                      width = 24;
                                                    }
                                         }
                             : boxed_row {
                                           label = "Save";
                                           : button {
                                                      label = "To File";
                                                      key = "WPROF";
                                                      width = 24;
                                                    }
                                           : button {
                                                      label = "To EMXS";
                                                      key = "PROF2PRO";
                                                      width = 24;
                                                    }
                                           : spacer {
                                                      width = 24;
                                                    }
                                         }
                             : boxed_row {
                                           label = "Draw";
                                           : button {
                                                      label = "To Screen";
                                                      key = "DPROF";
                                                      width = 24;
                                                    }
                                           : spacer {
                                                      width = 24;
                                                    }
                                           : spacer {
                                                      width = 24;
                                                    }
                                         }
                             : row {
                                     : cancel_button {
                                                       key = "CANCEL";
                                                       label = "Cancel";
                                                     }
                                   }
                           }
                }
