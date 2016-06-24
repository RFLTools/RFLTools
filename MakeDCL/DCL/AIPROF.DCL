AIPROF : dialog {
                  label = "RFL AI Vertical Profile Creator";
                  : row {
                          : column {
                                     : toggle {
                                                label = "Draw Profile";
                                                key = "DPROF";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Label Slope";
                                                key = "LSLOPE";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Label Curve Length";
                                                key = "LL";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Label K Value";
                                                key = "LK";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Circle Nodes";
                                                key = "CNODES";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Draw PVI";
                                                key = "DPVI";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Label PVI";
                                                key = "LPVI";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Label BVC/EVC";
                                                key = "LBVC";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Label High Chainage";
                                                key = "LHIGH";
                                                value = "0";
                                              }
                                     : toggle {
                                                label = "Label Curve Elevations";
                                                key = "LELEVATIONS";
                                                value = "1";
                                              }
                                     : toggle {
                                                label = "Reverse Above/Below";
                                                key = "RAB";
                                                value = "0";
                                              }
                                   }
                          : column {
                                     : image {
                                               key = "IMAGE";
                                               width = 80;
                                               height = 20;
                                               color = 0;
                                             }
                                   }
                          : column {
                                     spacer;
                                     spacer;
                                     : boxed_column {
                                                      label = "Precisions";
                                                      : edit_box {
                                                                   label = "K : ";
                                                                   key = "KPREC";
                                                                   value = "3";
                                                                   edit_width = 1;
                                                                 }
                                                      : edit_box {
                                                                   label = "L : ";
                                                                   key = "LPREC";
                                                                   value = "3";
                                                                   edit_width = 1;
                                                                 }
                                                      : edit_box {
                                                                   label = "Slope : ";
                                                                   key = "SLOPEPREC";
                                                                   value = "2";
                                                                   edit_width = 1;
                                                                 }
                                                      : edit_box {
                                                                   label = "Sta : ";
                                                                   key = "STAPREC";
                                                                   value = "3";
                                                                   edit_width = 1;
                                                                 }
                                                      : edit_box {
                                                                   label = "Elev : ";
                                                                   key = "ELEVPREC";
                                                                   value = "3";
                                                                   edit_width = 1;
                                                                 }
                                                    }
                                     spacer;
                                     spacer;
                                   }
                        }
                  : row {
                          : ok_button {
                                        label = "OK";
                                        key = "OK";
                                        is_default = true;
                                      }
                          : row {
                                  : radio_button {
                                                   label = "Left";
                                                   key = "DIRLEFT";
                                                 }
                                  : radio_button {
                                                   label = "Right";
                                                   key = "DIRRIGHT";
                                                 }
                                  : radio_button {
                                                   label = "Up Chainage";
                                                   key = "DIRUP";
                                                 }
                                  : radio_button {
                                                   label = "Down Chainage";
                                                   key = "DIRDOWN";
                                                 }
                                }
                          : cancel_button {
                                            label = "Cancel";
                                            key = "CANCEL";
                                          }
                        }
                }
