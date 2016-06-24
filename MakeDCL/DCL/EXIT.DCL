EXIT : dialog {
                label = "RFL Exit Ramp Creator";
                : column {
                           alignment = centered;
                           : boxed_row {
                                         label = "Start settings:";
                                         : column {
                                                    : row {
                                                            : radio_button {
                                                                             label = "Left";
                                                                             key = "LEFT";
                                                                             value = "1";
                                                                           }
                                                            : radio_button {
                                                                             label = "Right";
                                                                             key = "RIGHT";
                                                                             value = "0";
                                                                           }
                                                          }
                                                    : edit_box {
                                                                 label = "Radius:";
                                                                 key = "RADIUS";
                                                                 value = "2000.000";
                                                                 edit_width = 12;
                                                               }
                                                    : edit_box {
                                                                 label = "Def. angle:";
                                                                 key = "DEFLECTION";
                                                                 value = "0.000";
                                                                 edit_width = 12;
                                                               }
                                                    : edit_box {
                                                                 label = "O/S:";
                                                                 key = "OFFSET";
                                                                 value = "0.000";
                                                                 edit_width = 12;
                                                               }
                                                  }
                                       }
                           : boxed_row {
                                         label = "Stepping:";
                                         : column {
                                                    : edit_box {
                                                                 label = "Start sta.:";
                                                                 key = "START";
                                                                 value = "0.000";
                                                                 edit_width = 12;
                                                               }
                                                    : edit_box {
                                                                 label = "End sta.:";
                                                                 key = "END";
                                                                 value = "0.000";
                                                                 edit_width = 12;
                                                               }
                                                    : edit_box {
                                                                 label = "Steps:";
                                                                 key = "STEPS";
                                                                 value = "1";
                                                                 edit_width = 12;
                                                               }
                                                  }
                                       }
                           : row {
                                   : ok_button {
                                                 label = "OK";
                                                 key = "OK";
                                                 is_default = true;
                                               }
                                   : cancel_button {
                                                     label = "Cancel";
                                                     key = "CANCEL";
                                                   }
                                 }
                         }
              }
