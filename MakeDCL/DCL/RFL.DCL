RFL : dialog {
               label = "RFL - Commands!";
               : row {
                       : list_box {
                                    label = "Command list:";
                                    key = "COMMANDLIST";
                                    width = 20;
                                  }
                       spacer;
                       spacer;
                       spacer;
                       : column {
                                  alignment = right;
                                  width = 40;
                                  : text {
                                           key = "RFLCOMMAND";
                                           value = "This is a test";
                                         }
                                  spacer;
                                  : text {
                                           key = "RFLUSAGE1";
                                           value = "Usage1";
                                         }
                                  : text {
                                           key = "RFLUSAGE2";
                                           value = "Usage2";
                                         }
                                  : text {
                                           key = "RFLUSAGE3";
                                           value = "Usage3";
                                         }
                                  : text {
                                           key = "RFLUSAGE4";
                                           value = "Usage4";
                                         }
                                  : text {
                                           key = "RFLUSAGE5";
                                           value = "Usage5";
                                         }
                                  spacer;
                                }
                     }
               : row {
                       : ok_button {
                                     key = "OK";
                                     label = "GO";
                                     is_default = true;
                                     fixed_width = true;
                                     alignment = left;
                                   }
                       : cancel_button {
                                         key = "CANCEL";
                                         label = "Cancel";
                                         fixed_width = true;
                                         alignment = right;
                                       }
                     }
             }
