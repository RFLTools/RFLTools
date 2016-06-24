VP : dialog {
              label = "RFL Profile Point Locator";
              initial_focus = "STATION";
              : column {
                         : row {
                                 : edit_box {
                                              label = "Sta:";
                                              key = "STATION";
                                              value = "";
                                              edit_width = 10;
                                            }
                                 : edit_box {
                                              label = "Elev:";
                                              key = "ELEV";
                                              value = "";
                                              edit_width = 10;
                                            }
                               }
                         : row {
                                 : button {
                                            label = "Pick";
                                            key = "PICK";
                                          }
                               }
                         : row {
                                 : cancel_button {
                                                   key = "CANCEL";
                                                   label = "Cancel";
                                                 }
                                 : ok_button {
                                               key = "OK";
                                               label = "OK";
                                             }
                               }
                       }
            }