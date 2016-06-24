TBAY : dialog {
                label = "RFL Turn Bay Creator";
                : column {
                           : edit_box {
                                        label = "Radius";
                                        key = "RADIUS";
                                        value = "150.0";
                                        edit_width = 10;
                                      }
                           : edit_box {
                                        label = "Delta";
                                        key = "DELTA";
                                        value = "5d0'0.000\"";
                                        edit_width = 10;
                                      }
                           : edit_box {
                                        label = "Offset";
                                        key = "OFFSET";
                                        value = "3.5";
                                        edit_width = 10;
                                      }
                           : edit_box {
                                        label = "Storage Length";
                                        key = "LS";
                                        value = "60.0";
                                        edit_width = 10;
                                      }
                           : radio_row {
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
                           : row {
                                   : cancel_button {
                                                     label = "Cancel";
                                                     key = "CANCEL";
                                                   }
                                   : ok_button {
                                                 label = "OK";
                                                 key = "OK";
                                               }
                                 }
                         }
              }
