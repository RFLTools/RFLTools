MAKEENT : dialog {
                   label = "RFL Block Creator";
                   : row {
                           : column {
                                      width = 20;
                                      : list_box {
                                                   key = "BLOCKNAME";
                                                 }
                                    }
                           : column {
                                      : image {
                                                key = "IMAGE";
                                                width = 40;
                                                height = 18;
                                                color = 0;
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