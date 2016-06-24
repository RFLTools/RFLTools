MDITCH : dialog {
                  label = "RFL 3D Polyline Offset";
                  : row {
                          : image {
                                    key = "IMAGE";
                                    width = 30;
                                    height = 20;
                                    color = 0;
                                  }
                        }
                  : boxed_row {
                                : edit_box {
                                             key = "D1";
                                             label = "D1 : ";
                                             edit_width = 6;
                                           }
                                : edit_box {
                                             key = "S1";
                                             label = "S1 : ";
                                             edit_width = 6;
                                           }
                                : edit_box {
                                             key = "D2";
                                             label = "D2 : ";
                                             edit_width = 6;
                                           }
                                : edit_box {
                                             key = "S2";
                                             label = "S2 : ";
                                             edit_width = 6;
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
