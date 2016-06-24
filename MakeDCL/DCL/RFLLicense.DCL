RFLLICENSE : dialog {
                      label = "RFL License Modification Tool";
                      initial_focus = "CANCEL";
                      : column {
                                 : spacer {
                                            height = 1;
                                          }
                                 :row {
                                        height = 2;
                                        : text {
                                                 value = "Package : ";
                                               }
                                        : text {
                                                 key = "PACKAGE";
                                                 is_bold = true;
                                               }
                                        : spacer {
                                                   width = 25;
                                                 }
                                      }
                                 :row {
                                        height = 2;
                                        : text {
                                                 value = "Version : ";
                                               }
                                        : text {
                                                 key = "VERSION";
                                               }
                                        : spacer {
                                                   width = 25;
                                                 }
                                      }
                                 :row {
                                        height = 2;
                                        : text {
                                                 value = "Client No. : ";
                                               }
                                        : text {
                                                 key = "CLIENT";
                                               }
                                        : spacer {
                                                   width = 25;
                                                 }
                                      }
                                 :row {
                                        height = 2;
                                        : text {
                                                 value = "Expiration Date : ";
                                               }
                                        : text {
                                                 key = "EXPIREDATE";
                                               }
                                        : spacer {
                                                   width = 25;
                                                 }
                                      }
                                 : spacer {
                                            height = 1;
                                          }
                                 : edit_box {
                                              key = "REQUEST";
                                              label = "Request Code : ";
                                              edit_width = 60;
                                            }
                                 : spacer {
                                            height = 1;
                                          }
                                 : edit_box {
                                              key = "LICENSE";
                                              label = "License Key : ";
                                              edit_width = 60;
                                            }
                                 : spacer {
                                            height = 1;
                                          }
                               }
                      : row {
                              : cancel_button {
                                                key = "CANCEL";
                                                label = "Cancel";
                                                width = 10;
                                                alignment = left;
                                                fixed_width = true;
                                              }
                              : button {
                                         key = "REMOVE";
                                         label = "Remove";
                                         width = 10;
                                         alignment = centered;
                                         fixed_width = true;
                                       }
                              : ok_button {
                                            key = "ACCEPT";
                                            label = "Accept";
                                            width = 10;
                                            alignment = right;
                                            fixed_width = true;
                                          }
                            }
                      : spacer {
                                 height = 1;
                               }
                      : text {
                               value = "Press 'Cancel' to close and make no changes.";
                             }
                      : text {
                               value = "Press 'Remove' to remove the entry from the registry.";
                             }
                      : text {
                               value = "Press 'Accept' to commit the updated licence key to the registry.";
                             }
                    }
