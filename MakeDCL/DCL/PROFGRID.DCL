PROFGRID : dialog {
                    label = "RFL Profile Grid Creator";
                    : boxed_row {
                                  label = "Grid";
                                  : row {
                                          : column {
                                                     : toggle {
                                                                label = "Hor";
                                                                key = "GRIDHOR";
                                                                value = "1";
                                                              }
                                                   }
                                          : column {
                                                     : row {
                                                             : edit_box {
                                                                          label = "Spacing";
                                                                          key = "GRIDHORSPACE";
                                                                          edit_width = 12;
                                                                          value = "20";
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Layer";
                                                                          key = "GRIDHORLAYER";
                                                                          edit_width = 12;
                                                                          value = "GRID";
                                                                        }
                                                           }
                                                   }
                                        }
                                  : row {
                                          : column {
                                                     : toggle {
                                                                label = "Ver";
                                                                key = "GRIDVER";
                                                                value = "1";
                                                              }
                                                   }
                                          : column {
                                                     : row {
                                                             : edit_box {
                                                                          label = "Spacing";
                                                                          key = "GRIDVERSPACE";
                                                                          edit_width = 12;
                                                                          value = "20";
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Layer";
                                                                          key = "GRIDVERLAYER";
                                                                          edit_width = 12;
                                                                          value = "GRID";
                                                                        }
                                                           }
                                                   }
                                        }
                                }
                    : boxed_row {
                                  label = "Fine";
                                  : row {
                                          : column {
                                                     : toggle {
                                                                label = "Hor";
                                                                key = "GRIDFHOR";
                                                                value = "0";
                                                              }
                                                   }
                                          : column {
                                                     : row {
                                                             : edit_box {
                                                                          label = "Spacing";
                                                                          key = "GRIDFHORSPACE";
                                                                          edit_width = 12;
                                                                          value = "20";
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Layer";
                                                                          key = "GRIDFHORLAYER";
                                                                          edit_width = 12;
                                                                          value = "GRIDF";
                                                                        }
                                                           }
                                                   }
                                        }
                                  : row {
                                          : column {
                                                     : toggle {
                                                                label = "Ver";
                                                                key = "GRIDFVER";
                                                                value = "0";
                                                              }
                                                   }
                                          : column {
                                                     : row {
                                                             : edit_box {
                                                                          label = "Spacing";
                                                                          key = "GRIDFVERSPACE";
                                                                          edit_width = 12;
                                                                          value = "20";
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Layer";
                                                                          key = "GRIDFVERLAYER";
                                                                          edit_width = 12;
                                                                          value = "GRIDF";
                                                                        }
                                                           }
                                                   }
                                        }
                                }
                    : boxed_row {
                                  label = "Text";
                                  : row {
                                          : column {
                                                     : toggle {
                                                                label = "Hor";
                                                                key = "GRIDTHOR";
                                                                value = "1";
                                                              }
                                                   }
                                          : column {
                                                     : row {
                                                             : edit_box {
                                                                          label = "Spacing";
                                                                          key = "GRIDTHORSPACE";
                                                                          edit_width = 12;
                                                                          value = "20";
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Layer";
                                                                          key = "GRIDTHORLAYER";
                                                                          edit_width = 12;
                                                                          value = "GRIDT";
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Gap";
                                                                          key = "GRIDTHORGAP";
                                                                          edit_width = 12;
                                                                          value = "2.5";
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Height";
                                                                          key = "GRIDTHORHEIGHT";
                                                                          edit_width = 12;
                                                                          value = "2.5";
                                                                        }
                                                           }
                                                   }
                                        }
                                  : row {
                                          : column {
                                                     : toggle {
                                                                label = "Ver";
                                                                key = "GRIDTVER";
                                                                value = "1";
//                                                                is_enabled = 0;
                                                              }
                                                   }
                                          : column {
                                                     : row {
                                                             : edit_box {
                                                                          label = "Spacing";
                                                                          key = "GRIDTVERSPACE";
                                                                          edit_width = 12;
                                                                          value = "20";
//                                                                          is_enabled = 0;
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Layer";
                                                                          key = "GRIDTVERLAYER";
                                                                          edit_width = 12;
                                                                          value = "GRIDT";
//                                                                          is_enabled = 0;
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Gap";
                                                                          key = "GRIDTVERGAP";
                                                                          edit_width = 12;
                                                                          value = "2.5";
//                                                                          is_enabled = 0;
                                                                        }
                                                           }
                                                     : row {
                                                             : edit_box {
                                                                          label = "Height";
                                                                          key = "GRIDTVERHEIGHT";
                                                                          edit_width = 12;
                                                                          value = "2.5";
//                                                                          is_enabled = 0;
                                                                        }
                                                           }
                                                   }
                                        }
                                }
                    : row {
                            : cancel_button {
                                              key = "CANCEL";
                                              label = "Cancel";
                                              alignment = left;
                                            }
                            : ok_button {
                                          key = "OK";
                                          label = "OK";
                                          alignment = right;
                                        }
                         }
                  }
