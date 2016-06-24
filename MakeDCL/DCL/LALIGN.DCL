LALIGN : dialog {
                  label = "RFL Horizontal Label Setup";
                  initial_focus = "CANCEL";
                  : column {
                             : boxed_row {
                                         label = "Node";
                                         : edit_box {
                                                      key = "NODEBLK";
                                                      label = "Block:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "NODEBLKSUFFIX";
                                                      label = "Suffix:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "NODEBLKCOLOR";
                                                      label = "Color:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "NODEBLKSCALE";
                                                      label = "Scale:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : spacer {
                                                    width = 40;
                                                  }
                                       }
                             : boxed_row {
                                         label = "Leader";
                                         : edit_box {
                                                      key = "LEADERX";
                                                      label = "'X':";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "LEADERY";
                                                      label = "'Y':";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "LEADERSUFFIX";
                                                      label = "Suffix:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "LEADERCOLOR";
                                                      label = "Color:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : spacer {
                                                    width = 40;
                                                  }
                                       }
                             : boxed_row {
                                         label = "Text";
                                         : edit_box {
                                                      key = "TEXTX";
                                                      label = "'X':";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "TEXTH";
                                                      label = "Height:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "TEXTSUFFIX";
                                                      label = "Suffix:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "TEXTCOLOR";
                                                      label = "Color:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "TEXTANG";
                                                      label = "Ang:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : toggle {
                                                    key = "TEXTALIGN";
                                                    label = "Aligned";
                                                    width = 18;
                                                    value = "1";
                                                  }
                                       }
                             : boxed_row {
                                         label = "Label";
                                         : edit_box {
                                                      key = "LBLBLK";
                                                      label = "Block:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "LBLSUFFIX";
                                                      label = "Suffix:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "LBLCOLOR";
                                                      label = "Color:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "LBLSCALE";
                                                      label = "Scale:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "LBLINC";
                                                      label = "Inc:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "LBLOS";
                                                      label = "Offset:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                       }
                             : boxed_row {
                                         label = "Tick";
                                         : edit_box {
                                                      key = "TICKBLK";
                                                      label = "Block:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "TICKSUFFIX";
                                                      label = "Suffix:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "TICKCOLOR";
                                                      label = "Color:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "TICKSCALE";
                                                      label = "Scale:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : edit_box {
                                                      key = "TICKINC";
                                                      label = "Inc:";
                                                      width = 18;
                                                      edit_width = 8;
                                                    }
                                         : spacer {
                                                    width = 18;
                                                  }
                                       }
                             : row {
                                   : cancel_button {
                                                     key = "CANCEL";
                                                     label = "Cancel";
                                                   }
                                   : text {
                                            label = "Note:  Error checking not done for character values or colors";
                                          }
                                   : ok_button {
                                                 key = "ACCEPT";
                                                 label = "OK";
                                               }
                                 }
                           }
                    }
