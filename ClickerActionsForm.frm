object frmClickerActions: TfrmClickerActions
  Left = 387
  Height = 720
  Top = 43
  Width = 888
  Caption = 'UI Clicker Actions'
  ClientHeight = 720
  ClientWidth = 888
  Constraints.MinHeight = 540
  Constraints.MinWidth = 888
  Font.Height = -11
  Font.Name = 'Tahoma'
  Icon.Data = {
    7E03000000000100010010100000010018006803000016000000280000001000
    0000200000000100180000000000000300006400000064000000000000000000
    0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFCFCFAF7898989444444
    F5F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFC5C5C5F5F5F54C4C4C5A5A5AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFC9E3BCC3D7B9C7DBBDCEE3C4CEE2C4CEE3C4323232767676383838939D8E
    CEE3C4CEE3C4CEE3C4CEE3C4CEE3C4CFE8C2D5EACBEAEAEAE9E9E9F6F6F6F8F8
    F8FDFDFD5151513D3D3D4848486F6F6F5B5B5BF2F2F2FBFBFBFEFEFEFEFEFEDE
    F1D4D6EACCF2F2F2F2F2F2F2F2F2F2F2F2F2F2F22121210000000000004F4F4F
    B6B6B6EFEFEFF3F3F3FFFFFFFDFDFDD9ECD0D5EACBE6E6E6E5E5E5E5E5E5E5E5
    E5E9E9E92323230000003D3D3D8E8C8CF2F2F2EBEBEBF1F1F1FFFFFFFDFDFDD9
    ECD0D6EBCCEAEAEAE3E3E3F3F3F3F4F4F4F0E7E52323231818187B7B7BF5F4F6
    F6F6F6F1F1F1F6F6F6FFFFFFFDFDFDD9ECD0D6EBCCF0F0F0F4F4F4FFFFFFFFFF
    FFDBD9E24A4A4A7C7C7CEEEBE8F2EFECE8E8E8E0E0E0FDFDFDFFFFFFFCFCFCD8
    EBCFD6EBCCEEEEEEE2E2E2FFFFFFFFFFFFDFDDDD707070FCFBFBEEEBE8F9F7F8
    F9F9F9F7F7F7FFFFFFFFFFFFF8F8F8D2E5C8D6EBCCF1F1F1ECECECFFFFFFFFFF
    FFF2F0F0F4E9E9F9F8F8E7D0BAEFC8A1F2C496ECC196F7C99AFDCB99F4CDA5D2
    E5C8D6EBCCF2F2F2E4E4E4EFEFEFFEFEFEFCFAFAF5EBEBF7F5F5E4DDD6F8EAE0
    F1E5DAECE1D6FCF1E6FCF1E6F5ECE3D2E5C8D6EACCE3E3E3D7D7D7D8D8D8EAEA
    EAF6F6F9ECECFBF5F5F8E6E3E0EFECECEBEAEAE2E2E2F0F0F0FFFFFFF8F8F8D2
    E5C8D6EACCD1D1D1E1E1E1ECECECF2F2F2E9E9E9F1F1F1F0F0F0E5E2E0EDE6E6
    E9E9E9EBEBEBECECECFFFFFFF9F9F9D3E6C9D5E9CBE6E5E5EBEBEBFBFBFBE9E9
    E9F3F2F2F6F6F6FCFBFBEDEBE8E8E7E7EAEAEAF7F7F7F7F7F7F7F7F7F7F7F7DC
    EED2C5E5B185CB607CCA5680CE5A82D05C82D05C82D05C82D05C82D05C82D05C
    82D05C82D05C82D05C81CC5C83AB96B8D2BEFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFF3FF800003FFC000000000000000000000020000000200000002000001820
    000018300000180000000000000000200000002000000000000000000000FFFF
    0000
  }
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  LCLVersion = '7.5'
  object PageControlMain: TPageControl
    Left = 8
    Height = 712
    Top = 0
    Width = 877
    ActivePage = TabSheetSettings
    Anchors = [akTop, akLeft, akRight, akBottom]
    Images = imglstMainPage
    TabIndex = 0
    TabOrder = 0
    object TabSheetSettings: TTabSheet
      Caption = 'Settings'
      ClientHeight = 685
      ClientWidth = 869
      ImageIndex = 0
      object lbePathToTemplates: TLabeledEdit
        Left = 8
        Height = 21
        Hint = 'The $AppDir$ replacement is available.'
        Top = 24
        Width = 277
        EditLabel.Height = 13
        EditLabel.Width = 277
        EditLabel.Caption = 'Path To Templates Dir'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '$AppDir$\ActionTemplates'
      end
      object btnBrowseActionTemplatesDir: TButton
        Left = 296
        Height = 21
        Top = 24
        Width = 25
        Caption = '...'
        OnClick = btnBrowseActionTemplatesDirClick
        TabOrder = 1
      end
      object grpVariables: TGroupBox
        Left = 640
        Height = 412
        Top = 0
        Width = 226
        Anchors = [akTop, akRight, akBottom]
        Caption = 'Available Variables / Replacements'
        ClientHeight = 394
        ClientWidth = 222
        TabOrder = 2
        object memVariables: TMemo
          Left = 10
          Height = 380
          Top = 0
          Width = 203
          Anchors = [akTop, akLeft, akRight, akBottom]
          Color = clBtnFace
          Lines.Strings = (
            '$Control_Text$='
            '$Control_Class$='
            '$Control_Handle$='
            '$Control_Left$='
            '$Control_Top$='
            '$Control_Right$='
            '$Control_Bottom$='
            '$Control_Width$='
            '$Control_Height$='
            '$Half_Control_Width$='
            '$Half_Control_Height$='
            '$ExecAction_Err$='
            '$LastAction_Status$='
            '$LastAction_Skipped$='
          )
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
      object cmbExecMode: TComboBox
        Left = 8
        Height = 19
        Hint = '[in work]'#13#10#13#10'Local - both Clicker and the target applications are running on the same machine.'#13#10'Client - this instance connects to a remote Clicker instance, which is running in server mode and it interacts with target applications.'#13#10'Server - this instance directly interacts with target applications, by receiving commands from a client instance.'#13#10#13#10'When changing mode from server to local or client, and the server module is active, it is automatically deactivated.'#13#10'When selecting client mode, a new thread is created, for polling the server for missing files (templates and bitmaps).'#13#10'This monitoring thread uses a separate connection and can work in "keep-alive" mode if the server allows it.'
        Top = 87
        Width = 84
        ItemHeight = 13
        ItemIndex = 0
        Items.Strings = (
          'Local'
          'Client'
          'Server'
        )
        OnChange = cmbExecModeChange
        ParentShowHint = False
        ShowHint = True
        Style = csOwnerDrawFixed
        TabOrder = 3
        Text = 'Local'
      end
      object lblExecMode: TLabel
        Left = 8
        Height = 13
        Top = 72
        Width = 76
        Caption = 'Execution mode'
      end
      object PageControlExecMode: TPageControl
        Left = 8
        Height = 144
        Hint = 'Settings are reloaded when changing execution modes.'
        Top = 120
        Width = 277
        ActivePage = TabSheetLocalMode
        ParentShowHint = False
        ShowHint = True
        TabIndex = 0
        TabOrder = 4
        object TabSheetLocalMode: TTabSheet
          Caption = 'Local mode'
          ClientHeight = 118
          ClientWidth = 269
          object lblLocalModeInfo: TLabel
            Left = 8
            Height = 26
            Top = 8
            Width = 227
            Caption = 'In local mode, the server module is deactivated'#13#10'and the client thread is stopped.'
          end
        end
        object TabSheetClientMode: TTabSheet
          Caption = 'Client mode'
          ClientHeight = 118
          ClientWidth = 269
          object lbeClientModeServerAddress: TLabeledEdit
            Left = 8
            Height = 21
            Hint = 'Machine address, where UIClicker runs in server mode.'#13#10'The expected format is "http://<address:port>/" (without quotes).'
            Top = 16
            Width = 184
            EditLabel.Height = 13
            EditLabel.Width = 184
            EditLabel.Caption = 'Server address'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            Text = 'http://127.0.0.1:5444/'
          end
          object btnTestConnection: TButton
            Left = 200
            Height = 21
            Hint = 'Test connection'
            Top = 16
            Width = 64
            Caption = 'Test'
            OnClick = btnTestConnectionClick
            TabOrder = 1
          end
          object chkSetExperimentsToClientMode: TCheckBox
            Left = 8
            Height = 19
            Hint = 'When checked, the template experiments are also set to connect to the same remote server.'
            Top = 40
            Width = 168
            Caption = 'Set experiments to client mode'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
          object lblFileMonitoringThreadInfo: TLabel
            Left = 8
            Height = 13
            Hint = 'This is used when the server is missing files (e.g. clktmpl or bmp files), so it can send them.'#13#10'The polling interval is min 200ms. If the server allows keeping connections alive, then this thread uses only socket per session.'
            Top = 104
            Width = 246
            Caption = 'The file monitoring thread is enabled in client mode.'
            Font.Color = clGreen
            Font.Height = -11
            Font.Name = 'Tahoma'
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
          end
          object lbeConnectTimeout: TLabeledEdit
            Left = 8
            Height = 21
            Top = 78
            Width = 117
            EditLabel.Height = 13
            EditLabel.Width = 117
            EditLabel.Caption = 'Connect timeout [ms]'
            TabOrder = 3
            Text = '1000'
          end
          object lblClientMode: TLabel
            Left = 179
            Height = 13
            Top = 78
            Width = 85
            Caption = 'Client mode off'
            Font.Color = clOlive
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object TabSheetServerMode: TTabSheet
          Caption = 'Server mode'
          ClientHeight = 118
          ClientWidth = 269
          object lbeServerModePort: TLabeledEdit
            Left = 8
            Height = 21
            Hint = 'Listening port'
            Top = 34
            Width = 56
            EditLabel.Height = 13
            EditLabel.Width = 56
            EditLabel.Caption = 'Port'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            Text = '5444'
          end
          object chkServerActive: TCheckBox
            Left = 206
            Height = 19
            Top = 34
            Width = 50
            Caption = 'Active'
            OnChange = chkServerActiveChange
            TabOrder = 1
          end
          object lblServerInfo: TLabel
            Left = 113
            Height = 13
            Top = 64
            Width = 143
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            Caption = 'Server module is inactive'
            Font.CharSet = ANSI_CHARSET
            Font.Color = clGray
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Pitch = fpVariable
            Font.Quality = fqDraft
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
          end
          object cmbFilesExistence: TComboBox
            Left = 8
            Height = 19
            Top = 95
            Width = 248
            ItemHeight = 13
            ItemIndex = 1
            Items.Strings = (
              'Expect files to exist on disk'
              'Expect files to be received from client'
            )
            Style = csOwnerDrawFixed
            TabOrder = 2
            Text = 'Expect files to be received from client'
          end
          object lblFilesExistence: TLabel
            Left = 8
            Height = 13
            Top = 80
            Width = 70
            Caption = 'Files existence'
          end
          object chkKeepAlive: TCheckBox
            Left = 81
            Height = 19
            Hint = 'The file monitoring thread, from client side, can benefit from keeping connection alive.'#13#10'It is also useful to prevent opening a new socket for every request (every 200ms).'#13#10'For most cases, leave this checked.'
            Top = 34
            Width = 69
            Caption = 'Keep alive'
            Checked = True
            State = cbChecked
            TabOrder = 3
          end
          object lblServerMode: TLabel
            Left = 165
            Height = 13
            Top = 78
            Width = 91
            Caption = 'Server mode off'
            Font.Color = clOlive
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
      end
      object lblAdminStatus: TLabel
        Left = 8
        Height = 13
        Top = 48
        Width = 73
        Caption = 'AdminStatus'
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Visible = False
      end
      object grpAllowedFileExtensionsForServer: TGroupBox
        Left = 8
        Height = 105
        Top = 280
        Width = 185
        Caption = 'Allowed file extensions for server'
        ClientHeight = 87
        ClientWidth = 181
        TabOrder = 5
        object memAllowedFileExtensionsForServer: TMemo
          Left = 8
          Height = 72
          Hint = 'Only files of these types are allowed to be sent to server, when the server reports them as missing files.'#13#10'One extension per line is allowed.'
          Top = 8
          Width = 160
          Lines.Strings = (
            '.clktmpl'
            '.bmp'
          )
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 0
          WordWrap = False
        end
      end
      object grpAllowedFileDirsForServer: TGroupBox
        Left = 8
        Height = 128
        Top = 400
        Width = 272
        Caption = 'Allowed file directories for server'
        ClientHeight = 110
        ClientWidth = 268
        TabOrder = 6
        object memAllowedFileDirsForServer: TMemo
          Left = 8
          Height = 96
          Hint = 'Only files from the following directories and their subdirectories, are allowed to be sent to server, when the server reports them as missing files.'#13#10'One directory path per line is allowed.'
          Top = 8
          Width = 248
          ParentShowHint = False
          ScrollBars = ssBoth
          ShowHint = True
          TabOrder = 0
          WordWrap = False
        end
      end
      object grpMissingFilesMonitoring: TGroupBox
        Left = 104
        Height = 58
        Top = 48
        Width = 181
        Caption = 'Missing files monitoring'
        ClientHeight = 40
        ClientWidth = 177
        TabOrder = 7
        object pnlMissingFilesRequest: TPanel
          Left = 8
          Height = 26
          Top = 8
          Width = 66
          Caption = 'Request'
          Color = clGreen
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          ParentColor = False
          ParentFont = False
          TabOrder = 0
        end
        object chkDisplayActivity: TCheckBox
          Left = 83
          Height = 19
          Top = 8
          Width = 92
          Caption = 'Display activity'
          Checked = True
          OnChange = chkDisplayActivityChange
          State = cbChecked
          TabOrder = 1
        end
      end
      object chkStayOnTop: TCheckBox
        Left = 552
        Height = 19
        Top = 0
        Width = 76
        Anchors = [akTop, akRight]
        Caption = 'Stay on top'
        OnClick = chkStayOnTopClick
        TabOrder = 8
      end
      object chkAutoSwitchToExecutingTab: TCheckBox
        Left = 640
        Height = 19
        Hint = 'When checked, the current execution tab is focused (current subtemplate).'#13#10'Not showing the current execution tab, may be a performance improvement when there is no GPU acceleration available (e.g. on VMs).'#13#10'There is also a performance gain when executing looped CallTemplate actions.'#13#10'Automatically switching tabs, is needed for debbugging, to display the current action.'
        Top = 416
        Width = 155
        Anchors = [akRight, akBottom]
        AutoSize = False
        Caption = 'Autoswitch to executing tab'
        Checked = True
        OnChange = chkAutoSwitchToExecutingTabChange
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 9
      end
      object chkAutoEnableSwitchingTabsOnDebugging: TCheckBox
        Left = 640
        Height = 19
        Hint = 'When enabled, the execution tabs are switched, even if autoswitching is off.'
        Top = 440
        Width = 214
        Anchors = [akRight, akBottom]
        AutoSize = False
        Caption = 'Autoenable switching tabs on debugging'
        Checked = True
        OnChange = chkAutoEnableSwitchingTabsOnDebuggingChange
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 10
      end
      object cmbImgPreviewGridType: TComboBox
        Left = 640
        Height = 19
        Top = 495
        Width = 115
        Anchors = [akRight, akBottom]
        ItemHeight = 13
        OnChange = cmbImgPreviewGridTypeChange
        Style = csOwnerDrawFixed
        TabOrder = 11
      end
      object lblGridType: TLabel
        Left = 640
        Height = 13
        Top = 478
        Width = 92
        Anchors = [akRight, akBottom]
        Caption = 'Displayed grid type'
      end
      object grpSelectionColors: TGroupBox
        Left = 336
        Height = 184
        Top = 24
        Width = 240
        Caption = 'Selection colors'
        ClientHeight = 166
        ClientWidth = 236
        TabOrder = 12
        object colcmbTopLeftValid: TColorBox
          Left = 96
          Height = 22
          Top = 13
          Width = 120
          Selected = clGreen
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
          ItemHeight = 16
          OnChange = colcmbTopLeftValidChange
          TabOrder = 0
        end
        object colcmbBotRightValid: TColorBox
          Left = 96
          Height = 22
          Top = 56
          Width = 120
          Selected = clGreen
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
          ItemHeight = 16
          OnChange = colcmbBotRightValidChange
          TabOrder = 1
        end
        object colcmbTopLeftInvalid: TColorBox
          Left = 96
          Height = 22
          Top = 96
          Width = 120
          Selected = clGreen
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
          ItemHeight = 16
          OnChange = colcmbTopLeftInvalidChange
          TabOrder = 2
        end
        object colcmbBotRightInvalid: TColorBox
          Left = 96
          Height = 22
          Top = 136
          Width = 120
          Selected = clGreen
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
          ItemHeight = 16
          OnChange = colcmbBotRightInvalidChange
          TabOrder = 3
        end
        object lblTopLeftValidColor: TLabel
          Left = 8
          Height = 13
          Top = 19
          Width = 74
          Caption = 'Top/Left (Valid)'
        end
        object lblBotRightValidColor: TLabel
          Left = 8
          Height = 13
          Top = 61
          Width = 78
          Caption = 'Bot/Right (Valid)'
        end
        object lblTopLeftInvalidColor: TLabel
          Left = 8
          Height = 13
          Top = 101
          Width = 74
          Caption = 'Top/Left (Valid)'
        end
        object lblBotRightInvalidColor: TLabel
          Left = 8
          Height = 13
          Top = 141
          Width = 78
          Caption = 'Bot/Right (Valid)'
        end
      end
    end
    object TabSheetTemplateExec: TTabSheet
      Caption = 'Template Execution'
      ClientHeight = 685
      ClientWidth = 869
      ImageIndex = 3
      object PageControlPlayer: TPageControl
        Left = 3
        Height = 681
        Top = 1
        Width = 847
        ActivePage = TabSheetExecMainPlayer
        Anchors = [akTop, akLeft, akRight, akBottom]
        Images = imglstCalledTemplates
        TabIndex = 0
        TabOrder = 0
        object TabSheetExecMainPlayer: TTabSheet
          Caption = 'Main Player'
          ClientHeight = 654
          ClientWidth = 839
          ImageIndex = 0
          object scrboxMain: TScrollBox
            Left = 3
            Height = 647
            Top = 3
            Width = 833
            HorzScrollBar.Smooth = True
            HorzScrollBar.Tracking = True
            VertScrollBar.Smooth = True
            VertScrollBar.Tracking = True
            Anchors = [akTop, akLeft, akRight, akBottom]
            AutoScroll = False
            TabOrder = 0
          end
        end
      end
    end
    object TabSheetExperiments1: TTabSheet
      Caption = 'Experiments 1'
      ClientHeight = 685
      ClientWidth = 869
      ImageIndex = 1
      object lblExp1: TLabel
        Left = 0
        Height = 688
        Top = 0
        Width = 872
        Anchors = [akTop, akLeft, akRight, akBottom]
        AutoSize = False
      end
    end
    object TabSheetExperiments2: TTabSheet
      Caption = 'Experiments 2'
      ClientHeight = 685
      ClientWidth = 869
      ImageIndex = 2
      object lblExp2: TLabel
        Left = 0
        Height = 764
        Top = 0
        Width = 872
        Anchors = [akTop, akLeft, akRight, akBottom]
        AutoSize = False
      end
    end
  end
  object imglstMainPage: TImageList
    Left = 596
    Top = 88
    Bitmap = {
      4C7A070000001000000010000000550300000000000078DAED964B6B135114C7
      F3310AE2A2F61B74D585D975514AA074E322EDC2BD42BBEAA659F8C045A01444
      082874E1C2952074615151416DB1B1B52F6D92B656934C26CF69DE9347F3F83B
      E7CA94183B99579229D80B8779DCFB3BE7CCFF9CCB1D003698B0357E190F366E
      C2F5E506EE6C39716F675AD5EE6E4EE3FE730F5EEDBDC77E6203519E472A9542
      AD5643229140219F47A954425D7A3E49C5118FC5904C26512E97FFB21FE93DF8
      929BD8DDDD452814822008C84B2CF9C8E572C866B3E0C3419C44A3EC1DC5A035
      9477B55AC5B7A897F11CC7C1EFF7231008B038A552555A5783285610E582C848
      4C4A7A1F0E87994FE22B950AF6F875F8529B2C66241241269341B3D944FDF414
      A214AB2C8A881CFB50490691E30F21847CA8E7A2CC44813BE32957392FB2BC14
      A328E55B9562C476DE20B3FE14F9550F840F1E94BD4FD0D8788CE2D767677C3A
      9D66B9C9FCAFE363CCCECE32FDD2927ED57205F4BDA499BCA6357FFAA662B178
      36777410C6F8F8385C2E17E3F31901F1781CC160108D4683ADABD7EB08A4B619
      4F9A9186854281F9C9C6780C0D0D61606000F3F3F3108B25564FFA4EBA124F7E
      0E841DEC27A5FA4BF5E1A51E908D6A45BCCD6683DD6EC7E2C202D398EA473E5A
      79AA1FCDB5DBE0E020E3C9262626B0B2B2C2EA4339B6C7A7FC2926E546BAD0DC
      E4E4248B3D3636869999194C4D4D31BFD4A35463B29FB9EFF0726FF1C9FF1A9F
      8FDE618B5BC55678955D878787313737879191113C5A5AFC33D766DBF18F50DC
      576B6B52FF8970381C703A9DD0BB2F9B8DBAD48765B8DD6E8C8E8E325FE7ADF3
      BE9024F22CC1A875836789F580D732D4F84EB17BC12BDDEBE1E5673DBCD2D0C2
      9FA7B99EF84A9A19E18DE8DF695E4BFF58BD7FCCF2D7DD5760D4BAC1938EBDE0
      F5EC7F25BE53EC5EF04AF77A78F9590FAF34B4F0E769AE27BE9266467823FA77
      9AD7D23F56EF1F33BCAD4BC3B17C0DB299618DF851E2B5FA51E3D5FC68E595FC
      E8E5DB7D18E15B7D58C15BA15FBFFBA75FFBE7228C87DC6D5C8DBE045DF5B2B7
      0EED68E5E9598F0E4ABC563DD578353F5A79253F7AF9761F46F8561F56F056E8
      D7EFFEE9D7FEB908A3DBE73F696896973535CBCB3535CBEB39BFF4F4A4115EED
      FCEE356F857EFDEE9FFFE9FCBFFCFF5737B9A7CCF2ED3D69846FCDC30ADE0AFD
      2EFFFFFF1DBF0100E7E31E
    }
  end
  object imglstCalledTemplates: TImageList
    BkColor = 15245824
    Left = 480
    Top = 328
    Bitmap = {
      4C7A020000001000000010000000DE0200000000000078DAC593FB4B935118C7
      FD67FA35C89439DD726ADACD124333ACC8AC30C48A4A490CBC54868AD20D95E9
      BC34A366170DA9D9451D5E732EAD401DDEB532A5D4B4D4E5CA7D3A6ABD243AF3
      82F4C09773DEF73C9FF39CF3E53CC6221C8C6BD4A0D681D56A8647C47AF0CB89
      7FF14BD55E0FDEDE7C25FC9FEF95F0F66239FC629EAFA4BE3DCF56C3AFC6FFA5
      D697F37ED6FAFEFF272FC261317D1BFFCAC0E78FB3EB369B8D844789943797D9
      CD9F27915FFFA686B88C98D97CCBCF294CBD8D183BEA245EA4CCDB6BF2FB0453
      D629E95F93D9C4FE0BFEB47C6E27AAFA3A9E25A73857779DAE2F1FE81AECA0B0
      E13E3A53B1945FF0389FDCE22C2C560B3D637D5CAC4EC233D38B20DD011CD33D
      D990B513456938276B52296FAFA4A2D5C0F36683C4378A7A272E87125F9E4CF8
      EB78BC1EFA23BBA9C443E38BFB2D5F9CD54A366ABDD9547494929E9A053E58AC
      DFD19A741C6BBD4450FD6954EA5D04BE8820C878869DCF0F21D32AD99C2BE7E0
      D368CC237D12AFD317606A69A0FB6B1FE7DF26B3431F8C5BBA1761AD2944F4DF
      20C8741AEFC7013809D631D785C3DA10AEDE49E559AD9EB1F13132EFDD205C9C
      3BAB5E83CF7D3F64190AFC8A43393BA4E1786712BB2B8EA028DCC6E61CB9900B
      FED9FE44A74772451DCFBB815E46C646846F6A3C925438E5B8A2C8F0E6D0AB18
      42CC0904BC8C40F96007CEF98A59DE4928B9368DC91F93D8B0CDBD0D318E4F7C
      235A1F8393468E2ACB977DF567F0AB3A8E57C95EC12AA5DA8E622CED7C2AB17F
      EB616B11CA4C775479BBF07E12889BCEE737EB22C935DD8DB6F76D4C4F4F2FE0
      3B873B39AA3D862C7B0BB20277E197EB3C76CFDDBD84ABC3884A3B85B9BB6501
      3F6A1925459F8A4C2DEE9A2797CE3C23B938477CC545EE96DD2634361843C38B
      05FCCC9DFA87FB893524A02AF099633532B66AB79356778D4FE39FB0FE98A2AA
      D1405BAFD96E1FCDFA699DA07DA883DAB66A42E38249CC8E6378746879BDF7F7
      5EA2C96A9A2A894C3B89A9D9B828FF0B167CA887
    }
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 5444
    ListenQueue = 30
    OnConnect = IdHTTPServer1Connect
    OnException = IdHTTPServer1Exception
    ReuseSocket = rsFalse
    UseNagle = False
    Scheduler = IdSchedulerOfThreadPool1
    KeepAlive = True
    OnCommandOther = IdHTTPServer1CommandOther
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 480
    Top = 256
  end
  object IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool
    MaxThreads = 30
    PoolSize = 10
    Left = 480
    Top = 208
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 596
    Top = 256
  end
  object tmrDisplayMissingFilesRequests: TTimer
    Interval = 100
    OnTimer = tmrDisplayMissingFilesRequestsTimer
    Left = 304
    Top = 88
  end
  object tmrUpdateColors: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrUpdateColorsTimer
    Left = 600
    Top = 144
  end
  object tmrUpdateSelectionColorsFromColorBoxes: TTimer
    Enabled = False
    Interval = 50
    OnTimer = tmrUpdateSelectionColorsFromColorBoxesTimer
    Left = 592
    Top = 344
  end
end
