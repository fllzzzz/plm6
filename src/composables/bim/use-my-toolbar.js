import { watch } from 'vue'
import { modelMenuBarEnum } from '@enum-ms/bim'
import { isNotBlank } from '@data-type/index'

export default function useMyToolbar({
  menuBar, bimModel, viewerPanel, colors, publicPath, isMobile,
  createLogisticsBtn, hideLogisticsBtn,
  clearProTreePanel, fetchProTree,
  clearArtifactInfoPanel, fetchArtifactInfo,
  clearStatusInfoPanel, fetchStatusInfo,
  clearSelectedComponents
}) {
  // 自定义工具条
  const MY_TOOLBAR_INFO = [{
    ...modelMenuBarEnum.PROJECT_TREE,
    hidden: isMobile,
    clickEvent: function (i) {
      menuBar.value = i
      fetchProTree()
    },
    unActiveEvent: function () {
      clearProTreePanel()
    }
  },
  {
    ...modelMenuBarEnum.COMPONENT_TREE,
    hidden: isMobile,
    clickEvent: function (i) {
      menuBar.value = i
      // fetchArtifactInfo()
    },
    unActiveEvent: function () {
      clearArtifactInfoPanel()
    }
  },
  {
    ...modelMenuBarEnum.PRODUCTION_STATE,
    hidden: false,
    clickEvent: function (i) {
      menuBar.value = i
      fetchStatusInfo()
    },
    unActiveEvent: function () {
      clearStatusInfoPanel()
    }
  },
  {
    ...modelMenuBarEnum.SHIPMENT_STATUS,
    hidden: false,
    clickEvent: function (i) {
      menuBar.value = i
      if (!isMobile) {
        createLogisticsBtn()
      }
      fetchStatusInfo()
    },
    unActiveEvent: function () {
      if (!isMobile) {
        hideLogisticsBtn()
      }
      clearStatusInfoPanel()
    }
  },
  {
    ...modelMenuBarEnum.INSTALL_STATE,
    hidden: isMobile,
    clickEvent: function (i) {
      menuBar.value = i
      fetchStatusInfo()
    },
    unActiveEvent: function () {
      clearStatusInfoPanel()
    }
  }
  ]

  watch(
    () => menuBar.value,
    (val, oldVal) => {
      if (isNotBlank(val)) {
        clearSelectedComponents()
        const btnEls = document.querySelectorAll(`${isMobile ? '.bf-toolbar-my-mobile' : '.bf-toolbar-my'}>.bf-button`)
        btnEls.forEach((el) => {
          if (el.innerText === modelMenuBarEnum.VL[val]) {
            el.className = 'bf-button bf-button-active'
          } else {
            // el.className = el.innerText === modelMenuBarEnum.INSTALL_STATE.L ? 'bf-button bf-button-disabled' : 'bf-button'
            el.className = 'bf-button'
          }
        })
        colors.value = MY_TOOLBAR_INFO[val - 1].COLORS
      }
      if (isNotBlank(oldVal) && oldVal !== val) {
        MY_TOOLBAR_INFO[oldVal - 1].unActiveEvent()
      }
    }, {
      immediate: true
    }
  )

  function createMyToolbar() {
    const _toolbarConfig = bimModel.getToolbarConfig()
    _toolbarConfig.id = 'MyToolbar'
    _toolbarConfig.className = `bf-toolbar ${isMobile ? 'bf-toolbar-my-mobile' : 'bf-toolbar-my'}`
    _toolbarConfig.element = document.getElementsByClassName('bf-container')[0]
    const _toolbar = bimModel.createToolbar(_toolbarConfig)

    for (let i = 0; i < MY_TOOLBAR_INFO.length; i++) {
      const btn = MY_TOOLBAR_INFO[i]
      if (btn.hidden) continue
      const _btnConfig = bimModel.getButtonConfig()
      _btnConfig.title = btn.L
      // if (btn.V === modelMenuBarEnum.INSTALL_STATE.V) {
      //   _btnConfig.className = 'bf-button bf-button-disabled'
      // }
      const _button = bimModel.createButton(_btnConfig)
      _button.setHtml(
        `<div style="display:flex;flex-direction: column;justify-content: center;align-items: center;width: 70px;">
        <img src="${publicPath}/bimface/${btn.ICON}" height="25" width="25"/>
        <span style="font-size:10px;">${btn.L}</span>
      </div>`
      )
      _button.addEventListener('Click', () => btn.clickEvent(btn.V))
      _toolbar.insertControl(i, _button)
    }
  }

  return {
    createMyToolbar,
    menuBar
  }
}
