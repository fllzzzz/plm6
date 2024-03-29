import { getStatusDetail, getBridgeStatusDetail, getIntegrateStatusDetail } from '@/api/bim/model'
import bfColorCard from '@/components-system/bim/bf-color-card.vue'
import { createApp, watch, ref } from 'vue'
import { modelMenuBarEnum } from '@enum-ms/bim'

export default function useColorCard({ props, isMobile, menuBar, colors, objectIdGroup, bimModel, viewerPanel, modelStatus, searchBySN, fetchArtifactStatus, isolateComponentsById, clearIsolation, hideComponentsById, showComponentsById, overrideComponentsColorById, isBridgeProject }) {
  const curElementIds = ref([])
  const ccApp = ref()

  async function statusChange(card) {
    if (!card) {
      curElementIds.value = []
      clearIsolation()
      showComponentsById(objectIdGroup.value[0])
      clearColorCardPanel()
    } else {
      const status = card.value
      let elementIds = objectIdGroup.value[status] || []
      console.log(elementIds, objectIdGroup.value, status, 'elementIds')
      if (!elementIds.length) {
        hideComponentsById(objectIdGroup.value[0])
        clearColorCardPanel()
      } else {
        showComponentsById(objectIdGroup.value[0])
        if ([2, 3, 4, 5, 6, 7, 10, 11, 12].indexOf(card.value) !== -1) {
          await fetchColorCardPanel(card)
          elementIds = curElementIds.value
        }
        isolateComponentsById(elementIds)
      }
    }
  }

  watch(
    () => menuBar.value,
    (val) => {
      if (val) {
        refresh()
      }
    }
  )

  function createColorCardHtml() {
    ccApp.value = createApp(bfColorCard, {
      statusChange: statusChange,
      colors: []
    })
    const _elementId = isMobile ? 'bfColorCardMobile' : 'bfColorCard'
    const colorCardDom = document.getElementById(_elementId)
    if (!colorCardDom) {
      const bfContainerDom = document.getElementsByClassName('bf-container')[0]
      const _dom = document.createElement('div')
      _dom.id = _elementId
      bfContainerDom.appendChild(_dom)
    }
    ccApp.value.mount(`#${_elementId}`)

    createColorCardPanel()
  }

  function refresh() {
    if (menuBar.value === modelMenuBarEnum.COMPONENT_TREE.V) {
      ccApp.value._container._vnode.component.props.selectAble = false
    } else {
      ccApp.value._container._vnode.component.props.selectAble = true
    }
    ccApp.value._container._vnode.component.props.colors = colors.value
    fetchArtifactStatus(menuBar.value)
  }

  function createColorCardPanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-color-card'
    _panelConfig.css.width = '300px'
    _panelConfig.css.height = '300px'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false

    viewerPanel.colorCard = {
      config: _panelConfig,
      panel: _panel
    }
  }

  async function fetchColorCardPanel(card) {
    const _panel = viewerPanel.colorCard.panel
    _panel.setTitleContent(card.title + '详情')
    viewerPanel.panelPositions[_panel._opt.className] = {
      left: 1200,
      top: 80
    }
    _panel.initPosition()
    _panel && _panel.show()

    try {
      let data
      if (props.showMonomerModel) {
        const getApi = isBridgeProject.value ? getBridgeStatusDetail : getStatusDetail
        data = await getApi({ fileId: modelStatus.value.fileId, status: card.value, menuBar: menuBar.value })
      } else {
        data = await getIntegrateStatusDetail({ projectId: props.projectId, status: card.value, menuBar: menuBar.value })
      }
      const { proportion, quantity, totalGrossWeight, basicsVOS, elementIds } = data
      curElementIds.value = elementIds
      const _el = document.getElementsByClassName('bf-panel-color-card')[0].getElementsByClassName('bf-panel-container')[0]
      _el.innerHTML = ''
      _el.innerHTML = `
        <div class="el-progress el-progress--line el-progress--text-inside"">
          <div class="el-progress-bar">
            <div class="el-progress-bar__outer" style="height: 26px;border-radius: 0;">
              <div class="el-progress-bar__inner" style="width: ${proportion}%; animation-duration: 3s;border-radius: 0;">
                <div class="el-progress-bar__innerText">
                <span>${card.title}率：${proportion}%</span>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="bf-panel-color-card-container">
          <div>
            <span>${card.title}总数（件）</span>
            <span>${quantity}</span>
          </div>
          <div>
            <span>${card.title}总量（kg）</span>
            <span>${totalGrossWeight}</span>
          </div>
          <div class="el-input el-input--mini el-input-group el-input-group--append">
            <input class="el-input__inner search-input" type="text" autocomplete="off" placeholder="输入构件号查询">
            <div class="el-input-group__append" style="width: 70px;">
              <button class="el-button search-btn" type="button">
                查询
              </button>
            </div>
          </div>
          <div>
            <span>名称</span>
            <span>编号</span>
            <span>数量</span>
            <span>重量(kg)</span>
          </div>
          <div class="search-content" style="flex-direction: column;">
            ${getListHtml(basicsVOS)}
          </div>
        </div>
      `

      bindClick()

      const inputDom = document.querySelector('.bf-panel-color-card-container .search-input')
      const btnDom = document.querySelector('.bf-panel-color-card-container .search-btn')
      const contentDom = document.querySelector('.bf-panel-color-card-container .search-content')
      btnDom.onclick = () => {
        contentDom.innerHTML = ''
        contentDom.innerHTML = getListHtml(basicsVOS, inputDom.value)
        bindClick()
      }
      console.log(inputDom, btnDom)
    } catch (error) {
      console.log(error, '获取状态下的信息')
    }
  }

  function bindClick() {
    const _els = document.getElementsByClassName('bf-panel-color-li')
    for (let i = 0; i < _els.length; i++) {
      _els[i].onclick = () => {
        const sn = _els[i].dataset.serialNumber
        searchBySN(sn)
      }
    }
  }

  function getListHtml(list, serialNumber) {
    if (list && list.length) {
      let str = ''
      for (let i = 0; i < list.length; i++) {
        const item = list[i]
        if (serialNumber && item.serialNumber.indexOf(serialNumber) === -1) {
          break
        }
        str += `
          <div class="bf-panel-color-li" style="cursor:pointer;" data-serial-number="${item.serialNumber}">
            <span>${item.name}</span>
            <span>${item.serialNumber}</span>
            <span>${item.quantity}</span>
            <span>${item.totalGrossWeight}</span>
          </div>
        `
      }
      return str
    } else {
      return ''
    }
  }

  function clearColorCardPanel() {
    const _panel = viewerPanel.colorCard.panel
    if (_panel.isShow)_panel.hide()
  }

  return {
    createColorCardHtml
  }
}
