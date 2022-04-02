import { getArtifactInfo } from '@/api/bim/model'
import { ref } from 'vue'
import { modelMenuBarEnum } from '@enum-ms/bim'

export default function useArtifactInfo({ menuBar, bimModel, viewerPanel, modelStatus, viewer }) {
  const currentInfo = ref({})

  function createArtifactInfoPanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-artifact-info'
    _panelConfig.title = '构件详情'
    _panelConfig.css.width = '265px'
    _panelConfig.css.height = '305px'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false

    // 创建零件列表弹窗
    createMachinePartListPanel()
    // 创建图纸弹窗
    createDrawingPanel()

    viewerPanel.artifactInfo = {
      config: _panelConfig,
      panel: _panel
    }
  }

  function createMachinePartListPanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-machine-part-list'
    _panelConfig.title = '零件列表'
    _panelConfig.css.width = '400px'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false

    viewerPanel.machinePartList = {
      config: _panelConfig,
      panel: _panel
    }
  }

  function createDrawingPanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-drawing'
    _panelConfig.title = '图纸'
    _panelConfig.css.width = '400px'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false

    viewerPanel.drawing = {
      config: _panelConfig,
      panel: _panel
    }
  }

  async function fetchArtifactInfo(elementId) {
    let _elementId = elementId

    if (!elementId) {
      const selectedIds = viewer.value.getSelectedComponents()
      _elementId = selectedIds[0]
    }

    if (!_elementId) return

    // 打开弹窗
    const _panel = viewerPanel.artifactInfo.panel
    const _panel2 = viewerPanel.statusInfo.panel
    let extraTop = 0
    if (_panel2.isShow) {
      const { css: _fCss, className: _fClassName } = _panel2._opt
      extraTop = viewerPanel.panelPositions[_fClassName].top + Number(_fCss.height.slice(0, -2)) + 10
    }
    viewerPanel.panelPositions[_panel._opt.className] = {
      left: 10,
      top: extraTop || 90
    }
    _panel.initPosition()
    _panel && _panel.show()

    await refreshArtifactInfoPanel(_elementId)
  }

  function fetchMachinePart() {
    // 打开弹窗
    const _panel = viewerPanel.machinePartList.panel
    const _fPanel = viewerPanel.artifactInfo.panel
    const { css: _fCss, className: _fClassName } = _fPanel._opt
    viewerPanel.panelPositions[_panel._opt.className] = {
      left: viewerPanel.panelPositions[_fClassName].left + Number(_fCss.width.slice(0, -2)) + 10,
      top: viewerPanel.panelPositions[_fClassName].top
    }
    _panel.initPosition()
    _panel && _panel.show()

    refreshMachinePartHtml()
  }

  function refreshMachinePartHtml() {
    const _el = document.getElementsByClassName('bf-panel-machine-part-list')[0].getElementsByClassName('bf-panel-container')[0]
    _el.innerHTML = '' // 清空旧数据
    const html = `
        <div class="bf-machine-part-list-container">
          <div>
            <span>零件编号</span>
            <span>规格</span>
            <span>材质</span>
            <span>数量</span>
            <span>单重</span>
          </div>
          ${machinePartListHtml(currentInfo.value.techMachinePartDTOS)}
        </div>
      `
    _el.innerHTML = html
  }

  function machinePartListHtml(list) {
    let str = ''
    for (let i = 0; i < list.length; i++) {
      const item = list[i]
      str += `
        <div>
          <span>${item.serialNumber}</span>
          <span>${item.specification}</span>
          <span>${item.material}</span>
          <span>${item.quantity}</span>
          <span>${item.grossWeight}</span>
        </div>
      `
    }
    return str
  }

  async function refreshArtifactInfoPanel(elementId) {
    const _panel = viewerPanel.artifactInfo.panel
    if (!_panel.isShow) return
    if (!elementId) return
    try {
      const info = await getArtifactInfo({ fileId: modelStatus.value.fileId, elementId, menuBar: menuBar.value })
      currentInfo.value = info

      const _el = document.getElementsByClassName('bf-panel-artifact-info')[0].getElementsByClassName('bf-panel-container')[0]
      _el.innerHTML = '' // 清空旧数据
      const html = `
        <div class="bf-artifact-info-container">
          <div>
            <span>名称</span>
            <span>${info.name}</span>
          </div>
          <div>
            <span>编号</span>
            <span>${info.serialNumber}</span>
          </div>
          <div>
            <span>规格</span>
            <span>${info.specification}</span>
          </div>
          <div>
            <span>材质</span>
            <span>${info.material}</span>
          </div>
          <div>
            <span>重量</span>
            <span>${info.grossWeight}kg</span>
          </div>
          <div>
            <span>单元</span>
            <span>${info.areaName}</span>
          </div>
          ${getMenuBarHtml(info)}
        </div>
      `
      _el.innerHTML = html

      if (viewerPanel.machinePartList.panel?.isShow) {
        refreshMachinePartHtml()
      }

      const bfMPBtnDom = document.getElementById('bfMPBtn')
      console.log(bfMPBtnDom, 'bfMPBtnDom')
      bfMPBtnDom && bfMPBtnDom.addEventListener('click', () => {
        fetchMachinePart()
      })
    } catch (error) {
      console.log('获取构件信息', error)
    }
  }

  function getMenuBarHtml(info) {
    switch (menuBar.value) {
      case modelMenuBarEnum.COMPONENT_TREE.V:
        return `
          <div>
            <span>表面积</span>
            <span>${info.surfaceArea}</span>
          </div>
          <div>
            <span>图纸</span>
            <span>查看</span>
          </div>
          <div>
            <span>构件树</span>
            <span id="bfMPBtn">查看</span>
          </div>
      `
      case modelMenuBarEnum.PRODUCTION_STATE.V:
        return `
          <div style="justify-content: center;">
            生产状态（已上报/已质检）
          </div>
          ${getProductionListHtml(info.quantity, info.processSummaryDetailsList)}
        `
      case modelMenuBarEnum.SHIPMENT_STATUS.V:
        return `
          <div>
            <span>表面积</span>
            <span>${info.surfaceArea}</span>
          </div>
        `
      case modelMenuBarEnum.INSTALL_STATE.V:
        return `
          <div>
            <span>表面积</span>
            <span>${info.surfaceArea}</span>
          </div>
        `
      default:
        return ''
    }
  }

  function getProductionListHtml(compareQuantity, list) {
    if (!list?.length) return
    let str = ''
    for (let i = 0; i < list.length; i++) {
      const item = list[i]
      const _completed = compareQuantity === item.completeQuantity && compareQuantity === item.inspectionQuantity
      const _processInfo = _completed ? `√` : `${item.completeQuantity} / ${item.inspectionQuantity}`
      str += `
        <div>
          <span>${item.name}</span>
          <span>${_processInfo}</span>
        </div>
      `
    }
    return str
  }

  function clearArtifactInfoPanel() {
    const _panel = viewerPanel.artifactInfo.panel
    if (_panel.isShow)_panel.hide()
    const _panel2 = viewerPanel.machinePartList.panel
    if (_panel2.isShow) {
      _panel2.hide()
    }
  }

  return {
    createArtifactInfoPanel,
    fetchArtifactInfo,
    clearArtifactInfoPanel,
    refreshArtifactInfoPanel
  }
}
