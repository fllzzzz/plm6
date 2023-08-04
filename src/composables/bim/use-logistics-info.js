import { getLogistics, getBridgeLogistics, getIntegrateLogistics } from '@/api/bim/model'
import { parseTime } from '@/utils/date'
import { ref } from 'vue'

export default function useLogisticsInfo({ props, bimModel, modelStatus, viewerPanel, monomerId, addBlinkByIds, removeBlink, isBridgeProject }) {
  const logisticsList = ref([])

  function createLogisticsBtn() {
    let logisticsBtnDom = document.getElementById('bfLogisticsBtn')
    if (!logisticsBtnDom) {
      const bfContainerDom = document.getElementsByClassName('bf-container')[0]
      logisticsBtnDom = document.createElement('div')
      logisticsBtnDom.id = 'bfLogisticsBtn'
      logisticsBtnDom.innerHTML = `
        <button class="el-button el-button--info el-button--small bf-artifact-search-btn-r" type="button">
        <span>物流信息</span>
        </button>
      `
      bfContainerDom.append(logisticsBtnDom)

      createLogisticsPanel()

      logisticsBtnDom && logisticsBtnDom.addEventListener('click', () => {
        fetchLogisticsInfo()
      })
    } else {
      logisticsBtnDom.style = 'display:block;'
    }
  }

  function createLogisticsPanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-logistics-info'
    _panelConfig.title = '物流详情'
    _panelConfig.css.width = '350px'
    _panelConfig.css.height = '300px'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false

    viewerPanel.logistics = {
      config: _panelConfig,
      panel: _panel
    }

    createShipmentPanel()
  }

  function createShipmentPanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-shipment'
    _panelConfig.title = '发运清单'
    _panelConfig.css.width = '300px'
    _panelConfig.css.height = '280px'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false

    viewerPanel.shipment = {
      config: _panelConfig,
      panel: _panel
    }
  }

  async function fetchLogisticsInfo() {
    const _panel = viewerPanel.logistics.panel
    viewerPanel.panelPositions[_panel._opt.className] = {
      left: 900,
      top: 20
    }
    _panel.initPosition()
    _panel && _panel.show()

    try {
      let data
      if (props.showMonomerModel) {
        const getApi = isBridgeProject.value ? getBridgeLogistics : getLogistics
        data = await getApi({ fileId: modelStatus.value.fileId })
      } else {
        data = await getIntegrateLogistics({ projectId: props.projectId })
      }
      const { content } = data
      logisticsList.value = content
      const _el = document.querySelector('.bf-panel-logistics-info .bf-panel-container')
      _el.innerHTML = ''
      _el.innerHTML = `
        <div class="bf-panel-logistics-info-container">
          <div>
            <span>序号</span>
            <span>车牌号</span>
            <span>车次</span>
            <span>发运日期</span>
          </div>
          ${getLogisticsListHtml(content)}
        </div>
      `

      bindClick()
    } catch (error) {
      console.log(error)
    }
  }

  function getLogisticsListHtml(list) {
    let str = ''
    for (let i = 0; i < list.length; i++) {
      const item = list[i]
      str += `
        <div class="bf-panel-logistics-li" style="cursor:pointer;" data-index="${i}">
          <span>${i + 1}</span>
          <span>${item.licensePlate}</span>
          <span>${item.serialNumber}</span>
          <span>${parseTime(item.auditTime, '{y}-{m}-{d}')}</span>
        </div>
      `
    }
    return str
  }

  function bindClick() {
    const _els = document.getElementsByClassName('bf-panel-logistics-li')
    for (let i = 0; i < _els.length; i++) {
      _els[i].onclick = () => {
        const index = _els[i].dataset.index
        addBlinkByIds(logisticsList.value[index].elementIds)
        fetchShipmentInfo(logisticsList.value[index].bimLogisticsLinkVOList)
      }
    }
  }

  function fetchShipmentInfo(list) {
    // 打开弹窗
    const _panel = viewerPanel.shipment.panel
    const _fPanel = viewerPanel.logistics.panel
    const { css: _fCss, className: _fClassName } = _fPanel._opt
    viewerPanel.panelPositions[_panel._opt.className] = {
      left: viewerPanel.panelPositions[_fClassName].left + Number(_fCss.width.slice(0, -2)) + 10,
      top: viewerPanel.panelPositions[_fClassName].top
    }
    _panel.initPosition()
    _panel && _panel.show()

    const _el = document.querySelector('.bf-panel-shipment .bf-panel-container')
    _el.innerHTML = ''
    _el.innerHTML = `
      <div class="bf-panel-shipment-container">
        <div>
          <span>序号</span>
          <span>名称</span>
          <span>编号</span>
          <span>数量</span>
        </div>
        ${getShipListHtml(list)}
      </div>
    `
  }

  function getShipListHtml(list) {
    let str = ''
    for (let i = 0; i < list.length; i++) {
      const item = list[i]
      str += `
        <div>
          <span>${i + 1}</span>
          <span>${item.name}</span>
          <span>${item.serialNumber}</span>
          <span>${item.quantity}</span>
        </div>
      `
    }
    return str
  }

  function hideLogisticsBtn() {
    const logisticsBtnDom = document.getElementById('bfLogisticsBtn')
    logisticsBtnDom.style = 'display:none;'

    removeBlink()

    const _panel = viewerPanel.logistics.panel
    if (_panel.isShow)_panel.hide()

    const _panel2 = viewerPanel.shipment.panel
    if (_panel2.isShow)_panel2.hide()
  }

  return {
    createLogisticsBtn,
    hideLogisticsBtn
  }
}
