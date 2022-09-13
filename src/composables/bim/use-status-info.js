import { getArtifactProduction, getIntegrateArtifactProduction } from '@/api/bim/model'
import { modelMenuBarEnum } from '@enum-ms/bim'

export default function useStatusInfo({ props, menuBar, bimModel, viewerPanel, modelStatus }) {
  function createStatusInfoPanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-status-info'
    _panelConfig.css.width = '480px'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false

    viewerPanel.statusInfo = {
      config: _panelConfig,
      panel: _panel
    }
  }

  async function fetchStatusInfo() {
    const _panel = viewerPanel.statusInfo.panel
    _panel.setTitleContent(modelMenuBarEnum.VL[menuBar.value])
    viewerPanel.panelPositions[_panel._opt.className] = {
      left: 10,
      top: 90
    }
    _panel.initPosition()
    console.log(_panel, 'fetchStatusInfo')
    _panel && _panel.show()

    try {
      let data
      if (props.showMonomerModel) {
        data = await getArtifactProduction({ fileId: modelStatus.value.fileId, menuBar: menuBar.value })
      } else {
        data = await getIntegrateArtifactProduction({ projectId: props.projectId, menuBar: menuBar.value })
      }
      const { content } = data
      const _el = document.getElementsByClassName('bf-panel-status-info')[0].getElementsByClassName('bf-panel-container')[0]
      _el.innerHTML = ''
      _el.innerHTML = `
        <table class="bf-table">
          ${infoTableHtml(content)}
        </table>
      `
    } catch (error) {
      console.log(error, '获取生产状态信息')
    }
  }

  function infoTableHtml(list) {
    let str = ''
    const tdList = modelMenuBarEnum.V[menuBar.value].COLORS
    for (let i = 0; i < list.length; i++) {
      const item = list[i]
      str += `
      <tbody class="bf-group">
        <tr class="bf-group-title">
          <td colspan="2">
            <i class="bf-icon"></i>
            ${props.showMonomerModel ? item.areaName : item.monomerName}
          </td>
          ${infoTableTdHtml(tdList, 'title')}
        </tr>
        <tr class="bf-group-content">
          <td>构件数量（件）</td>
          <td>${item.quantity}</td>
          ${infoTableTdHtml(tdList, 'qField', item)}
        </tr>
        <tr class="bf-group-content">
          <td>构件重量（kg）</td>
          <td>${item.totalGrossWeight}</td>
          ${infoTableTdHtml(tdList, 'wField', item)}
        </tr>
      </tbody>
    `
    }
    return str
  }

  function infoTableTdHtml(list, field, data) {
    let str = ''
    for (let i = 0; i < list.length; i++) {
      const item = list[i]
      str += `
          <td>${data ? data[item[field]] : item[field]}</td>
    `
    }
    return str
  }

  function clearStatusInfoPanel() {
    const _panel2 = viewerPanel.artifactInfo.panel
    if (_panel2.isShow) _panel2.hide()
    if ([modelMenuBarEnum.PRODUCTION_STATE.V, modelMenuBarEnum.INSTALL_STATE.V, modelMenuBarEnum.SHIPMENT_STATUS.V].indexOf(menuBar.value) !== -1) return
    const _panel = viewerPanel.statusInfo.panel
    if (_panel.isShow)_panel.hide()
  }

  return {
    createStatusInfoPanel,
    fetchStatusInfo,
    clearStatusInfoPanel
  }
}
