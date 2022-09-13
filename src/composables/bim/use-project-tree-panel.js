import { getProjectTree, getIntegrateMonomer } from '@/api/bim/model'
import { ref, watch } from 'vue'

export default function useProjectTreePanel({ props, bimModel, modelStatus, viewerPanel, viewProAreaTree, setSelectedComponentsByObjectData, clearSelectedComponents, addBlinkByIds, removeBlink, getModelViewSize }) {
  const areaList = ref([])
  const monomerMap = ref({})

  watch(
    () => props.monomerId,
    () => {
      areaList.value = []
    },
    { immediate: true }
  )

  function createProTreePanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-pro-tree'
    _panelConfig.title = props.showMonomerModel ? '区域单元' : '单体单元'
    _panelConfig.css.width = '230px'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false
    console.log(_panel, '_panel')
    const closeDom = document.querySelector('.bf-panel-pro-tree>.bf-close')
    closeDom.addEventListener('click', () => {
      const _panel2 = viewerPanel.artifactListByArea.panel
      if (_panel2?.isShow) {
        _panel2.hide()
        removeBlink()
        const { tree, treeApi } = viewProAreaTree.value
        treeApi.clear(tree)
      }
    })

    // 创建构件明细弹窗
    createArtifactListByAreaPanel()

    viewerPanel.proTree = {
      config: _panelConfig,
      panel: _panel
    }
  }

  function createArtifactListByAreaPanel() {
    const _panelConfig = bimModel.getPanelConfig()
    _panelConfig.className = 'bf-panel bf-panel-artifact-list-by-area'
    _panelConfig.title = props.showMonomerModel ? '区域构件明细' : '单体构件明细'
    _panelConfig.element = document.getElementsByClassName('bf-container')[0]
    _panelConfig.css.width = '300px'
    _panelConfig.css.height = '300px'
    const _panel = bimModel.createPanel(_panelConfig)
    _panel.isShow = false

    viewerPanel.artifactListByArea = {
      config: _panelConfig,
      panel: _panel
    }
  }

  async function fetchProTree() {
    // 区域单元弹窗设置展示位置并展示
    const _panel = viewerPanel.proTree.panel
    viewerPanel.panelPositions[_panel._opt.className] = {
      left: 10,
      top: 90
    }
    _panel.initPosition()
    _panel && _panel.show()

    if (props.showMonomerModel) {
      await toArea()
    } else {
      await toMonomer()
    }
  }

  async function toMonomer() {
    const _treeConfig = bimModel.getTreeConfig()
    _treeConfig.className = 'bf-tree bf-tree-monomer'
    _treeConfig.hasCheckbox = false
    _treeConfig.selection = false
    _treeConfig.title = '单体列表'
    const _el = document.getElementsByClassName('bf-panel-pro-tree')[0].getElementsByClassName('bf-panel-container')[0]
    _treeConfig.element = _el
    _el.innerHTML = ``
    const _tree = bimModel.createTree(_treeConfig)
    _tree.setData(-1, `${props.projectName}`)

    try {
      const { content } = await getIntegrateMonomer(props.projectId)
      for (let i = 0; i < content.length; i++) {
        const _monomerItem = content[i]
        const _treeNodeConfig = bimModel.getTreeConfig()
        _treeNodeConfig.hasCheckbox = false
        // _treeNodeConfig.hasExpand = true
        _treeNodeConfig.hasExpand = false
        _treeNodeConfig.id = _monomerItem.monomerId
        console.log(_treeNodeConfig, '_treeNodeConfig')
        const _treeNode = bimModel.createTree(_treeNodeConfig)
        _treeNode.setData({ ..._monomerItem, type: 'monomer' }, `${_monomerItem.monomerName}`)
        _tree.addChildNode(_treeNode)
        monomerMap.value[_monomerItem.monomerId] = {}
        monomerMap.value[_monomerItem.monomerId].treeNode = _treeNode
        monomerMap.value[_monomerItem.monomerId].areaList = []
        // const iconDom = _treeNode.expandIcon
        // iconDom.dataset.monomerId = _monomerItem.monomerId
        // iconDom.addEventListener('click', async (e) => {
        //   const { monomerId } = e.target.dataset
        //   await toAreaByMonomer(monomerId, monomerMap.value[monomerId].treeNode)
        // })
      }

      _tree.expand()
      const _treeApi = bimModel.getTreeApi(_tree)
      console.log(_treeApi, '_treeApi')
      _treeApi.addEventListener('SelectionChanged', (node, selected) => {
        const { type } = node.id
        if (type === 'monomer') {
          if (selected) {
            fetchArtifactList(node)
            const { fileId } = node.id
            setSelectedComponentsByObjectData([{ 'fileId': String(fileId) }])
          } else {
            clearSelectedComponents()
          }
        }
        if (type === 'area') {
          clearSelectedComponents()
          if (selected) {
          // 若选中 渲染之前区域单元 并展示该区域下构件数量和重量明细
            fetchArtifactList(node)
            const { elementIds } = node.id
            addBlinkByIds(elementIds)
          } else {
          // 取消选中 则关闭构件明细
            const _panel = viewerPanel.artifactListByArea.panel
            if (_panel.isShow) _panel.hide()
            removeBlink()
          }
        }
      })
      viewProAreaTree.value = {
        config: _treeConfig,
        tree: _tree,
        treeApi: _treeApi
      }
    } catch (error) {
      console.log('集成模型获取单体信息失败', error)
    }
  }

  // async function toAreaByMonomer(monomerId, parentNode) {
  //   const { areaList = [] } = monomerMap.value[monomerId]
  //   // 如果有数据 不重新渲染 直接打开即可
  //   if (areaList && areaList.length) return
  //   // 获取区域数据
  //   try {
  //     const { content } = await getProjectTree(monomerId)
  //     // const content = [{ area: { id: 1, name: '区域1' }}, { area: { id: 2, name: '区域2' }}]
  //     monomerMap.value[monomerId].areaList = content
  //     for (let i = 0; i < content.length; i++) {
  //       const _areaItem = content[i]
  //       const _treeNodeConfig = bimModel.getTreeConfig()
  //       _treeNodeConfig.hasCheckbox = false
  //       const _treeNode = bimModel.createTree(_treeNodeConfig)
  //       _treeNode.setData({ ..._areaItem, type: 'area' }, _areaItem.area.name)
  //       parentNode.addChildNode(_treeNode)
  //     }
  //   } catch (error) {
  //     console.log(error, '获取区域')
  //   }
  // }

  async function toArea() {
    // 如果有数据 不重新渲染 直接打开即可
    if (areaList.value && areaList.value.length) return

    // 获取区域单元
    const _treeConfig = bimModel.getTreeConfig()
    _treeConfig.className = 'bf-tree bf-tree-area'
    _treeConfig.hasCheckbox = false
    _treeConfig.selection = false
    _treeConfig.title = '区域列表'
    const _el = document.getElementsByClassName('bf-panel-pro-tree')[0].getElementsByClassName('bf-panel-container')[0]
    _treeConfig.element = _el
    _el.innerHTML = ``
    const _tree = bimModel.createTree(_treeConfig)
    _tree.setData(-1, `${props.projectName}>${props.monomerName}`)
    let _treeApi
    // 获取区域数据
    try {
      const { content } = await getProjectTree(modelStatus.value.fileId)
      areaList.value = content
      for (let i = 0; i < content.length; i++) {
        const _areaItem = content[i]
        const _treeNodeConfig = bimModel.getTreeConfig()
        _treeNodeConfig.hasCheckbox = false
        const _treeNode = bimModel.createTree(_treeNodeConfig)
        _treeNode.setData(_areaItem, _areaItem.area.name)
        _tree.addChildNode(_treeNode)
      }
      _tree.expand()
      _treeApi = bimModel.getTreeApi(_tree)
      _treeApi.addEventListener('SelectionChanged', (node, selected) => {
        if (selected) {
          // 若选中 渲染之前区域单元 并展示该区域下构件数量和重量明细
          fetchArtifactList(node)
          const { elementIds, fileId } = node.id
          if (modelStatus.value.modelType === 'file') {
            addBlinkByIds(elementIds)
          } else {
            setSelectedComponentsByObjectData([{ 'fileId': String(fileId) }])
          }
        } else {
          // 取消选中 则关闭构件明细
          const _panel = viewerPanel.artifactListByArea.panel
          if (_panel.isShow) _panel.hide()
          if (modelStatus.value.modelType === 'file') {
            removeBlink()
          } else {
            clearSelectedComponents()
          }
        }
      })

      viewProAreaTree.value = {
        config: _treeConfig,
        tree: _tree,
        treeApi: _treeApi
      }
    } catch (error) {
      console.log(error, '获取区域')
    }
  }

  function fetchArtifactList(node) {
    // 打开弹窗
    const _panel = viewerPanel.artifactListByArea.panel
    const { css } = _panel._opt
    const modelViewSize = getModelViewSize()
    console.log(modelViewSize.width, modelViewSize, css, _panel, 'modelViewSize')
    viewerPanel.panelPositions[_panel._opt.className] = {
      left: modelViewSize.width - Number(css.width.slice(0, -2)),
      top: 0
    }
    _panel.initPosition()
    _panel.setHeight(modelViewSize.height)
    _panel && _panel.show()

    // 渲染构件明细弹窗内容
    const { basicsVOS: artifactList, totalGrossWeight, quantity } = node.id
    const unitName = node.name

    const _el = document.querySelector('.bf-panel-artifact-list-by-area .bf-panel-container')
    _el.innerHTML = '' // 清空旧数据
    const html = `
      <div class="bf-area-artifact-container">
        <div>
          <span>单元</span>
          <span>${unitName}</span>
        </div>
        <div>
          <span>构件数量（件）</span>
          <span>${quantity}</span>
        </div>
        <div>
          <span>构件重量（kg）</span>
          <span>${totalGrossWeight}</span>
        </div>
        <div>
          <span>编号</span>
          <span>数量</span>
          <span>重量</span>
        </div>
        ${artifactListHtml(artifactList)}
      </div>
    `
    _el.innerHTML = html
  }

  function artifactListHtml(list) {
    let str = ''
    for (let i = 0; i < list.length; i++) {
      const item = list[i]
      str += `
        <div>
          <span>${item.serialNumber}</span>
          <span>${item.quantity}</span>
          <span>${item.totalGrossWeight}</span>
        </div>
      `
    }
    return str
  }

  function clearProTreePanel() {
    removeBlink()
    clearSelectedComponents()
    const { tree, treeApi } = viewProAreaTree.value
    treeApi.clear(tree)
    const _panel = viewerPanel.proTree.panel
    if (_panel.isShow)_panel.hide()
    const _panel2 = viewerPanel.artifactListByArea.panel
    if (_panel2.isShow) {
      _panel2.hide()
    }
  }

  return {
    createProTreePanel,
    fetchProTree,
    viewProAreaTree,
    clearProTreePanel
  }
}
