import { getArtifactStatus, getIntegrationArtifactStatus } from '@/api/bim/model.js'
import { ElLoading } from 'element-plus'
import { computed } from 'vue'
import { modelMenuBarEnum } from '@enum-ms/bim'

import { arr2obj } from '@/utils/convert/type'

export default function useArtifactColoring({ props, bimModel, modelStatus, viewer, colors, objectIdGroup }) {
  const colorsMap = computed(() => {
    return arr2obj(colors.value, 'value')
  })

  // 初始化模型 全部默认灰色
  function initModelColor() {
    const loading = ElLoading.service({
      target: '#modelView',
      lock: true,
      text: '请稍后，正在染色',
      fullscreen: false
    })
    try {
      // 预先将全部构件设置为灰色
      const color = bimModel.getColor('#999999', 1)
      viewer.value.getModel().overrideAllComponentsColor(color)
      viewer.value.render()
    } catch (error) {
      console.log('error', error)
    } finally {
      loading.close()
    }
  }

  // 初始化根据构件状态着色
  async function fetchArtifactStatus(menuBar) {
    if (menuBar === modelMenuBarEnum.PROJECT_TREE.V || menuBar === modelMenuBarEnum.INSTALL_STATE.V) {
      initModelColor()
      return
    }
    const loading = ElLoading.service({
      target: '#modelView',
      lock: true,
      text: '请稍后，正在染色',
      fullscreen: false
    })
    try {
      let objectIdMap = {}
      if (props.showMonomerModel) {
        // 模型objectId，根据生产安装状态分组
        objectIdMap = await getArtifactStatus({ fileId: modelStatus.value.fileId, menuBar })
      } else {
        objectIdMap = await getIntegrationArtifactStatus({ projectId: props.projectId, menuBar })
      }

      for (const i in objectIdMap) {
        if (objectIdMap[i] && objectIdMap[i].length > 0 && colorsMap.value[i]) {
          const color = bimModel.getColor(colorsMap.value[i].color, colorsMap.value[i].opacity)
          viewer.value.overrideComponentsColorById(objectIdMap[i], color)
        }
      }
      objectIdGroup.value = objectIdMap
      viewer.value.render()
    } catch (error) {
      console.log('error', error)
    } finally {
      loading.close()
    }
  }

  // 根据id给构件着色
  function overrideComponentsColorById(elementIds, { color, opacity = 0.8 }) {
    const _color = bimModel.getColor(color, opacity)
    viewer.value.overrideComponentsColorById(elementIds, _color)
    viewer.value.render()
  }

  // 根据id给构件着色
  function resetComponentsColorById(elementIds) {
    viewer.value.restoreComponentsColorById(elementIds)
    viewer.value.render()
  }

  // 增加强调效果
  function addBlinkByIds(elementIds) {
    viewer.value.clearAllBlinkComponents()
    viewer.value.addBlinkComponentsById(elementIds)
    viewer.value.setBlinkColor(bimModel.getColor('#ff1818', 1))
    viewer.value.enableBlinkComponents(true)
    viewer.value.setBlinkIntervalTime(500)
    viewer.value.render()
  }

  // 删除强调效果
  function removeBlink() {
    viewer.value.clearAllBlinkComponents()
    viewer.value.render()
  }

  // 隔离构件
  function isolateComponentsById(elementIds) {
    // 隔离构件时，其他构件可设置成两种状态，用于Viewer3D  HideOthers：隐藏其他的构件  MakeOthersTranslucent：半透明其他的构件
    const isolateOption = bimModel.getIsolateOption().HideOthers
    console.log(isolateOption, 'isolateOption')
    viewer.value.isolateComponentsById(elementIds, isolateOption)
    viewer.value.render()
  }

  // 取消隔离
  function clearIsolation() {
    viewer.value.clearIsolation()
    viewer.value.render()
  }

  // 隐藏构件
  function hideComponentsById(elementIds) {
    console.log(elementIds, 'hideComponentsById')
    viewer.value.hideComponentsById(elementIds)
    viewer.value.render()
  }

  // 显示构件
  function showComponentsById(elementIds) {
    viewer.value.showComponentsById(elementIds)
    viewer.value.render()
  }

  function setSelectedComponentsByObjectData(conditions) {
    viewer.value.setSelectedComponentsByObjectData(conditions)
    viewer.value.render()
  }

  function clearSelectedComponents() {
    viewer.value.clearSelectedComponents()
  }

  return {
    initModelColor,
    fetchArtifactStatus,
    overrideComponentsColorById,
    resetComponentsColorById,
    addBlinkByIds,
    removeBlink,
    isolateComponentsById,
    clearIsolation,
    hideComponentsById,
    showComponentsById,
    clearSelectedComponents,
    setSelectedComponentsByObjectData
  }
}
