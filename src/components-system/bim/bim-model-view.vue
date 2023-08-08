<template>
  <div class="model-container" :style="{ height: `${maxHeight}px` }">
    <el-tag
      v-if="tip !== commonTipStatusEnum.SUCCESS.V"
      :type="commonTipStatusEnum.V[tip]?.T"
      :style="isPreview || !showMonomerModel ? 'margin-left: 10px;margin-top: 10px;' : ''"
    >
      {{ commonTipStatusEnum.VL[tip] }} {{ modelStatus.reason }}
    </el-tag>
    <div v-if="tip === commonTipStatusEnum.SUCCESS.V" id="modelView"></div>
  </div>
</template>

<script setup>
import * as bimModel from '../../../public/assets/bimface/bimfaceAPI.js'
import { getTranslate, getIMTranslate } from '@/api/bim/model.js'
import { defineProps, watch, ref, reactive, computed, defineExpose } from 'vue'

import { mapGetters } from '@/store/lib'
import { constantize } from '@/utils/enum/base'
import { isBlank } from '@/utils/data-type'

import { projectTypeEnum } from '@/utils/enum/modules/contract'
import { modelTranslateStatusEnum, modelIntegrationStatusEnum as imTipStatusEnum, modelMenuBarEnum } from '@enum-ms/bim'

import useMyToolbar from '@compos/bim/use-my-toolbar'
import useColorCard from '@compos/bim/use-color-card'
import useProjectTreePanel from '@compos/bim/use-project-tree-panel'
import useArtifactColoring from '@compos/bim/use-artifact-coloring'
import useArtifactSearch from '@compos/bim/use-artifact-search'
import useArtifactInfo from '@compos/bim/use-artifact-info'
import useStatusInfo from '@compos/bim/use-status-info'
import useLogisticsInfo from '@compos/bim/use-logistics-info'
import useDrawing from '@compos/bim/use-drawing'
import usePreview from '@compos/bim/use-preview'
// import useRightClickEvent from '@compos/bim/use-right-click-event'

const { globalProject } = mapGetters(['globalProject'])

const props = defineProps({
  monomerId: {
    type: Number
  },
  areaId: {
    type: [Number, String]
  },
  projectId: {
    type: Number
  },
  showMonomerModel: {
    type: Boolean,
    default: true
  },
  maxHeight: {
    type: Number
  },
  isPreview: {
    type: Boolean,
    default: false
  },
  isMobile: {
    type: Boolean,
    default: false
  },
  previewShowAll: {
    type: Boolean,
    default: false
  },
  serialNumber: {
    type: String
  },
  productId: {
    type: Number,
    default: undefined
  },
  productType: {
    type: Number,
    default: undefined
  },
  monomerName: {
    type: String
  },
  projectName: {
    type: String
  }
})

const publicPath = import.meta.env.BASE_URL + 'assets'

const tipStatusEnum = {
  PROCESSING_NO: { L: '3D模型未集成', K: 'PROCESSING_NO', V: 'processing_no', T: 'warning' },
  PROCESSING: { L: '3D模型正在转换，请稍后刷新重试', K: 'PROCESSING', V: 'processing', T: 'warning' },
  SUCCESS: { L: '成功', K: 'SUCCESS', V: 'success', T: 'success' },
  FAILED: { L: '3D模型转换失败，请联系管理员或重新上传', K: 'FAILED', V: 'failed', T: 'danger' },
  QUERY: { L: '查询中...', K: 'QUERY', V: 'query', T: '' },
  UNEXIST: { L: '未上传3D模型', K: 'UNEXIST', V: 'unexist', T: 'info' },
  UPLOAD_PROCESSING: { L: '模型正在上传', K: 'UPLOAD_PROCESSING', V: 'uploadProcessing', T: 'warning' },
  UPLOAD_SUCCESS: { L: '模型未转换', K: 'UPLOAD_SUCCESS', V: 'uploadSuccess', T: 'info' },
  UPLOAD_FAILED: { L: '模型上传失败', K: 'UPLOAD_FAILED', V: 'uploadFailed', T: 'danger' }
}
constantize(tipStatusEnum)

const modelLoaded = ref(false)
const tip = ref(tipStatusEnum.QUERY.V)
const modelStatus = ref({
  reason: '', // 若转换失败，返回失败原因
  status: 'success', // 转换状态
  viewToken: '' // 模型访问令牌
})

const _viewer3DEvent = ref()
const _viewer = ref()
const _3DConfig = ref()

const commonTipStatusEnum = computed(() => {
  return modelStatus.value.modelType === 'integration' ? imTipStatusEnum : tipStatusEnum
})
const isBridgeProject = computed(() => {
  return globalProject.value?.projectType === projectTypeEnum.BRIDGE.V
})

const viewerPanel = reactive({
  panelPositions: {},
  proTree: {
    config: null,
    panel: null
  },
  artifactListByArea: {
    config: null,
    panel: null
  },
  artifactInfo: {
    config: null,
    panel: null
  },
  drawing: {
    config: null,
    panel: null
  },
  machinePartList: {
    config: null,
    panel: null
  },
  statusInfo: {
    config: null,
    panel: null
  },
  logistics: {
    config: null,
    panel: null
  },
  shipment: {
    config: null,
    panel: null
  },
  colorCard: {
    config: null,
    panel: null
  }
})
const viewProAreaTree = ref({})
const colors = ref([])
const menuBar = ref()
const objectIdGroup = ref({})

function getModelViewSize() {
  const dom = document.getElementById('modelView')
  console.log(dom.clientHeight, dom.clientWidth, 'getModelViewSize')
  return {
    width: dom.clientWidth,
    height: dom.clientHeight
  }
}

const {
  initModelColor,
  fetchArtifactStatus,
  addBlinkByIds,
  removeBlink,
  isolateComponentsById,
  clearIsolation,
  hideComponentsById,
  showComponentsById,
  overrideComponentsColorById,
  setSelectedComponentsByObjectData,
  clearSelectedComponents
} = useArtifactColoring({ props, bimModel, modelStatus, viewer: _viewer, colors, objectIdGroup, isBridgeProject })
const { createDrawing, fetchDrawing } = useDrawing()
const { createArtifactInfoPanel, fetchArtifactInfo, clearArtifactInfoPanel } = useArtifactInfo({
  props,
  menuBar,
  bimModel,
  viewer: _viewer,
  viewerPanel,
  modelStatus,
  fetchDrawing,
  addBlinkByIds,
  isBridgeProject
})
const { createStatusInfoPanel, fetchStatusInfo, clearStatusInfoPanel } = useStatusInfo({
  props,
  menuBar,
  bimModel,
  viewerPanel,
  modelStatus,
  isBridgeProject
})
const { createProTreePanel, clearProTreePanel, fetchProTree } = useProjectTreePanel({
  props,
  bimModel,
  modelStatus,
  viewerPanel,
  viewProAreaTree,
  setSelectedComponentsByObjectData,
  clearSelectedComponents,
  addBlinkByIds,
  removeBlink,
  getModelViewSize,
  isBridgeProject
})
const { createLogisticsBtn, hideLogisticsBtn } = useLogisticsInfo({
  props,
  modelStatus,
  bimModel,
  viewerPanel,
  monomerId: computed(() => props.monomerId),
  addBlinkByIds,
  removeBlink,
  isBridgeProject
})
const { createMyToolbar } = useMyToolbar({
  menuBar,
  publicPath,
  bimModel,
  viewerPanel,
  viewProAreaTree,
  colors,
  isMobile: props.isMobile,
  createLogisticsBtn,
  hideLogisticsBtn,
  clearProTreePanel,
  fetchProTree,
  clearArtifactInfoPanel,
  fetchArtifactInfo,
  clearStatusInfoPanel,
  fetchStatusInfo,
  clearSelectedComponents
})
const { createSearchHtml, searchBySN } = useArtifactSearch({ props, modelStatus, addBlinkByIds, removeBlink, isBridgeProject })
const { createColorCardHtml } = useColorCard({
  props,
  isMobile: props.isMobile,
  menuBar,
  colors,
  objectIdGroup,
  bimModel,
  viewerPanel,
  modelStatus,
  searchBySN,
  fetchArtifactStatus,
  isolateComponentsById,
  clearIsolation,
  hideComponentsById,
  showComponentsById,
  overrideComponentsColorById,
  isBridgeProject
})
// const { addRightEventListener } = useRightClickEvent({ viewerPanel, fetchArtifactInfo })
const previewSNElementIds = ref([])

watch(
  () => [props.monomerId, props.showMonomerModel, props.projectId, props.areaId],
  () => {
    fetchTranslate(props.monomerId)
  },
  { immediate: true }
)

function init() {
  modelStatus.value = {
    reason: '',
    status: 'success',
    viewToken: ''
  }
  menuBar.value = null
  colors.value = []
}

async function fetchTranslate(monomerId) {
  if (props.showMonomerModel && !monomerId) return
  if (!props.showMonomerModel && !props.projectId) return
  // 获取加载model所需的访问令牌
  modelLoaded.value = false
  tip.value = tipStatusEnum.QUERY.V
  init()
  try {
    if (props.showMonomerModel) {
      const { viewToken, reason, status, fileId, modelType, integrationStatus } = await getTranslate(monomerId, props.areaId)
      modelStatus.value = {
        fileId,
        viewToken,
        reason,
        status,
        modelType,
        integrationStatus
      }
      tip.value = modelType === 'file' ? status : integrationStatus
    } else {
      const { viewToken, reason, status, fileId } = await getIMTranslate(props.projectId)
      modelStatus.value = {
        fileId,
        viewToken,
        reason,
        status
      }
      tip.value = status
    }
    if (tip.value === modelTranslateStatusEnum.SUCCESS.V) {
      loadModel(modelStatus.value.viewToken)
    }
  } catch (error) {
    console.log('获取模型viewToken', error)
  }
}

async function loadModel(viewToken) {
  try {
    // viewToken = 'a40b9998ef634f8c9f638a19c66c5e9a'
    const metaData = await bimModel.initBimfaceApp({ viewToken })
    _3DConfig.value = bimModel.getConfig()
    _3DConfig.value.Toolbars = ['MainToolbar']
    if (props.isPreview) {
      _3DConfig.value.Buttons = ['Home', 'FullScreen']
    }
    const _el = document.getElementById('modelView')
    _el.innerHTML = '' // 清除旧数据
    _3DConfig.value.domElement = _el
    const app = bimModel.getApp(_3DConfig.value)
    if (metaData.viewToken) {
      app.addView(metaData.viewToken)
    } else {
      app.addModel(metaData)
    }
    console.log(app, 'app')

    _viewer.value = app.getViewer()
    console.log(_viewer, '_viewer')
    _viewer3DEvent.value = bimModel.getViewer3DEvent()
    _viewer.value.addEventListener(
      _viewer3DEvent.value.ViewAdded,
      async () => {
        if (!props.isPreview) {
          viewerPanel.panelPositions = bimModel.getPanelPositions()
          // 生成自定义的工具条
          createMyToolbar()
          createArtifactInfoPanel()
          createProTreePanel()
          createStatusInfoPanel()
          createSearchHtml()
          createColorCardHtml()
        } else {
          if (props.isMobile) {
            viewerPanel.panelPositions = bimModel.getPanelPositions()
            createArtifactInfoPanel()
            createMyToolbar()
            createStatusInfoPanel()
            createColorCardHtml()
          }
          const { serialNumberElementIds } = usePreview({
            props,
            modelStatus,
            initModelColor,
            overrideComponentsColorById,
            isolateComponentsById,
            isBridgeProject
          })
          previewSNElementIds.value = serialNumberElementIds
        }
        createDrawing()

        // 调用viewer3D对象的Method，可以继续扩展功能
        // 自适应屏幕大小
        window.onresize = function () {
          _viewer.value.resize(document.documentElement.clientWidth, document.documentElement.clientHeight - 40)
          _viewer.value.clearSelectedComponents()
        }

        initModelColor()
        modelLoaded.value = true
      },
      { passive: false }
    )
    // 添加右键
    // addRightEventListener({ viewer: _viewer, viewer3DEvent: _viewer3DEvent })
    console.log(_viewer3DEvent, '_viewer3DEvent')
    _viewer.value.addEventListener(
      _viewer3DEvent.value.ComponentsSelectionChanged,
      (component) => {
        console.log(component, 'ComponentsSelectionChanged')
        if (menuBar.value && menuBar.value !== modelMenuBarEnum.PROJECT_TREE.V) {
          const selectedIds = _viewer.value.getSelectedComponents()
          if (isBlank(selectedIds)) {
            if (isBridgeProject.value) {
              removeBlink()
            }
            clearArtifactInfoPanel()
          } else {
            fetchArtifactInfo(selectedIds[0])
          }
        }
      },
      { passive: false }
    )
  } catch (error) {
    console.log(error)
  }
}

// 移动端预览页面使用
defineExpose({
  modelLoaded,
  clearIsolation,
  isolateComponentsById,
  previewSNElementIds
})
</script>

<style lang="scss" scoped>
.model-container {
  position: relative;
}
.model-container {
  height: 100vh;
  width: 100%;
  overflow: hidden;
}

#modelView {
  height: 100%;
  background-color: #fff;
  flex: 1;
}
</style>

<style lang="scss">
.bf-container {
  #bfColorCard {
    position: absolute;
    top: 20px;
    right: 160px;
  }

  #bfColorCardMobile {
    position: absolute;
    top: 20px;
    left: 20px;
  }

  #bfLogisticsBtn {
    position: absolute;
    top: 20px;
    left: 807px;
  }

  #bfDrawingView {
    width: 100%;
    height: 100%;
  }

  .bf-artifact-search {
    position: absolute;
    left: 480px;
    top: 20px;
    display: flex;
  }

  .bf-toolbar-my {
    position: absolute;
    left: 10px;
    top: 10px;
    height: 70px;
  }

  .bf-toolbar-my-mobile {
    display: flex;
    position: absolute;
    left: 50%;
    transform: translateX(-50%);
    bottom: 10px;
    height: 70px;

    .bf-button {
      width: auto;
    }
  }

  .bf-button-active {
    opacity: 1;
  }

  .bf-button-active {
    opacity: 1;
  }

  .bf-button.bf-button-disabled {
    opacity: 0.3;
    cursor: no-drop;
  }
  .bf-button.bf-button-disabled:hover {
    opacity: 0.3;
  }

  .bf-area-artifact-container,
  .bf-artifact-info-container,
  .bf-machine-part-list-container,
  .bf-panel-color-card-container,
  .bf-panel-logistics-info-container,
  .bf-panel-shipment-container {
    font-size: 12px;
    line-height: 25px;

    div {
      display: flex;
      border-bottom: 1px solid #666;

      span {
        flex: 1;
        text-align: center;
        &:not(:last-child) {
          border-right: 1px solid #666;
        }

        &[id] {
          cursor: pointer;
        }
      }
    }
  }

  .bf-panel-artifact-list-by-area {
    opacity: 0.8;
  }

  .bf-artifact-info-container,
  .bf-panel-logistics-info-container,
  .bf-panel-shipment-container {
    div > span:first-child {
      flex: 0.5;
    }
  }
}
</style>
