<template>
  <div id="main-container">
    <div class="app-model-container">
      <div v-if="tip !== tipStatusEnum.SUCCESS.V" class="tip-box">
        <el-tag :type="tipStatusEnum.V?.[tip]?.T" size="medium">{{ tipStatusEnum.V?.[tip]?.L }}</el-tag>
      </div>
      <div v-if="modelStatus.viewToken" id="modelView" />
    </div>
  </div>
</template>

<script setup>
import { getModel } from '@/api/external/model'
import { ref, computed, watch, onBeforeUnmount } from 'vue'
import { useRoute } from 'vue-router'

import { constantize } from '@/utils/enum/base'
import { isNotBlank } from '@data-type'
import * as bimModel from '../../../../public/assets/bimface/bimfaceAPI.js'
import { ElMessage } from 'element-plus'

const route = useRoute()

// 状态提示枚举
const tipStatusEnum = {
  PROCESSING_NO: { L: '3D模型未集成', K: 'PROCESSING_NO', V: 'processing_no', T: 'warning' },
  PROCESSING: { L: '3D模型正在转换，请稍后刷新重试', K: 'PROCESSING', V: 'processing', T: 'warning' },
  SUCCESS: { L: '成功', K: 'SUCCESS', V: 'success', T: 'success' },
  FAILED: { L: '3D模型转换失败，请联系管理员或重新上传', K: 'FAILED', V: 'failed', T: 'danger' },
  QUERY: { L: '查询中...', K: 'QUERY', V: 'query', T: '' },
  MISSING: { L: '参数缺失，查询失败', K: 'MISSING', V: 'missing', T: 'warning' },
  UNEXIST: { L: '未上传3D模型', K: 'UNEXIST', V: 'unexist', T: 'info' },
  UPLOAD_PROCESSING: { L: '模型正在上传', K: 'UPLOAD_PROCESSING', V: 'uploadProcessing', T: 'warning' },
  UPLOAD_SUCCESS: { L: '模型未转换', K: 'UPLOAD_SUCCESS', V: 'uploadSuccess', T: 'info' },
  UPLOAD_FAILED: { L: '模型上传失败', K: 'UPLOAD_FAILED', V: 'uploadFailed', T: 'danger' }
}
constantize(tipStatusEnum)

const _viewer3DEvent = ref()
const _viewer = ref()
const _3DConfig = ref()
const app = ref()

const modelInfo = ref([])
const modelStatus = ref({
  fileId: '', // 文件id
  status: 'query', // 转换状态
  viewToken: '' // 模型访问令牌
})
const tip = ref(tipStatusEnum.QUERY.V)

// 文件id
const fileId = computed(() => {
  if (isNotBlank(route.query?.fileId)) {
    return Number(route.query.fileId)
  }
  return undefined
})

// 基础请求路径
const requestUrl = computed(() => {
  return window.location.href.split('/external/model')[0]
})

watch(
  () => fileId.value,
  (id) => {
    initModel()
    fetchModelInfo(id)
  },
  { immediate: true }
)

// 离开页面时，销毁模型
onBeforeUnmount(() => {
  viewerDestroy()
})

// 模型信息初始化
function initModel() {
  viewerDestroy()
  modelInfo.value = {}
  modelStatus.value = {
    fileId: '', // 文件id
    status: 'query', // 转换状态
    viewToken: '' // 模型访问令牌
  }
}

// 注销
function viewerDestroy() {
  _viewer.value?.view && _viewer.value.destroy()
}

// 获取模型信息
async function fetchModelInfo(fileId) {
  // 获取加载model所需的访问令牌
  try {
    if (fileId) {
      modelInfo.value = (await getModel(requestUrl.value, { fileId })) || {}
      modelStatus.value = {
        fileId: modelInfo.value.fileId,
        viewToken: modelInfo.value.viewToken,
        status: modelInfo.value.status
      }
      loadModel(modelStatus.value.viewToken)
    } else {
      modelInfo.value = {}
      tip.value = tipStatusEnum.MISSING.V
    }
  } catch (error) {
    modelInfo.value = {}
    tip.value = tipStatusEnum.UNEXIST.V
    console.log('获取模型viewToken', error)
  }
}

// 加载模型
async function loadModel(viewToken) {
  try {
    const metaData = await bimModel.initBimfaceApp({ viewToken })
    _3DConfig.value = bimModel.getConfig()
    _3DConfig.value.domElement = document.getElementById('modelView')
    _3DConfig.value.navigatorType = 'Web'
    _3DConfig.value.Buttons = []
    _3DConfig.value.Toolbars = []
    app.value = bimModel.getApp(_3DConfig.value)
    if (metaData.viewToken) {
      app.value.addView(metaData.viewToken)
    } else {
      app.value.addModel(metaData)
    }
    tip.value = modelStatus.value.status

    _viewer.value = app.value.getViewer()

    // setInterval(() => {
    // 获取相机位置
    //   console.log('_viewer.value.setCameraStatus: ', _viewer.value.getCameraStatus())
    // }, 1000)
    _viewer3DEvent.value = bimModel.getViewer3DEvent()
    _viewer.value.addEventListener(
      _viewer3DEvent.value.ViewAdded,
      async () => {
        // 调用viewer3D对象的Method，可以继续扩展功能
        // 自适应屏幕大小
        // window.onresize = function() {
        //   _viewer.value.resize(document.documentElement.clientWidth, document.documentElement.clientHeight - 40)
        //   if (_drawableContainer && _drawableContainer.getAllItems()) {
        //     _drawableContainer.clear()
        //   }
        //   _viewer.value.clearSelectedComponents()
        // }
        // 设置相机位置
        _viewer.value.setCameraStatus({
          'name': 'persp',
          'position': {
            'x': 54.547698323782186,
            'y': 86.83118048858243,
            'z': 77.13685483363594
          },
          'target': {
            'x': 2140.3670003318134,
            'y': 2042.015833744974,
            'z': -1534.4280786281722
          },
          'up': {
            'x': 0.358267488959695,
            'y': 0.33582501629812855,
            'z': 0.8711291320940178
          },
          'near': 0.21709201041907406,
          'far': 1862.4471785859046,
          'zoom': 12.659755707466722,
          'version': 1,
          'fov': 45,
          'aspect': 2.049092849519744,
          'coordinateSystem': 'world'
        })
        // 隐藏构件
        _viewer.value.hideComponentsById(['741'])
        // 是否开启构件线框
        _viewer.value.enableWireframe(false)
        _viewer.value.toggleContextMenuDisplay(false)
        _viewer.value.render()
        // autoRotate()
      },
      { passive: false }
    )
  } catch (error) {
    console.log('加载模型失败', error)
    ElMessage.error('加载模型失败，请刷新重试')
  }
}

// 自动旋转
// function autoRotate(speed = 2) {
//   _viewer.value.startAutoRotate(speed)
// }
</script>

<style lang="scss" scoped>
#main-container {
  min-height: 100%;
}
.app-model-container {
  width: 100%;
  height: 100vh;
  overflow: hidden;
}
::v-deep(#modelView) {
  height: 100%;
  .bf-container {
    background: transparent !important;
  }
  .bf-house {
    display: none;
  }
}
.tip-box {
  width: 100%;
  box-sizing: border-box;
  padding: 25% 0;
  display: flex;
  justify-content: center;
  align-items: center;
}
</style>
