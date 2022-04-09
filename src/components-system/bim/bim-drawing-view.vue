<template>
  <div class="drawing-container">
    <el-tag
      v-if="tip !== tipStatusEnum.SUCCESS.V && isPreview"
      :type="tipStatusEnum.V[tip]?.T"
      :style="isPreview ? 'margin-left: 10px;margin-top: 10px;' : ''"
    >
      {{ tipStatusEnum.VL[tip] }} {{ drawingStatus.reason }}
    </el-tag>
    <div v-else id="drawingView"></div>
  </div>
</template>

<script setup>
import * as bimModel from '../../../public/assets/bimface/bimfaceAPI.js'
import { getBimDrawing } from '@/api/bim/model.js'
import { ref, defineExpose, inject } from 'vue'
import { ElNotification } from 'element-plus'

import { modelTranslateStatusEnum } from '@enum-ms/bim'
import { constantize } from '@/utils/enum/base'

// const publicPath = import.meta.env.BASE_URL + 'assets'
const tipStatusEnum = {
  ERROR: { L: '获取图纸失败', K: 'ERROR', V: 'error', T: 'danger' },
  IS_NOT: { L: '非DWG文件', K: 'IS_NOT', V: 'isNot', T: 'warning' },
  PROCESSING: { L: '图纸正在转换，请稍后刷新重试', K: 'PROCESSING', V: 'processing', T: 'warning' },
  SUCCESS: { L: '成功', K: 'SUCCESS', V: 'success', T: 'success' },
  FAILED: { L: '图纸转换失败，请联系管理员或重新上传', K: 'FAILED', V: 'failed', T: 'danger' },
  QUERY: { L: '查询中...', K: 'QUERY', V: 'query', T: '' },
  UNEXIST: { L: '未上传图纸', K: 'UNEXIST', V: 'unexist', T: 'info' },
  UPLOAD_PROCESSING: { L: '图纸正在上传', K: 'UPLOAD_PROCESSING', V: 'uploadProcessing', T: 'warning' },
  UPLOAD_SUCCESS: { L: '图纸未转换', K: 'UPLOAD_SUCCESS', V: 'uploadSuccess', T: 'info' },
  UPLOAD_FAILED: { L: '图纸上传失败', K: 'UPLOAD_FAILED', V: 'uploadFailed', T: 'danger' }
}
constantize(tipStatusEnum)

const drawingStatus = ref({
  reason: '', // 若转换失败，返回失败原因
  status: 'success', // 转换状态
  viewToken: '' // 模型访问令牌
})

const viewer2DEvent = ref()
const viewer2D = ref()
const _2DConfig = ref()
const tip = ref(tipStatusEnum.QUERY.V)

const productId = inject('productId')
const productType = inject('productType')
const isPreview = inject('isPreview', false)
const boolBim = inject('boolBim', true)

async function fetchTranslate() {
  if (!boolBim) {
    tip.value = tipStatusEnum.IS_NOT.V
    return
  }
  // 获取加载model所需的访问令牌
  try {
    tip.value = tipStatusEnum.QUERY.V
    const { viewToken, reason, status, fileId } = await getBimDrawing({
      productId: productId.value,
      productType: productType.value
    })
    drawingStatus.value = {
      fileId,
      viewToken,
      reason,
      status
    }
    tip.value = status
    if (status === modelTranslateStatusEnum.SUCCESS.V) {
      loadDrawing(viewToken)
    } else {
      ElNotification({ title: status ? tipStatusEnum.VL[status] : '获取图纸失败', type: 'error', duration: 2000 })
    }
  } catch (error) {
    const tip = '获取图纸失败'
    tip.value = tipStatusEnum.ERROR.V
    ElNotification({ title: tip, type: 'error', duration: 2000 })
    console.log('获取模型viewToken', error)
  }
}

async function loadDrawing(viewToken) {
  try {
    // viewToken = '80d8ebaa14b442db88c8300cfc52f95c'
    const metaData = await bimModel.initBimfaceApp({ viewToken })
    _2DConfig.value = bimModel.get2DConfig()
    const _el = document.getElementById('drawingView')
    _el.innerHTML = '' // 清除旧数据
    _2DConfig.value.domElement = _el
    if (isPreview) {
      _2DConfig.value.Toolbars = ['MainToolbar']
      _2DConfig.value.Buttons = ['Home', 'FullScreen']
    } else {
      _2DConfig.value.Buttons = []
    }
    console.log(_2DConfig, isPreview, '_2DConfig')
    const app = bimModel.get2DApp(_2DConfig.value)

    viewer2D.value = app.getViewer()

    console.log(viewer2D, app, 'viewer2D')
    if (metaData.viewToken) {
      viewer2D.value.loadDrawing({ viewToken: metaData.viewToken, modelId: metaData.modelId || '1832708127827872' })
    }
    viewer2DEvent.value = bimModel.getViewer2DEvent()
    viewer2D.value.addEventListener(
      viewer2DEvent.value.Loaded,
      async () => {
        // 设置默认背景色
        const _color = bimModel.getColor('#525659')
        viewer2D.value.setBackgroundColor(_color)

        // 调用viewer3D对象的Method，可以继续扩展功能
        // 自适应屏幕大小
        window.onresize = function () {
          viewer2D.value.resize(document.documentElement.clientWidth, document.documentElement.clientHeight - 40)
        }
      },
      { passive: false }
    )
  } catch (error) {
    console.log(error)
  }
}

function scaleZoom() {
  if (viewer2D.value) {
    viewer2D.value.zoomIn()
    viewer2D.value.render()
  }
}
function scaleZoomOut() {
  if (viewer2D.value) {
    viewer2D.value.zoomOut()
    viewer2D.value.render()
  }
}

function reset() {
  if (viewer2D.value) {
    viewer2D.value.home()
    viewer2D.value.render()
  }
}

defineExpose({
  fetchDrawing: fetchTranslate,
  reset,
  scaleZoomOut,
  scaleZoom
})
</script>

<style lang="scss" scoped>
.drawing-container {
  width: 100%;
  height: 100%;
  overflow: hidden;
}

#drawingView {
  height: 100%;
  background-color: #fff;
  flex: 1;
}
</style>
