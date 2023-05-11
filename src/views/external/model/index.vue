<template>
  <div id="main-container">
    <div class="app-model-container">
      <div v-if="_viewer" class="button-wrap">
        <div
          v-for="(item, index) in cameraList"
          :key="item.name"
          :class="{ 'button-active': index === activeIndex }"
          @click="toggleCamera(index)"
        >
          {{ item.name }}
        </div>
      </div>
      <div v-if="tip !== tipStatusEnum.SUCCESS.V" class="tip-box">
        <el-tag :type="tipStatusEnum.V?.[tip]?.T" size="medium">{{ tipStatusEnum.V?.[tip]?.L }}</el-tag>
      </div>
      <div v-if="modelStatus.viewToken" id="modelView" />
    </div>
  </div>
</template >

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
const _Walkthrough = ref()
const app = ref()

const modelInfo = ref([])
const modelStatus = ref({
  fileId: '', // 文件id
  status: 'query', // 转换状态
  viewToken: '' // 模型访问令牌
})
const tip = ref(tipStatusEnum.QUERY.V)

const activeIndex = ref(0)
const cameraList = ref([
  {
    name: '正面',
    value: {
      name: 'persp',
      position: {
        x: 171.99075760854674,
        y: -15.391350034282624,
        z: 152.54541292963427
      },
      target: {
        x: 929.380242073717,
        y: 2423.8089822370894,
        z: -1908.326643164817
      },
      up: {
        x: 0.18621727045506586,
        y: 0.5997147603777349,
        z: 0.7782450349146733
      },
      near: 0.21709201041909434,
      far: 1545.341286089034,
      zoom: 4.548904719074391,
      version: 1,
      fov: 45,
      aspect: 2.3673076923076923,
      coordinateSystem: 'world'
    }
  },
  {
    name: '主干道',
    value: {
      name: 'persp',
      position: {
        x: 171.154035384427,
        y: 43.727673752221065,
        z: 12.549368200061702
      },
      target: {
        x: 1123.6670051151998,
        y: 3167.5316674890983,
        z: -311.6394269956372
      },
      up: {
        x: 0.028812214303061252,
        y: 0.09448710620770526,
        z: 0.9951090608910403
      },
      near: 0.21709201041907783,
      far: 1756.3522655883291,
      zoom: 15.966612954237787,
      version: 1,
      fov: 45,
      aspect: 2.3673076923076923,
      coordinateSystem: 'world'
    }
  },
  {
    name: '十字路口',
    value: {
      name: 'persp',
      position: {
        x: 206.16760753916435,
        y: 155.35643981990083,
        z: 2.678801893325513
      },
      target: {
        x: 1131.2234090995528,
        y: 3303.969997349961,
        z: 34.85259651164435
      },
      up: {
        x: -0.00276247161759803,
        y: -0.009406301388149282,
        z: 0.9999519439676875
      },
      near: 0.21709201041905127,
      far: 1636.4999390922596,
      zoom: 150.31111184424995,
      version: 1,
      fov: 45,
      aspect: 2.3673076923076923,
      coordinateSystem: 'world'
    }
  },
  {
    name: '背面',
    value: {
      name: 'persp',
      position: {
        x: 365.01941858196045,
        y: 501.72222565591625,
        z: 249.5491420854246
      },
      target: {
        x: -264.6506030141447,
        y: -1544.3251550768643,
        z: -2237.9669892035186
      },
      up: {
        x: -0.22294211087979623,
        y: -0.7244328809367975,
        z: 0.6522988703148831
      },
      near: 0.2170920104191613,
      far: 1058.069368826025,
      zoom: 1.9132374488635353,
      version: 1,
      fov: 45,
      aspect: 2.3673076923076923,
      coordinateSystem: 'world'
    }
  },
  {
    name: '左侧',
    value: {
      name: 'persp',
      position: {
        x: -13.809337234099722,
        y: 330.5027892649982,
        z: 164.78470317352242
      },
      target: {
        x: 2497.682712879007,
        y: -440.14556224585095,
        z: -1802.2027203388704
      },
      up: {
        x: 0.5729838672818774,
        y: -0.175824007599945,
        z: 0.8004844821645192
      },
      near: 0.21709201041909074,
      far: 1624.2097080685296,
      zoom: 81.68153043540985,
      version: 1,
      fov: 45,
      aspect: 2.3673076923076923,
      coordinateSystem: 'world'
    }
  },
  {
    name: '右侧',
    value: {
      name: 'persp',
      position: {
        x: 619.9434711654932,
        y: 164.12513567223382,
        z: 148.02857030709887
      },
      target: {
        x: -1960.0548900325814,
        y: 948.72999527131,
        z: -1722.4083824480826
      },
      up: {
        x: -0.5452781780806435,
        y: 0.16582040570569334,
        z: 0.8216905144643287
      },
      near: 0.21709201041904855,
      far: 1311.886999828111,
      zoom: 1.326241710336435,
      version: 1,
      fov: 45,
      aspect: 2.3673076923076923,
      coordinateSystem: 'world'
    }
  },
  {
    name: '俯视',
    value: {
      name: 'persp',
      position: {
        x: 238.72595094508185,
        y: 250.62483776141917,
        z: 480.93584736745646
      },
      target: {
        x: 242.8634840016967,
        y: 264.3312737199932,
        z: -2800.8815970000865
      },
      up: {
        x: 0.28921838698406643,
        y: 0.9572532019036534,
        z: 0.0043625767086145064
      },
      near: 418.408770637194,
      far: 509.3180862266013,
      zoom: 0.7379570652336586,
      version: 1,
      fov: 45,
      aspect: 2.3673076923076923,
      coordinateSystem: 'world'
    }
  }
])

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

// 构造天空盒管理器
function initSkyBox() {
  const skyBoxManagerConfig = {
    viewer: _viewer.value,
    style: 'BlueSky', // 天空蓝
    customizedImage: {
      // 设置自定义天空盒的图片资源
      front: 'https://static.bimface.com/attach/0d178c31584d432f93b3df90832d6ba1_EnvMap_posz.jpg',
      back: 'https://static.bimface.com/attach/c02b7114af6d4966b3f1fd7d483fcdd9_EnvMap_negz.jpg',
      left: 'https://static.bimface.com/attach/6c2f5045467b4c51a4e506524e74a65c_EnvMap_negx.jpg',
      right: 'https://static.bimface.com/attach/ec541f626f194a979d49ec5f52ca32bb_EnvMap_posx.jpg',
      top: 'https://static.bimface.com/attach/01700a9a6f7542af8df76bc923b065b9_EnvMap_posy.jpg',
      bottom: 'https://static.bimface.com/attach/031a2a1a51374fc88fe8acf1d490b7c0_EnvMap_negy.jpg'
    }
  }
  const skyBoxManager = bimModel.getSkyBoxManager(skyBoxManagerConfig)
  skyBoxManager.enableSkyBox(true)
}

// 加载模型
async function loadModel(viewToken) {
  try {
    const metaData = await bimModel.initBimfaceApp({ viewToken })
    _3DConfig.value = bimModel.getConfig()
    _3DConfig.value.domElement = document.getElementById('modelView')
    _3DConfig.value.navigatorType = 'Web'
    _3DConfig.value.enableToggleContextMenuDisplay = false
    _3DConfig.value.enableWireframe = false
    _3DConfig.value.enableViewHouse = false
    _3DConfig.value.enableReplaceMaterial = false
    _3DConfig.value.Toolbars = []
    app.value = bimModel.getApp(_3DConfig.value)
    if (metaData.viewToken) {
      app.value.addView(metaData.viewToken)
    } else {
      app.value.addModel(metaData)
    }
    tip.value = modelStatus.value.status

    _viewer.value = app.value.getViewer()
    _viewer3DEvent.value = bimModel.getViewer3DEvent()
    _viewer.value.addEventListener(
      _viewer3DEvent.value.ViewAdded,
      async () => {
        // 调用viewer3D对象的Method，可以继续扩展功能
        // 自适应屏幕大小
        // window.onresize = function() {
        //   _viewer.value.resize(document.documentElement.clientWidth, document.documentElement.clientHeight-40)
        //   if (_drawableContainer && _drawableContainer.getAllItems()) {
        //     _drawableContainer.clear()
        //   }
        //   _viewer.value.clearSelectedComponents()
        // }
        // setInterval(() => {
        //   // 获取相机位置
        //   console.log('相机当前位置信息：', _viewer.value?.getCameraStatus())
        // }, 5000)

        // 自定义动画（关键帧）
        const _WalkthroughConfig = bimModel.getWalkthroughConfig()
        _WalkthroughConfig.time = 0.8
        _WalkthroughConfig.viewer = _viewer.value
        _Walkthrough.value = bimModel.getWalkthrough(_WalkthroughConfig)

        // 设置相机位置
        toggleCamera(0)

        //  设置HOME视角
        // _viewer.value.recordCustomHomeview(homeView)
        // 隐藏构件
        _viewer.value.hideComponentsById(['741'])
        // 设置天空盒
        initSkyBox()
        // 锁定相机绕轴旋转范围
        _viewer.value.lockAxis(bimModel.getAxisOptionZ(), [0, Math.PI / 2])
        _viewer.value.render()
        //   _viewer.value.startAutoRotate(2)
      },
      { passive: false }
    )
  } catch (error) {
    console.log('加载模型失败', error)
    ElMessage.error('加载模型失败，请刷新重试')
  }
}

// 切换相机角度
function toggleCamera(index) {
  // 暂停
  _Walkthrough.value.stop()
  // 清除动画
  _Walkthrough.value.clearKeyFrames()
  // 主干道和十字路口直接来回切换不需要自定义动画
  if ((activeIndex.value === 1 || activeIndex.value === 2) && activeIndex.value + index === 3) {
    _viewer.value.setCameraStatus(cameraList.value[index].value)
  } else {
    const list = [
      {
        name: 'persp',
        position: {
          x: 40.860117905227426,
          y: 32.36410583927067,
          z: 148.56024204530684
        },
        target: {
          x: 1935.6363336908712,
          y: 1927.1472815137968,
          z: -1746.2090138258889
        },
        up: {
          x: 0,
          y: -0.0000036732051030000714,
          z: 0.9999999999932537
        },
        near: 0.21709201041913276,
        far: 1832.5996653812253,
        zoom: 7.454464988180051,
        version: 1,
        fov: 45,
        aspect: 2.3673076923076923,
        coordinateSystem: 'world'
      }
    ]
    list.push(cameraList.value[index].value)
    _Walkthrough.value.setKeyFrames(list)
    _Walkthrough.value.play()
  }
  activeIndex.value = index
}
</script>

<style lang="scss" scoped>
#main-container {
  min-height: 100%;
}
.app-model-container {
  width: 1231px;
  height: 520px;
  overflow: hidden;
  position: relative;
  .button-wrap {
    pointer-events: auto;
    position: absolute;
    z-index: 9999999;
    top: 4px;
    right: 14px;
    > div {
      margin: 8px 0 12px;
      text-align: center;
      height: 24px;
      line-height: 24px;
      width: 70px;
      font-size: 13px;
      background-image: linear-gradient(to right bottom, #79eefb, #4459ef);
      transition: background-image 0.3s;
      border: none;
      border-radius: 25px;
      cursor: pointer;
      color: #fff;
      font-weight: 600;
      box-shadow: 0 5px 0 #243c47;
      &:hover {
        background-image: linear-gradient(to right bottom, #4459ef, #7c84eb);
      }
      &.button-active {
        box-shadow: 0 3px 0 #243c47;
        transform: translateY(1px);
        background-image: linear-gradient(to right bottom, #4459ef, #7c84eb);
      }
    }
  }
}
::v-deep(#modelView) {
  height: 100%;
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
