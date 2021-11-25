import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取WMS模块配置信息
const useWmsConfig = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.wms.loaded.config)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('wms/fetchWmsConfig')
  }

  // 加载成功回调
  if (loadedCallBack) {
    const monitor = watch(
      loaded,
      (flag) => {
        if (flag) {
          setTimeout(() => {
            loadedCallBack()
            monitor()
          }, 0)
        }
      },
      { immediate: true }
    )
  }

  return {
    loaded,
    baseUnit: computed(() => store.state.wms.baseUnit), // wms基础单位
    inboundSteelCfg: computed(() => store.state.wms.inboundSteelCfg), // 钢材入库配置
    inboundFillWayCfg: computed(() => store.state.wms.inboundFillWayCfg), // 入库填写方式
    outboundAuxMatWayCfg: computed(() => store.state.wms.outboundAuxMatWayCfg) // 辅材出库配置
  }
}

export default useWmsConfig
