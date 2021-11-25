import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取WMS基础单位配置
const useMatBaseUnit = (basicClass, loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.wms.loaded.config)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('wms/fetchWmsConfig') // 目前该信息未走接口，此处无用
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
  let baseUnit
  if (basicClass) {
    baseUnit = computed(() => store.state.wms.baseUnit[basicClass])
  } else {
    baseUnit = computed(() => store.state.wms.baseUnit)
  }

  return {
    // loaded,
    baseUnit // wms基础单位
  }
}

export default useMatBaseUnit
