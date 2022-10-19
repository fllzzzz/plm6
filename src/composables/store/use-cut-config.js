import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取生产班组全部数据
const useCutConfig = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.cutConfigs)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchCutConfig')
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
    cutConfigs: computed(() => store.state.config.cutConfigs || []),
    cutConfigKV: computed(() => store.state.config.cutConfigKV),
    loaded
  }
}

export default useCutConfig
