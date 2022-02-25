import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取钢材材料分类配置
const useSteelMaterialClassify = (loadedCallBack, reload = false) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.steelMaterialClassify)
  // 未加载则拉取
  if (!loaded.value || reload) {
    store.commit('config/SET_LOADED', { key: 'steelMaterialClassify', loaded: false })
    store.dispatch('config/fetchSteelMaterialClassify')
  }

  // 加载成功回调
  if (loadedCallBack) {
    const monitor = watch(
      loaded,
      (flag) => {
        if (flag) {
          setTimeout(() => {
            loadedCallBack(store.state.config.steelMaterialClassify)
            monitor()
          }, 0)
        }
      },
      { immediate: true }
    )
  }

  return {
    loaded,
    steelMaterialClassify: computed(() => store.state.config.steelMaterialClassify)
  }
}

export default useSteelMaterialClassify
