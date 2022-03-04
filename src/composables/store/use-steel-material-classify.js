import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取钢材材料分类配置
const useSteelClassifyConf = (loadedCallBack, reload = false) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.steelClassifyConf)
  // 未加载则拉取
  if (!loaded.value || reload) {
    store.commit('config/SET_LOADED', { key: 'steelClassifyConf', loaded: false })
    store.dispatch('config/fetchSteelClassifyConf')
  }

  // 加载成功回调
  if (loadedCallBack) {
    const monitor = watch(
      loaded,
      (flag) => {
        if (flag) {
          setTimeout(() => {
            loadedCallBack(store.state.config.steelClassifyConfICKV)
            monitor()
          }, 0)
        }
      },
      { immediate: true }
    )
  }

  return {
    loaded,
    steelClassifyConf: computed(() => store.state.config.steelClassifyConf),
    steelClassifyConfICKV: computed(() => store.state.config.steelClassifyConfICKV)
  }
}

export default useSteelClassifyConf
