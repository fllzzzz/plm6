import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取生产班组全部数据
const useProductionTeam = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.productionTeam)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchProductionTeam')
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
    productionTeam: computed(() => store.state.config.productionTeam),
    productionTeamKV: computed(() => store.state.config.productionTeamKV),
    loaded
  }
}

export default useProductionTeam
