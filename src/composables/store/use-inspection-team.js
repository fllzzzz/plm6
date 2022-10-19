import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取质检班组全部数据
const useInspectionTeam = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.inspectionTeam)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchInspectionTeam')
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
    inspectionTeam: computed(() => store.state.config.inspectionTeam),
    inspectionTeamKV: computed(() => store.state.config.inspectionTeamKV),
    loaded
  }
}

export default useInspectionTeam
