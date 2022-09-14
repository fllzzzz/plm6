import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取问题分类
const useQualityProblemType = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.qualityProblemType)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchQualityProblemType')
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
    qualityProblemType: computed(() => store.state.config.qualityProblemType),
    loaded
  }
}

export default useQualityProblemType
