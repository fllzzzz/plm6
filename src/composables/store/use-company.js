import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取公司信息
const useCompany = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.company)
  // 拉取未加载的公司信息
  if (!loaded.value) {
    store.dispatch('config/fetchCompany')
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
    company: computed(() => store.state.config.company),
    loaded
  }
}

export default useCompany
