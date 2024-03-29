import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取物料分类列表
const useMatClsList = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.matClsTree)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('config/fetchMatClsTree')
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
    manufClsLeafList: computed(() => store.state.config.manufClsLeafList),
    rawMatClsLeafList: computed(() => store.state.config.rawMatClsLeafList),
    matClsLeafList: computed(() => store.state.config.matClsLeafList),
    rawMatClsKV: computed(() => store.state.config.rawMatClsKV),
    loaded
  }
}

export default useMatClsList
