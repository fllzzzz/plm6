import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取物料分类树
const useMatClsTree = (loadedCallBack) => {
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
    rawMatClsTree: computed(() => store.state.config.rawMatClsTree),
    matClsTree: computed(() => store.state.config.matClsTree),
    loaded
  }
}

export default useMatClsTree
