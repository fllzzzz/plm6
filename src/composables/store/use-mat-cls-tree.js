import { computed, watch } from 'vue'
import { useStore } from 'vuex'

/**
 * 获取物料分类树
 * @param {function} loadedCallBack 加载成功回调
 * @param {boolean} reload 重新加载（更新）
 * @returns
 */
const useMatClsTree = (loadedCallBack, reload = false) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.matClsTree)
  // 未加载则拉取
  if (!loaded.value || reload) {
    store.commit('SET_LOADED', { key: 'matClsTree', loaded: false })
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
    manufClsTree: computed(() => store.state.config.manufClsTree),
    matClsTree: computed(() => store.state.config.matClsTree),
    loaded
  }
}

export default useMatClsTree
