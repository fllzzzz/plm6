import { computed, ref, onUnmounted, onMounted, watch } from 'vue'
import { mapGetters } from '@/store/lib'
import RAF from '@/utils/raf'

export default function useDashboardIndex({ headRef, scrollBoxRef, crud, CRUD, pageSize = 100, intervalTime = 250, beforeRefreshHook }) {
  const { globalProjectId } = mapGetters(['globalProjectId'])
  const boardList = ref([])
  watch(
    globalProjectId,
    (val) => {
      boardList.value = []
      crud.query.monomerId = undefined
      crud.query.areaId = undefined
    },
    { immediate: true }
  )

  const boxScale = computed(() => {
    if (headRef.value) {
      checkHasScrollBar()
      return headRef.value.boxScale
    }
    return 1
  })

  const boxStyle = computed(() => {
    return {
      'font-size': `${(16 * boxScale.value).toFixed(0)}px`,
      width: `${(120 * boxScale.value).toFixed(0)}px`,
      height: `${(120 * boxScale.value).toFixed(0)}px`
    }
  })

  onMounted(() => {
    // 处理容器一开始撑满，size改变之后，未撑满的情况
    window.addEventListener('resize', checkHasScrollBar, { passive: false })
  })

  onUnmounted(() => {
    window.removeEventListener('resize', checkHasScrollBar)
  })

  function checkHasScrollBar() {
    RAF.clearInterval()
    const distance = 200
    const boxEl = scrollBoxRef.value
    const flag = !boxEl || !crud.page.hasNextPage || boxEl.scrollHeight > boxEl.clientHeight + distance
    if (flag) return
    let pollingTimes = 0 // 避免异常无限轮询
    console.log(intervalTime)
    RAF.setInterval(() => {
      const _flag = boxEl && crud.page.hasNextPage && boxEl.scrollHeight < boxEl.clientHeight + distance
      if (_flag && ++pollingTimes <= 10) {
        load()
      } else {
        RAF.clearInterval()
      }
    }, intervalTime)
  }

  async function load() {
    if (crud.firstLoaded && crud.page.hasNextPage) {
      await crud.pageChangeHandler(++crud.page.page)
    }
  }

  CRUD.HOOK.beforeRefresh = () => {
    if (typeof beforeRefreshHook === 'function') beforeRefreshHook()
    crud.page.size = pageSize
    if (crud.page.page === 1) {
      boardList.value = []
    }
  }

  CRUD.HOOK.afterRefresh = () => {
    crud.data.forEach((component) => {
      boardList.value.push(component)
    })
  }

  return {
    globalProjectId,
    boxStyle,
    load,
    boardList
  }
}
