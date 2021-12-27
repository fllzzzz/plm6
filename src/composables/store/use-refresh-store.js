import store from '@/store'
import { isNotBlank } from '@/utils/data-type'

/**
 * 更新缓存
 * @param {array/string} keys 需要更新的缓存
 * @param {boolean} reset 重置加载状态（默认不重置）
 * @returns
 */
export default function useRefreshStore(keys = [], reset = false) {
  if (isNotBlank(keys)) {
    if (typeof keys === 'string') keys = [keys]
    keys.forEach((key) => {
      switch (key) {
        case 'classification':
          if (reset) store.commit('SET_LOADED', { key: 'matClsTree', loaded: false })
          store.dispatch('config/fetchMatClsTree')
      }
    })
  } else {
    // 更新所有缓存？
  }
}
