import { ref } from 'vue'

/**
 *
 * @param {*} queryParams {productionLineTypeEnum,areaIdList}
 */
export default function useGetBoxTypeList({ getApi, initHook }, isRequireLineType = false) {
  const boxTypeList = ref([])

  async function fetch({ productionLineTypeEnum, areaIdList }) {
    if (isRequireLineType) {
      if (!productionLineTypeEnum || !areaIdList?.length) {
        boxTypeList.value = []
        return
      }
    } else {
      if (!areaIdList?.length) {
        boxTypeList.value = []
        return
      }
    }
    try {
      const { content } = await getApi({ productionLineTypeEnum, areaIdList })
      boxTypeList.value = content
      if (typeof initHook === 'function') initHook()
    } catch (error) {
      console.log('获取分段排产类型汇总错误', error)
    }
  }

  return {
    boxTypeList,
    refreshBoxType: fetch
  }
}
