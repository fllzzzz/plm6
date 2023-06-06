import { ref } from 'vue'
import { getSeparateOrder } from '@/api/bridge/work-order-manage/machine-part.js'

// 获取零件工单-分拣单信息
export default function useGetSeparateOrder(params) {
  const separateOrderInfo = ref([])
  const separateLoading = ref(false)

  // 获取分拣单信息
  async function fetchSeparateOrder() {
    try {
      separateLoading.value = true
      const { content } = await getSeparateOrder(params.value)
      console.log(content)
      const columnNum = 6 // 每行展示的生产线列数
      const _separateList = []
      for (let i = 0; i < content.length; i++) {
        const v = content[i]
        v.list = v.list || []
        let remainCol = columnNum
        let productionLineList = []
        for (let o = 0; o < v.list.length; o++) {
          const p = v.list[o]
          productionLineList.push({
            groupName: p.groupName,
            workShopName: p.workShopName,
            productionLineName: p.productionLineName,
            quantity: p.quantity
          })
          remainCol--
          if (remainCol === 0) {
            _separateList.push({
              productionLineList: productionLineList,
              serialNumber: null,
              picturePath: null
            })
            productionLineList = []
            remainCol = columnNum
          }
        }
        if (remainCol > 0) {
          for (let z = 0; z < remainCol; z++) {
            productionLineList.push({})
          }
          _separateList.push({
            productionLineList: productionLineList,
            serialNumber: v.serialNumber,
            picturePath: v.picturePath
          })
        }
      }
      console.log({ _separateList })
      separateOrderInfo.value = _separateList || []
    } catch (error) {
      console.log('获取零件分拣单失败', error)
    } finally {
      separateLoading.value = false
    }
  }

  return {
    separateLoading,
    separateOrderInfo,
    fetchSeparateOrder
  }
}
