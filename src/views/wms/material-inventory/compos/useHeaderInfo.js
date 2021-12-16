import { inject, watch, ref } from 'vue'
import { useRouter } from 'vue-router'
import { mapGetters } from '@/store/lib'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'

export default function useMainInfo({ defaultBasicClass }) {
  const router = useRouter()
  const permission = inject('permission')

  // 查询参数
  const defaultQuery = {
    projectId: { value: undefined, resetAble: false }, // 项目id
    projectWarehouseType: { value: projectWarehouseTypeEnum.PUBLIC.V, resetAble: false },
    basicClass: { value: defaultBasicClass, resetAble: false }
  }

  const { crud, query } = regHeader(defaultQuery)

  // 出库清单组件
  const currentUserOutboundListRef = ref()
  // 显示批量出库
  const batchOutboundHandlingVisible = ref(false)
  // 显示批量调拨
  const batchTransferHandlingVisible = ref(false)

  // 全局项目id
  const { globalProjectId } = mapGetters('globalProjectId')
  // 选中项目库时， 根据项目id的变化刷新列表
  watch(
    globalProjectId,
    (val) => {
      if (query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V) {
        crud.query.projectId = val
        crud.toQuery()
      }
    },
    { immediate: true }
  )

  // 去出库记录
  function toOutboundRecord() {
    router.push({ name: 'WMSOutboundRecord' })
  }

  // 批量出库
  function toBatchOutbound() {
    batchOutboundHandlingVisible.value = true
  }

  // 批量出库成功
  function handleBatchOutbound() {
    updateListNumber()
    crud.toQuery()
  }

  // 批量调拨
  function toBatchTransfer() {
    batchTransferHandlingVisible.value = true
  }

  // 批量调拨成功
  function handleTransferOutbound() {
    crud.toQuery()
  }

  // 打开冻结记录
  function openFreezeRecords() {
    router.push({ name: 'WMSRawMatFreezeList' })
  }

  // 更新出库清单
  function updateListNumber() {
    currentUserOutboundListRef.value && currentUserOutboundListRef.value.updateListNumber()
  }

  return {
    crud,
    query,
    permission,
    currentUserOutboundListRef,
    batchOutboundHandlingVisible,
    batchTransferHandlingVisible,
    updateListNumber,
    toOutboundRecord,
    toBatchOutbound,
    handleBatchOutbound,
    toBatchTransfer,
    handleTransferOutbound,
    openFreezeRecords
  }
}
