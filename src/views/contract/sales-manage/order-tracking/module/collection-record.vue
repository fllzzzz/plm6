<template>
  <common-dialog
    ref="dialogRef"
    title="收款记录"
    append-to-body
    :visible="visible"
    width="1300px"
    :close-on-click-modal="false"
    :before-close="handleClose"
    custom-class="collection-record"
    show-close
    top="10vh"
  >
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="props.permission?.print"
          api-key="projectCollectionDetail"
          :params="{ ...params }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="collectionDate" label="收款日期" align="center" width="100" show-overflow-tooltip />
      <el-table-column prop="collectionAmount" label="收款额" align="center" min-width="120" show-overflow-tooltip />
      <el-table-column prop="collectionReason" label="收款事由" align="center" width="100" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ dict.label?.['payment_reason']?.[row.collectionReason] }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="collectionMode" label="收款方式" align="center" width="100" show-overflow-tooltip />
      <el-table-column prop="collectionUnit" label="收款单位" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column prop="collectionDepositBank" label="收款开户行" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column prop="paymentUnit" label="付款单位" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column prop="writtenByName" label="办理人" align="center" min-width="100" show-overflow-tooltip />
      <el-table-column prop="auditorName" label="审核人" align="center" min-width="100" show-overflow-tooltip />
    </common-table>
    <!--分页组件-->
    <el-pagination
      :total="total"
      :current-page="queryPage.pageNumber"
      :page-size="queryPage.pageSize"
      style="margin-top: 8px"
      layout="total, prev, pager, next, sizes"
      @size-change="handleSizeChange"
      @current-change="handleCurrentChange"
    />
  </common-dialog>
</template>

<script setup>
import { collectionRecord } from '@/api/contract/sales-manage/order-tracking'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { paymentFineModeEnum } from '@enum-ms/finance'
import { auditTypeEnum } from '@enum-ms/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import useDict from '@compos/store/use-dict'

const { decimalPrecision } = useDecimalPrecision()

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const dict = useDict(['payment_reason'])
const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 请求参数
const params = computed(() => {
  return {
    projectId: props.detailInfo.project?.id,
    auditStatus: auditTypeEnum.PASS.V
  }
})

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const list = ref([])
const dialogRef = ref()
const tableLoading = ref(false)
const dataFormat = computed(() => {
  return [
    ['collectionDate', ['parse-time', '{y}-{m}-{d}']],
    ['collectionMode', ['parse-enum', paymentFineModeEnum]],
    ['collectionAmount', ['to-thousand', decimalPrecision.value.contract]],
    ['taxRate', ['to-fixed', 2]]
  ]
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.collection-record',
    extraBox: '.el-dialog__header',
    wrapperBox: '.el-dialog__body',
    extraHeight: '5vh',
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  dialogRef
)

// 获取收款记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await collectionRecord({ ...params.value, ...queryPage })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取收款记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
