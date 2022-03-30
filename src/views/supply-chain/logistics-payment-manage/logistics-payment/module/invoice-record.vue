<template>
  <common-drawer
    ref="dialogRef"
    title="开票记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #titleRight>
      <div class="print-wrap">
        <!-- <print-table
          v-permission="props.permission?.print"
          api-key="projectInvoiceDetail"
          :params="{ ...params }"
          size="mini"
          type="warning"
        /> -->
      </div>
    </template>
    <template #content>
      <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="invoiceDate" label="开票日期" align="center" width="100" show-overflow-tooltip />
        <el-table-column prop="invoiceAmount" label="开票额" align="center" min-width="120" show-overflow-tooltip />
        <el-table-column prop="invoiceType" label="开票类型" align="center" width="110" show-overflow-tooltip />
        <el-table-column prop="taxRate" label="税率" align="center" width="70" show-overflow-tooltip>
          <template #default="{ row }">
            <span>{{ row.taxRate }}%</span>
          </template>
        </el-table-column>
        <el-table-column prop="invoiceUnit" label="购方单位" align="center" min-width="140" show-overflow-tooltip />
        <el-table-column prop="collectionUnit" label="销售单位" align="center" min-width="140" show-overflow-tooltip />
        <el-table-column prop="invoiceNo" label="发票编号" align="center" min-width="100" show-overflow-tooltip />
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
    </template>
  </common-drawer>
</template>

<script setup>
import { invoiceRecord } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { invoiceTypeEnum } from '@enum-ms/contract'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

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

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 请求参数
const params = computed(() => {
  return {
    orderId: props.detailInfo.id,
    propertyType: props.detailInfo.propertyType
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
const dataFormat = ref([
  ['invoiceType', ['parse-enum', invoiceTypeEnum]],
  ['invoiceDate', ['parse-time', '{y}-{m}-{d}']],
  ['invoiceAmount', 'to-thousand'],
  ['taxRate', ['to-fixed', 2]]
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.invoice-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    extraHeight: '5vh',
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  dialogRef
)

// 获取开票记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await invoiceRecord({ ...params.value, ...queryPage })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取开票记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
