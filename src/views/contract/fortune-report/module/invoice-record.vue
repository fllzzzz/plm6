<template>
  <common-drawer
    customClass="invoice-record-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="`开票记录 ${props.detailRow.project}`"
    :wrapper-closable="true"
    size="70%"
  >
    <template #titleRight>
      <el-tag effect="plain" type="warning" size="medium">开票总额：{{ props.detailRow.invoiceAmount }}</el-tag>
      <div class="print-wrap">
        <print-table
          v-permission="permission.printDetail"
          api-key="projectInvoiceRecord"
          :params="{ projectId: props.detailRow.id }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <template #content>
      <common-table :data="list" :data-format="columnsDataFormat" :max-height="maxHeight">
        <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
        <el-table-column key="invoiceDate" prop="invoiceDate" label="开票日期" show-overflow-tooltip align="center" />
        <el-table-column key="invoiceAmount" prop="invoiceAmount" label="开票金额" show-overflow-tooltip align="right" />
        <el-table-column key="invoiceType" prop="invoiceType" label="发票类型" show-overflow-tooltip align="center" />
        <el-table-column key="taxRate" prop="taxRate" label="税率" show-overflow-tooltip align="center">
          <template #default="{ row: { sourceRow: row } }">
            <div v-if="row.invoiceType !== invoiceTypeEnum.RECEIPT.V">{{ !row.boolIncludeTax && isNotBlank(row.boolIncludeTax) && row.invoiceType?'免税':(isNotBlank(row.taxRate)? row.taxRate+'%': '-') }}</div>
            <div v-else>-</div>
          </template>
        </el-table-column>
        <el-table-column key="invoiceUnit" prop="invoiceUnit" label="开票单位" show-overflow-tooltip align="center" />
        <el-table-column key="collectionUnit" prop="collectionUnit" label="收票单位" show-overflow-tooltip align="center" />
        <el-table-column key="invoiceNo" prop="invoiceNo" label="发票编号" show-overflow-tooltip align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getInvoiceList } from '@/api/contract/fortune-report/fortune-report'
import { ref, defineEmits, defineProps, watch, inject } from 'vue'

import { isNotBlank } from '@/utils/data-type'
import { invoiceTypeEnum } from '@enum-ms/finance'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailRow: {
    type: Object,
    default: () => {}
  },
  secondPickerTime: {
    type: Object,
    default: () => {}
  }
})

const list = ref([])
const listLoading = ref(false)

const emit = defineEmits(['update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const permission = inject('permission')

watch(
  () => visible.value,
  (val) => {
    if (val) {
      getList()
    }
  }
)

// 列格式转换
const columnsDataFormat = [
  ['invoiceAmount', 'to-thousand'],
  // ['taxRate', ['suffix', '%']],
  ['invoiceDate', ['parse-time', '{y}-{m}-{d}']],
  ['invoiceType', ['parse-enum', invoiceTypeEnum]]
]

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.invoice-record-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body']
  },
  visible
)

// 获取列表
async function getList() {
  try {
    listLoading.value = true
    list.value = (await getInvoiceList({ projectId: props.detailRow.id, secondStartDate: props.secondPickerTime.startDate, secondEndDate: props.secondPickerTime.endDate })) || []
  } catch (error) {
    console.log('获取开票记录失败', error)
  } finally {
    listLoading.value = false
  }
}
</script>
