<template>
  <common-drawer
    customClass="collection-record-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="`收款记录 ${props.detailRow.project}`"
    :wrapper-closable="true"
    size="70%"
  >
    <template #titleRight>
      <el-tag effect="plain" type="warning" size="medium">收款总额：{{ props.detailRow.collectionAmount }}</el-tag>
      <div class="print-wrap">
        <print-table
          v-permission="permission.printDetail"
          api-key="projectCollectionRecord"
          :params="{ projectId: props.detailRow.id, secondStartDate: props.secondPickerTime.startDate, secondEndDate: props.secondPickerTime.endDate }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <template #content>
      <common-table :data="list" :data-format="columnsDataFormat" :max-height="maxHeight">
        <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
        <el-table-column key="collectionDate" prop="collectionDate" label="收款日期" show-overflow-tooltip align="center" />
        <el-table-column key="collectionAmount" prop="collectionAmount" label="收款金额" show-overflow-tooltip align="right" />
        <el-table-column key="collectionReason" prop="collectionReason" label="收款事由" show-overflow-tooltip align="center">
          <template #default="{ row }">
            <span>{{ dict.label?.['payment_reason']?.[row.collectionReason] }}</span>
          </template>
        </el-table-column>
        <el-table-column key="collectionMode" prop="collectionMode" label="收款方式" show-overflow-tooltip align="center" />
        <el-table-column key="collectionUnit" prop="collectionUnit" label="收款单位" show-overflow-tooltip align="center" />
        <el-table-column key="collectionDepositBank" prop="collectionDepositBank" label="收款银行" show-overflow-tooltip align="center" />
        <el-table-column key="paymentUnit" prop="paymentUnit" label="付款单位" show-overflow-tooltip align="center" />
        <el-table-column key="collectionBankAccount" prop="collectionBankAccount" label="银行卡号" show-overflow-tooltip align="center" />
        <el-table-column key="noteMark" prop="noteMark" label="备注" show-overflow-tooltip align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getCollectionList } from '@/api/contract/fortune-report/fortune-report'
import { ref, defineEmits, defineProps, watch, inject } from 'vue'

import { paymentFineModeEnum } from '@enum-ms/finance'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useDict from '@compos/store/use-dict'

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

const dict = useDict(['payment_reason'])

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
  ['collectionAmount', 'to-thousand'],
  ['collectionDate', ['parse-time', '{y}-{m}-{d}']],
  ['collectionMode', ['parse-enum', paymentFineModeEnum]]
]

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.collection-record-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body']
  },
  visible
)

// 获取列表
async function getList() {
  try {
    listLoading.value = true
    list.value = (await getCollectionList({ projectId: props.detailRow.id, secondStartDate: props.secondPickerTime.startDate, secondEndDate: props.secondPickerTime.endDate })) || []
  } catch (error) {
    console.log('获取收款记录失败', error)
  } finally {
    listLoading.value = false
  }
}
</script>
