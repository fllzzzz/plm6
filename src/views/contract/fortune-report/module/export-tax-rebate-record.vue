<template>
  <common-drawer
    customClass="export-record-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="`收款记录 ${props.detailRow.project}`"
    :wrapper-closable="true"
    size="70%"
  >
    <template #titleRight>
      <el-tag effect="plain" type="warning" size="medium">出口退税：{{ props.detailRow.exportTaxRebate }}</el-tag>
      <div class="print-wrap">
        <print-table v-permission="permission.printDetail" api-key="exportTaxRebateList" :params="params" size="mini" type="warning" />
      </div>
    </template>
    <template #content>
      <common-table :data="list" v-loading="listLoading" :data-format="columnsDataFormat" :max-height="maxHeight">
        <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
        <el-table-column key="drawbackDate" prop="drawbackDate" label="退税日期" show-overflow-tooltip align="center" />
        <el-table-column key="sendAmount" prop="sendAmount" label="发货额" show-overflow-tooltip align="right" />
        <el-table-column key="drawbackAmount" prop="drawbackAmount" label="退税总额" show-overflow-tooltip align="right" />
        <el-table-column key="accountantName" prop="accountantName" label="核算人" show-overflow-tooltip align="center" />
        <el-table-column key="agentName" prop="agentName" label="办理人" show-overflow-tooltip align="center" />
        <el-table-column key="auditorName" prop="auditorName" label="审核人" show-overflow-tooltip align="center" />
        <el-table-column key="remark" prop="remark" label="备注" show-overflow-tooltip align="left" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { get } from '@/api/contract/export-tax-rebate'
import { ref, computed, defineEmits, defineProps, watch, inject } from 'vue'

import { auditTypeEnum } from '@enum-ms/contract'

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

const params = computed(() => {
  return {
    projectId: props.detailRow.id,
    auditStatus: auditTypeEnum.PASS.V,
    secondStartDate: props.secondPickerTime.startDate,
    secondEndDate: props.secondPickerTime.endDate
  }
})

watch(
  () => visible.value,
  (val) => {
    if (val) {
      getList()
    }
  }
)

// 列格式转换
const columnsDataFormat = ref([
  ['sendAmount', 'to-thousand'],
  ['drawbackAmount', 'to-thousand'],
  ['drawbackDate', ['parse-time', '{y}-{m}-{d}']]
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.export-record-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body']
  },
  visible
)

// 获取列表
async function getList() {
  try {
    listLoading.value = true
    list.value = (await get({ ...params.value })) || []
  } catch (error) {
    console.log('获取出口退税失败', error)
  } finally {
    listLoading.value = false
  }
}
</script>
