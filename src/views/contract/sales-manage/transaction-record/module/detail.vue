<template>
  <common-drawer
    ref="drawerRef"
    :title="info.name"
    v-model="drawerVisible"
    direction="rtl"
    :close-on-click-modal="false"
    :before-close="handleClose"
    size="60%"
  >
    <template #titleAfter>
      <el-tag v-if="info.startDate" effect="plain" size="medium">
        <span>统计日期：</span>
        <span v-parse-time="{ val: info.startDate, fmt: '{y}-{m}' }" /> ~
        <span v-parse-time="{ val: info.endDate, fmt: '{y}-{m}' }" />
      </el-tag>
      <el-tag type="success" effect="plain" size="medium">
        <span>累计合同额：</span>
        <span v-thousand="info.totalContractAmount" v-empty-text />
      </el-tag>
      <el-tag type="success" effect="plain" size="medium">
        <span>累计结算额：</span>
        <span v-thousand="info.totalSettlementAmount" v-empty-text />
      </el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="permission.printDetail"
          api-key="transactionRecord"
          :params="info"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :data="list"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="project.shortName" prop="project.shortName" :show-overflow-tooltip="true" label="项目"  min-width="250" >
          <template #default="{ row }">
            <span class="project-name">{{ projectNameFormatter(row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column key="contractAmount" prop="contractAmount" align="center" min-width="120" label="合同额">
          <template #default="{ row }">
            <span v-thousand="row.contractAmount" v-empty-text />
          </template>
        </el-table-column>
        <el-table-column key="settlementAmount" prop="settlementAmount" align="center" min-width="120" label="结算额">
          <template #default="{ row }">
            <span v-thousand="row.settlementAmount" v-empty-text />
          </template>
        </el-table-column>
        <el-table-column key="signingDate" prop="signingDate" label="签订日期" align="center" width="100">
          <template #default="{ row }">
            <span v-parse-time="{ val: row.signingDate, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
        <el-table-column key="signerName" prop="signerName" align="center" width="120" label="业务负责人">
          <template #default="{ row }">
            <span v-empty-text>{{ row.signerName }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/contract/sales-manage/transaction-record'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import { projectNameFormatter } from '@/utils/project'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

const tableLoading = ref(false)
const list = ref([])
const permission = inject('permission')

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content } = await detail(props.info)
    _list = content
  } catch (error) {
    console.log('获取详情列表失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
