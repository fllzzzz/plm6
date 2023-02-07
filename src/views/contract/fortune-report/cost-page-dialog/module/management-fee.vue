<template>
  <div class="app-container">
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div style="width: 300px">
        <print-table
          v-permission="permission.print"
          api-key="managementFee"
          :params="{ projectId: props.costTypeData.projectId }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
      <el-tag>合计（单位：元）：{{ toThousand(props.costTypeData?.amount) }}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      :data="detailData"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column prop="subject" key="subject" label="科目" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.subject }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="amount" key="amount" label="总额（元）" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.amount) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="totalProduction" key="totalProduction" label="项目摊销比" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.totalProduction }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="fees" key="fees" label="费用" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.fees) }}</span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>
<script setup>
// import { workOrderTypeEnum } from '@enum-ms/mes'
import { getManagementList } from '@/api/contract/fortune-report/detail-fee'
import { ref, defineProps, watch } from 'vue'
import { toThousand } from '@data-type/number'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  costTypeData: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const tableRef = ref()
const detailData = ref([])

const { maxHeight } = useMaxHeight({
  paginate: true
})

watch(
  () => props.costTypeData?.projectId,
  (val) => {
    fetchWaterElectricFee()
  },
  { immediate: true, deep: true }
)

async function fetchWaterElectricFee() {
  try {
    const { content } = await getManagementList({ projectId: props.costTypeData?.projectId })
    detailData.value = content || []
  } catch (error) {
    console.log('获取管理费失败', error)
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['amount', 'fees'],
    toThousandFields: ['amount', 'fees']
  })
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
