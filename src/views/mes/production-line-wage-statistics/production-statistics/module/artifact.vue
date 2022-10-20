<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    :before-close="handleClose"
    :title="`工序: ${productionData.team}`"
    :wrapper-closable="false"
    size="60%"
  >
    <template #content>
      <div style="display: flex; justify-content: space-between; margin-bottom: 8px;">
        <el-tag style="align-self: center; font-weight: 700">班组：{{ productionData.team }}</el-tag>
        <print-table
          :api-key="apiKey"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
      <common-table
        ref="tableRef"
        :data="tableData"
        :max-height="maxHeight"
        style="width: 100%"
        return-source-data
        :showEmptySymbol="false"
        show-summary
        :summary-method="getSummaries"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column key="name" prop="name" align="center" :show-overflow-tooltip="true" label="项目" min-width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.projectNumber }}-{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" align="center" :show-overflow-tooltip="true" label="编号">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification"  align="center" :show-overflow-tooltip="true" label="规格">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="interfaceType" prop="interfaceType" align="center" :show-overflow-tooltip="true" label="截面类型">
          <template v-slot="scope">
            <span>{{ scope.row.interfaceType }}</span>
          </template>
        </el-table-column>
        <el-table-column key="nuclear" prop="nuclear" align="center" :show-overflow-tooltip="true" label="核算单位">
          <template v-slot="scope">
            <span>{{ scope.row.nuclear }}</span>
          </template>
        </el-table-column>
        <el-table-column key="totalMete" prop="totalMete" align="center" :show-overflow-tooltip="true" label="生产量">
          <template v-slot="scope">
            <span>{{ scope.row.totalMete }}</span>
          </template>
        </el-table-column>
        <el-table-column key="unitPrice" prop="unitPrice" align="center" :show-overflow-tooltip="true" label="单价">
          <template v-slot="scope">
            <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice) : 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column key="totalPrice" prop="totalPrice" align="center" :show-overflow-tooltip="true" label="总额（元）">
          <template v-slot="scope">
            <span>{{ scope.row.totalPrice ? toThousand(scope.row.totalPrice) : 0 }}</span>
          </template>
        </el-table-column>
        <el-table-column key="productionDate" prop="productionDate" align="center" :show-overflow-tooltip="true" label="生产日期">
          <template v-slot="scope">
            <span>{{ scope.row.productionDate ? parseTime(scope.row.productionDate,'{y}/{m}/{d}'): '-' }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, watch } from 'vue'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import { tableSummary } from '@/utils/el-extra'
import { occurLog } from '@/api/contract/contract-ledger'
import { toThousand } from '@data-type/number'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  projectId: {
    type: [String, Number],
    default: undefined
  },
  productionData: {
    type: Object,
    default: () => {}
  }
})

const tableData = [
  { projectNumber: '11111', projectName: '项目', serialNumber: 'GL-3', specification: 'BH400*200*6*9', interfaceType: '>400<500', nuclear: '米', totalMete: 200, unitPrice: 10, totalPrice: '200', productionDate: 1654300000 }
]
// const tableData = ref([])
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const { maxHeight } = useMaxHeight({
  wrapperBox: '.occurAmountLog',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      getOccurLog()
    } else {
      tableData.value = []
    }
  },
  { deep: true, immediate: true }
)

async function getOccurLog() {
  try {
    const { content } = await occurLog({ projectId: props.projectId })
    tableData.value = content || []
  } catch (e) {
    console.log('获取变更金额记录', e)
  }
}
function getSummaries(param) {
  return tableSummary(param, {
    props: ['totalMete', 'totalPrice']
  })
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
