<template>
  <div class="app-container">
    <div class="head-container">
      <div style="float: left">
        <el-date-picker
          v-model="year"
          type="year"
          size="small"
          style="width: 100px"
          placeholder="选择年"
          class="filter-item"
          format="YYYY"
          value-format="YYYY"
          clearable
          :disabled-date="disabledDate"
        />
        <project-cascader v-model="projectId" class="filter-item" />
      </div>
      <div style="float: right">
        <export-button class="filter-item"> 检测费清单 </export-button>
      </div>
    </div>
    <common-table
      ref="tableRef"
      :data="testingList"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column type="index" label="序号" prop="indx" align="center" />
      <el-table-column label="项目" prop="projectName" align="center" min-width="160px" />
      <el-table-column label="产量（吨）" prop="production" align="center" />
      <el-table-column label="原材料复检" prop="rawMaterialReinspection" align="center" />
      <el-table-column label="焊接试板" prop="weldTestPlates" align="center" />
      <el-table-column label="抗滑移试验" prop="antiSlipTest" align="center" />
      <el-table-column label="试件加工" prop="specimenProcessing" align="center" />
      <el-table-column label="焊缝探伤" prop="weldFlawDetection" align="center" />
      <el-table-column label="合计" prop="totalAmount" align="center" />
      <el-table-column label="平均检测费（元/吨）" prop="averageTestingFee" align="center" />
    </common-table>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
// import crudApi from ''
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import { tableSummary } from '@/utils/el-extra'
import projectCascader from '@comp-base/project-cascader'
import ExportButton from '@comp-common/export-button/index.vue'

const year = ref(parseTime(new Date(), '{y}'))
const projectId = ref()
// const testingFeeList = ref([])

const tableRef = ref()

// onMounted(() => {
//   fetchTestingFee()
// })
// async function fetchTestingFee() {
//   const { content } = await getTestingFee({
//     year: year.value,
//     projectId: projectId.value
//   })
//   testingFeeList.value = content || []
// }

function disabledDate(time) {
  return time > new Date()
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [''],
    toThousandFields: ['']
  })
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>
<style lang="scss" scoped>
</style>
